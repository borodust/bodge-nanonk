(cl:defpackage :nanonk
  (:nicknames :nnk)
  (:use :cl)
  (:export #:make-renderer
           #:destroy-renderer
           #:renderer-font
           #:render-nuklear))
(cl:in-package :nanonk)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-font (path)
    (let ((bytes (alexandria:read-file-into-byte-vector path)))
      (static-vectors:make-static-vector (length bytes) :initial-contents bytes))))


(defvar *default-font*
  (read-font (asdf:system-relative-pathname :bodge-nanonk "font/NotoMono-Regular.ttf")))


(cffi:defcstruct font-user-data
  (context :pointer)
  (font-id :int))


(defstruct nnk-renderer
  context
  font-id
  font-height
  font-ascender
  nk-font)


(defun renderer-font (renderer)
  (nnk-renderer-nk-font renderer))


(claw:defcallback default-font-width-callback :float ((font-handle :pointer)
                                                      (font-height :float)
                                                      (text-ptr :pointer)
                                                      (len :int))
  (declare (ignore font-height))
  (claw:c-val ((font-handle (:struct (%nk:user-font))))
    (cffi:with-foreign-slots ((context font-id) (claw:ptr (font-handle :userdata))
                              (:struct font-user-data))
      (%nvg:font-face-id context font-id)
      (%nvg:text-bounds context 0f0 0f0 text-ptr (cffi:inc-pointer text-ptr len) nil))))


(defun make-renderer (&key (font-height 16f0))
  "Requires bound OpenGL 3 Core context with stencil buffer."
  (let* ((nvg-context (nvg:make-context :antialias :stencil-strokes))
         (nvg-font-id (%nvg:create-font-mem nvg-context "default-font"
                                        (static-vectors:static-vector-pointer *default-font*)
                                        (length *default-font*)
                                        0))
         (font-ascender (claw:c-with ((ascender :float))
                          (%nvg:text-metrics nvg-context (ascender &) nil nil)
                          ascender))
         (font-user-data (cffi:foreign-alloc '(:struct font-user-data)))
         (nk-font (nk:make-user-font font-height
                                     'default-font-width-callback
                                     font-user-data)))
    (cffi:with-foreign-slots ((context font-id) font-user-data (:struct font-user-data))
      (setf context (claw:ptr nvg-context)
            font-id nvg-font-id))
    (%nvg:add-fallback-font-id nvg-context nvg-font-id nvg-font-id)
    (make-nnk-renderer :context nvg-context
                       :font-id nvg-font-id
                       :font-height (f font-height)
                       :font-ascender font-ascender
                       :nk-font nk-font)))


(defun destroy-renderer (renderer)
  (nvg:destroy-context (nnk-renderer-context renderer)))


(defmacro as-command ((cmd-var type) &body body)
  `(claw:c-val ((,cmd-var (:struct (,type))))
     ,@body))


(declaim (inline f))
(defun f (value)
  (float value 0f0))


(declaim (inline clamp))
(defun clamp (value)
  (min (max (/ value 255f0) 0f0) 1f0))


(defmacro with-nvg-color ((var nk-color) &body body)
  (let ((color (gensym)))
    `(claw:c-let ((,color (:struct (%nk:color)) :from ,nk-color))
       (claw:c-with ((,var %nvg:color))
         (setf (,var :r) (clamp (,color :r))
               (,var :g) (clamp (,color :g))
               (,var :b) (clamp (,color :b))
               (,var :a) (clamp (,color :a)))
         ,@body))))


(defun stroke-path (renderer nk-color line-width)
  (let ((ctx (nnk-renderer-context renderer)))
    (%nvg:stroke-width ctx (f line-width))
    (with-nvg-color (color-v nk-color)
      (%nvg:stroke-color ctx color-v))
    (%nvg:stroke ctx)))


(defun fill-path (renderer nk-color)
  (let ((ctx (nnk-renderer-context renderer)))
    (with-nvg-color (color-v nk-color)
      (%nvg:fill-color ctx color-v))
    (%nvg:fill ctx)))


(defun render-scissor (cmd renderer)
  (as-command (cmd %nk:command-scissor)
    (%nvg:scissor (nnk-renderer-context renderer)
                  (f (cmd :x)) (f (cmd :y)) (f (cmd :w)) (f (cmd :h)))))


(defun render-line (cmd renderer)
  (as-command (cmd %nk:command-line)
    (%nvg:move-to (nnk-renderer-context renderer) (f (cmd :begin :x)) (f (cmd :begin :y)))
    (%nvg:line-to (nnk-renderer-context renderer) (f (cmd :end :x)) (f (cmd :end :y)))
    (stroke-path renderer (cmd :color) (cmd :line-thickness))))


(defun render-curve (cmd renderer)
  (let ((ctx (nnk-renderer-context renderer)))
    (as-command (cmd %nk:command-curve)
      (%nvg:move-to ctx (cmd :begin :x) (cmd :begin :y))
      (%nvg:bezier-to ctx
                      (cmd :ctrl 0 :x) (cmd :ctrl 0 :y)
                      (cmd :ctrl 1 :x) (cmd :ctrl 1 :y)
                      (cmd :end :x) (cmd :end :y))
      (stroke-path renderer (cmd :color) (cmd :line-thickness)))))


(defun draw-rounded-rect (renderer x y w h r)
  (%nvg:rounded-rect (nnk-renderer-context renderer) (f x) (f y) (f w) (f h) (f r)))


(defun render-rect (cmd renderer)
  (as-command (cmd %nk:command-rect)
    (draw-rounded-rect renderer (cmd :x) (cmd :y) (cmd :w) (cmd :h) (cmd :rounding))
    (stroke-path renderer (cmd :color) (cmd :line-thickness))))


(defun render-rect-filled (cmd renderer)
  (as-command (cmd %nk:command-rect-filled)
    (draw-rounded-rect renderer (cmd :x) (cmd :y) (cmd :w) (cmd :h) (cmd :rounding))
    (fill-path renderer (cmd :color))))


(defun draw-ellipse (renderer x y w h)
  (let ((rx (f (/ w 2f0)))
        (ry (f (/ h 2f0))))
    (%nvg:ellipse (nnk-renderer-context renderer) (f (+ x rx)) (f (+ y ry)) rx ry)))


(defun render-circle (cmd renderer)
  (as-command (cmd %nk:command-circle)
    (draw-ellipse renderer (cmd :x) (cmd :y) (cmd :w) (cmd :h))
    (stroke-path renderer (cmd :color) (cmd :line-thickness))))


(defun render-circle-filled (cmd renderer)
  (as-command (cmd %nk:command-circle-filled)
    (draw-ellipse renderer (cmd :x) (cmd :y) (cmd :w) (cmd :h))
    (fill-path renderer (cmd :color))))


(defun draw-arc (renderer cx cy r a0 a1)
  (%nvg:arc (nnk-renderer-context renderer)
            (f cx) (f cy) (f r) (f a0) (f a1) %nvg:+cw+))


(defun render-arc (cmd renderer)
  (let ((ctx (nnk-renderer-context renderer)))
    (as-command (cmd %nk:command-arc)
      (draw-arc ctx (cmd :cx) (cmd :cy) (cmd :r) (cmd :a 0) (cmd :a 1))
      (stroke-path renderer (cmd :color) (cmd :line-thickness)))))


(defun render-arc-filled (cmd renderer)
  (let ((ctx (nnk-renderer-context renderer)))
    (as-command (cmd %nk:command-arc-filled)
      (draw-arc ctx (cmd :cx) (cmd :cy) (cmd :r) (cmd :a 0) (cmd :a 1))
      (fill-path renderer (cmd :color)))))


(defun draw-polyline (renderer head-x head-y &rest rest-coordinates)
  (let ((ctx (nnk-renderer-context renderer)))
    (%nvg:move-to ctx head-x head-y)
    (loop for (x y) on rest-coordinates by #'cddr
          do (%nvg:line-to ctx x y))))


(defun render-triangle (cmd renderer)
  (as-command (cmd %nk:command-triangle)
    (draw-polyline renderer
                   (f (cmd :a :x)) (f (cmd :a :y))
                   (f (cmd :b :x)) (f (cmd :b :y))
                   (f (cmd :c :x)) (f (cmd :c :y)))
    (stroke-path renderer (cmd :color) (cmd :line-thickness))))


(defun render-triangle-filled (cmd renderer)
  (as-command (cmd %nk:command-triangle-filled)
    (draw-polyline renderer
                   (f (cmd :a :x)) (f (cmd :a :y))
                   (f (cmd :b :x)) (f (cmd :b :y))
                   (f (cmd :c :x)) (f (cmd :c :y)))
    (fill-path renderer (cmd :color))))


(defun draw-polygon (renderer count x-supplier y-supplier)
  (let ((vertices (loop for i from 0 below count
                        collect (f (funcall x-supplier i))
                        collect (f (funcall y-supplier i)))))
    (unless (null vertices)
      (apply #'draw-polyline renderer vertices)
      (%nvg:line-to (nnk-renderer-context renderer)
                    (first vertices) (second vertices)))))


(defun render-polygon (cmd renderer)
  (as-command (cmd %nk:command-polygon)
    (draw-polygon renderer (cmd :point-count)
                  (lambda (idx) (cmd :points idx :x))
                  (lambda (idx) (cmd :points idx :y)))
    (stroke-path renderer (cmd :color) (cmd :line-thickness))))


(defun render-polygon-filled (cmd renderer)
  (as-command (cmd %nk:command-polygon-filled)
    (draw-polygon renderer (cmd :point-count)
                  (lambda (idx) (cmd :points idx :x))
                  (lambda (idx) (cmd :points idx :y)))
    (fill-path renderer (cmd :color))))


(defun render-polyline (cmd renderer)
  (as-command (cmd %nk:command-polyline)
    (let ((vertices (loop for i from 0 below (cmd :point-count)
                          collect (f (cmd :points i :x))
                          collect (f (cmd :points i :y)))))
      (unless (null vertices)
        (apply #'draw-polyline renderer vertices)))
    (stroke-path renderer (cmd :color) (cmd :line-thickness))))


(defun render-text (cmd renderer)
  (as-command (cmd %nk:command-text)
    (with-nvg-color (color-v (cmd :foreground))
      (%nvg:fill-color (nnk-renderer-context renderer) color-v))
    (%nvg:text (nnk-renderer-context renderer)
               (f (cmd :x)) (f (+ (cmd :y) (nnk-renderer-font-ascender renderer)))
               (cmd :string &) (cffi:inc-pointer (cmd :string &) (cmd :length)))))


(defun render-image (cmd renderer)
  (declare (ignore cmd renderer)))


(defun render-rect-multi-color (cmd renderer)
  (declare (ignore cmd renderer)))


(defun render-nuklear (renderer nk-context width height &optional (pixel-ratio 1f0))
  (%nvg:begin-frame (nnk-renderer-context renderer) width height pixel-ratio)
  (%nvg:font-face-id (nnk-renderer-context renderer) (nnk-renderer-font-id renderer))
  (%nvg:font-size (nnk-renderer-context renderer) (nnk-renderer-font-height renderer))

  (nk:docommands (cmd nk-context)
    (%nvg:begin-path (nnk-renderer-context renderer))
    (case (nk:command-type cmd)
      (:nop)
      (:scissor (render-scissor cmd renderer))
      (:line (render-line cmd renderer))
      (:curve (render-curve cmd renderer))
      (:rect (render-rect cmd renderer))
      (:rect-filled (render-rect-filled cmd renderer))
      (:rect-multi-color (render-rect-multi-color cmd renderer))
      (:circle (render-circle cmd renderer))
      (:circle-filled (render-circle-filled cmd renderer))
      (:arc (render-arc cmd renderer))
      (:arc-filled (render-arc-filled cmd renderer))
      (:triangle (render-triangle cmd renderer))
      (:triangle-filled (render-triangle-filled cmd renderer))
      (:polygon (render-polygon cmd renderer))
      (:polygon-filled (render-polygon-filled cmd renderer))
      (:polyline (render-polyline cmd renderer))
      (:text (render-text cmd renderer))
      (:image (render-image cmd renderer))))

  (%nvg:end-frame (nnk-renderer-context renderer)))
