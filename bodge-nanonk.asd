(asdf:defsystem :bodge-nanonk
  :description "NanoVG renderer for Nuklear"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (alexandria cffi static-vectors claw
                          nanovg-blob bodge-nanovg nuklear-blob bodge-nuklear)
  :components ((:file "renderer")))
