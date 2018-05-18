(TeX-add-style-hook
 "superlearner_vs_clinicians_manuscript_0.0.0.9000"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "10pt" "letterpaper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "top=0.85in") ("inputenc" "utf8x") ("lineno" "right") ("xcolor" "table")))
   (add-to-list 'LaTeX-verbatim-environments-local "alltt")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "graphicx"
    "color"
    "framed"
    "alltt"
    "geometry"
    "amsmath"
    "amssymb"
    "changepage"
    "inputenc"
    "textcomp"
    "marvosym"
    "cite"
    "nameref"
    "hyperref"
    "lineno"
    "microtype"
    "xcolor"
    "array"
    "caption"
    "adjustbox"
    "upquote")
   (TeX-add-symbols
    '("hlkwd" 1)
    '("hlkwc" 1)
    '("hlkwb" 1)
    '("hlkwa" 1)
    '("hlstd" 1)
    '("hlopt" 1)
    '("hlcom" 1)
    '("hlstr" 1)
    '("hlnum" 1)
    "maxwidth"
    "hlipl"
    "FrameCommand")
   (LaTeX-add-environments
    "kframe"
    "knitrout")
   (LaTeX-add-xcolor-definecolors
    "fgcolor"
    "shadecolor"
    "messagecolor"
    "warningcolor"
    "errorcolor"))
 :latex)

