(TeX-add-style-hook
 "plos_superlearner_vs_clinicians_manuscript_1.0.3"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "10pt" "letterpaper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "top=0.85in" "left=2.75in" "footskip=0.75in") ("inputenc" "utf8x") ("lineno" "right") ("xcolor" "table") ("caption" "aboveskip=1pt" "labelfont=bf" "labelsep=period" "justification=raggedright" "singlelinecheck=off")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
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
    "lastpage"
    "fancyhdr"
    "graphicx"
    "epstopdf"
    "adjustbox")
   (TeX-add-symbols
    '("thickcline" 1)
    "thickhline"
    "lorem"
    "ipsum")
   (LaTeX-add-labels
    "tab:superlearner-library"
    "fig:roc_plot"
    "fig:mortality_plot")
   (LaTeX-add-bibliographies
    "bibliography")
   (LaTeX-add-lengths
    "savedwidth")
   (LaTeX-add-array-newcolumntypes
    "+"))
 :latex)

