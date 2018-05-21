(TeX-add-style-hook
 "superlearner_vs_clinicians_manuscript_0.0.0.9000"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "10pt" "letterpaper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "top=0.85in") ("inputenc" "utf8x") ("lineno" "right") ("xcolor" "table")))
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
    "adjustbox")
   (LaTeX-add-labels
    "fig:roc_plot"
    "fig:calibration_plot"
    "fig:mortality_plot"))
 :latex)

