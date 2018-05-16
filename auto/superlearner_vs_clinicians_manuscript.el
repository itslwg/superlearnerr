(TeX-add-style-hook
 "superlearner_vs_clinicians_manuscript"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "10pt" "letterpaper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("geometry" "top=0.85in") ("inputenc" "utf8x") ("lineno" "right") ("xcolor" "table")))
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
    "array")
   (LaTeX-add-bibitems
    "Brohi2017"
    "GBD2017"
    "UN2018"
    "Yeboah2014"
    "OReilly2013"
    "Roy2017"
    "EAST2010"
    "NICE2016"
    "Voskens2018"
    "Granstrom2018"
    "ASCOT2014"
    "Tignanelli2018"
    "Benjamin2018"
    "VanRein2018"
    "SATG2012"
    "ESI2012"
    "Baker2013"
    "Choi2017"
    "Beam2018"
    "Nevin2018"
    "Liu2017"
    "Talbert2007"
    "Pearl2008"
    "Scerbo2014"
    "Follin2016"
    "Levin2018"
    "WHOICD"
    "GCSAID"
    "Rehn2011"
    "R"
    "SuperLearner"))
 :latex)

