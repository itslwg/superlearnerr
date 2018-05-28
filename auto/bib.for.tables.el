(TeX-add-style-hook
 "bib.for.tables"
 (lambda ()
   (LaTeX-add-bibitems
    "gam"
    "xgboost"
    "randomforest"
    "glmnet"))
 :bibtex)

