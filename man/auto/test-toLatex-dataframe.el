(TeX-add-style-hook
 "test-toLatex-dataframe"
 (lambda ()
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "dcolumn"
    "booktabs"
    "libertine"))
 :latex)

