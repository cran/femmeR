(TeX-add-style-hook "femmeR"
 (lambda ()
    (LaTeX-add-index-entries
     "#1"
     "read.st1"
     "plot.st1"
     "read.o1"
     "plot.o1"
     "read.o2"
     "plot.o2"
     "read.sns"
     "plot.sns"
     "read.crl"
     "plot.crl"
     "read.pcv"
     "plot.pcv"
     "summary.pcv"
     "read.bay"
     "plot.bay"
     "pairs.bay"
     "raftery.diag"
     "read.par"
     "write.par")
    (LaTeX-add-bibliographies
     "R")
    (LaTeX-add-labels
     "eq:msqr"
     "eq:mabs"
     "eq:mean"
     "eq:max"
     "eq:min"
     "sec:short-introduction-r"
     "sec:getting-help"
     "sec:data"
     "sec:subplots")
    (TeX-add-symbols
     '("fn" 1)
     '("Rcode" 1)
     '("femmecode" 1)
     "R"
     "Fortran")
    (TeX-run-style-hooks
     "hyperref"
     "colorlinks"
     "makeidx"
     "amsmath"
     "fancyhdr"
     "natbib"
     "color"
     "graphicx"
     "booktabs"
     "latex2e"
     "scrartcl10"
     "scrartcl")))

