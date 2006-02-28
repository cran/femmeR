# Makefile for Sweave files
#--------------------------
# Change the FILE statement below to the basename of your FILE

FILE = femmeR

# run like this
# 'make pdf'  to create a pdf
# 'make R'    to produce the R code without LaTeX



pdf: $(FILE).tex
	texi2dvi -p $(FILE).tex

$(FILE).tex: $(FILE).Rnw
	echo "Sweave('$(FILE).Rnw')"|R --vanilla --slave

R: $(FILE).Rnw
	echo "Stangle('$(FILE).Rnw')"|R --vanilla --slave

clean:
	rm $(FILE).aux $(FILE).out $(FILE).toc $(FILE).ilg $(FILE).ind $(FILE).idx $(FILE).log 