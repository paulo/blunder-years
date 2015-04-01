all:
	pdflatex reportpt1.tex
	pdflatex reportpt1.tex

clean:
	rm *.aux *.log *.out *.toc *.bbl *.blg *~ *xml *backup *blx*

