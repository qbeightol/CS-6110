.PHONY: clean

6.pdf: 6.tex 6.1.tex 6.2.tex 6.3.tex 6.4.tex 6.5.tex
	latexmk 6.tex
	dvipdf 6.dvi
	open 6.pdf

clean:
	latexmk -C
