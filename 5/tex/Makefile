.PHONY: clean

5.pdf: 5.tex 5.1.tex 5.2.tex 5.3.tex 5.4.tex 5.5.tex
	latexmk 5.tex
	dvipdf 5.dvi
	open 5.pdf

clean:
	latexmk -C
