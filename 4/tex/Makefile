.PHONY: clean

4.pdf: 4.tex 4.1.tex 4.2.tex 4.3.tex 4.4.tex
	latexmk 4.tex
	dvipdf 4.dvi
	open 4.pdf

clean:
	latexmk -C
