.PHONY: clean

3.pdf: 3.tex
	latexmk 3.tex
	dvipdf 3.dvi
	open 3.pdf

clean:
	latexmk -C
