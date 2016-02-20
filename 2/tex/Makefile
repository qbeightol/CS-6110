.PHONY: clean

2.pdf: 2.tex
	latexmk
	dvipdf 2.dvi
	open 2.pdf

clean:
	-rm 2.pdf
	-rm 2.aux
	-rm 2.dvi
	-rm 2.log
	-rm 2.fdb_latexmk
	-rm 2.fls
