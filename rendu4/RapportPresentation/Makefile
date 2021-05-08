all: rapport presentation

rapport: rapport-fouine-jourde-nardino.tex biblio.bib
	xelatex $<
	bibtex rapport-fouine-jourde-nardino
	xelatex $<
	xelatex $<

presentation: presentation-fouine-jourde-nardino.tex
	pdflatex -shell-escape $<
