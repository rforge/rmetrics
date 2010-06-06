SHELL = /bin/sh
techdoc = TechnicalDocument
userguide = UsingSchwartz97

# idea from http://ctan.unsw.edu.au/help/uk-tex-faq/Makefile
all : $(techdoc).dvi $(userguide).dvi
	$(TEXI2DVI) --pdf $(techdoc).tex
	$(TEXI2DVI) --pdf $(userguide).tex
	rm *0[01]?.pdf *0[01]?.eps *.bbl *.blg *.out *.aux *.dvi *.log Rplots.pdf

$(techdoc).dvi : 
	$(TEXI2DVI) $(techdoc).tex

$(userguide).dvi : 
	$(TEXI2DVI) $(userguide).tex

#clean:
#	rm *0[01]?.pdf *0[01]?.eps *.bbl *.blg *.out *.aux *.dvi *.log Rplots.pdf


#delete:
#	rm $(techdoc).pdf $(userguide).pdf $(techdoc).tex $(userguide).tex