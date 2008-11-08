TEXI2DVI = texi2dvi --pdf
RM = rm -rf

all: randtoolbox.pdf clean

randtoolbox.pdf : randtoolbox.tex
	$(TEXI2DVI) randtoolbox.tex

clean:
	$(RM) Rplots*.eps Rplots*.pdf Rplots*.jpg randtoolbox-*.eps randtoolbox-*.pdf randtoolbox-*.jpg *.aux *.log *.blg *.bbl *.brf *.out *.tex *.rel *.toc *~ auto
