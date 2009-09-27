TEXI2DVI = R CMD texi2dvi --pdf
SWEAVE = R CMD Sweave
RM = rm -rf

all: randtoolbox.pdf clean

noclean: randtoolbox.pdf

randtoolbox.pdf : randtoolbox.Rnw src/shortintro.Rnw src/fullpres.Rnw
	$(SWEAVE) randtoolbox.Rnw
	$(TEXI2DVI) randtoolbox.tex

clean:
	$(RM) Rplots*.eps Rplots*.pdf Rplots*.jpg randtoolbox-*.eps randtoolbox-*.pdf randtoolbox-*.jpg *.aux *.log *.blg *.bbl *.brf *.out *.tex *.rel *.toc *~ auto

allclean: 
	$(RM) Rplots*.eps Rplots*.pdf Rplots*.jpg randtoolbox-*.eps randtoolbox-*.pdf randtoolbox-*.jpg *.aux *.log *.blg *.bbl *.brf *.out *.tex *.rel *.toc *~ auto randtoolbox.pdf
