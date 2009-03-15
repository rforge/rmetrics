TEXI2DVI = texi2dvi --pdf
SWEAVE = R CMD Sweave
RM = rm -rf

all: randtoolbox.pdf clean

randtoolbox.pdf : randtoolbox.Rnw
	$(SWEAVE) randtoolbox.Rnw
	$(TEXI2DVI) randtoolbox.tex

clean:
	$(RM) Rplots*.eps Rplots*.pdf Rplots*.jpg randtoolbox-*.eps randtoolbox-*.pdf randtoolbox-*.jpg *.aux *.log *.blg *.bbl *.brf *.out *.tex *.rel *.toc *~ auto
