#!/bin/bash
#--------------------------------------------------------------------------
# What: Run Sweave and LaTeX directly from command line
# $Id: Sweave.sh,v 1.5.1 2005/02/23 12:18:17 ggorjan Exp $
#--------------------------------------------------------------------------
# Needed:
#  - R http://www.r-project.com
#  - texi2dvi http://www.gnu.org/software/texinfo/texinfo.html
#--------------------------------------------------------------------------
# Initial idea taken from:
# http://www.ci.tuwien.ac.at/~leisch/Sweave/FAQ.html#x1-5000A.3
#
# There was quite some talk on this issue on:
# https://stat.ethz.ch/pipermail/r-help/2005-February/065047.html
#
# Some modifications by Vincent Goulet:
#    - add option "--quiet" to R
#    - option "-l" does not imply "-d" and "-p"
#    - use dvipdfm instead of pdflatex for PDF creation
#--------------------------------------------------------------------------
# Nothing to configure below!
#---------------------------------------------------------------------------

NAME=$(basename $0)

# Usage: usage
# Print the usage.
usage () {
    cat <<EOF
NAME
    $NAME - Run Sweave and LaTeX directly from command line

SYNOPSIS
    $NAME [OPTION] ...

DESCRIPTION
    Run Sweave directly from command line with given options and optionally
    also LaTeX. Following options can be used:

    -c, --clean
      Remove all intermediate files. These are PDF and EPS pictures and TeX
      file from R run of Sweave and DVI file from LaTeX run. Works only
      with option -l.

    -d, --pdf
      Create PDF in LaTeX run. Automatically implies option -l.

    -h, --help
      Print this output.

    -l, --latex
      Run LaTeX on prepared file by Sweave. Look under details for set
      of LaTeX commands etc. By default, both postscript and PDF are
      created. Use options -p or -d to create only one of them.

    -p, --ps
      Create postscript in LaTeX run. Automatically implies option -l.

    -m, --mode
      Direct mode might be usefull for multiple input files. By default all
      input files are first processed with Sweave and then all by LaTeX.
      With this option each file is processed with Sweave and directly
      after that with LaTeX and next file with Sweave and ... There is no
      difference for only one input file.

DETAILS
    R is launched with following options:
     - don't save it i.e. --no-save
     - don't restore anything i.e. --no-restore

    LaTeX in the context means either:
     - texi2dvi if it is found, otherwise
     - hardoced set of LaTeX and friends is used (look below), where latex
       can be pure latex of pdflatex. There may appear some errors on
       screen if there is no need for bibtex and/or makeindex, since these
       two are also on the list.

       The list of commands is:
        latex
        bibtex
        makeindex
        latex
        latex

    Options might not be combined like -cld, but should be given
    separatelly like -c -l -d. Look in examples.

EXAMPLE
    Process only with Sweave
    $NAME Sweave-test-1.Rnw

    Create only DVI
    $NAME -l Sweave-test-1.Rnw

    Create only PDF
    $NAME -d Sweave-test-1.Rnw

    Create only PDF with direct mode
    $NAME -d -m Sweave-test-1.Rnw

AUTHOR
    Gregor GORJANC <gregor.gorjanc at bfro.uni-lj.si>
    Vincent GOULET <vincent.goulet@act.ulaval.ca>

EOF
}

# Get options
OPTIONS=$@
for OPTION in $@; do
    case "$OPTION" in
        -c | --clean )
        CLEAN=yes
        OPTIONS=$(echo $OPTIONS | sed -e "s/--clean//" -e "s/-c//")
        OPTIONS_ECHO="$OPTIONS_ECHO - clean after end\n"
        ;;
        -d | --pdf )
        LATEX=yes
        PDF=yes
        OPTIONS=$(echo $OPTIONS | sed -e "s/--pdf//" -e "s/-d//")
        OPTIONS_ECHO="$OPTIONS_ECHO - create only PDF\n"
        ;;
        -h | --help )
        usage
        exit 0
        ;;
        -l | --latex )
        LATEX=yes
        OPTIONS=$(echo $OPTIONS | sed -e "s/--latex//" -e "s/-l//")
        OPTIONS_ECHO="$OPTIONS_ECHO - use LaTeX after Sweave\n"
        ;;
        -p | --ps )
        LATEX=yes
        PS=yes
        OPTIONS=$(echo $OPTIONS | sed -e "s/--ps//" -e "s/-p//")
        OPTIONS_ECHO="$OPTIONS_ECHO - use LaTeX after Sweave\n"
        OPTIONS_ECHO="$OPTIONS_ECHO - create only PS\n"
        ;;
        -m | --mode )
        MODE=yes
        OPTIONS=$(echo $OPTIONS | sed -e "s/--mode//" -e "s/-m//")
        OPTIONS_ECHO="$OPTIONS_ECHO - direct mode\n"
        ;;
        * )
        ;;
    esac
done

# Defaults for R
ROPTIONS="--no-save --no-restore --quiet"

# Defaults for LaTeX
#if [ "$LATEX" = "yes" -a ! -n "$PS" -a ! -n "$PDF" ]; then
#    PS=yes
#    PDF=yes
#fi

# File list
FILES=$OPTIONS

# Print usage if there is no input files
if [ ! -n "$FILES" ]; then
    echo -e "\nError: There is no input files! Usage is:\n"
    usage
    exit 1
fi

# Functions
# --- The R machine ---
sweave_R ()
{
    echo "Sweave(\"$1\")" | R $ROPTIONS
}

# --- The LaTeX machine ---
sweave_latex ()
{
    # Strip of .Rnw or .Snw from filename
    FILE=$(echo $1 | sed -e 's/\.Rnw//' -e 's/\.Snw//')

    # texi2dvi
    if [ `which texi2dvi` ]; then
        echo -e " - using 'texi2dvi'"
        if [ "$PS" = "yes" ]; then
	    texi2dvi --clean --quiet ${FILE}.tex
            echo -e "\nPostscript creation"
            dvips ${FILE}.dvi -o ${FILE}.ps
        fi
        if [ "$PDF" = "yes" ]; then
	    texi2dvi --clean --quiet --pdf ${FILE}.tex
        fi
    else
    # LaTeX and friends
        echo -e " - using hardcoded list of 'LaTeX and friends' commands"
	latex $FILE
	bibtex $FILE
	makeindex $FILE
	latex $FILE
	latex $FILE
        if [ "$PS" = "yes" ]; then
            echo -e "\nPostscript creation"
            dvips ${FILE}.dvi -o ${FILE}.ps
        fi
        if [ "$PDF" = "yes" ]; then
            echo -e "\nPDF creation"
            dvipdfm -o ${FILE}.ps ${FILE}.dvi
        fi
    fi

    # Remove PDF and EPS pictures and DVI file
    if [ "$CLEAN" = "yes" ]; then
        rm -f ${FILE}-*.pdf ${FILE}-*.eps ${FILE}.dvi
        rm -f Rplots.ps
    fi
}

# Title
echo -e "\nSweave directly from command line ..."

# Report options
echo -e " - R options are: $ROPTIONS"
echo -e "$OPTIONS_ECHO"

# Main program
for FILE in $FILES; do
    sweave_R $FILE
    if [ "$LATEX" = "yes" -a "$MODE" = "yes" ]; then
        echo -e "\nLaTeX on produced tex files"
        sweave_latex $FILE
    fi
done

if [ "$LATEX" = "yes" ]; then
    echo -e "\nLaTeX on produced tex files"
    for FILE in $FILES; do
        sweave_latex $FILE
    done
else
    exit 0
fi

# Exit
exit 0

#--------------------------------------------------------------------------
# Sweave.sh ends here