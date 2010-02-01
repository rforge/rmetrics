PKG=NormalLaplace
TOP=../..
SUITE=doRUnit.R
R=R
LEVEL=1

all: inst test

inst: # Install package -- but where ?? -- will that be in R_LIBS ?
	cd ${TOP}/..;\
	${R} CMD INSTALL ${PKG}

test: # Run unit tests
	export RCMDCHECK=FALSE;\
	export LEVEL=${LEVEL};\
	export PKG=${PKG};\
	cd ${TOP}/tests;\
	${R} --vanilla --slave < ${SUITE}

clean:
	rm *~ *.Rout .RData .Rhistory

