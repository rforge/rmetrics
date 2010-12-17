PACKAGE_NAME=fCertificates

RUnit:
	R --slave --vanilla < R/runRUnitTests.R > RUnit-protocol.txt 2> RUnit-protocol.txt

clean:
	rm -rf build/$(PACKAGE_NAME)/

check:
	R CMD check $(PACKAGE_NAME)

build:
	R CMD build $(PACKAGE_NAME)

install:
	R CMD INSTALL $(PACKAGE_NAME)