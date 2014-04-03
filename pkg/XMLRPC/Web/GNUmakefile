DOCS=../inst/doc/implementation.pdf ../inst/doc/annotation.pdf

include $(OMEGA_HOME)/R/Config/GNUmakefile.Web

HTML_STYLESHEET=http://www.omegahat.org/OmegaTech.css

EXAMPLE_FILES=$(wildcard ../Examples/*.swf ../Examples/*.mxml)

shipExamples:
	scp $(EXAMPLE_FILES) $(TARGET_DIR)/Examples


