#-*- Makefile -*-
#
include $(R_HOME)/etc$(R_ARCH)/Makeconf

SOURCES= ./donlp2.c 

OBJS = $(SOURCES:.c=.o)

PKG_CPPFLAGS = -I.

all: libdonlp2.a

libdonlp2.a: $(OBJS)
	$(AR) crs $@ $(OBJS)
