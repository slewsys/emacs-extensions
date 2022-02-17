# Makefile: Emacs source compilation
#
EMC := /usr/local/bin/emc

.SUFFIXES: .el .elc

.el.elc:
	$(EMC) $<

SRCS = $(wildcard *.el)
OBJS = $(SRCS:.el=.elc)

.PHONY: all clean

all: $(OBJS)

clean:
	rm -f $(OBJS)
