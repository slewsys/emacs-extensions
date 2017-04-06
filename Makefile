# Makefile: Emacs source compilation
#
EMACS = emacs

.SUFFIXES: .el .elc

.el.elc:
	$(EMACS) -batch -q -eval '(byte-compile-file "$<")'

SRCS = $(wildcard *.el)
OBJS = $(SRCS:.el=.elc)

.PHONY: all clean

all: $(OBJS)

clean:
	rm -f $(OBJS)
