
PROGRAM = build/adams
PREFIX = /usr/local
LISP = sbcl --dynamic-space-size 2048
LISP_LOAD = ${LISP} --load

all: ${PROGRAM}

deps:
	LANG=C.UTF-8 ${LISP_LOAD} prepare-build.lisp --quit

build/systems.lisp: prepare-build.lisp adams.asd
	LANG=C.UTF-8 ${LISP_LOAD} prepare-build.lisp --quit

${PROGRAM}: build.lisp config.lisp build/systems.lisp toplevel.lisp
	LANG=C.UTF-8 ${LISP_LOAD} build.lisp --quit

clean:
	rm -rf build/*

install:
	install -m 0755 ${PROGRAM} ${PREFIX}/bin

.PHONY: all clean deps install ${PROGRAM}
