
PROGRAM = build/adams
PREFIX = /usr/local
LISP = sbcl
LISP_LOAD = ${LISP} --load

all: ${PROGRAM}

${PROGRAM}: build.lisp
	LANG=C.UTF-8 ${LISP_LOAD} build.lisp

clean:
	rm -rf build/*

install: ${PROGRAM}
	install -m 0755 ${PROGRAM} ${PREFIX}/bin

.PHONY: all clean install ${PROGRAM}
