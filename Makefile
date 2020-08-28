PACKAGE = adams
VERSION = 0.3.1
RELEASE_DIR = ${PACKAGE}-${VERSION}
RELEASE_TARBALL = ${PACKAGE}-${VERSION}.tar.gz
RELEASE_DEPS_TARBALL = ${PACKAGE}-${VERSION}.deps.tar.gz
PROGRAM = build/adams
PREFIX = /usr/local
LISP = sbcl --dynamic-space-size 2048
LISP_LOAD = ${LISP} --load
CLEANFILES = build/*
DISTCLEANFILES = ${RELEASE_DIR} ${RELEASE_TARBALL} ${RELEASE_DEPS_TARBALL}

all: ${PROGRAM}

deps:
	LANG=C.UTF-8 ${LISP_LOAD} prepare-build.lisp --quit

build/systems.lisp: prepare-build.lisp adams.asd
	LANG=C.UTF-8 ${LISP_LOAD} prepare-build.lisp --quit

${PROGRAM}: build.lisp config.lisp build/systems.lisp toplevel.lisp
	LANG=C.UTF-8 ${LISP_LOAD} build.lisp --quit

install:
	install -m 0755 ${PROGRAM} ${PREFIX}/bin

release: ${RELEASE_TARBALL} ${RELEASE_DEPS_TARBALL}

${RELEASE_TARBALL}:
	mkdir ${RELEASE_DIR}
	find . -name build -prune -or -name '*.lisp' -or -name '*.asd' -or -name '*.md' | cpio -pd ${RELEASE_DIR}
	tar czf ${RELEASE_TARBALL} ${RELEASE_DIR}

${RELEASE_DEPS_TARBALL}:
	tar czf ${RELEASE_DEPS_TARBALL} build/*.lisp

clean:
	rm -rf ${CLEANFILES}

distclean:
	rm -rf ${DISTCLEANFILES}

.PHONY: all clean deps install ${PROGRAM} release
