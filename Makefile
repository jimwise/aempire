#
#	Copyright (C) 1987, 1988 Chuck Simmons
#
# $Id: Makefile,v 1.1 2006/02/21 17:33:41 jwise Exp $
#
# See the file COPYING, distributed with empire, for restriction
# and warranty information.

VERSION=1.3_ALPHA2

#
# 1.) Pick where to install cempire
#	The cempire binary will be installed in BINDIR
#
#	The cempire man page will be installed in MANDIR
#
PREFIX=/usr/local
BINDIR=${PREFIX}/bin
MANDIR=${PREFIX}/man/man6

#
# 2.) Pick your compiler and `install' program
#	If you want to use your system's native compiler, use the following.
#	It MUST be at least relatively ANSI compliant for this to work.
#       	CC=cc
#
#	Pick the following to use GCC (or EGCS).  Needed if your system's
#	native compiler is not ANSI compliant (or not present)
#		CC=gcc
#
#	If your system ships with a BSD-compatible install, do:
#		INSTALL=install
#
#	On Solaris, do:
#		INSTALL=/usr/ucb/install
#
#	Otherwise, do:
#		INSTALL=./install-sh
#
CC=gcc
INSTALL=install

#
# 3.) Pick your preprocessor defines
#	choose from the following options:
#		-DUSE_NCURSES		you need to turn this on if you want
#					to use the ncurses library.  Use this if
#					cempire doesn't work with your system's
#					native curses library
#		-DUSE_COLOR		Turn this on if your curses library supports
#					color.  This is true for ncurses and just about
#					anything else still available.  Note that color
#					can still be enabled/disabled from the command line.
#		-D__EXTENSIONS__	you need this on Solaris 2.5.1, and probably earlier
#					versions as well.  If you don't define this, you don't
#					get a declaration of getopt(3), and curses.h also assumes
#					you're on a BSD system (since L_ctermid doesn't get defined
#					in stdio.h...)
#		-DUSE_ZLIB		define this to compress saved files on write and
#					decompress them on reads.  Saves a lot of space.
#
DEFINES=-D__EXTENSIONS__ -DUSE_ZLIB -DUSE_COLOR

#
# 4.) Pick your library specifications
#	for System V curses, or other non-termcap curses implementations, use:
#		LIBS=-lcurses
#
#	for traditional BSD curses implementations, which rely on the
#	termcap libraries, use:
#		LIBS=-lcurses -ltermcap
#
#	if you want to use ncurses.  Change /usr/local/lib to wherever
#	you have the ncurses library installed.  You also need to turn on
#	-DUSE_NCURSES above, and check your includes below.
#		LIBS=-L/usr/local/lib -lncurses
#
#	use the following if you have ncurses installed using NetBSD's
#	wonderful package system.
#		LIBS=-L/usr/pkg/lib -lncurses
#
#	if you defined USE_ZLIB above, you need to use the following. 
#	Change /usr/local/lib to wherever you have the zlib library
#	installed, or remove the -L/usr/local/lib if you are on a
#	system which has zlib in its default library path.
#		LIBS=-L/usr/local/lib -lz
#
LIBS=-lz -lcurses

#
# 5.) Pick your includes
#
#	If your system curses works, use this:
#		INCLUDES=
#
# 	if you are using ncurses, you need this.  Change /usr/local/include to
#	wherever you have the ncurses headers installed, and turn on
#	-DUSE_NCURSES and your curses library, above
#		INCLUDES=-I/usr/local/include
#
#	use this if you have ncurses installed under NetBSD's package system.
#		INCLUDES=-I/usr/pkg/include
#
INCLUDES=

#
# The following turns on _extremely_ pedantic error checking for gcc or egcs.
# Don't try to use this if you are using another compiler.  If cempire
# isn't happy this way on your system, please let me know...
#
WARNS=-ansi -pedantic-errors -Werror -Wall -W -pedantic \
	-Wshadow -Wpointer-arith -Wcast-qual -Wcast-align -Wwrite-strings \
	-Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations \
	-Wnested-externs -Winline -Wundef -Wbad-function-cast -Wsign-compare

# The following warnings warn about acceptable code in this software
# -Wconversion -Wredundant-decls -Waggregate-return

#
# If you want to debug cempire, you can turn on debugging here.  If you
# define DEBUG, various debugging checks in the program are turned on.
#
# If you want to turn off even normal diagnostic checks, you can define
# NDEBUG here
#
DEBUG=-g -DDEBUG

#
# Here's where you put your favorite c compiler options
#
COPTS=

#
# You shouldn't have to modify anything below this line.
#

TARGET = cempire

CPPFLAGS= $(DEFINES) $(INCLUDES)
CFLAGS= $(CPPFLAGS) $(COPTS) $(DEBUG) $(WARNS)
LINTFLAGS = -Habcnrsuxz -w

# Code noted by the following lint checks has been checked for errors
# -aa -e -h 

# The following flags in the above make lint less strict:
# -n -- do not check against standard library
# -u do not complain about funcs used but not defined
# -z do not complain about use of pointer to undefined struct

MAKEDEPEND	= mkdep
DEPENDFLAGS	=

SRCS= attack.c compmove.c data.c display.c edit.c empire.c game.c main.c \
	map.c math.c object.c term.c usermove.c util.c

HEADERS= empire.h extern.h

OBJS= attack.o compmove.o data.o display.o edit.o empire.o game.o main.o \
	map.o math.o object.o term.o usermove.o util.o

MISCFILES= READ.ME COPYING BUGS Makefile cempire.6 .cvsignore

SOURCES= $(MISCFILES) $(SRCS) $(HEADERS)

ARCHIVES= cempire-$(VERSION).tar cempire-$(VERSION).tar.gz \
	cempire-$(VERSION).shar

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) -o $(TARGET) $(OBJS) $(LIBS)

lint: $(SRCS)
	lint $(LINTFLAGS) $(CPPFLAGS) $(SRCS) $(LIBS)

clean:
	rm -f *.o $(TARGET) cempire-$(VERSION).tar cempire-$(VERSION).tar.gz cempire-$(VERSION).shar cempire-$(VERSION).tar.gz.asc

cleandir: clean
	rm -f .depend

install:
	${INSTALL} -c -s -m 0755 cempire ${BINDIR}
	${INSTALL} -c -m 0644 cempire.6 ${MANDIR}


dist:	pgp
tar:	cempire-$(VERSION).tar
tgz:	cempire-$(VERSION).tar.gz
shar:	cempire-$(VERSION).shar
pgp:	cempire-$(VERSION).tar.gz.asc

cempire-$(VERSION).tar: $(SOURCES)
	(cd ..; \
	 tar -cvf cempire-$(VERSION)/cempire-$(VERSION).tar \
	 `for X in $(SOURCES); do echo cempire-$(VERSION)/$$X; done`)

cempire-$(VERSION).tar.gz: cempire-$(VERSION).tar
	gzip -9 -f cempire-$(VERSION).tar

cempire-$(VERSION).shar: $(SOURCES)
	shar $(SOURCES) >cempire.shar

cempire-$(VERSION).tar.gz.asc: cempire-$(VERSION).tar.gz
	gpg -bat cempire-$(VERSION).tar.gz

depend:
	$(MAKEDEPEND) $(DEPENDFLAGS) $(CFLAGS) $(SRCS)

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.
