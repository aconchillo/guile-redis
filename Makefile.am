#
# Makefile.am
#
# Copyright (C) 2013-2019 Aleix Conchillo Flaque <aconchillo@gmail.com>
#
# This file is part of guile-redis.
#
# guile-redis is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# guile-redis is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with guile-redis. If not, see https://www.gnu.org/licenses/.
#

SUBDIRS = redis

ACLOCAL_AMFLAGS = -I build-aux

dist-hook:
	$(SHELL) $(top_srcdir)/ChangeLog > $(top_distdir)/ChangeLog

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
objdir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_nodist_obj_DATA = $(GOBJECTS)

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/env $(GUILD) compile $(GUILE_WARNINGS) -o "$@" "$<"

SOURCES = redis.scm

CLEANFILES = $(GOBJECTS)

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
