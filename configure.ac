#
# configure.ac
#
# Copyright (C) 2013-2018 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

AC_INIT([guile-redis], [1.1.0], [aconchillo@gmail.com])
AC_CONFIG_SRCDIR(redis.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability])
AM_SILENT_RULES([yes])

dnl We require pkg.m4 (from pkg-config) and guile.m4 (from Guile.)
dnl Make sure they are available.
m4_pattern_forbid([PKG_CHECK_MODULES])
m4_pattern_forbid([^GUILE_PKG])

dnl Check for Guile 2.x, and substitute 'GUILE_EFFECTIVE_VERSION'.
GUILE_PKG([2.2 2.0])
GUILE_PROGS

AC_CONFIG_FILES([Makefile redis/Makefile])
AC_CONFIG_FILES([env], [chmod +x env])

AC_OUTPUT

# configure.ac ends here
