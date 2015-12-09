# -*- Autoconf -*-
#
# guile-websocket --- WebSocket client/server
# Copyright © 2015 David Thompson <davet@gnu.org>
#
# This file is part of guile-websocket.
#
# Guile-websocket is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# Guile-websocket is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with guile-websocket.  If not, see
# <http://www.gnu.org/licenses/>.

AC_INIT(guile-websocket, 0.1)
AC_CONFIG_SRCDIR([web])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])

GUILE_PROGS([2.0.9])

AC_OUTPUT
