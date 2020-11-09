# subfix

Copyright (C) Jonathan Lamothe <jonathan@jlamothe.net>

## Your Rights

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

## What It Does

This program was designed to make captioning videos easier.  It reads
a SubRip (.SRT) subtitle from standard input and performs some
transformations which are then written to standard output.  There are
no command line arguments.

Any caption group that begins with a caret (^) will be formatted to
display at the top of the screen.  Additionally, any instance of a
double number sign (##) found anywhere within a caption group will be
replaced with a music note.
