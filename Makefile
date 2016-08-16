# Jane data file generator
# Copyright (C) 2016 Daniel Beer <dlbeer@gmail.com>
#
# Permission to use, copy, modify, and/or distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

SRC_COMMON = \
    trees.fs \
    linter.fs \
    scanner.fs \
    csv.fs \
    nexus.fs \
    jane.fs \
    links.fs

SRC_GUI = $(SRC_COMMON) gui.fs
SRC_CLI = $(SRC_COMMON) getopt.fs cli.fs

all: janex.exe janex-cli.exe

janex.exe: $(SRC_GUI)
	fsharpc --target:winexe -o $@ $^ \
	    -r:System.Windows.Forms.dll \
	    -r:System.Drawing.dll
	chmod 755 $@

janex-cli.exe: $(SRC_CLI)
	fsharpc -o $@ $^
	chmod 755 $@

clean:
	rm -f janex.exe
	rm -f janex-cli.exe
