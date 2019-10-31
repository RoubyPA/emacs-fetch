## Makefile --- Emacs-fetch Makefile.             -*- lexical-binding: t; -*-

# Copyright (C) 2018-2019  Pierre-Antoine Rouby

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

EMACS = emacs -Q -q --batch -nw
EMACS_COMPILE = -f emacs-lisp-byte-compile
EMACS_RUN_TESTS = -f run

EMACS_DIR = ~/.emacs.d/efetch/

check: efetch-tests

%.elc: %.el
	$(info Compiling    $<)
	@$(EMACS) $< $(EMACS_COMPILE)

efetch-tests: efetch-tests.elc
	$(info CHECK)
	@$(EMACS) --load $< --load ../efetch.elc $(EMACS_RUN_TESTS)
