## Makefile --- Emacs-fetch Makefile.             -*- lexical-binding: t; -*-

# Copyright (C) 2018  Pierre-Antoine Rouby

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
EMACS_LOAD = -l efetch-mode.el
EMACS_COMPILE = -f emacs-lisp-byte-compile
EMACS_DIR = ~/.emacs.d/efetch-mode/

IMAGES_DIR = images/
INST_IMAGES_DIR = $(EMACS_DIR)$(IMAGES_DIR)

SED_SOURCES = $(wildcard *.el.in)

SOURCES = $(wildcard *.el)
SOURCES += $(SED_SOURCES:.el.in=.el)

COMPILED_FILE += $(SOURCES:.el=.elc)

.PHONY: clean install uninstall compile copy-sources copy-images

all: compile

compile: $(SOURCES) $(COMPILED_FILE) $(EMACS_PAYLOAD)

%.elc: %.el
	$(info Compiling    $@)
	@$(EMACS) $(EMACS_LOAD) $< $(EMACS_COMPILE)

%.el: %.el.in
	$(info Sed          $@)
	@sed 's|@INST_IMAGES_DIR@|$(INST_IMAGES_DIR)|' < $< >$@

copy-sources:
	$(info Install      *.elc)
	@mkdir $(EMACS_DIR) -p
	@cp *.el  $(EMACS_DIR) -v
	@cp *.elc $(EMACS_DIR) -v

copy-images:
	$(info Install      *.png)
	@mkdir $(INST_IMAGES_DIR) -p
	@cp ./$(IMAGES_DIR)*.png $(INST_IMAGES_DIR) -v

install: compile copy-sources copy-images

uninstall:
	$(info Uninstall)
	@rm -f -r $(EMACS_DIR) -v

clean:
	$(info Removing)
	@rm -v -f *.elc
	@rm -v -f efetch-func.el
	@rm -v -f $(EMACS_PAYLOAD)

## Makefile ends here
