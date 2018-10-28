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
EMACS_COMPILE = -f emacs-lisp-byte-compile
EMACS_DIR = ~/.emacs.d/efetch/

IMAGES_DIR = images/
INST_IMAGES_DIR = $(EMACS_DIR)$(IMAGES_DIR)

ASCII_DIR = ascii-arts/
INST_ASCII_DIR = $(EMACS_DIR)$(ASCII_DIR)

SOURCES = efetch.el
COMPILED_FILE += $(SOURCES:.el=.elc)

.PHONY: clean install uninstall compile copy-sources copy-images

all: compile

compile: $(SOURCES) $(COMPILED_FILE) $(EMACS_PAYLOAD)

%.elc: %.el
	$(info Compiling    $@)
	@$(EMACS) $< $(EMACS_COMPILE)

copy-sources:
	$(info Install      *.elc)
	@mkdir -p $(EMACS_DIR)
	@cp -v *.el  $(EMACS_DIR)
	@cp -v *.elc $(EMACS_DIR)
	@cp -v LICENSE $(EMACS_DIR)

copy-images:
	$(info Install      *.png)
	@mkdir $(INST_IMAGES_DIR) -p
	@cp -v ./$(IMAGES_DIR)*.png $(INST_IMAGES_DIR)
	@cp -v ./$(IMAGES_DIR)COPYRIGHT $(INST_IMAGES_DIR)

copy-ascii:
	@mkdir $(INST_ASCII_DIR) -p
	@cp -v ./$(ASCII_DIR)* $(INST_ASCII_DIR)

install: compile copy-sources copy-images copy-ascii

uninstall:
	$(info Uninstall)
	@rm -v -f -r $(EMACS_DIR)

clean:
	$(info Removing)
	@rm -v -f *.elc
	@rm -v -f $(EMACS_PAYLOAD)

## Makefile ends here
