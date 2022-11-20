EMACS ?= emacs
CASK ?= cask
ELS = composer.el
AUTOLOADS = composer-autoloads.el
ELCS = $(ELS:.el=.elc)

.el.elc: .cask
	$(EMACS) -Q -batch -L . --eval \
	"(let ((default-directory (expand-file-name \".cask\" default-directory))) \
	   (require 'package) \
	   (normal-top-level-add-subdirs-to-load-path))" \
	-f package-initialize -f batch-byte-compile $<

.cask: Cask
	$(CASK)

all: clean autoloads $(ELCS)

autoloads: $(AUTOLOADS)

$(AUTOLOADS): $(ELCS)
	$(EMACS) -Q -batch -L . --eval \
	"(progn \
	   (require 'package) \
	   (normal-top-level-add-subdirs-to-load-path) \
	   (package-generate-autoloads \"composer\" default-directory))"

clean:
	-rm -f $(ELCS) $(AUTOLOADS)

clobber: clean
	-rm -f .cask

.PHONY: all autoloads clean clobber
