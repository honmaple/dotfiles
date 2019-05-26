EMACS= /usr/bin/emacs --batch
INITFILE=~/.emacs.d/init.el

install:
	@$(EMACS) --load $(INITFILE)

clean:
	@rm ~/.emacs.d/cache/autoloads.pkg.el*

upgrade:
	@$(EMACS) --load $(INITFILE) --eval '(maple-package-upgrade)'

compile:
	@$(EMACS) --load $(INITFILE) --eval '(maple-package-force-initialize)'

all: install

.PHONY: all install clean upgrade compile
