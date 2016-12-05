CASK = cask
EMACS = emacs
EMACSOPTS =
ERTSELECTOR = t

EMACSBATCH = $(EMACS) -Q --batch $(EMACSOPTS)

HAVE_CASK := $(shell sh -c "command -v $(CASK)")
ifndef HAVE_CASK
$(warning "$(CASK) is not available.  See \
http://cask.readthedocs.io/en/latest/guide/installation.html \
for installation instructions.")
endif

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

CASK_EMACS = $(EMACS)
export EMACS
export CASK_EMACS

SRCS = syntactic-close.el
OBJS = $(SRCS:.el=.elc)

.PHONY: compile
compile: $(OBJS)

.PHONY: clean
clean:
	rm -rf $(OBJS)

.PHONY: clean-deps
clean-deps:
	rm -rf .cask/

.PHONY: test
test: compile
	$(EMACSBATCH) --script test/run.el '$(ERTSELECTOR)'

.PHONY: deps
deps: $(PKGDIR)

$(PKGDIR): Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc: %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<
