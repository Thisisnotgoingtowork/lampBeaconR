VERSION:=$(shell grep Version: DESCRIPTION|sed 's/Version: //')
NAME:=$(shell grep Package: DESCRIPTION|sed 's/Package: //')
PACKAGEFILE:=../$(NAME)_$(VERSION).tar.gz

all: $(PACKAGEFILE) README.md

.PHONY: all install localInstall cranCheck

install:
	R -e 'devtools::install_github("Thisisnotgoingtowork/$(NAME)")'

localInstall:
	R -e 'devtools::install()'

cranCheck:
	R -e 'rhub::check_for_cran()'

man: R/*.R
	R -e 'devtools::document()'
	touch man


README.md: README.Rmd R/*.R
	make localInstall
	R -e 'knitr::opts_chunk$$set(fig.path="README_files/");knitr::knit("README.Rmd")'


$(PACKAGEFILE): man R/*.R DESCRIPTION
	sed -i "s/^Date:.*$$/Date: `date +%Y-%m-%d`/" DESCRIPTION
	R -e 'devtools::check();devtools::build()'

