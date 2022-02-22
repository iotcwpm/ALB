
PDFS:=$(patsubst %.Rmd,%.pdf,$(wildcard *.Rmd))

all: clean $(PDFS) tidy

%.pdf: %.Rmd references.bib
	Rscript -e "rmarkdown::render('$<', clean=TRUE)"

references.bib: ${BIBINPUTS}/references.bib
	pubs refs $<

.PHONY: all clean cleanall

tidy:
	rm -f *.log 

clean:
	rm -f *.pdf

cleanall: clean
	rm -rf *_cache
	rm -rf *_files
	rm -f *.tex *.log *.aux
