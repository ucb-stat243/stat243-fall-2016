rmds= $(wildcard *.Rmd)
pdfs= $(patsubst %.Rmd,%.pdf,$(rmds))

.PHONY: all clean


all: $(pdfs)


%.pdf: %.Rmd
	Rscript -e "library(rmarkdown); render('$<')"

clean:
	rm -f *.pdf
