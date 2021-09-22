default:
	Rscript -e 'capture.output(library(fpp3), type="message", file="startup.txt")'
	sed -i -f startup.sed startup.txt
	Rscript -e 'rmarkdown::render("frb2021.Rmd", quiet=FALSE)'

clean:
	rm -rf frb2021.pdf
	latexmk -c
	rm -rf *_cache
	rm -rf *_files

