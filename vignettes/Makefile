all: vignette
vignette:
	./knit
	pdflatex ss3sim-vignette.tex
	bibtex ss3sim-vignette
	pdflatex ss3sim-vignette.tex
	pdflatex ss3sim-vignette.tex
	cp ss3sim-vignette.pdf ~/Dropbox/Public/ss3sim-vignette.pdf

clean:
	rm *.aux *.log *.toc ss3sim-vignette.tex *.blg *.bbl
	rm -rf cache
	rm -rf figure

