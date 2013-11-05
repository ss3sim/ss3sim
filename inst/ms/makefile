all: pdf docx cleanbib

pdf:
	pandoc -S --no-wrap --bibliography=refs.bib --csl=plos.csl --bibliography=refs.bib --latex-engine=xelatex ss3sim-ms.md -o manuscript.tex
	perl -p -i -e "s/Fig. /Fig.~/g" manuscript.tex
	perl -p -i -e "s/e\.g\. /e\.g\.~/g" manuscript.tex
	perl -p -i -e "s/i\.e\. /i\.e\.~/g" manuscript.tex
	xelatex ss3sim-ms
	rm manuscript.tex *.log *.aux
	cp ss3sim-ms.pdf ~/Dropbox/Public/

docx:
	pandoc -S --bibliography=refs.bib --csl=plos.csl --reference-docx=reference.docx ss3sim-ms.md -o ss3sim-ms.docx
	mv ss3sim-ms.docx ~/Dropbox/Public/

md:
	pandoc -S --bibliography=refs.bib --csl=plos.csl ss3sim-ms.md -o ss3sim-ms-with-refs.md

cleanbib:
	bibtool refs.bib -s > refs2.bib
	mv refs2.bib refs.bib

vignette:
	./knit
	pandoc -S -N ss3sim-vignette.md -o ss3sim-vignette.pdf
	rm ss3sim-vignette.md
