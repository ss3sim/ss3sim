all: pdf docx cleanbib

pdf:
	pandoc -S --no-wrap --bibliography=refs.bib --csl=icesjms.csl --bibliography=refs.bib --latex-engine=xelatex ss3sim-ms.md -o manuscript.tex
	perl -p -i -e "s/Fig. /Fig.~/g" manuscript.tex
	xelatex ss3sim-ms
	rm manuscript.tex *.log *.aux

docx:
	pandoc --bibliography=refs.bib --csl=icesjms.csl --reference-docx=reference.docx ss3sim-ms.md -o ss3sim-ms.docx

md:
	pandoc --bibliography=refs.bib --csl=icesjms.csl ss3sim-ms.md -o ss3sim-ms-refs.md

cleanbib:
	bibtool refs.bib -s > refs2.bib
	mv refs2.bib refs.bib

vignette:
	./knit
	pandoc -S -N ss3sim-vignette.md -o ss3sim-vignette.pdf
	#rm ss3sim-vignette.md
