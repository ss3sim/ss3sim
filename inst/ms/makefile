all: plos

plos:
	pandoc -S --no-wrap --bibliography=ss3sim-ms.bib --natbib ss3sim-ms.md -o manuscript.tex
	perl -p -i -e "s/Fig. /Fig.~/g" manuscript.tex
	perl -p -i -e "s/citep/cite/g" manuscript.tex
	perl -p -i -e "s/citet/cite/g" manuscript.tex
	perl -p -i -e "s/e\.g\. /e\.g\.~/g" manuscript.tex
	perl -p -i -e "s/i\.e\. /i\.e\.~/g" manuscript.tex
	perl -p -i -e "s/\\\section/\\\section\*/g" manuscript.tex
	perl -p -i -e "s/\\\subsection/\\\subsection\*/g" manuscript.tex
	perl -p -i -e "s/Andre Punt/Andr\\\'{e} Punt/g" manuscript.tex
	pdflatex ss3sim-ms
	cp ss3sim-ms.pdf ~/Dropbox/Public/

letter:
	xelatex ss3sim-letter

bib:
	pdflatex ss3sim-ms
	bibtex ss3sim-ms
	pdflatex ss3sim-ms
	pdflatex ss3sim-ms

clean:
	rm *.log
	rm *.aux
	rm *.bbl
	rm *.blg

docx:
	pandoc -S --bibliography=ss3sim-ms.bib --csl=plos.csl --reference-docx=reference.docx ss3sim-ms.md -o ss3sim-ms.docx
	mv ss3sim-ms.docx ~/Dropbox/Public/

rft:
	latex2rtf ss3sim-ms.tex
	mv ss3sim-ms.rtf ~/Dropbox/Public/

md:
	pandoc -S --bibliography=ss3sim-ms.bib --csl=plos.csl ss3sim-ms.md -o ss3sim-ms-with-refs.md

cleanbib:
	bibtool ss3sim-ms.bib -s > ss3sim-ms.bib
	mv ss3sim-ms.bib ss3sim-ms.bib

vignette:
	./knit
	pandoc -S -N ss3sim-vignette.md -o ss3sim-vignette.pdf
	rm ss3sim-vignette.md
