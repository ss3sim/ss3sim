all: pdf

pdf:
	pandoc --bibliography=refs.bib --csl=icesjms.csl --latex-engine=xelatex --number-sections manuscript.md -o manuscript.pdf

docx:
	pandoc --bibliography=refs.bib --csl=icesjms.csl --reference-docx=reference.docx manuscript.md -o manuscript.docx

md:
	pandoc --bibliography=refs.bib --csl=icesjms.csl manuscript.md -o manuscript-with-refs.md
#TODO need to fix et al.s - should be italic - check csl and file bug if needed

cleanbib:
	bibtool refs.bib -s > refs2.bib
	mv refs2.bib refs.bib
