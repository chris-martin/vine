dpi=600

vine.pdf: vine.tex vine.bib chris.jpg flip.png segment.png
	latexmk -pdf vine.tex

flip.png: flip.svg
	inkscape -d $(dpi) -e flip.png flip.svg

segment.png: segment.svg
	inkscape -d $(dpi) -e segment.png segment.svg