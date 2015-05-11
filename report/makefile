DOCNAME=report

default: ${DOCNAME}.pdf

${DOCNAME}.pdf: *.tex *.bib

	pdflatex ${DOCNAME}.tex
	bibtex ${DOCNAME}
	pdflatex ${DOCNAME}.tex
	pdflatex ${DOCNAME}.tex

clean: clean-meta
	rm -f ${DOCNAME}.pdf

clean-meta:
	rm -f *.aux
	rm -f *.bbl
	rm -f *.blg
	rm -f *.log
	rm -f *.out
	rm -f *.toc

references:
	pdflatex ${DOCNAME}.tex
	pdflatex ${DOCNAME}.tex

light:
	pdflatex ${DOCNAME}.tex

run:
	evince ${DOCNAME}.pdf
