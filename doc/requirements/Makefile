VPATH = src build

LC = pdflatex
LFLAGS = -interaction=nonstopmode -output-directory=build -synctex=1

blackfly-requirements.pdf:

%.pdf: %.tex
	$(LC) $(LFLAGS) $<
	$(LC) $(LFLAGS) $<

.PHONY: clean
clean:
	rm -rfv *.docx *.tex build/* src/*~
