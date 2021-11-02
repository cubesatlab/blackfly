VPATH = src build

LC = pdflatex
LFLAGS = -interaction=nonstopmode -output-directory=build -synctex=1

all: blackfly-requirements.pdf

blackfly-requirements.pdf:
blackfly-requirements.docx:

%.docx: %.tex
	pandoc $< -o $@

%.pdf: %.tex
	$(LC) $(LFLAGS) $<
	$(LC) $(LFLAGS) $<

blackfly-requirements.tex: structure.tex header.tex \
intro.tex \
description.tex \
interface.tex \
system_features.tex \
nonfunctional.tex \
appendix.tex \
glossary.tex \
analysis_models.tex \
tbd.tex \
footer.tex
	cat $^ > $@

.PHONY: clean
clean:
	rm -rfv *.docx *.tex build/* src/*~
