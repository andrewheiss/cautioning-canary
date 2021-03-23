# Put this Makefile and the accompanying pandoc support folder in the same
# directory as the paper you're writing. You can use it to create .html, .docx
# (through LibreOffice's .odt), and .pdf (through xelatex) files from your
# Markdown or R Markdown file.
#
# Instructions:
#	1. Ensure you have the following things installed:
#		- R (and these packages: tidyverse, rvest, stringi)
#		- python3
#		- LibreOffice
#		- pandoc
#		- pandoc-include (pip install pandoc-include)
#		- pandoc-crossref (brew install pandoc-crossref on macOS)
#		- bibtools (brew install bib-tools on macOS)
#		- gawk (brew install gawk on macOS)
#		- Fonts (as needed) in pandoc/fonts/
#
#	2. Set SRC and BIB_FILE to the appropriate file names
#
#	3. Change other variables in the "Modifiable variables" section as needed
#
#	4. Run one of the following:
#		- make md:		Convert R Markdown to regular Markdown
#		- make html:	Create HTML file
#		- make tex:		Create nice PDF through xelatex in TEX_DIR folder
#		- make mstex: 	Create manuscripty PDF through xelatex in TEX_DIR folder
#		- make odt: 	Create ODT file
#		- make docx:	Create Word file (through LibreOffice)
#		- make ms: 		Create manuscripty ODT file
#		- make msdocx:	Create manuscripty Word file (through LibreOffice)
#		- make bib:		Extract bibliography references to a standalone .bib file
#		- make count:	Count the words in the manuscript
#		- make clean:	Remove all output files
#
#	    You can also combine these: e.g. `make html tex mstex docx bib count`
#
# By default, all targets run `make clean` beforehand. You can remove that to
# rely on make's timestamp checking. However, if you do this, you'll need to add
# all the document's images, etc. as dependencies, which means it might be
# easier to just clean and delete everything every time you rebuild.


# ----------------------
# Modifiable variables
# ----------------------
# Main document (either .md or .Rmd)
SRC = manuscript.Rmd

# Output directory (use . if using the current directory)
# Do NOT:
# - include a trailng slash
# - include an initial slash (unless you're really using systemwide absolute paths)
# - set this to /
OUT_DIR = output

# Bibliography file
BIB_FILE = bibliography.bib

# Move all figures and tables to the end (only happens in mstex target)
ENDFLOAT = FALSE

# Remove identifying information (only happens in mstex and msdocx targets)
# Use pandoc/bin/replacements.csv to map identifying information to anonymous output
ANONYMIZED = FALSE

# Add version control information in footer (only happens in tex target)
VC_ENABLE = FALSE

# CSL stylesheet
# Download CSL files from https://github.com/citation-style-language/styles
# These are included in pandoc/csl/:
#	- american-political-science-association
#	- chicago-fullnote-bibliography
#	- chicago-fullnote-no-bib
#	- chicago-syllabus-no-bib
#	- apa
#	- apsa-no-bib
CSL = chicago-author-date

# LaTeX doesn't use pandoc-citeproc + CSL and instead lets biblatex handle the
# heavy lifting. There are three possible styles built in to the template:
#	- bibstyle-chicago-notes
#	- bibstyle-chicago-authordate
#	- bibstyle-apa
TEX_BIBSTYLE = bibstyle-chicago-authordate


# ----------------
# Pandoc options
# ----------------
#
# You shouldn't really have to change anything here
#
# Location of Pandoc support folder
PREFIX = pandoc

# Move figures and tables to the end
ifeq ($(ENDFLOAT), TRUE)
	ENDFLOAT_PANDOC = -V endfloat
else
	ENDFLOAT_PANDOC =
endif

# Anonymize stuff if needed
ifeq ($(ANONYMIZED), TRUE)
	ANONYMIZE = | $(PREFIX)/bin/accecare.py $(PREFIX)/bin/replacements.csv
else
	ANONYMIZE =
endif

# Enable fancy version control footers if needed
ifeq ($(VC_ENABLE), TRUE)
	VC_COMMAND = cd $(PREFIX)/bin && ./vc
	VC_PANDOC = -V pagestyle=athgit -V vc
else
	VC_COMMAND =
	VC_PANDOC =
endif


# --------------------
# Target definitions
# --------------------
MD_EXT = $(suffix $(SRC))
BASE = $(basename $(SRC))

# Targets
HTML=$(OUT_DIR)/$(SRC:$(MD_EXT)=.html)
PDF=$(OUT_DIR)/$(SRC:$(MD_EXT)=.pdf)
MSPDF=$(OUT_DIR)/$(SRC:$(MD_EXT)=-ms.pdf)
DOCX=$(OUT_DIR)/$(SRC:$(MD_EXT)=.docx)
BIB=$(OUT_DIR)/$(SRC:$(MD_EXT)=.bib)

.PHONY: clean count

## all	:	Convert manuscript to Markdown, HTML, .odt, .docx, manuscripty .odt,
## 		manuscripty .docx, and PDF *and* extract all citations into a standalone
## 		.bib file
all:	clean $(HTML) $(PDF) $(DOCX) $(BIB) count

## html	:	Convert manuscript to HTML
html:	$(HTML)

## pdf	:	Convert manuscript to PDF (through XeLaTeX)
pdf:	$(PDF)

## mspdf	:	Convert manuscript to manuscripty PDF (through XeLaTeX)
mspdf:	$(MSPDF)

## docx	:	Convert manuscript to .docx
docx:	$(DOCX)

## bib	:	Extract bibliography to standalone .bib file
bib:	$(BIB)


# --------------------
# Build instructions
# --------------------
# order-only-prerequisite to make the output folder if it doesn't exist
# https://stackoverflow.com/a/38525905/120898
$(OUT_DIR):
	mkdir -p $@

$(OUT_DIR)/%.html:	%.Rmd | $(OUT_DIR)
	@echo "$(WARN_COLOR)Converting R Markdown to HTML...$(NO_COLOR)"
	Rscript ${PREFIX}/bin/render_html.R -input $< -output $@ -csl $(PREFIX)/csl/$(CSL).csl
	@echo "$(OK_COLOR)All done!$(NO_COLOR)"
	@echo

$(OUT_DIR)/%.pdf:	%.Rmd | $(OUT_DIR)
	$(VC_COMMAND)
	@echo "$(WARN_COLOR)Converting R Markdown to PDF through XeLaTeX using hikma-article template...$(NO_COLOR)"
	Rscript ${PREFIX}/bin/render_pdf.R -input $< -output $@ -bibstyle $(TEX_BIBSTYLE)
	@echo "$(OK_COLOR)All done!$(NO_COLOR)"
	@echo

$(OUT_DIR)/%-ms.pdf:	%.Rmd | $(OUT_DIR)
	$(VC_COMMAND)
	@echo "$(WARN_COLOR)Converting R Markdown to PDF through XeLaTeX using manuscripty template...$(NO_COLOR)"
	Rscript ${PREFIX}/bin/render_pdf-ms.R -input $< -output $@ -bibstyle $(TEX_BIBSTYLE)
	@echo "$(OK_COLOR)All done!$(NO_COLOR)"
	@echo

$(OUT_DIR)/%.docx: %.Rmd | $(OUT_DIR)
	@echo "$(WARN_COLOR)Converting R Markdown to .docx...$(NO_COLOR)"
	Rscript ${PREFIX}/bin/render_docx.R -input $< -output $@ -csl $(PREFIX)/csl/$(CSL).csl
	@echo "$(OK_COLOR)All done!$(NO_COLOR)"
	@echo

$(OUT_DIR)/%.bib: %.Rmd | $(OUT_DIR)
	@echo "$(WARN_COLOR)Extracing all citations into a standalone .bib file...$(NO_COLOR)"
	python ${PREFIX}/bin/bib_extract.py --bibtex_file $(BIB_FILE) --bibtools_resource ${PREFIX}/bin/bibtool.rsc $< $@
	@echo

## count	:	Get Word-like word count
count: html
	@echo "$(WARN_COLOR)Count manuscript words like Word...$(NO_COLOR)"
	Rscript ${PREFIX}/bin/word_count.R $(OUT_DIR)/$(BASE).html
	@echo

## clean	:	Delete all manuscript-related targets
clean:
	@echo "$(WARN_COLOR)Deleting all existing targets...$(NO_COLOR)"
	rm -rf  $(OUT_DIR)/$(addsuffix .html, $(BASE)) $(OUT_DIR)/$(addsuffix _files, $(BASE)) \
	$(OUT_DIR)/$(addsuffix .pdf, $(BASE)) $(OUT_DIR)/$(addsuffix -ms.pdf, $(BASE)) \
	$(OUT_DIR)/$(addsuffix .docx, $(BASE)) $(OUT_DIR)/$(addsuffix .bib, $(BASE))
	@echo

# Self-documenting Makefiles from The Carpentries
# https://swcarpentry.github.io/make-novice/08-self-doc/index.html
## help	:	Show possible targets
.PHONY: help
help: Makefile
	@sed -n 's/^##//p' $<


# -------------------
# Color definitions
# -------------------
NO_COLOR    = \x1b[0m
BOLD_COLOR	= \x1b[37;01m
OK_COLOR    = \x1b[32;01m
WARN_COLOR  = \x1b[33;01m
ERROR_COLOR = \x1b[31;01m
