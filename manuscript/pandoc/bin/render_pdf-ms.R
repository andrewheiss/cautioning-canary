#!/usr/bin/env Rscript

args <- R.utils::cmdArgs()

# Check arguments
stopifnot("Specify an input file using -input\n(e.g. `-input manuscript.Rmd`)" = is.character(args$input),
          "Specify an output file using -output\n(e.g. `-output manuscript-ms.pdf`)" = is.character(args$output),
          "Specify one of these options using -bibstyle:
- bibstyle-chicago-notes
- bibstyle-chicago-authordate
- bibstyle-apa
(e.g. `-bibstyle bibstyle-chicago-notes`)" = is.character(args$bibstyle))

pandoc_suport <- ifelse(is.null(args$pandoc), "pandoc", args$pandoc)

rmarkdown::render(
  input = args$input,
  output_file = args$output,
  bookdown::pdf_document2(
    template = paste0(pandoc_suport, "/templates/xelatex-manuscript.tex"),
    latex_engine = "xelatex", 
    dev = "cairo_pdf",
    pandoc_args = c("--top-level-division=section", 
                    "--shift-heading-level-by=0", 
                    "-V", args$bibstyle),
    md_extensions = "+raw_tex+smart-autolink_bare_uris",
    toc = FALSE,
    keep_tex = FALSE,
    citation_package = "biblatex"
  )
)
