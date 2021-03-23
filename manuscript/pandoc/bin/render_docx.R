#!/usr/bin/env Rscript

args <- R.utils::cmdArgs()

# Check arguments
stopifnot("Specify an input file using -input\n(e.g. `-input manuscript.Rmd`)" = is.character(args$input),
          "Specify an output file using -output\n(e.g. `-output manuscript.docx`)" = is.character(args$output),
          "Specify a CSL file using -csl\n(e.g. `-csl pandoc/csl/apa.csl`)" = is.character(args$csl))

pandoc_suport <- ifelse(is.null(args$pandoc), "pandoc", args$pandoc)

# Knit
rmarkdown::render(
  input = args$input,
  output_file = args$output,
  bookdown::word_document2(
    reference_docx = paste0(pandoc_suport, "/templates/ath-manuscript.docx"),
    pandoc_args = c(paste0("--csl=", args$csl)),
    md_extensions = "+raw_tex+smart-autolink_bare_uris",
    toc = FALSE,
    number_sections = FALSE
  )
)
