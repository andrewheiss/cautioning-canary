#!/usr/bin/env Rscript

args <- R.utils::cmdArgs()

# Check arguments
stopifnot("Specify an input file using -input\n(e.g. `-input manuscript.Rmd`)" = is.character(args$input),
          "Specify an output file using -output\n(e.g. `-output manuscript.html`)" = is.character(args$output),
          "Specify a CSL file using -csl\n(e.g. `-csl pandoc/csl/apa.csl`)" = is.character(args$csl))

pandoc_suport <- ifelse(is.null(args$pandoc), "pandoc", args$pandoc)

# Add CSS file as a dependency so it goes into the _files directory
dep <- htmltools::htmlDependency(
  name = "ath",
  version = "1.0.0",
  paste0(pandoc_suport, "/css"),
  stylesheet = "ath-clean.css"
)
extra_dependencies <- list(dep)

# Knit
rmarkdown::render(
  input = args$input,
  output_file = args$output,
  bookdown::html_document2(
    template = paste0(pandoc_suport, "/templates/html.html"),
    pandoc_args = c("--metadata", "link-citations=true",
                    "--metadata", "linkReferences=true",
                    paste0("--csl=", args$csl)),
    md_extensions = "+raw_tex+smart-autolink_bare_uris+ascii_identifiers",
    toc = TRUE,
    number_sections = FALSE,
    self_contained = FALSE,
    theme = NULL,
    extra_dependencies = extra_dependencies
  )
)
