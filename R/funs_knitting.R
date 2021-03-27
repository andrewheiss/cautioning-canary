render_html <- function(input, output, csl) {
  # Add CSS file as a dependency so it goes into the _files directory
  dep <- htmltools::htmlDependency(
    name = "ath",
    version = "1.0.0",
    "pandoc/css",
    stylesheet = "ath-clean.css"
  )
  extra_dependencies <- list(dep)

  rmarkdown::render(
    input = input,
    output_file = output,
    bookdown::html_document2(
      template = "pandoc/templates/html.html",
      pandoc_args = c("--metadata", "link-citations=true",
                      "--metadata", "linkReferences=true",
                      paste0("--csl=", csl)),
      md_extensions = "+raw_tex+smart-autolink_bare_uris+ascii_identifiers",
      toc = TRUE,
      number_sections = FALSE,
      self_contained = FALSE,
      theme = NULL,
      extra_dependencies = extra_dependencies
    )
  )
}

render_pdf <- function(input, output, bibstyle) {
  rmarkdown::render(
    input = input,
    output_file = output,
    bookdown::pdf_document2(
      template = "pandoc/templates/xelatex.tex",
      latex_engine = "xelatex",
      dev = "cairo_pdf",
      pandoc_args = c("--top-level-division=section",
                      "--shift-heading-level-by=0",
                      "-V", bibstyle,
                      "-V", "chapterstyle=hikma-article"),
      md_extensions = "+raw_tex+smart-autolink_bare_uris",
      toc = FALSE,
      keep_tex = FALSE,
      citation_package = "biblatex"
    )
  )
}

render_pdf_ms <- function(input, output, bibstyle) {
  rmarkdown::render(
    input = input,
    output_file = output,
    bookdown::pdf_document2(
      template = "pandoc/templates/xelatex-manuscript.tex",
      latex_engine = "xelatex",
      dev = "cairo_pdf",
      pandoc_args = c("--top-level-division=section",
                      "--shift-heading-level-by=0",
                      "-V", bibstyle),
      md_extensions = "+raw_tex+smart-autolink_bare_uris",
      toc = FALSE,
      keep_tex = FALSE,
      citation_package = "biblatex"
    )
  )
}

render_docx <- function(input, output, csl) {
  rmarkdown::render(
    input = input,
    output_file = output,
    bookdown::word_document2(
      reference_docx = "pandoc/templates/ath-manuscript.docx",
      pandoc_args = c(paste0("--csl=", csl)),
      md_extensions = "+raw_tex+smart-autolink_bare_uris",
      toc = FALSE,
      number_sections = FALSE
    )
  )
}
