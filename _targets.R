library(targets)
library(tarchetypes)

# General variables
csl <- "pandoc/csl/apa.csl"
bibstyle <- "bibstyle-chicago-authordate"

source("R/funs_data-cleaning.R")
# source("R/funs_knitting.R")
source("R/funs_notebook.R")

options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

future::plan(future::multisession)

tar_option_set(packages = c("tidyverse", "countrycode", "states", "WDI", "here",
                            "readxl", "haven", "sf", "lubridate", "scales", "naniar",
                            "janitor", "kableExtra", "huxtable", "modelsummary",
                            "knitr", "withr", "flextable", "testthat", "DT",
                            "brms", "tidybayes", "broom", "broom.mixed", "scico",
                            "ggtext", "GGally", "colorspace", "lme4"))

list(
  # Define raw data files
  tar_target(chaudhry_raw_file,
             here("data", "raw_data", "Chaudhry restrictions", "SC_Expanded.dta"),
             format = "file"),
  tar_target(pts_raw_file,
             here("data", "raw_data", "Political Terror Scale", "PTS-2019.RData"),
             format = "file"),
  tar_target(journalists_raw_file,
             here("data", "raw_data", "Gohdes Carey journalists",
                  "journalist-data-incl-pts.RData"),
             format = "file"),
  tar_target(ucdp_raw_file,
             here("data", "raw_data", "UCDP PRIO", "ucdp-prio-acd-191.csv"),
             format = "file"),
  tar_target(vdem_raw_file,
             here("data", "raw_data", "Country_Year_V-Dem_Full+others_R_v10",
                  "V-Dem-CY-Full+Others-v10.rds"),
             format = "file"),
  tar_target(un_pop_raw_file,
             here("data", "raw_data", "UN data",
                  "WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"),
             format = "file"),
  tar_target(un_gdp_constant_raw_file,
             here("data", "raw_data", "UN data",
                  "UNdata_Export_20210118_034054729.csv"),
             format = "file"),
  tar_target(un_gdp_current_raw_file,
             here("data", "raw_data", "UN data",
                  "UNdata_Export_20210118_034311252.csv"),
             format = "file"),
  tar_target(naturalearth_raw_file,
             here("data", "raw_data", "ne_110m_admin_0_countries",
                  "ne_110m_admin_0_countries.shp"),
             format = "file"),

  # Define helper functions
  tar_target(plot_funs, here("lib", "graphics.R"), format = "file"),

  # Load and clean data
  tar_target(world_map, load_world_map(naturalearth_raw_file)),
  tar_target(skeleton, create_panel_skeleton()),
  tar_target(wdi_clean, load_clean_wdi(skeleton)),
  tar_target(chaudhry_clean, load_clean_chaudhry(chaudhry_raw_file)),
  tar_target(pts_clean, load_clean_pts(pts_raw_file, skeleton)),
  tar_target(killings_all, load_clean_journalists(journalists_raw_file)),
  tar_target(ucdp_prio_clean, load_clean_ucdp(ucdp_raw_file)),
  tar_target(vdem_clean, load_clean_vdem(vdem_raw_file)),
  tar_target(un_pop, load_clean_un_pop(un_pop_raw_file, skeleton, wdi_clean)),
  tar_target(un_gdp, load_clean_un_gdp(un_gdp_constant_raw_file,
                                       un_gdp_current_raw_file, skeleton)),
  tar_target(panel_done, combine_data(skeleton, chaudhry_clean, pts_clean,
                                      killings_all, ucdp_prio_clean, vdem_clean,
                                      un_pop, un_gdp)),
  tar_target(panel_lagged, lag_data(panel_done)),
  tar_target(panel_done_trimmed, trim_data(panel_done)),
  tar_target(panel_lagged_trimmed, trim_data(panel_lagged)),

  # Render the analysis notebook
  tar_notebook_pages()

  # tarchetypes::tar_render() automatically detects target dependencies in Rmd
  # files and knits them, but there's no easy way to pass a custom rendering
  # script like bookdown::html_document2(), so two things happen here:
  #   1. Set a file-based target with tar_target_raw() and use tar_knitr_deps()
  #      to detect the target dependencies in the Rmd file
  #   2. Use a bunch of other file-based targets to actually render the document
  #      through different custom functions
  # tar_target_raw("main_manuscript", "manuscript/manuscript.Rmd",
  #                format = "file",
  #                deps = tar_knitr_deps("manuscript/manuscript.Rmd")),
  # tar_target(rendered_html,
  #            render_html(
  #              input = main_manuscript,
  #              output = "output/manuscript.html",
  #              csl = csl),
  #            format = "file"),
  # tar_target(rendered_pdf,
  #            render_pdf(
  #              input = main_manuscript,
  #              output = "output/manuscript.pdf",
  #              bibstyle = bibstyle),
  #            format = "file"),
  # tar_target(rendered_mspdf,
  #            render_pdf_ms(
  #              input = main_manuscript,
  #              output = "output/manuscript-ms.pdf",
  #              bibstyle = bibstyle),
  #            format = "file"),
  # tar_target(rendered_docx,
  #            render_docx(
  #              input = main_manuscript,
  #              output = "output/manuscript.docx",
  #              csl = csl),
  #            format = "file")
)
