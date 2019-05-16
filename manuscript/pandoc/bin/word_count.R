suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(stringi))

ms_raw <- read_html(here("manuscript", "manuscript.html"))

# Extract just the article, ignoring title, abstract, etc.
ms <- ms_raw %>% 
  html_nodes("article") 

# Get rid of figures, tables, and math
xml_remove(ms %>% html_nodes("figure"))
xml_remove(ms %>% html_nodes("table"))
xml_remove(ms %>% html_nodes(".display"))  # Block math
xml_replace(ms %>% html_nodes(".inline"), read_xml("<span>MATH</span>")) %>% 
  invisible()

# Go through each child element in the article and extract it
ms_cleaned_list <- map(html_children(ms), ~ {
  .x %>% 
    html_text(trim = TRUE) %>% 
    # ICU counts hyphenated words as multiple words, so replace - with DASH
    str_replace_all("\\-", "DASH") %>% 
    # ICU also counts / as multiple words, so URLs go crazy. Replace / with SLASH
    str_replace_all("\\/", "SLASH") %>% 
    # ICU *also* counts [things] in bracketss multiple times, so kill those too
    str_replace_all("\\[|\\]", "") %>% 
    # Other things to ignore
    str_replace_all("×", "")
  })

# Get count of words! (close enough to Word)
cat(scales::comma(sum(stri_count_words(ms_cleaned_list))), "words in manuscript\n")

# Word counts in each paragraph
# stri_count_words(ms_cleaned_list)

# Export intermediate Word file for comparison
# paste(ms_cleaned_list, collapse = "\n\n") %>%
#   pander::Pandoc.convert(text = ., format = "docx")
