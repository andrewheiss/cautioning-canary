library(scales)
library(countrycode)
library(readxl)
library(haven)
library(jsonlite)
suppressPackageStartupMessages(library(sf))

# The PTS data is saved as an .RData file, so it loads into R with its original
# object name. This function lets you load an .RData file directly to a new
# object instead of bringing in the original name. (via
# https://stackoverflow.com/a/25455968/120898)
load_rdata <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}


create_panel_skeleton <- function() {
  library(states)

  microstates <- gwstates %>%
    filter(microstate) %>% distinct(gwcode)

  # In both COW and GW codes, modern Vietnam is 816, but countrycode() thinks the
  # COW code is 817, which is old South Vietnam (see issue
  # https://github.com/vincentarelbundock/countrycode/issues/16), so we use
  # custom_match to force 816 to recode to 816
  #
  # Also, following Gleditsch and Ward, we treat Serbia after 2006 dissolution of
  # Serbia & Montenegro as 345 in COW codes (see
  # https://www.andybeger.com/states/articles/differences-gw-cow.html)
  #
  # Following V-Dem, we treat Czechoslovakia (GW/COW 315) and Czech Republic
  # (GW/COW 316) as the same continuous country (V-Dem has both use ID 157).
  #
  # Also, because the World Bank doesn't include it in the WDI, we omit
  # Taiwan (713). We also omit East Germany (265) and South Yemen (680).

  panel_skeleton <- state_panel(1990, 2014, partial = "any") %>%
    # Remove microstates
    filter(!(gwcode %in% microstates$gwcode)) %>%
    # Deal with Czechia
    mutate(gwcode = recode(gwcode, `315` = 316L)) %>%
    # Remove East Germany, South Yemen, Taiwan, the Bahamas, Belize, and Brunei
    filter(!(gwcode %in% c(265, 680, 713, 31, 80, 835))) %>%
    mutate(cowcode = countrycode(gwcode, origin = "gwn", destination = "cown",
                                 custom_match = c("816" = 816L, "340" = 345L)),
           country = countrycode(cowcode, origin = "cown", destination = "country.name",
                                 custom_match = c("678" = "Yemen")),
           iso2 = countrycode(cowcode, origin = "cown", destination = "iso2c",
                              custom_match = c("345" = "RS", "347" = "XK", "678" = "YE")),
           iso3 = countrycode(cowcode, origin = "cown", destination = "iso3c",
                              custom_match = c("345" = "SRB", "347" = "XKK", "678" = "YEM")),
           # Use 999 as the UN country code for Kosovo
           un = countrycode(cowcode, origin = "cown", destination = "un",
                            custom_match = c("345" = 688, "347" = 999, "678" = 887))) %>%
    # There are two entries for "Yugoslavia" in 2006 after recoding 340 as 345;
    # get rid of one
    filter(!(gwcode == 340 & cowcode == 345 & year == 2006)) %>%
    # Make Serbia 345 in GW codes too, for joining with other datasets
    mutate(gwcode = recode(gwcode, `340` = 345L)) %>%
    mutate(country = recode(country, `Yugoslavia` = "Serbia")) %>%
    arrange(gwcode, year)

  skeleton_lookup <- panel_skeleton %>%
    group_by(gwcode, cowcode, country, iso2, iso3, un) %>%
    summarize(years_included = n()) %>%
    ungroup() %>%
    arrange(country)

  return(list(panel_skeleton = panel_skeleton,
              microstates = microstates,
              skeleton_lookup = skeleton_lookup))
}


load_clean_chaudhry <- function(path) {
  regulations <- tribble(
    ~question, ~barrier,       ~question_clean,                  ~ignore_in_index,
    "q1a",     "association",  "const_assoc",                    TRUE,
    "q1b",     "association",  "political_parties",              TRUE,
    "q2a",     "entry",        "ngo_register",                   TRUE,
    "q2b",     "entry",        "ngo_register_burden",            FALSE,
    "q2c",     "entry",        "ngo_register_appeal",            FALSE,
    "q2d",     "entry",        "ngo_barrier_foreign_funds",      FALSE,
    "q3a",     "funding",      "ngo_disclose_funds",             TRUE,
    "q3b",     "funding",      "ngo_foreign_fund_approval",      FALSE,
    "q3c",     "funding",      "ngo_foreign_fund_channel",       FALSE,
    "q3d",     "funding",      "ngo_foreign_fund_restrict",      FALSE,
    "q3e",     "funding",      "ngo_foreign_fund_prohibit",      FALSE,
    "q3f",     "funding",      "ngo_type_foreign_fund_prohibit", FALSE,
    "q4a",     "advocacy",     "ngo_politics",                   FALSE,
    "q4b",     "advocacy",     "ngo_politics_intimidation",      TRUE,
    "q4c",     "advocacy",     "ngo_politics_foreign_fund",      FALSE
  )

  # Chaudhry restrictions
  # In this data Sudan (625) splits into North Sudan (626) and South Sudan (525)
  # in 2011, but in the other datasets regular Sudan stays 625 and South Sudan
  # becomes 626, so adjust the numbers here
  #
  # Also, Chad is in the dataset, but all values are missing, so we drop it
  chaudhry_raw <- read_dta(path) %>%
    filter(ccode != 483) %>%  # Remove Chad
    mutate(ccode = case_when(
      scode == "SSU" ~ 626,
      scode == "SDN" ~ 625,
      TRUE ~ ccode
    )) %>%
    mutate(gwcode = countrycode(ccode, origin = "cown", destination = "gwn",
                                custom_match = c("679" = 678L, "818" = 816L,
                                                 "342" = 345L, "341" = 347L,
                                                 "348" = 341L, "315" = 316L)))

  chaudhry_2014 <- expand_grid(gwcode = unique(chaudhry_raw$gwcode),
                               year = 2014)

  chaudhry_long <- chaudhry_raw %>%
    # Bring in 2014 rows
    bind_rows(chaudhry_2014) %>%
    # Ethiopia and Czech Republic have duplicate rows in 1993 and 1994 respectively, but
    # the values are identical, so just keep the first of the two
    group_by(gwcode, year) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(gwcode, year) %>%
    # Reverse values for q2c
    mutate(q2c = 1 - q2c) %>%
    # Rescale 2-point questions to 0-1 scale
    mutate_at(vars(q3e, q3f, q4a), ~rescale(., to = c(0, 1), from = c(0, 2))) %>%
    # q2d and q4c use -1 to indicate less restriction/burdensomeness. Since we're
    # concerned with an index of restriction, we make the negative values zero
    mutate_at(vars(q2d, q4c), ~ifelse(. == -1, 0, .)) %>%
    pivot_longer(cols = starts_with("q"), names_to = "question") %>%
    left_join(regulations, by = "question") %>%
    group_by(gwcode) %>%
    mutate(all_missing = all(is.na(value))) %>%
    group_by(gwcode, question) %>%
    # Bring most recent legislation forward in time
    fill(value) %>%
    # For older NA legislation that can't be brought forward, set sensible
    # defaults. Leave countries that are 100% 0 as NA.
    mutate(value = ifelse(!all_missing & is.na(value), 0, value)) %>%
    ungroup()

  chaudhry_registration <- chaudhry_long %>%
    select(gwcode, year, question_clean, value) %>%
    pivot_wider(names_from = "question_clean", values_from = "value")

  chaudhry_summed <- chaudhry_long %>%
    filter(!ignore_in_index) %>%
    group_by(gwcode, year, barrier) %>%
    summarize(total = sum(value)) %>%
    ungroup()

  chaudhry_clean <- chaudhry_summed %>%
    pivot_wider(names_from = barrier, values_from = total) %>%
    mutate_at(vars(entry, funding, advocacy),
              list(std = ~. / max(., na.rm = TRUE))) %>%
    mutate(barriers_total = advocacy + entry + funding,
           barriers_total_std = advocacy_std + entry_std + funding_std) %>%
    left_join(chaudhry_registration, by = c("gwcode", "year"))

  return(chaudhry_clean)
}


load_clean_pts <- function(path, skeleton) {
  # 260: West Germany
  # 678/680: North/South Yemen. COW uses 679 for modern Yemen; GW uses 678
  # 946: Kiribati
  # 947: Tuvalu
  # 955: Tonga

  pts_raw <- load_rdata(path) %>% as_tibble()

  pts_clean <- pts_raw %>%
    filter(!(COW_Code_N %in% c(skeleton$microstates$gwcode,
                               260, 678, 680, 946, 947, 955))) %>%
    # Get rid of Soviet-era Yugoslavia, combine Yugoslavia/Serbia and Montenegro
    # (1991-2006) with Serbia (2007-2018)
    filter(Country_OLD != "Yugoslavia",
           !(Country_OLD == "Serbia" & Year <= 2006),
           !(Country_OLD == "Yugoslavia/Serbia and Montenegro" & Year >= 2007)) %>%
    mutate(COW_Code_N = ifelse(Country_OLD == "Serbia", 345, COW_Code_N)) %>%
    # Use the State Department score unless it's missing, then use Amnesty's
    mutate(PTS = coalesce(PTS_S, PTS_A)) %>%
    mutate(gwcode = countrycode(COW_Code_N, origin = "cown", destination = "gwn",
                                custom_match = c("679" = 678L, "816" = 816L))) %>%
    # Get rid of things like the EU, USSR, Crimea, Palestine, and Western Sahara
    filter(!is.na(gwcode)) %>%
    select(year = Year, gwcode, PTS) %>%
    mutate(PTS_factor = factor(PTS, levels = 1:5,
                               labels = paste0("Level ", 1:5),
                               ordered = TRUE))

  return(pts_clean)
}


load_clean_ucdp <- function(path) {
  ucdp_prio_raw <- read_csv(path, col_types = cols())

  ucdp_prio_clean <- ucdp_prio_raw %>%
    mutate(gwcode_raw = str_split(gwno_a, pattern = ", ")) %>%
    unnest(gwcode_raw) %>%
    mutate(gwcode = as.integer(gwcode_raw)) %>%
    group_by(gwcode, year) %>%
    summarize(armed_conflict = sum(intensity_level) > 0) %>%
    ungroup()

  return(ucdp_prio_clean)
}


load_clean_vdem <- function(path) {
  # 403: Sao Tome and Principe
  # 591: Seychelles
  # 679: Yemen (change to 678 for GW)
  # 935: Vanuatu

  vdem_raw <- read_rds(path) %>% as_tibble()

  vdem_clean <- vdem_raw %>%
    filter(year >= 1990) %>%
    select(country_name, year, cowcode = COWcode,

           # Civil society stuff
           v2cseeorgs,  # CSO entry and exit
           v2csreprss,  # CSO repression
           v2cscnsult,  # CSO consultation
           v2csprtcpt,  # CSO participatory environment
           v2csgender,  # CSO women's participation
           v2csantimv,  # CSO anti-system movements
           v2xcs_ccsi,  # Core civil society index (entry/exit, repression, participatory env)

           # Freedom of expression stuff
           v2x_freexp,  # Freedom of expression index
           v2x_freexp_altinf,  # Freedom of expression and alternative sources of information index
           v2clacfree,  # Freedom of academic and cultural expression
           v2meslfcen,  # Media self-censorship

           # Repression stuff
           v2csrlgrep,  # Religious organization repression
           v2mecenefm,  # Govt censorship effort - media
           # v2mecenefi,  # Internet censorship effort
           v2meharjrn,  # Harassment of journalists

           # Rights indexes
           v2x_civlib,  # Civil liberties index
           v2x_clphy,   # Physical violence index
           v2x_clpriv,  # Private civil liberties index
           v2x_clpol,   # Political civil liberties index

           # Democracy and governance stuff
           v2x_polyarchy,  # Polyarchy index (for electoral democracies)
           v2x_libdem,     # Liberal democracy index (for democracies in general)
           v2x_regime,     # Regimes of the world
           v2x_corr,       # Political corruption index
           v2x_rule,       # Rule of law index
    ) %>%
    # Get rid of East Germany
    filter(cowcode != 265) %>%
    mutate(gwcode = countrycode(cowcode, origin = "cown", destination = "gwn",
                                custom_match = c("403" = 403L, "591" = 591L,
                                                 "679" = 678L, "935" = 935L,
                                                 "816" = 816L, "260" = 260L,
                                                 "315" = 316L))) %>%
    # Get rid of Hong Kong, Palestine (West Bank and Gaza), and Somaliland
    filter(!is.na(cowcode)) %>%
    select(-country_name, -cowcode)

  return(vdem_clean)
}


load_clean_wdi <- function(skeleton) {
  library(WDI)

  # World Bank World Development Indicators (WDI)
  # http://data.worldbank.org/data-catalog/world-development-indicators
  wdi_indicators <- c("NY.GDP.PCAP.PP.KD",  # GDP per capita, ppp (constant 2011 international $)
                      "NY.GDP.MKTP.PP.KD",  # GDP, ppp (constant 2010 international $)
                      "SP.POP.TOTL")     # Population, total

  wdi_raw <- WDI(country = "all", wdi_indicators, extra = TRUE, start = 1990, end = 2018)

  wdi_clean <- wdi_raw %>%
    filter(iso2c %in% unique(skeleton$panel_skeleton$iso2)) %>%
    mutate_at(vars(income, region), as.character) %>%  # Don't use factors
    mutate(gwcode = countrycode(iso2c, origin = "iso2c", destination = "gwn",
                                custom_match = c("YE" = 678L, "XK" = 347L,
                                                 "VN" = 816L, "RS" = 345L))) %>%
    mutate(region = ifelse(gwcode == 343, "Europe & Central Asia", region),
           income = ifelse(gwcode == 343, "Upper middle income", income)) %>%
    select(country, gwcode, year, region, income, population = SP.POP.TOTL)

  return(wdi_clean)
}


# Population
# Total Population - Both Sexes
# https://population.un.org/wpp/Download/Standard/Population/
load_clean_un_pop <- function(path, skeleton, wdi) {
  # The UN doesn't have population data for Kosovo, so we use WDI data for that
  kosovo_population <- wdi %>%
    select(gwcode, year, population) %>%
    filter(gwcode == 347, year >= 2008)

  un_pop_raw <- read_excel(path, skip = 16)

  un_pop <- un_pop_raw %>%
    filter((`Country code` %in% unique(skeleton$panel_skeleton$un))) %>%
    select(-c(Index, Variant, Notes, `Region, subregion, country or area *`,
              `Parent code`, Type),
           un_code = `Country code`) %>%
    pivot_longer(names_to = "year", values_to = "population", -un_code) %>%
    mutate(gwcode = countrycode(un_code, "un", "gwn",
                                custom_match = c("887" = 678, "704" = 816, "688" = 345))) %>%
    mutate(year = as.integer(year),
           population = as.numeric(population) * 1000) %>%  # Values are in 1000s
    select(gwcode, year, population) %>%
    bind_rows(kosovo_population)

  return(un_pop)
}


load_clean_un_gdp <- function(path_constant, path_current, skeleton) {
  # GDP by Type of Expenditure at constant (2015) prices - US dollars
  # http://data.un.org/Data.aspx?q=gdp&d=SNAAMA&f=grID%3a102%3bcurrID%3aUSD%3bpcFlag%3a0
  un_gdp_raw <- read_csv(path_constant, col_types = cols()) %>%
    rename(country = `Country or Area`) %>%
    mutate(value_type = "Constant")

  # GDP by Type of Expenditure at current prices - US dollars
  # http://data.un.org/Data.aspx?q=gdp&d=SNAAMA&f=grID%3a101%3bcurrID%3aUSD%3bpcFlag%3a0
  un_gdp_current_raw <- read_csv(path_current, col_types = cols()) %>%
    rename(country = `Country or Area`) %>%
    mutate(value_type = "Current")

  un_gdp <- bind_rows(un_gdp_raw, un_gdp_current_raw) %>%
    filter(Item %in% c("Gross Domestic Product (GDP)",
                       "Exports of goods and services",
                       "Imports of goods and services")) %>%
    filter(!(country %in% c("Former USSR", "Former Netherlands Antilles",
                            "Yemen: Former Democratic Yemen",
                            "United Republic of Tanzania: Zanzibar"))) %>%
    filter(!(country == "Yemen: Former Yemen Arab Republic" & Year >= 1989)) %>%
    filter(!(country == "Former Czechoslovakia" & Year >= 1990)) %>%
    filter(!(country == "Former Yugoslavia" & Year >= 1990)) %>%
    filter(!(country == "Former Ethiopia" & Year >= 1990)) %>%
    mutate(country = recode(country,
                            "Former Sudan" = "Sudan",
                            "Yemen: Former Yemen Arab Republic" = "Yemen",
                            "Former Czechoslovakia" = "Czechia",
                            "Former Yugoslavia" = "Serbia")) %>%
    mutate(iso3 = countrycode(country, "country.name", "iso3c",
                              custom_match = c("Kosovo" = "XKK"))) %>%
    left_join(select(skeleton$skeleton_lookup, iso3, gwcode), by = "iso3") %>%
    filter(!is.na(gwcode))

  un_gdp_wide <- un_gdp %>%
    select(gwcode, year = Year, Item, Value, value_type) %>%
    pivot_wider(names_from = c(value_type, Item), values_from = Value) %>%
    rename(exports_constant_2015 = `Constant_Exports of goods and services`,
           imports_constant_2015 = `Constant_Imports of goods and services`,
           gdp_constant_2015 = `Constant_Gross Domestic Product (GDP)`,
           exports_current = `Current_Exports of goods and services`,
           imports_current = `Current_Imports of goods and services`,
           gdp_current = `Current_Gross Domestic Product (GDP)`) %>%
    mutate(gdp_deflator = gdp_current / gdp_constant_2015 * 100) %>%
    mutate(un_trade_pct_gdp = (imports_current + exports_current) / gdp_current)

  un_gdp_final <- un_gdp_wide %>%
    select(gwcode, year, un_trade_pct_gdp, un_gdp = gdp_constant_2015)

  return(un_gdp_final)
}


load_clean_latent_hr <- function(path, skeleton) {
  latent_hr_raw <- read_csv(path, col_types = cols())

  latent_hr <- latent_hr_raw %>%
    filter(YEAR >= 1990, COW %in% c(skeleton$skeleton_lookup$cowcode, 679),
           COW != 678) %>%
    # Deal with Yemen and Vietnam
    mutate(gwcode = countrycode(COW, "cown", "gwn",
                                custom_match = c("679" = 678L, "816" = 816))) %>%
    select(year = YEAR, gwcode,
           latent_hr_mean = theta_mean, latent_hr_sd = theta_sd)

  return(latent_hr)
}


combine_data <- function(skeleton, chaudhry_clean,
                         pts_clean, latent_hr, ucdp_prio_clean,
                         vdem_clean, un_pop, un_gdp) {
  # Only look at countries in Suparna's data
  chaudhry_countries <- chaudhry_clean %>% distinct(gwcode)

  panel_skeleton <- skeleton$panel_skeleton %>%
    filter(gwcode %in% chaudhry_countries$gwcode)

  panel_done <- panel_skeleton %>%
    left_join(un_gdp, by = c("gwcode", "year")) %>%
    left_join(un_pop, by = c("gwcode", "year")) %>%
    mutate(gdpcap = un_gdp / population,
           gdp_log = log(un_gdp),
           gdpcap_log = log(gdpcap),
           population_log = log(population)) %>%
    left_join(chaudhry_clean, by = c("gwcode", "year")) %>%
    # Indicator for Chaudhry data coverage
    # Chaudhry's Serbia data starts with 2006 and doesn't include pre-2006 stuff,
    # so we mark those as false. Also, Chaudhry starts in 1992 for Russia and 1993
    # for Czechia, so we mark those as false too
    mutate(laws = year %in% 1990:2013) %>%
    mutate(laws = case_when(
      gwcode == 345 & year <= 2005 ~ FALSE,  # Serbia
      gwcode == 316 & year <= 1992 ~ FALSE,  # Czechia
      gwcode == 365 & year <= 1991 ~ FALSE,  # Russia
      TRUE ~ laws  # Otherwise, use FALSE
    )) %>%
    left_join(pts_clean, by = c("gwcode", "year")) %>%
    left_join(latent_hr, by = c("gwcode", "year")) %>%
    left_join(vdem_clean, by = c("gwcode", "year")) %>%
    left_join(ucdp_prio_clean, by = c("gwcode", "year")) %>%
    mutate(armed_conflict = coalesce(armed_conflict, FALSE),
           armed_conflict_num = as.numeric(armed_conflict)) %>%
    mutate(pred_group = ifelse(year >= 2011, "Testing", "Training"))

  testthat::expect_equal(nrow(panel_skeleton),
                         nrow(panel_done))

  return(panel_done)
}


lag_data <- function(df) {
  panel_lagged <- df %>%
    group_by(gwcode) %>%
    # Indicate changes in laws
    mutate(across(c(advocacy, entry, funding, barriers_total),
                  list(new = ~. - lag(.),
                       worse = ~(. - lag(.)) > 0,
                       cat = ~cut(. - lag(.),
                                  breaks = c(-Inf, -1, 0, Inf),
                                  labels = c("New better law", "No new laws",
                                             "New worse law"),
                                  ordered_result = TRUE)))) %>%
    # Lag all the time-varying variables
    mutate(across(c(starts_with("advocacy"), starts_with("entry"),
                    starts_with("funding"), starts_with("barriers"),
                    starts_with("population"), starts_with("PTS"),
                    starts_with("gh"), starts_with("v2"),
                    un_trade_pct_gdp,
                    starts_with("armed_"), starts_with("gdp")),
                  list(lag1 = ~lag(., 1), lag2 = ~lag(., 2)))) %>%
    # Lead outcome variables so we can use DV_lead1 instead of IVs_lag1
    mutate(across(c(PTS, PTS_factor, v2x_clphy, v2x_clpriv, starts_with("latent_hr")),
                  list(lead1 = ~lead(., 1), lead2 = ~lead(., 2)))) %>%
    # To do fancy Bell and Jones adjustment (https://doi.org/10.1017/psrm.2014.7)
    # (aka Mundlak devices), we split explanatory variables into a meaned
    # version (\bar{x}) and a de-meaned version (x - \bar{x}) so that they
    # can explain the within-country and between-country variation.
    mutate(across(c(starts_with("advocacy"), starts_with("entry"),
                    starts_with("funding"), starts_with("barriers"),
                    starts_with("population"), starts_with("gh"),
                    starts_with("v2"), starts_with("gdp"), un_trade_pct_gdp,
                    -contains("_worse"), -contains("_cat")),  # Not the categorical stuff
                  list(between = ~mean(., na.rm = TRUE),  # Between
                       within = ~. - mean(., na.rm = TRUE)))) %>%  # Within
    ungroup()

  return(panel_lagged)
}

create_training <- function(df) {
  df %>% filter(pred_group == "Training")
}

create_testing <- function(df) {
  df %>% filter(pred_group == "Testing")
}

trim_data <- function(df) {
  df %>% filter(year < 2014)
}


load_world_map <- function(path) {
  world_map <- read_sf(path) %>%
    filter(ISO_A3 != "ATA")

  return(world_map)
}

# Civicus Monitor
# We downloaded the standalone embeddable widget
# (https://monitor.civicus.org/widgets/world/) as an HTML file with
# `wget https://monitor.civicus.org/widgets/world/` and saved it as index_2021-03-19.html
#
# We then extracted the COUNTRIES_DATA variable embedded in a <script> tag
# (xpath = /html/body/script[5]), which is JSON-ish, but not quite. jsonlite
# can't parse it for whatever reason, but some online JSON formatter and
# validator could, so we ran it through that and saved the resulting clean file
load_clean_civicus <- function(path) {
  civicus_raw <- read_json(path) %>% as_tibble() %>% slice(1)

  civicus_lookup <- tribble(
    ~value, ~category,
    1, "Closed",
    2, "Repressed",
    3, "Obstructed",
    4, "Narrowed",
    5, "Open"
  ) %>%
    mutate(category = fct_inorder(category, ordered = TRUE))

  civicus_clean <- civicus_raw %>%
    pivot_longer(everything(), names_to = "name", values_to = "value") %>%
    mutate(value = map_chr(value, ~.)) %>%
    mutate(value = parse_number(value, na = c("", "NA", "None"))) %>%
    mutate(country_name = countrycode(name, "iso3c", "country.name",
                                      custom_match = c("KOSOVO" = "XKK",
                                                       "SVT" = "VCT")),
           iso3c = countrycode(country_name, "country.name", "iso3c",
                               custom_match = c("XKK" = "Kosovo",
                                                "VCT" = "Saint Vincent and the Grenadines"))) %>%
    left_join(civicus_lookup, by = "value") %>%
    select(-name, -value, -country_name)

  return(civicus_clean)
}

create_civicus_map_data <- function(civicus, map) {
  map_with_civicus <- map %>%
    # Fix some Natural Earth ISO weirdness
    mutate(ISO_A3 = ifelse(ISO_A3 == "-99", as.character(ISO_A3_EH), as.character(ISO_A3))) %>%
    mutate(ISO_A3 = case_when(
      .$ISO_A3 == "GRL" ~ "DNK",
      .$NAME == "Norway" ~ "NOR",
      .$NAME == "Kosovo" ~ "XKK",
      TRUE ~ ISO_A3
    )) %>%
    left_join(civicus, by = c("ISO_A3" = "iso3c"))

  return(map_with_civicus)
}
