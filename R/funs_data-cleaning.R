# The PTS data and journalist data are saved as .RData files, so they load into
# R with their original object names. This function lets you load an .RData file
# directly to a new object instead of bringing in the original name.
# (via https://stackoverflow.com/a/25455968/120898)
load_rdata <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}


create_panel_skeleton <- function() {
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
  # Also, because the World Bank doesn't include it in the WDI, we omit Taiwan (713)

  panel_skeleton <- state_panel(1995, 2014, partial = "any") %>%
    # Remove microstates
    filter(!(gwcode %in% microstates$gwcode)) %>%
    # Remove Taiwan, the Bahamas, Belize, and Brunei
    filter(!(gwcode %in% c(713, 31, 80, 835))) %>%
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
    select(year = Year, gwcode, PTS) %>%
    mutate(PTS_factor = factor(PTS, levels = 1:5, labels = paste0("Level ", 1:5)))

  return(pts_clean)
}


load_clean_journalists <- function(path) {
  gh_journalists_raw <- load_rdata(path) %>% as_tibble()

  gh_journalists <- gh_journalists_raw %>%
    select(year = Year, gwno, country = Country, in_rog, in_cpj, in_ipi, date = Date,
           perpetrator = Perpetrator, foreigner = Foreigner, perp_cat = perp.cat, pts,
           intra, inter, international, minor, major, armedconf)

  # Count of perpetrators
  killings_perp <- gh_journalists %>%
    mutate(perp_cat = recode(perp_cat, "non-state" = "polgroup")) %>%
    group_by(year, gwno, perp_cat) %>%
    summarize(n = n()) %>%
    pivot_wider(names_from = perp_cat, values_from = n)

  # Count of foreign vs. local journalist
  killings_foreigner <- gh_journalists %>%
    mutate(foreigner = recode(foreigner, "no" = "local", "yes" = "foreign")) %>%
    group_by(year, gwno, foreigner) %>%
    summarize(n = n()) %>%
    pivot_wider(names_from = foreigner, values_from = n)

  # Combine everything
  killings_all <- gh_journalists %>%
    group_by(year, gwno) %>%
    summarize(alljourn = n()) %>%
    left_join(killings_perp, by = c("year", "gwno")) %>%
    left_join(killings_foreigner, by = c("year", "gwno")) %>%
    mutate_at(vars(-year, -gwno), ~coalesce(., 0L)) %>%  # Replace all NAs with 0
    mutate(stateunknown = state + unknown) %>%
    rename_at(vars(-year, -gwno), ~paste0("gh_", .)) %>%
    rename(gwcode = gwno)

  return(killings_all)
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
    filter(year >= 1995) %>%
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

           # Democracy
           v2x_polyarchy, v2x_regime_amb,
    ) %>%
    mutate(gwcode = countrycode(cowcode, origin = "cown", destination = "gwn",
                                custom_match = c("403" = 403L, "591" = 591L,
                                                 "679" = 678L, "935" = 935L,
                                                 "816" = 816L))) %>%
    select(-country_name, -cowcode)

  return(vdem_clean)
}


load_clean_wdi <- function(skeleton) {
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


combine_data <- function(skeleton, chaudhry_clean, pts_clean, killings_all,
                         ucdp_prio_clean, vdem_clean, un_pop, un_gdp) {
  # Only look at countries in Suparna's data
  chaudhry_countries <- chaudhry_clean %>% distinct(gwcode)

  panel_skeleton <- skeleton$panel_skeleton %>%
    filter(gwcode %in% chaudhry_countries$gwcode)

  panel_done <- panel_skeleton %>%
    left_join(un_gdp, by = c("gwcode", "year")) %>%
    left_join(un_pop, by = c("gwcode", "year")) %>%
    mutate(gdpcap = un_gdp / population,
           gdpcap_log = log(gdpcap),
           population_log = log(population)) %>%
    left_join(chaudhry_clean, by = c("gwcode", "year")) %>%
    # Indicator for Chaudhry data coverage
    # Chaudhry's Serbia data starts with 2006 and doesn't include pre-2006 stuff,
    # so we mark those as false. Also, Chaudhry starts in 1992 for Russia and 1993
    # for Czechia, so we mark those as false too
    mutate(laws = year %in% 1990:2014) %>%
    mutate(laws = case_when(
      gwcode == 345 & year <= 2005 ~ FALSE,  # Serbia
      # gwcode == 316 & year <= 1992 ~ FALSE,  # Czechia
      # gwcode == 365 & year <= 1991 ~ FALSE,  # Russia
      TRUE ~ laws  # Otherwise, use FALSE
    )) %>%
    left_join(pts_clean, by = c("gwcode", "year")) %>%
    left_join(killings_all, by = c("gwcode", "year")) %>%
    mutate_at(vars(starts_with("gh")), ~coalesce(., 0L)) %>%
    left_join(vdem_clean, by = c("gwcode", "year")) %>%
    left_join(ucdp_prio_clean, by = c("gwcode", "year")) %>%
    mutate(armed_conflict = coalesce(armed_conflict, FALSE),
           armed_conflict_num = as.numeric(armed_conflict))


  testthat::expect_equal(nrow(panel_skeleton),
                         nrow(panel_done))

  return(panel_done)
}


lag_data <- function(df) {
  panel_lagged <- df %>%
    group_by(gwcode) %>%
    # Lag all the time-varying variables
    mutate_at(vars(starts_with("advocacy"), starts_with("entry"),
                   starts_with("funding"), starts_with("barriers"),
                   starts_with("population"), starts_with("PTS"),
                   starts_with("gh"), starts_with("v2"), v2x_polyarchy,
                   starts_with("armed_"), starts_with("gdp")),
              list(lag1 = ~lag(., 1), lag2 = ~lag(., 2), lag3 = ~lag(., 3),
                   lag4 = ~lag(., 4), lag5 = ~lag(., 5))) %>%
    # Lead PTS in case we want to use PTS_lead1 instead of IVs_lag1
    mutate_at(vars(PTS, PTS_factor),
              list(lead1 = ~lead(., 1), lead2 = ~lead(., 2), lead3 = ~lead(., 3),
                   lead4 = ~lead(., 4), lead5 = ~lead(., 5))) %>%
    ungroup()

  return(panel_lagged)
}


trim_data <- function(df) {
  df %>% filter(year >= 2000)
}


load_world_map <- function(path) {
  world_map <- read_sf(path) %>%
    filter(ISO_A3 != "ATA")

  return(world_map)
}
