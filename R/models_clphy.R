# Settings ----------------------------------------------------------------

clphy_setup <- function() {
  options(worker_options)

  # Settings
  CHAINS <- 4
  ITER <- 2000
  WARMUP <- 1000
  BAYES_SEED <- 4045  # From random.org

  # Priors
  priors_vague <- c(set_prior("normal(0, 10)", class = "Intercept"),
                    set_prior("normal(0, 3)", class = "b"),
                    set_prior("cauchy(0, 1)", class = "sd"))

  return(list(chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
              priors_vague = priors_vague))
}


# Regular models ----------------------------------------------------------

f_clphy_baseline <- function(dat) {
  clphy_settings <- clphy_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clphy_lead1 ~ v2x_clphy +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clphy_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clphy_settings$chains, iter = clphy_settings$iter,
    warmup = clphy_settings$warmup, seed = clphy_settings$seed)

  return(model)
}

f_clphy_total <- function(dat) {
  clphy_settings <- clphy_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clphy_lead1 ~ barriers_total + barriers_total_lag1 +
         v2x_clphy +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clphy_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clphy_settings$chains, iter = clphy_settings$iter,
    warmup = clphy_settings$warmup, seed = clphy_settings$seed)

  return(model)
}

f_clphy_total_new <- function(dat) {
  clphy_settings <- clphy_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clphy_lead1 ~ barriers_total_new + barriers_total_new_lag1 +
         v2x_clphy +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clphy_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clphy_settings$chains, iter = clphy_settings$iter,
    warmup = clphy_settings$warmup, seed = clphy_settings$seed)

  return(model)
}

f_clphy_advocacy <- function(dat) {
  clphy_settings <- clphy_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clphy_lead1 ~ advocacy + advocacy_lag1 +
         v2x_clphy +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clphy_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clphy_settings$chains, iter = clphy_settings$iter,
    warmup = clphy_settings$warmup, seed = clphy_settings$seed)

  return(model)
}

f_clphy_entry <- function(dat) {
  clphy_settings <- clphy_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clphy_lead1 ~ entry + entry_lag1 +
         v2x_clphy +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clphy_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clphy_settings$chains, iter = clphy_settings$iter,
    warmup = clphy_settings$warmup, seed = clphy_settings$seed)

  return(model)
}

f_clphy_funding <- function(dat) {
  clphy_settings <- clphy_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clphy_lead1 ~ funding + funding_lag1 +
         v2x_clphy +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clphy_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clphy_settings$chains, iter = clphy_settings$iter,
    warmup = clphy_settings$warmup, seed = clphy_settings$seed)

  return(model)
}

f_clphy_v2csreprss <- function(dat) {
  clphy_settings <- clphy_setup()

  model <- brm(
    bf(v2x_clphy_lead1 ~ v2csreprss + v2csreprss_lag1 +
         v2x_clphy +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clphy_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clphy_settings$chains, iter = clphy_settings$iter,
    warmup = clphy_settings$warmup, seed = clphy_settings$seed)

  return(model)
}
