# Settings ----------------------------------------------------------------

clpriv_setup <- function() {
  # Settings
  CHAINS <- 4
  ITER <- 2000
  WARMUP <- 1000
  BAYES_SEED <- 9324  # From random.org

  # Priors
  priors_vague <- c(set_prior("normal(0, 10)", class = "Intercept"),
                    set_prior("normal(0, 3)", class = "b"),
                    set_prior("cauchy(0, 1)", class = "sd"))

  return(list(chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
              priors_vague = priors_vague))
}

clpriv_settings <- clpriv_setup()


# Regular models ----------------------------------------------------------

f_clpriv_baseline <- function(dat) {
  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clpriv_lead1 ~ v2x_clpriv +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}

f_clpriv_total <- function(dat) {
  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clpriv_lead1 ~ barriers_total + barriers_total_lag1 +
         v2x_clpriv +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}

f_clpriv_total_new <- function(dat) {
  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clpriv_lead1 ~ barriers_total_new + barriers_total_new_lag1 +
         v2x_clpriv +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}

f_clpriv_advocacy <- function(dat) {
  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clpriv_lead1 ~ advocacy + advocacy_lag1 +
         v2x_clpriv +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}

f_clpriv_entry <- function(dat) {
  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clpriv_lead1 ~ entry + entry_lag1 +
         v2x_clpriv +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}

f_clpriv_funding <- function(dat) {
  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clpriv_lead1 ~ funding + funding_lag1 +
         v2x_clpriv +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}

f_clpriv_v2csreprss <- function(dat) {
  model <- brm(
    bf(v2x_clpriv_lead1 ~ v2csreprss + v2csreprss_lag1 +
         v2x_clpriv +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}


# REWB models -------------------------------------------------------------

f_clpriv_baseline_rewb <- function(dat) {
  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clpriv_lead1 ~ v2x_clpriv +
         v2x_polyarchy_within + v2x_polyarchy_between +
         gdpcap_log_within + gdpcap_log_between +
         un_trade_pct_gdp_within + un_trade_pct_gdp_between +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}

f_clpriv_total_rewb <- function(dat) {
  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clpriv_lead1 ~ barriers_total_within + barriers_total_between +
         barriers_total_lag1_within + barriers_total_lag1_between +
         v2x_clpriv +
         v2x_polyarchy_within + v2x_polyarchy_between +
         gdpcap_log_within + gdpcap_log_between +
         un_trade_pct_gdp_within + un_trade_pct_gdp_between +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}

f_clpriv_advocacy_rewb <- function(dat) {
  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clpriv_lead1 ~ advocacy_within + advocacy_between +
         advocacy_lag1_within + advocacy_lag1_between +
         v2x_clpriv +
         v2x_polyarchy_within + v2x_polyarchy_between +
         gdpcap_log_within + gdpcap_log_between +
         un_trade_pct_gdp_within + un_trade_pct_gdp_between +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}

f_clpriv_entry_rewb <- function(dat) {
  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clpriv_lead1 ~ entry_within + entry_between +
         entry_lag1_within + entry_lag1_between +
         v2x_clpriv +
         v2x_polyarchy_within + v2x_polyarchy_between +
         gdpcap_log_within + gdpcap_log_between +
         un_trade_pct_gdp_within + un_trade_pct_gdp_between +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}

f_clpriv_funding_rewb <- function(dat) {
  dat <- dat %>% filter(laws)

  model <- brm(
    bf(v2x_clpriv_lead1 ~ funding_within + funding_between +
         funding_lag1_within + funding_lag1_between +
         v2x_clpriv +
         v2x_polyarchy_within + v2x_polyarchy_between +
         gdpcap_log_within + gdpcap_log_between +
         un_trade_pct_gdp_within + un_trade_pct_gdp_between +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}

f_clpriv_v2csreprss_rewb <- function(dat) {
  model <- brm(
    bf(v2x_clpriv_lead1 ~ v2csreprss_within + v2csreprss_between +
         v2csreprss_lag1_within + v2csreprss_lag1_between +
         v2x_clpriv +
         v2x_polyarchy_within + v2x_polyarchy_between +
         gdpcap_log_within + gdpcap_log_between +
         un_trade_pct_gdp_within + un_trade_pct_gdp_between +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = clpriv_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    chains = clpriv_settings$chains, iter = clpriv_settings$iter,
    warmup = clpriv_settings$warmup, seed = clpriv_settings$seed)

  return(model)
}
