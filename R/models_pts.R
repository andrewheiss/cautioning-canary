# Settings ----------------------------------------------------------------

# Run this inside each model function instead of outside so that future workers
# use these options internally
pts_setup <- function() {
  options(worker_options)

  # Settings
  CHAINS <- 4
  ITER <- 2000
  WARMUP <- 1000
  BAYES_SEED <- 2009  # From random.org

  # Priors
  priors_vague <- c(set_prior("normal(0, 10)", class = "Intercept"),
                    set_prior("normal(0, 3)", class = "b"),
                    set_prior("cauchy(0, 1)", class = "sd"))

  return(list(chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
              priors_vague = priors_vague))
}


# Regular models ----------------------------------------------------------

f_pts_baseline <- function(dat) {
  pts_settings <- pts_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(PTS_factor_lead1 ~ PTS_factor +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = cumulative(),
    prior = pts_settings$priors_vague,
    data = dat,
    chains = pts_settings$chains, iter = pts_settings$iter,
    warmup = pts_settings$warmup, seed = pts_settings$seed)

  return(model)
}

f_pts_total <- function(dat) {
  pts_settings <- pts_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(PTS_factor_lead1 ~ barriers_total + barriers_total_lag1 +
         PTS_factor +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = cumulative(),
    prior = pts_settings$priors_vague,
    data = dat,
    chains = pts_settings$chains, iter = pts_settings$iter,
    warmup = pts_settings$warmup, seed = pts_settings$seed)

  return(model)
}

f_pts_total_new <- function(dat) {
  pts_settings <- pts_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(PTS_factor_lead1 ~ barriers_total_new + barriers_total_new_lag1 +
         PTS_factor +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = cumulative(),
    prior = pts_settings$priors_vague,
    data = dat,
    chains = pts_settings$chains, iter = pts_settings$iter,
    warmup = pts_settings$warmup, seed = pts_settings$seed)

  return(model)
}

f_pts_advocacy <- function(dat) {
  pts_settings <- pts_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(PTS_factor_lead1 ~ advocacy + advocacy_lag1 +
         PTS_factor +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = cumulative(),
    prior = pts_settings$priors_vague,
    data = dat,
    chains = pts_settings$chains, iter = pts_settings$iter,
    warmup = pts_settings$warmup, seed = pts_settings$seed)

  return(model)
}

f_pts_entry <- function(dat) {
  pts_settings <- pts_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(PTS_factor_lead1 ~ entry + entry_lag1 +
         PTS_factor +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = cumulative(),
    prior = pts_settings$priors_vague,
    data = dat,
    chains = pts_settings$chains, iter = pts_settings$iter,
    warmup = pts_settings$warmup, seed = pts_settings$seed)

  return(model)
}

f_pts_funding <- function(dat) {
  pts_settings <- pts_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(PTS_factor_lead1 ~ funding + funding_lag1 +
         PTS_factor +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = cumulative(),
    prior = pts_settings$priors_vague,
    data = dat,
    chains = pts_settings$chains, iter = pts_settings$iter,
    warmup = pts_settings$warmup, seed = pts_settings$seed)

  return(model)
}

f_pts_v2csreprss <- function(dat) {
  pts_settings <- pts_setup()

  model <- brm(
    bf(PTS_factor_lead1 ~ v2csreprss + v2csreprss_lag1 +
         PTS_factor +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = cumulative(),
    prior = pts_settings$priors_vague,
    data = dat,
    chains = pts_settings$chains, iter = pts_settings$iter,
    warmup = pts_settings$warmup, seed = pts_settings$seed)

  return(model)
}
