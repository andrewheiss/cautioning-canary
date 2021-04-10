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
    warmup = pts_settings$warmup, seed = pts_settings$seed,
    threads = threading(getOption("n.threads", default = 1)))

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
    warmup = pts_settings$warmup, seed = pts_settings$seed,
    threads = threading(getOption("n.threads", default = 1)))

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
    warmup = pts_settings$warmup, seed = pts_settings$seed,
    threads = threading(getOption("n.threads", default = 1)))

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
    warmup = pts_settings$warmup, seed = pts_settings$seed,
    threads = threading(getOption("n.threads", default = 1)))

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


# REWB models -------------------------------------------------------------

f_pts_baseline_rewb <- function(dat) {
  pts_settings <- pts_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(PTS_factor_lead1 ~ PTS_factor +
         v2x_polyarchy_within + v2x_polyarchy_between +
         gdpcap_log_within + gdpcap_log_between +
         un_trade_pct_gdp_within + un_trade_pct_gdp_between +
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

f_pts_total_rewb <- function(dat) {
  pts_settings <- pts_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(PTS_factor_lead1 ~ barriers_total_within + barriers_total_between +
         barriers_total_lag1_within + barriers_total_lag1_between +
         PTS_factor +
         v2x_polyarchy_within + v2x_polyarchy_between +
         gdpcap_log_within + gdpcap_log_between +
         un_trade_pct_gdp_within + un_trade_pct_gdp_between +
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

f_pts_advocacy_rewb <- function(dat) {
  pts_settings <- pts_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(PTS_factor_lead1 ~ advocacy_within + advocacy_between +
         advocacy_lag1_within + advocacy_lag1_between +
         PTS_factor +
         v2x_polyarchy_within + v2x_polyarchy_between +
         gdpcap_log_within + gdpcap_log_between +
         un_trade_pct_gdp_within + un_trade_pct_gdp_between +
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

f_pts_entry_rewb <- function(dat) {
  pts_settings <- pts_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(PTS_factor_lead1 ~ entry_within + entry_between +
         entry_lag1_within + entry_lag1_between +
         PTS_factor +
         v2x_polyarchy_within + v2x_polyarchy_between +
         gdpcap_log_within + gdpcap_log_between +
         un_trade_pct_gdp_within + un_trade_pct_gdp_between +
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

f_pts_funding_rewb <- function(dat) {
  pts_settings <- pts_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(PTS_factor_lead1 ~ funding_within + funding_between +
         funding_lag1_within + funding_lag1_between +
         PTS_factor +
         v2x_polyarchy_within + v2x_polyarchy_between +
         gdpcap_log_within + gdpcap_log_between +
         un_trade_pct_gdp_within + un_trade_pct_gdp_between +
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

f_pts_v2csreprss_rewb <- function(dat) {
  pts_settings <- pts_setup()

  model <- brm(
    bf(PTS_factor_lead1 ~ v2csreprss_within + v2csreprss_between +
         v2csreprss_lag1_within + v2csreprss_lag1_between +
         PTS_factor +
         v2x_polyarchy_within + v2x_polyarchy_between +
         gdpcap_log_within + gdpcap_log_between +
         un_trade_pct_gdp_within + un_trade_pct_gdp_between +
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
