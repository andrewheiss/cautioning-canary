# Settings ----------------------------------------------------------------

lhr_setup <- function() {
  options(worker_options)

  # Settings
  CHAINS <- 4
  ITER <- 2000
  WARMUP <- 1000
  BAYES_SEED <- 4045  # From random.org
  threads <- getOption("mc.threads")

  # Priors
  priors_vague <- c(set_prior("normal(0, 10)", class = "Intercept"),
                    set_prior("normal(0, 3)", class = "b"),
                    set_prior("cauchy(0, 1)", class = "sd"))

  return(list(chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
              threads = threads, priors_vague = priors_vague))
}


# Regular models ----------------------------------------------------------

f_lhr_baseline <- function(dat) {
  lhr_settings <- lhr_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(latent_hr_mean_lead1 ~ latent_hr_mean +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = lhr_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    threads = threading(lhr_settings$threads),
    chains = lhr_settings$chains, iter = lhr_settings$iter,
    warmup = lhr_settings$warmup, seed = lhr_settings$seed)

  return(model)
}

f_lhr_total <- function(dat) {
  lhr_settings <- lhr_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(latent_hr_mean_lead1 ~ barriers_total + barriers_total_lag1 +
         latent_hr_mean +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = lhr_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    threads = threading(lhr_settings$threads),
    chains = lhr_settings$chains, iter = lhr_settings$iter,
    warmup = lhr_settings$warmup, seed = lhr_settings$seed)

  return(model)
}

f_lhr_total_new <- function(dat) {
  lhr_settings <- lhr_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(latent_hr_mean_lead1 ~ barriers_total_new + barriers_total_new_lag1 +
         latent_hr_mean +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = lhr_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    threads = threading(lhr_settings$threads),
    chains = lhr_settings$chains, iter = lhr_settings$iter,
    warmup = lhr_settings$warmup, seed = lhr_settings$seed)

  return(model)
}

f_lhr_advocacy <- function(dat) {
  lhr_settings <- lhr_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(latent_hr_mean_lead1 ~ advocacy + advocacy_lag1 +
         latent_hr_mean +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = lhr_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    threads = threading(lhr_settings$threads),
    chains = lhr_settings$chains, iter = lhr_settings$iter,
    warmup = lhr_settings$warmup, seed = lhr_settings$seed)

  return(model)
}

f_lhr_entry <- function(dat) {
  lhr_settings <- lhr_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(latent_hr_mean_lead1 ~ entry + entry_lag1 +
         latent_hr_mean +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = lhr_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    threads = threading(lhr_settings$threads),
    chains = lhr_settings$chains, iter = lhr_settings$iter,
    warmup = lhr_settings$warmup, seed = lhr_settings$seed)

  return(model)
}

f_lhr_funding <- function(dat) {
  lhr_settings <- lhr_setup()

  dat <- dat %>% filter(laws)

  model <- brm(
    bf(latent_hr_mean_lead1 ~ funding + funding_lag1 +
         latent_hr_mean +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = lhr_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    threads = threading(lhr_settings$threads),
    chains = lhr_settings$chains, iter = lhr_settings$iter,
    warmup = lhr_settings$warmup, seed = lhr_settings$seed)

  return(model)
}

f_lhr_v2csreprss <- function(dat) {
  lhr_settings <- lhr_setup()

  model <- brm(
    bf(latent_hr_mean_lead1 ~ v2csreprss + v2csreprss_lag1 +
         latent_hr_mean +
         v2x_polyarchy +
         gdpcap_log +
         un_trade_pct_gdp +
         armed_conflict +
         (1 | gwcode)
    ),
    family = gaussian(),
    prior = lhr_settings$priors_vague,
    control = list(adapt_delta = 0.9),
    data = dat,
    threads = threading(lhr_settings$threads),
    chains = lhr_settings$chains, iter = lhr_settings$iter,
    warmup = lhr_settings$warmup, seed = lhr_settings$seed)

  return(model)
}
