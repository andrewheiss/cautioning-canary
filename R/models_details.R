create_model_df <- function() {
  models <- tribble(
    ~model, ~outcome_var, ~explan_var,
    # Models for the political terror score (PTS)
    "m_pts_baseline", "Political terror", "Baseline",
    "m_pts_total", "Political terror", "Total legal barriers",
    "m_pts_total_new", "Political terror", "New legal barriers",
    "m_pts_advocacy", "Political terror", "Barriers to advocacy",
    "m_pts_entry", "Political terror", "Barriers to entry",
    "m_pts_funding", "Political terror", "Barriers to funding",
    "m_pts_v2csreprss", "Political terror", "Civil society repression",

    # Models for physical violence (v2x_clphy)
    "m_clphy_baseline", "Physical violence", "Baseline",
    "m_clphy_total", "Physical violence", "Total legal barriers",
    "m_clphy_total_new", "Physical violence", "New legal barriers",
    "m_clphy_advocacy", "Physical violence", "Barriers to advocacy",
    "m_clphy_entry", "Physical violence", "Barriers to entry",
    "m_clphy_funding", "Physical violence", "Barriers to funding",
    "m_clphy_v2csreprss", "Physical violence", "Civil society repression",

    # Models for private civil liberties (v2x_clpriv)
    "m_clpriv_baseline", "Private civil liberties", "Baseline",
    "m_clpriv_total", "Private civil liberties", "Total legal barriers",
    "m_clpriv_total_new", "Private civil liberties", "New legal barriers",
    "m_clpriv_advocacy", "Private civil liberties", "Barriers to advocacy",
    "m_clpriv_entry", "Private civil liberties", "Barriers to entry",
    "m_clpriv_funding", "Private civil liberties", "Barriers to funding",
    "m_clpriv_v2csreprss", "Private civil liberties", "Civil society repression",

    # Models for latent human rights (latent_hr_mean)
    "m_lhr_baseline", "Latent human rights", "Baseline",
    "m_lhr_total", "Latent human rights", "Total legal barriers",
    "m_lhr_total_new", "Latent human rights", "New legal barriers",
    "m_lhr_advocacy", "Latent human rights", "Barriers to advocacy",
    "m_lhr_entry", "Latent human rights", "Barriers to entry",
    "m_lhr_funding", "Latent human rights", "Barriers to funding",
    "m_lhr_v2csreprss", "Latent human rights", "Civil society repression",

    ## Models for PTS using training data
    "m_pts_baseline_train", "Political terror", "Baseline",
    "m_pts_total_train", "Political terror", "Total legal barriers",
    "m_pts_advocacy_train", "Political terror", "Barriers to advocacy",
    "m_pts_entry_train", "Political terror", "Barriers to entry",
    "m_pts_funding_train", "Political terror", "Barriers to funding",
    "m_pts_v2csreprss_train", "Political terror", "Civil society repression",

    ## Models for physical violence using training data
    "m_clphy_baseline_train", "Physical violence", "Baseline",
    "m_clphy_total_train", "Physical violence", "Total legal barriers",
    "m_clphy_advocacy_train", "Physical violence", "Barriers to advocacy",
    "m_clphy_entry_train", "Physical violence", "Barriers to entry",
    "m_clphy_funding_train", "Physical violence", "Barriers to funding",
    "m_clphy_v2csreprss_train", "Physical violence", "Civil society repression",

    ## Models for private civil liberties using training data
    "m_clpriv_baseline_train", "Private civil liberties", "Baseline",
    "m_clpriv_total_train", "Private civil liberties", "Total legal barriers",
    "m_clpriv_advocacy_train", "Private civil liberties", "Barriers to advocacy",
    "m_clpriv_entry_train", "Private civil liberties", "Barriers to entry",
    "m_clpriv_funding_train", "Private civil liberties", "Barriers to funding",
    "m_clpriv_v2csreprss_train", "Private civil liberties", "Civil society repression"
  ) %>%
    mutate(family = ifelse(str_detect(model, "_pts"), "Ordered logit", "OLS"),
           training = ifelse(str_detect(model, "_train"), "Training", "Full data"))

  return(models)
}

# Running modelsummary() on Bayesian models takes *forever* because of all the
# calculations involved in creating the confidence intervals and all the GOF
# statistics. With
# https://github.com/vincentarelbundock/modelsummary/commit/55d0d91, though,
# it's now possible to build the base model with modelsummary(..., output =
# "modelsummary_list", estimate = "", statistic = ""), save that as an
# intermediate object, and then feed it through modelsummary() again with
# whatever other output you want. The modelsummary_list-based object thus acts
# like an output-agnostic ur-model.

build_modelsummary <- function(models) {
  msl <- modelsummary(models,
                      output = "modelsummary_list",
                      statistic = "[{conf.low}, {conf.high}]")
  return(msl)
}


build_coef_list <- function() {
  list(
    "b_barriers_total" = "Total legal barriers",
    "b_barriers_total_lag1" = "Total legal barriers (t - 1)",
    "b_barriers_total_new" = "New legal barriers",
    "b_barriers_total_new_lag1" = "New legal barriers (t - 1)",
    "b_advocacy" = "Barriers to advocacy",
    "b_advocacy_lag1" = "Barriers to advocacy (t - 1)",
    "b_entry" = "Barriers to entry",
    "b_entry_lag1" = "Barriers to entry (t - 1)",
    "b_funding" = "Barriers to funding",
    "b_funding_lag1" = "Barriers to funding (t - 1)",
    "b_v2csreprss" = "Civil society repression",
    "b_v2csreprss_lag1" = "Civil society repression (t - 1)",
    "b_PTS_factorLevel2" = "PTS = 2",
    "b_PTS_factorLevel3" = "PTS = 3",
    "b_PTS_factorLevel4" = "PTS = 4",
    "b_PTS_factorLevel5" = "PTS = 5",
    "b_v2x_clphy" = "Physical violence index (t)",
    "b_v2x_clpriv" = "Private civil liberties index (t)",
    "b_latent_hr_mean" = "Latent human rights (t)",
    "b_v2x_polyarchy" = "Polyarchy index",
    "b_gdpcap_log" = "Log GDP per capita",
    "b_un_trade_pct_gdp" = "Trade as % of GDP",
    "b_armed_conflictTRUE" = "Armed conflict",
    "b_Intercept.1." = "Cutpoint 1/2",
    "b_Intercept.2." = "Cutpoint 2/3",
    "b_Intercept.3." = "Cutpoint 3/4",
    "b_Intercept.4." = "Cutpoint 4/5",
    "b_Intercept" = "Intercept"
  )
}
