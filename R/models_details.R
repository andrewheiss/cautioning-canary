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

    "m_pts_baseline_rewb", "Political terror", "Baseline",
    "m_pts_total_rewb", "Political terror", "Total legal barriers",
    "m_pts_advocacy_rewb", "Political terror", "Barriers to advocacy",
    "m_pts_entry_rewb", "Political terror", "Barriers to entry",
    "m_pts_funding_rewb", "Political terror", "Barriers to funding",
    "m_pts_v2csreprss_rewb", "Political terror", "Civil society repression",

    # Models for physical violence (v2x_clphy)
    "m_clphy_baseline", "Physical violence", "Baseline",
    "m_clphy_total", "Physical violence", "Total legal barriers",
    "m_clphy_total_new", "Physical violence", "New legal barriers",
    "m_clphy_advocacy", "Physical violence", "Barriers to advocacy",
    "m_clphy_entry", "Physical violence", "Barriers to entry",
    "m_clphy_funding", "Physical violence", "Barriers to funding",
    "m_clphy_v2csreprss", "Physical violence", "Civil society repression",

    "m_clphy_baseline_rewb", "Physical violence", "Baseline",
    "m_clphy_total_rewb", "Physical violence", "Total legal barriers",
    "m_clphy_advocacy_rewb", "Physical violence", "Barriers to advocacy",
    "m_clphy_entry_rewb", "Physical violence", "Barriers to entry",
    "m_clphy_funding_rewb", "Physical violence", "Barriers to funding",
    "m_clphy_v2csreprss_rewb", "Physical violence", "Civil society repression",

    # Models for private civil liberties (v2x_clpriv)
    "m_clpriv_baseline", "Private civil liberties", "Baseline",
    "m_clpriv_total", "Private civil liberties", "Total legal barriers",
    "m_clpriv_total_new", "Private civil liberties", "New legal barriers",
    "m_clpriv_advocacy", "Private civil liberties", "Barriers to advocacy",
    "m_clpriv_entry", "Private civil liberties", "Barriers to entry",
    "m_clpriv_funding", "Private civil liberties", "Barriers to funding",
    "m_clpriv_v2csreprss", "Private civil liberties", "Civil society repression",

    "m_clpriv_baseline_rewb", "Private civil liberties", "Baseline",
    "m_clpriv_total_rewb", "Private civil liberties", "Total legal barriers",
    "m_clpriv_advocacy_rewb", "Private civil liberties", "Barriers to advocacy",
    "m_clpriv_entry_rewb", "Private civil liberties", "Barriers to entry",
    "m_clpriv_funding_rewb", "Private civil liberties", "Barriers to funding",
    "m_clpriv_v2csreprss_rewb", "Private civil liberties", "Civil society repression",

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

    "m_pts_baseline_rewb_train", "Political terror", "Baseline",
    "m_pts_total_rewb_train", "Political terror", "Total legal barriers",
    "m_pts_advocacy_rewb_train", "Political terror", "Barriers to advocacy",
    "m_pts_entry_rewb_train", "Political terror", "Barriers to entry",
    "m_pts_funding_rewb_train", "Political terror", "Barriers to funding",
    "m_pts_v2csreprss_rewb_train", "Political terror", "Civil society repression",

    ## Models for physical violence using training data
    "m_clphy_baseline_train", "Physical violence", "Baseline",
    "m_clphy_total_train", "Physical violence", "Total legal barriers",
    "m_clphy_advocacy_train", "Physical violence", "Barriers to advocacy",
    "m_clphy_entry_train", "Physical violence", "Barriers to entry",
    "m_clphy_funding_train", "Physical violence", "Barriers to funding",
    "m_clphy_v2csreprss_train", "Physical violence", "Civil society repression",

    "m_clphy_baseline_rewb_train", "Physical violence", "Baseline",
    "m_clphy_total_rewb_train", "Physical violence", "Total legal barriers",
    "m_clphy_advocacy_rewb_train", "Physical violence", "Barriers to advocacy",
    "m_clphy_entry_rewb_train", "Physical violence", "Barriers to entry",
    "m_clphy_funding_rewb_train", "Physical violence", "Barriers to funding",
    "m_clphy_v2csreprss_rewb_train", "Physical violence", "Civil society repression",

    ## Models for private civil liberties using training data
    "m_clpriv_baseline_train", "Private civil liberties", "Baseline",
    "m_clpriv_total_train", "Private civil liberties", "Total legal barriers",
    "m_clpriv_advocacy_train", "Private civil liberties", "Barriers to advocacy",
    "m_clpriv_entry_train", "Private civil liberties", "Barriers to entry",
    "m_clpriv_funding_train", "Private civil liberties", "Barriers to funding",
    "m_clpriv_v2csreprss_train", "Private civil liberties", "Civil society repression",

    "m_clpriv_baseline_rewb_train", "Private civil liberties", "Baseline",
    "m_clpriv_total_rewb_train", "Private civil liberties", "Total legal barriers",
    "m_clpriv_advocacy_rewb_train", "Private civil liberties", "Barriers to advocacy",
    "m_clpriv_entry_rewb_train", "Private civil liberties", "Barriers to entry",
    "m_clpriv_funding_rewb_train", "Private civil liberties", "Barriers to funding",
    "m_clpriv_v2csreprss_rewb_train", "Private civil liberties", "Civil society repression"
  ) %>%
    mutate(re = ifelse(str_detect(model, "rewb"), "REWB", "RE"),
           family = ifelse(str_detect(model, "_pts"), "Ordered logit", "OLS"),
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
    "b_barriers_total_within" = "Total legal barriers (within)",
    "b_barriers_total_between" = "Total legal barriers (between)",
    "b_barriers_total_lag1_within" = "Total legal barriers (within; t - 1)",
    "b_barriers_total_lag1_between" = "Total legal barriers (between; t - 1)",
    "b_advocacy" = "Barriers to advocacy",
    "b_advocacy_lag1" = "Barriers to advocacy (t - 1)",
    "b_advocacy_within" = "Barriers to advocacy (within)",
    "b_advocacy_between" = "Barriers to advocacy (between)",
    "b_advocacy_lag1_within" = "Barriers to advocacy (within; t - 1)",
    "b_advocacy_lag1_between" = "Barriers to advocacy (between; t - 1)",
    "b_entry" = "Barriers to entry",
    "b_entry_lag1" = "Barriers to entry (t - 1)",
    "b_entry_within" = "Barriers to entry (within)",
    "b_entry_between" = "Barriers to entry (between)",
    "b_entry_lag1_within" = "Barriers to entry (within; t - 1)",
    "b_entry_lag1_between" = "Barriers to entry (between; t - 1)",
    "b_funding" = "Barriers to funding",
    "b_funding_lag1" = "Barriers to funding (t - 1)",
    "b_funding_within" = "Barriers to funding (within)",
    "b_funding_between" = "Barriers to funding (between)",
    "b_funding_lag1_within" = "Barriers to funding (within; t - 1)",
    "b_funding_lag1_between" = "Barriers to funding (between; t - 1)",
    "b_v2csreprss" = "Civil society repression",
    "b_v2csreprss_lag1" = "Civil society repression (t - 1)",
    "b_v2csreprss_within" = "Civil society repression (within)",
    "b_v2csreprss_between" = "Civil society repression (between)",
    "b_v2csreprss_lag1_within" = "Civil society repression (within; t - 1)",
    "b_v2csreprss_lag1_between" = "Civil society repression (between; t - 1)",
    "b_PTS_factorLevel2" = "PTS = 2",
    "b_PTS_factorLevel3" = "PTS = 3",
    "b_PTS_factorLevel4" = "PTS = 4",
    "b_PTS_factorLevel5" = "PTS = 5",
    "b_v2x_clphy" = "Physical violence index (t)",
    "b_v2x_clpriv" = "Private civil liberties index (t)",
    "b_latent_hr_mean" = "Latent human rights (t)",
    "b_v2x_polyarchy" = "Polyarchy index",
    "b_v2x_polyarchy_within" = "Polyarchy index (within)",
    "b_v2x_polyarchy_between" = "Polyarchy index (between)",
    "b_gdpcap_log" = "Log GDP per capita",
    "b_gdpcap_log_within" = "Log GDP per capita (within)",
    "b_gdpcap_log_between" = "Log GDP per capita (between)",
    "b_un_trade_pct_gdp" = "Trade as % of GDP",
    "b_un_trade_pct_gdp_within" = "Trade as % of GDP (within)",
    "b_un_trade_pct_gdp_between" = "Trade as % of GDP (between)",
    "b_armed_conflictTRUE" = "Armed conflict",
    "b_Intercept.1." = "Cutpoint 1/2",
    "b_Intercept.2." = "Cutpoint 2/3",
    "b_Intercept.3." = "Cutpoint 3/4",
    "b_Intercept.4." = "Cutpoint 4/5",
    "b_Intercept" = "Intercept"
  )
}
