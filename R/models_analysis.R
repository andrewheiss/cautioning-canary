generate_mfx <- function(models, is_categorical = FALSE) {
  models <- models %>%
    mutate(plot_var_nice = fct_inorder(plot_var_nice, ordered = TRUE))

  mfx <- models %>%
    mutate(fx = map2(model, plot_var,
                     ~conditional_effects(.x, effects = .y,
                                          categorical = is_categorical)[[1]])) %>%
    select(-model) %>%
    unnest(fx)

  return(mfx)
}
