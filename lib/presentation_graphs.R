library(tidyverse)
library(targets)
library(patchwork)
library(sf)
library(here)

# Load data from targets --------------------------------------------------
withr::with_dir(here(), {
  source(tar_read(plot_funs))

  canary <- tar_read(panel)
  tar_load(civicus_map_data)
  tar_load(civicus_clean)

  # Load marginal effects
  tar_load(c(mfx_e1a_pts, mfx_e1b_clphy, mfx_e1c_clpriv,
             mfx_e2a_pts, mfx_e2b_clphy, mfx_e2c_clpriv))
})


# Map of restrictions -----------------------------------------------------
ggplot() +
  geom_sf(data = civicus_map_data, aes(fill = fct_rev(category)), size = 0.15, color = "black") +
  coord_sf(crs = st_crs("ESRI:54030"), datum = NA) +  # Robinson
  scale_fill_viridis_d(option = "plasma", end = 0.9,
                       na.translate = FALSE, name = NULL) +
  theme_void(base_size = 18, base_family = "Inter Light") +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.7, "lines"))

ggsave(here("admin", "Presentations", "figures", "civicus_map.pdf"),
       width = 40/3, height = 6.1, units = "in", device = cairo_pdf)


# E1a marginal effects (total laws) ---------------------------------------
mfx_e1a_pts %>%
  mutate(x_rounded = floor(effect1__)) %>%
  group_by(x_rounded, effect2__, plot_var_nice) %>%
  slice(1) %>%
  ungroup() %>%
  filter(plot_var == "barriers_total") %>%
  ggplot(aes(x = x_rounded, y = estimate__, color = effect2__)) +
  geom_line(aes(group = effect2__), size = 0.5, color = "grey90") +
  geom_pointrange(aes(ymin = lower__, ymax = upper__),
                  size = 2.5,
                  position = position_dodge(width = 0.2),
                  fatten = 1.5) +
  scale_x_continuous(breaks = 0:9) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) +
  labs(x = "Count of legal barriers\n",
       y = "Predicted probability of category",
       color = "Predicted political terror scale", fill = "Predicted political terror scale") +
  facet_wrap(vars(plot_var_nice), scales = "free_x") +
  theme_ngo(base_size = 18) +
  theme(panel.grid.major.x = element_blank())

ggsave(here("admin", "Presentations", "figures", "mfx_e1a_total.pdf"),
       width = 40/3, height = 6.1, units = "in", device = cairo_pdf)


# E1a marginal effects (separate laws) ------------------------------------
mfx_e1a_pts %>%
  mutate(x_rounded = floor(effect1__)) %>%
  group_by(x_rounded, effect2__, plot_var_nice) %>%
  slice(1) %>%
  ungroup() %>%
  filter(plot_var != "barriers_total") %>%
  ggplot(aes(x = x_rounded, y = estimate__, color = effect2__)) +
  geom_line(aes(group = effect2__), size = 0.5, color = "grey90") +
  geom_pointrange(aes(ymin = lower__, ymax = upper__),
                  size = 2.5,
                  position = position_dodge(width = 0.2),
                  fatten = 1.5) +
  scale_x_continuous(breaks = 0:9) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  guides(color = guide_legend(override.aes = list(size = 0.5))) +
  labs(x = "Count of legal barriers\n",
       y = "Predicted probability of category",
       color = "Predicted political terror scale", fill = "Predicted political terror scale") +
  facet_wrap(vars(plot_var_nice), scales = "free_x") +
  theme_ngo(base_size = 18) +
  theme(panel.grid.major.x = element_blank())

ggsave(here("admin", "Presentations", "figures", "mfx_e1a_others.pdf"),
       width = 40/3, height = 6.1, units = "in", device = cairo_pdf)


# E1b marginal effects ----------------------------------------------------
mfx_e1b_clphy %>%
  mutate(x_rounded = floor(effect1__)) %>%
  group_by(x_rounded, plot_var_nice) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(plot_var_nice = fct_relabel(plot_var_nice, ~str_to_sentence(str_remove(., "Barriers to ")))) %>%
  ggplot(aes(x = x_rounded, y = estimate__, color = plot_var_nice)) +
  geom_line(size = 0.5, color = "grey90") +
  geom_pointrange(aes(ymin = lower__, ymax = upper__),
                  size = 2.5, fatten = 1.5) +
  scale_x_continuous(breaks = 0:9) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  coord_cartesian(ylim = c(0.60, 0.71)) +
  guides(color = FALSE) +
  labs(x = NULL,
       y = "Predicted physical violence index\n(higher values = less violence)") +
  facet_grid(cols = vars(plot_var_nice), scales = "free_x", space = "free_x") +
  theme_ngo(base_size = 18) +
  theme(panel.grid.major.x = element_blank())

ggsave(here("admin", "Presentations", "figures", "mfx_e1b.pdf"),
       width = 40/3, height = 6.1, units = "in", device = cairo_pdf)


# E1c marginal effects ----------------------------------------------------
mfx_e1c_clpriv %>%
  mutate(x_rounded = floor(effect1__)) %>%
  group_by(x_rounded, plot_var_nice) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(plot_var_nice = fct_relabel(plot_var_nice, ~str_to_sentence(str_remove(., "Barriers to ")))) %>%
  ggplot(aes(x = x_rounded, y = estimate__, color = plot_var_nice)) +
  geom_line(size = 0.5, color = "grey90") +
  geom_pointrange(aes(ymin = lower__, ymax = upper__),
                  size = 2.5, fatten = 1.5) +
  scale_x_continuous(breaks = 0:9) +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  coord_cartesian(ylim = c(0.60, 0.71)) +
  guides(color = FALSE) +
  labs(x = "Count of legal barriers",
       y = "Predicted civil liberties index\n(higher values = better protection)") +
  facet_grid(cols = vars(plot_var_nice), scales = "free_x", space = "free_x") +
  theme_ngo(base_size = 18) +
  theme(panel.grid.major.x = element_blank())

ggsave(here("admin", "Presentations", "figures", "mfx_e1c.pdf"),
       width = 40/3, height = 6.1, units = "in", device = cairo_pdf)


# E2a marginal effects ----------------------------------------------------
mfx_e2a_pts %>%
  ggplot(aes(x = effect1__, y = estimate__, color = effect2__, fill = effect2__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.4, color = NA) +
  geom_line(size = 1.75) +
  scale_x_reverse() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  scale_fill_viridis_d(option = "plasma", end = 0.9) +
  labs(x = "Civil society repression\n(scale reversed; greater repression on the right)\n",
       y = "Predicted probability of category",
       color = "Political terror scale", fill = "Political terror scale") +
  theme_ngo(base_size = 18)
ggsave(here("admin", "Presentations", "figures", "mfx_e2a.pdf"),
       width = 40/3, height = 6.1, units = "in", device = cairo_pdf)


# E2b marginal effects ----------------------------------------------------
mfx_e2b_clphy %>%
  ggplot(aes(x = effect1__, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.4,
              fill = "#433E85", color = NA) +
  geom_line(size = 1.75, color = "#433E85") +
  scale_x_reverse() +
  coord_cartesian(ylim = c(0.5, 0.75)) +
  labs(x = "Civil society repression\n(scale reversed; greater repression on the right)\n",
       y = "Predicted physical violence index\n(higher values = less violence)") +
  theme_ngo(base_size = 18)
ggsave(here("admin", "Presentations", "figures", "mfx_e2b.pdf"),
       width = 40/3, height = 6.1, units = "in", device = cairo_pdf)


# E2c marginal effects ----------------------------------------------------
mfx_e2c_clpriv %>%
  ggplot(aes(x = effect1__, y = estimate__)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.4,
              fill = "#4DC36B", color = NA) +
  geom_line(size = 1.75, color = "#4DC36B") +
  scale_x_reverse() +
  coord_cartesian(ylim = c(0.5, 0.75)) +
  labs(x = "Civil society repression\n(scale reversed; greater repression on the right)\n",
       y = "Predicted civil liberties index\n(higher values = less violence)") +
  theme_ngo(base_size = 18)
ggsave(here("admin", "Presentations", "figures", "mfx_e2c.pdf"),
       width = 40/3, height = 6.1, units = "in", device = cairo_pdf)
