library(tidyverse)

# une autre rajoute un commentaire
tl2 <- read_csv("https://raw.githubusercontent.com/oecd-cfe-eds/ccsa-excess-mortality/master/excess-mortality_jan-aug.csv") |>
  filter(tl == 2)

tl2_ordered <- tl2 |>
  filter(!is.na(deaths_growth_1619)) |>
  group_by(country_code) |>
  mutate(max_deaths_growth = max(deaths_growth_1619)) |>
  ungroup() |>
  mutate(country_code = fct_reorder(country_code, max_deaths_growth))

# Un commentaire, modifié
ggplot(tl2_ordered) +
  geom_point(
    aes(deaths_growth_1619, country_code),
    color = "pink",
    alpha = .5) +
  scale_x_continuous(labels = scales::number_format(suffix = " %")) +
  labs(
    title = "Increase in deaths between January and August 2020",
    subtitle = "with respect to the average of the same period in the previous four years"
    ) +
  theme(axis.title = element_blank())
ggsave("figures/tl2.png")
