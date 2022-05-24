library(tidyverse)

tl3 <- read_csv("https://raw.githubusercontent.com/oecd-cfe-eds/ccsa-excess-mortality/master/excess-mortality_jan-aug.csv") |>
  filter(tl == 3)

tl3_ordered <- tl3 |>
  filter(!is.na(deaths_growth_1619), !is.na(typology)) |>
  group_by(country_code) |>
  mutate(max_deaths_growth = max(deaths_growth_1619)) |>
  ungroup() |>
  mutate(
    typology = factor(
      typology,
      levels = c(
        "Metropolitan region",
        "Regions near a metropolitan area",
        "Regions far from a metropolitan area")
      ),
    country_code = fct_reorder(country_code, max_deaths_growth)
    )

ggplot(tl3_ordered) +
  geom_point(
    aes(deaths_growth_1619, country_code, colour = typology),
    alpha = .5,
    show.legend = FALSE) +
  scale_x_continuous(labels = scales::number_format(suffix = " %")) +
  facet_grid(~ typology) +
  labs(
    title = "Increase in deaths between January and August 2020",
    subtitle = "with respect to the average of the same period in the previous four years"
    ) +
  theme(axis.title = element_blank())
