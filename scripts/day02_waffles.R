library(tidyverse)

# Set up data

completion_data <- tibble(year = c("<span style='font-family:Poppins'>**2021**</span><br>Produced one dataviz<br>per prompt",
                                   "<span style='font-family:Poppins'>**2022**</span><br>Combined 3 prompts<br>into each dataviz",
                                   "<span style='font-family:Poppins'>**2023**</span><br>Who knows,<br>this is only Day 2!"),
                          complete = c(24, 12, 2),
                          incomplete = c(6, 18, 28)) %>%
  pivot_longer(cols = c(complete, incomplete)) %>%
  mutate(waffle_colour = case_when(grepl("2021", year) ~ "#f835bf",
                                   grepl("2022", year) ~ "#fffe0d",
                                   grepl("2023", year) ~ "#0791c6"))

# Plot it!
ggplot(completion_data) +
  waffle::geom_waffle(aes(fill = case_when(name == "incomplete" ~ alpha(waffle_colour, 0.1),
                                           TRUE ~ waffle_colour),
                          colour = waffle_colour,
                          values = value),
                      n_rows = 6,
                      flip = TRUE,
                      height = 0.8,
                      width = 0.8,
                      size = 0.8,
                      radius = unit(2, "pt")) +
  scale_colour_identity() +
  scale_fill_identity() +
  facet_grid(~ year) +
  coord_equal() +
  theme_void() +
  labs(title = "Number of prompts I completed in each<br>#30DayChartChallenge",
       caption = "#30DayChartChallenge | Day 2 | Comparisons: Waffle<br>Dataviz: Cara Thompson") +
  theme(strip.text = ggtext::element_markdown(family = "Noah", size = 12, lineheight = 1.2,
                                              colour = "#3d3c45",
                                              margin = margin(t = 0.25, b = 0.125, unit = "cm")),
        plot.title = ggtext::element_textbox_simple(halign = 0.5, hjust = 0.5,
                                                    margin = margin(t = 12, b = 24, unit = "pt"),
                                                    family = "Poppins", face = "bold",
                                                    lineheight = 1.4,
                                                    colour = "#3d3c45"),
        plot.caption = ggtext::element_textbox_simple(family = "Noah",
                                                      colour = "#3d3c45",
                                                      halign = 1,
                                                      margin = margin(t = 0.5,
                                                                      r = 0.25, unit = "cm")),
        plot.margin = margin(0.5, 2.5, 0.5, 2.5, unit = "cm"))

ggsave(filename = here::here("plots", "day02_waffles.png"), height = 5, width = 10, dpi = 400,
                             bg = "#d6d6d6")

# Colour inspiration: https://twitter.com/womensart1/status/1484103196142362625
