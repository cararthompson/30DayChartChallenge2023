library(tidyverse)

# Data: https://heycar.co.uk/blog/electric-cars-statistics-and-projections

cars_by_year <- readxl::read_xlsx(here::here("data", "cars.xlsx"))

ggplot(cars_by_year) +
  # Masking bits of the grid
  geom_rect(aes(ymin = 100500, ymax = Inf,
                xmin = -Inf, xmax = 2019.995),
            fill = "#49966a") +
  geom_rect(aes(ymin = 200500, ymax = Inf,
                xmin = -Inf, xmax = 2020.995),
            fill = "#49966a") +
  geom_rect(aes(ymin = 500, ymax = Inf,
                xmin = -Inf, xmax = 2016.995),
            fill = "#49966a") +
  geom_rect(aes(ymin = 300500, ymax = Inf,
                xmin = -Inf, xmax = Inf),
            fill = "#49966a") +
  # Road
  ggfx::with_outer_glow(geom_line(aes(x = Year, y = Count),
                                  stat = "smooth",
                                  linetype = 1,
                                  size = 30,
                                  colour = "#666b71"),
                        sigma = 10,
                        colour = "#50bd86") +
  # Road markings
  geom_line(aes(x = Year, y = Count),
            stat = "smooth",
            linetype = 2,
            size = 1.5,
            colour = "#ecf0f2") +
  scale_y_continuous(limits = c(-1000, 300500),
                     labels = function(x) grkmisc::pretty_num(x, no_dot_zero = TRUE),
                     position = "right") +
  ggtext::geom_textbox(aes(x = 2017, y = 200000),
                       label = "Driving into the Future",
                       hjust = 0, halign = 0,
                       size = 8,
                       width = unit(20, "lines"),
                       fontface = "bold",
                       box.colour = NA,
                       colour = "#ecf0f2",
                       fill = NA,
                       vjust = 0, family = "Noah") +
  ggtext::geom_textbox(aes(x = 2017, y = 200000),
                       label = "Electric vehicle sales in the UK",
                       width = unit(20, "lines"),
                       family = "Noah",
                       size = 6,
                       colour = "#ecf0f2",
                       box.colour = NA,
                       fill = NA,
                       hjust = 0, halign = 0,
                       vjust = 1) +
  labs(caption = "#30DayChartChallenge | Source: https://heycar.co.uk/blog/electric-cars-statistics-and-projections | Data Visualisation: Cara Thompson") +
  theme_minimal() +
  theme(text = element_text(family = "Noah", colour = "#ecf0f2"),
        axis.text = element_text(colour = "#ecf0f2", size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#50bd86"),
        panel.background = element_rect(fill = "#49966a",
                                        colour = "#49966a"),
        plot.background = element_rect(fill = "#49966a",
                                       colour = "#49966a"),
        plot.caption = element_text(margin = margin(t = 0.5, r = 0.25, unit = "cm"),
                                    hjust = 1.55),
        plot.margin = margin(t = 0, r = 0.25, b = 0.25, l = 0, unit = "cm"))

# Export
ggsave(filename = here::here("plots/day10_hybrid.png"), height = 5, width = 8, dpi = 400)




