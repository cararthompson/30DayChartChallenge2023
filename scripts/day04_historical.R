library(tidyverse)

moon <- readxl::read_xlsx(here::here("data/moon.xlsx")) %>%
  group_by(walk_orbit, year) %>%
  mutate(missions_by_year = paste0(unique(mission), collapse = " ")) %>%
  summarise(count = length(unique(who)), missions = unique(missions_by_year)) %>%
  ungroup() %>%
  mutate(gender = "Men") %>%
  bind_rows(select(., c(missions, year, walk_orbit))) %>%
  replace_na(list(count = 0, gender = "Women")) %>%
  group_by(walk_orbit, gender) %>%
  mutate(cumulative = cumsum(count)) %>%
  ungroup() %>%
  mutate(text_colour = case_when(gender == "Women" ~ "#5c7e80",
                                 TRUE ~ "#3b4f72"),
         text_bar_colour = case_when(gender == "Women" ~ "#5c7e80",
                                 TRUE ~ "#c9c5c2"))

ggplot(moon) +
  geom_bar(aes(y = cumulative,
               x = year),
           stat = "identity",
           fill = "#3b4f72") +
  geom_bar(aes(y = cumulative - count,
               x = year),
           stat = "identity",
           fill = "white",
           alpha = 0.5) +
  facet_grid(walk_orbit ~ gender, scales = "free", switch = "y") +
  coord_flip() +
  scale_x_reverse() +
  scale_y_continuous(expand = expansion(c(0, 0))) +
  ggtext::geom_textbox(data = filter(moon, gender == "Women"),
                       aes(y = -2,
                           x = year,
                           label = missions),
                       family = "Poppins",
                       hjust = 0,
                       halign = 0,
                       colour = "#282726",
                       maxwidth = unit(6, "lines"),
                       box.colour = NA,
                       fill = NA) +
  ggtext::geom_textbox(data = filter(moon, gender == "Women"),
                       aes(y = 2,
                           x = year,
                           label = ""),
                       hjust = 0,
                       halign = 0.5,
                       maxwidth = unit(6, "lines"),
                       box.colour = NA,
                       fill = NA) +
  ggtext::geom_textbox(data = filter(moon, gender == "Men"),
                       aes(y = 20,
                           x = year,
                           label = year),
                       hjust = 1,
                       halign = 1,
                       family = "Poppins",
                       fontface = "bold",
                       colour = "#282726",
                       maxwidth = unit(6, "lines"),
                       box.colour = NA,
                       fill = NA) +
  ggtext::geom_textbox(aes(y = cumulative,
                           x = year,
                           colour = text_bar_colour,
                           label = count),
                       hjust = 1, halign = 1,
                       vjust = 0.6,
                       family = "Poppins",
                       size = 8,
                       fontface = "bold",
                       box.colour = NA,
                       fill = NA) +
  ggtext::geom_textbox(data = filter(moon, year < 1972),
                       aes(y = cumulative,
                           x = year,
                           colour = text_colour,
                           label = "+"),
                       hjust = 0, halign = 0,
                       vjust = 0.6,
                       family = "Poppins",
                       fontface = "bold",
                       size = 8,
                       box.colour = NA,
                       fill = NA) +
  ggtext::geom_textbox(data = filter(moon, year == 1972),
                       aes(y = cumulative,
                           x = year,
                           label = paste("<span style='padding-right:0.5em'>=  </span>", cumulative),
                           colour = text_colour),
                       hjust = 0, halign = 0,
                       vjust = 0.6,
                       family = "Poppins",
                       fontface = "bold",
                       size = 8,
                       box.colour = NA,
                       fill = NA) +
  theme_void() +
  scale_colour_identity() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(family = "Poppins",
                                  hjust = 0.5,
                                  size = 16,
                                  colour = "#484645",
                                  face = "bold",
                                  margin = margin(b = 0.25, r = 0.25, l = 0.25, unit = "cm")),
        plot.title = element_text(family = "Poppins",
                                  hjust = 0.5,
                                  size = 22,
                                  colour = "#282726",
                                  face = "bold",
                                  margin = margin(t = 1, b = 0.5, unit = "cm")),
        plot.caption = element_text(hjust = 0.5, family = "Poppins", colour = "#282726",
                                    margin = margin(t = 0.5, b = 0.25, unit = "cm"),
                                    size = 8),
        panel.background = element_rect("#c9c5c2", colour = "#c9c5c2"),
        plot.margin = margin(0, 0.5, 0, 0, unit = "cm")) +
  labs(title = "Total number of Men and Women who have
ever Orbited or Walked on the Moon",
       caption = "Data visualisation: Cara Thompson
Sources: solarsystem.nasa.gov/news/890/who-has-walked-on-the-moon, Wikipedia
#30DayChartChallenge | Day 4 | Comparisons: Historical")

ggsave(filename = here::here("plots/day04_historical.png"),
       height = 9, width = 10, bg = "#E2E0DF")


