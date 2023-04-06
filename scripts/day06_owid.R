
library(tidyverse)

# Data explorations
betteraves <- read.csv("data/sugar-beet-production.csv") %>%
  filter(!Code %in% c("", "OWID_WRL", "OWID_USS")) %>%
  rename(Tonnes = Sugar.beet...00000157....Production...005510....tonnes)

# Find the two largest producers
betteraves_summary <- betteraves %>%
  group_by(Entity) %>%
  summarise(sum(Tonnes))

# Check whether there has been a global decline
betteraves_years <- betteraves %>%
  group_by(Year) %>%
  summarise(sum(Tonnes))

# Plot
ggplot(filter(betteraves, Entity %in% c("France", "United States"))) +
 geomtextpath::geom_textline(aes(x = Year, y = Tonnes,
                                  group = Entity,
                                 label = toupper(Entity)),
                              span = 0.3,
                              stat = "smooth",
                              colour = "#010101",
                              size = 8,
                             linewidth = 4,
                             family = "Noah",
                             fontface = "bold") +
  scale_y_continuous(breaks = c(10000000, 20000000, 30000000),
                     labels = function(x) paste0(grkmisc::pretty_num(x), "<br><span style='font-size:8pt'>tonnes</span>")) +
  labs(caption = "Data visualisation: Cara Thompson<br>Source: Our World In Data<br>#30DayChartChallenge | Day 6 | Comparisons: OWID Data") +
  theme_minimal() +
  ggtext::geom_textbox(aes(x = 1962,
                           y = 35000000),
                       label = "BETTERAVES<span style='font-size:21pt'><br>Où sont-elles passées?</span>",
                       hjust = 0, halign = 0,
                       family = "Noah",
                       lineheight = 0.2,
                       width = unit(25, "lines"),
                       size = 12,
                       box.colour = NA,
                       fill = NA,
                       fontface = "bold") +
  ggtext::geom_textbox(aes(x = 2020,
                           y = 25000000),
                       label = "The two largest producers of sugarbeet have
                       seen a decline in production since 2017,
                       mirroring a global decline in the production of sugarbeet
                       from 313.9M tonnes in 2017 to 253M tonnes in 2020.",
                       hjust = 1, halign = 1,
                       vjust = 1,
                       family = "Noah",
                       colour = "#010101",
                       box.colour = NA,
                       fill = NA,
                       lineheight = 1.4,
                       width = unit(13, "lines"),
                       size = 4,
                       fontface = "bold") +
  theme(text = element_text(family = "Noah", colour = "#010101"),
        plot.background = element_rect(colour = "#cc0000", size = 20),
        plot.margin = margin(1, 1, 1, 1, unit = "cm"),
        panel.grid = element_line(colour = "#F4F4F4"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 11, colour = "#010101"),
        axis.title.y = element_blank(),
        axis.text.y = ggtext::element_markdown(colour = "#010101",
                                               hjust = 0.5,
                                               lineheight = 0.8,
                                               size = 11),
        plot.caption = ggtext::element_textbox_simple(family = "Noah",
                                                      colour = "#010101",
                                                      halign = 1,
                                                      margin = margin(t = 0.5, unit = "cm")))

# Export
ggsave(filename = here::here("plots/day06_owid.png"), height = 7, width = 8, dpi = 400)
