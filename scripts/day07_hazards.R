# Hazards

library(tidyverse)

# Theme
theme_betteraves <- function() {
  theme_minimal() +
    theme(text = element_text(family = "Noah", colour = "#010101"),
          plot.background = element_rect(colour = "#cc0000", size = 20),
          plot.margin = margin(1, 1, 1, 1, unit = "cm"),
          panel.grid = element_line(colour = "#F4F4F4"),
          axis.title.x = element_blank(),
          axis.text.x = ggtext::element_markdown(colour = "#010101",
                                                 hjust = 0.5,
                                                 lineheight = 0.8,
                                                 size = 11),
          axis.title.y = element_blank(),
          axis.text.y = ggtext::element_markdown(colour = "#010101",
                                                 hjust = 0.5,
                                                 lineheight = 0.8,
                                                 size = 11),
          plot.caption = ggtext::element_textbox_simple(family = "Noah",
                                                        colour = "#010101",
                                                        halign = 1,
                                                        margin = margin(t = 0.5, unit = "cm")))
}

  betteraves <- read.csv("data/sugar-beet-production.csv") %>%
    filter(!Code %in% c("", "OWID_WRL", "OWID_USS")) %>%
    rename(Tonnes = Sugar.beet...00000157....Production...005510....tonnes)

  # Production per country
  betteraves_summary <- betteraves %>%
    filter(Entity == "France")

  # Plot
ggplot(betteraves_summary) +
    geom_histogram(aes(x = Tonnes),
                   fill = "#010101",
                   colour = "#FFFFFF") +
    ggtext::geom_textbox(aes(x = 10000000,
                             y = 9.12),
                         label = "<span style='font-size:32pt'>BETTERAVES</span><br>Distribution of Tonnes<br>produced per Year in France<br>from 1961 to 2020",
                         hjust = -0.005, halign = 0,
                         vjust = 1, valign = 1,
                         family = "Noah",
                         lineheight = 1.4,
                         width = unit(25, "lines"),
                         size = 5.5,
                         box.colour = NA,
                         fill = NA,
                         fontface = "bold") +
    scale_x_continuous(breaks = c(10000000,
                                  20000000,
                                  30000000,
                                  40000000),
                       labels = function(x)
                         paste0(grkmisc::pretty_num(x),
                                "<br><span style='font-size:8pt'>tonnes</span>")) +
    labs(caption = "Data visualisation: Cara Thompson<br>Source: Our World In Data<br>#30DayChartChallenge | Day 7 | Distribution: Hazards") +
    theme_betteraves()

  # Export
  ggsave(filename = here::here("plots/day07_hazards.png"), height = 7, width = 8, dpi = 400)
