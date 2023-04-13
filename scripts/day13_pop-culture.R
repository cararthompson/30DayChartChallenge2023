
library(tidyverse)

# Read in and rework data
fm_df <- read.csv(here::here("data/fleetwood-mac.csv")) %>%
  mutate(band_colour = case_when(band_member %in% c("Mick Fleetwood",
                                          "John McVie",
                                          "Lindsey Buckingham") ~ "Male",
                                 band_member %in% c("Christine McVie",
                                 "Stevie Nicks") ~ "Female",
                                 TRUE ~ "Temp")) %>%
  group_by(band_member) %>%
  mutate(entry = case_when(year == min(year) ~ "Entry"))

# Plot it
ggplot(fm_df, aes(year, rank, colour = band_colour,
                  group = band_member)) +
  geom_rect(data = NULL, aes(xmin = 1965, xmax = 2020,
                             ymin = -2, ymax = 6),
            fill = NA,
            colour = "#85857d") +
  ggbump::geom_bump(smooth = 10, size = 1.5, alpha = 0.2) +
  ggbump::geom_bump(data = fm_df %>% filter(instrument != "Exit"),
            smooth = 10, alpha = 0.8, size = 1.5) +
  scale_y_continuous(breaks = sort(c(unique(fm_df$rank), 7)),
                   labels  = c("Gone their own way", "Taking a break",
                               "Guitar 3", "Guitar 2", "Drums",
                               "Bass", "", "", "", "",
                               "Keys", "", "", "Guitar 1", "", "", "Vocals",
                               "", "Going solo", "")) +
  geom_point(data = fm_df %>% filter(entry == "Entry"),
             aes(x = year - .2, fill = band_colour),
             size = 4,
             shape = 21) +
  ggtext::geom_textbox(data = fm_df %>% filter(entry == "Entry"),
            aes(label = band_member,
                x = year-.2),
            colour = "#eddfc8",
            box.colour = NA,
            fill = "#2b2821",
            box.padding = unit(c(0,0,0,0), "pt"),
            width = NULL, box.r = unit(0.1, "pt"),
            alpha = 0.7,
            nudge_y = .4,
            nudge_x = -.4,
            size = 2.5,
            family = "TitlingGothicFB Comp",
            hjust = 0) +
  scale_colour_manual(values = c("#afc0ba", "#8d7c44", "#66836b")) +
  scale_fill_manual(values = c("#afc0ba", "#8d7c44", "#66836b")) +
  labs(title = "Loving you isn't the right thing to do",
       subtitle = "Band members, instruments and relationships within Fleetwood Mac from its foundation to 2018",
       caption = "#30DayChartChallenge | Data Visualisation: Cara Thompson| Sources: wikipedia.org, ranker.com, rollingstone.com") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "#2b2821", colour = "#2b2821"),
        plot.background = element_rect(fill = "#2b2821", colour = "#2b2821"),
        text = element_text(family = "TitlingGothicFB Comp", colour = "#cdbea0"),
        axis.text = element_text(colour = "#eddbcb"),
        plot.title = ggfx::with_shadow(element_text(hjust = 0.5, size = 32, colour = "#9f4b41",
                                                    family = "Hadijah Free Trial"),
                                      sigma = 0, x_offset = 6, y_offset = 6, colour = "#eddfc8",
                                       margin = margin(t = 1.5, b = 0.5, unit = "cm")),
        plot.subtitle = element_text(hjust = 0.5, size = 18, colour = "#eddbcb",
                                     lineheight = 1.1,
                                     margin = margin(b = 1, unit = "cm")),
        plot.caption = element_text(hjust = 0.5, size = 8, family = "Hadijah Free Trial",
                                    margin = margin(t = 1, unit = "cm")),
        plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"))

## Export plot ----
ggsave(filename = here::here("plots/day13_pop-culture.png"),
       dpi = 400, width = 12, height = 7)

## Full links to sources ----
# https://www.ranker.com/list/true-stories-behind-fleetwood-mac-rumours-album/melissa-sartore
# https://en.wikipedia.org/wiki/Lindsey_Buckingham
# https://en.wikipedia.org/wiki/Fleetwood_Mac
# https://www.rollingstone.com/music/music-news/broken-chain-a-history-of-fleetwood-mac-firings-and-departures-628871/
