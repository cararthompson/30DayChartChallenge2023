library(tidyverse)

# Scrape the data
daylight_data <- rvest::read_html("https://www.visitnorthwest.com/sunrise-sunset-times/edinburgh/") %>%
  html_table() %>%
  tibble(.[[1]]) %>%
  # https://www.statmethods.net/input/dates.html
  mutate(date = as.Date(Date, format = "%A, %B %d, %Y"),
         length = as.numeric(lubridate::hm(`Length of Day (Hours:Minutes)`))) %>%
  filter(date < as.Date("2024-04-11"))


# Plot it
ggplot(daylight_data) +
  geom_col(aes(x = date,
               y = length),
           fill = "#f9f8f3",
           size = 0.2,
           colour = "#316386") +
  coord_polar(start = -1.22) +
  labs(title = "Daylight hours in Edinburgh<br>through the year",
       subtitle = "From December to April, the number of daylight hours almost doubles. No wonder we're feeling brighter!",
       caption = "#30DayChartChallenge | Data visualisation: Cara Thompson<br>Source: visitnorthwest.com/sunrise-sunset-times/edinburgh") +
  ylim(c(-20000, 100000)) +
  theme_void() +
  ggtext::geom_textbox(data = filter(daylight_data, date %in% as.Date(c("2023-04-11", "2023-06-21", "2023-08-15", "2023-12-21"))) %>%
                         rowwise() %>%
                         mutate(pretty_date = verbaliseR::prettify_date(lubridate::ymd(date)),
                                date_name = case_when(date == as.Date("2023-04-11") ~ "Today",
                                                      date == as.Date("2023-06-21") ~ "Longest day",
                                                      date == as.Date("2023-08-15") ~ "Mid Festival",
                                                      date == as.Date("2023-12-21") ~ "Shortest Day")),
                       aes(x = date,
                           y = length + 22000,
                           label = paste0("<span style=font-family:Enriqueta>**", date_name, "**<br>",
                                          pretty_date, "</span><br><br>",
                                          gsub(":", " hours ",
                                               `Length of Day (Hours:Minutes)`),
                                          " minutes<br>Dark at ",
                                          `Sunset Time in Edinburgh`),
                           hjust = case_when(date == as.Date("2023-04-11") ~ 0.75,
                                             date == as.Date("2023-08-15") ~ 0.25,
                                             TRUE ~ 0.5),
                           vjust = case_when(date == as.Date("2023-06-21") ~ 0.25,
                                         date == as.Date("2023-12-21") ~ 0.75,
                                         TRUE ~ 0.5)),
                       box.colour = NA,
                       fill = NA,
                       colour = "#DDE3E6",
                       halign = 0.5,
                       family = "Noah") +
  theme(plot.title = ggtext::element_textbox_simple(family = "Enriqueta", size = 22, halign = 0.5,
                                                    margin = margin(t = 1, b = 0.5, unit = "cm"),
                                                    colour = "#fafafa"),
        plot.subtitle = ggtext::element_textbox_simple(family = "Noah", size = 16, halign = 0.5,
                                                       margin = margin(t = 0.5, b = 1.5, unit = "cm"),
                                                       colour = "#fafafa"),
        plot.caption = ggtext::element_textbox_simple(family = "Enriqueta", size = 8, halign = 0.5,
                                                       margin = margin(t = 0.5, b = 0.5, unit = "cm"),
                                                      colour = "#DDE3E6"),
        plot.background = element_rect(fill = "#003352", colour = "#003352"),
        plot.margin = margin(t = 0.5, b = 0.5, l = 1.5, r = 1.5, unit = "cm"))

# Exports for Making of
ggsave(filename = file.path(here::here("making-of/temp",
                                       paste0("day11_circular_",
                                              format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))),
       dpi = 400, width = 8, height = 9)



# Export final plot
ggsave(filename = here::here("plots/day11_circular.png"),
       dpi = 400, width = 8, height = 9)
