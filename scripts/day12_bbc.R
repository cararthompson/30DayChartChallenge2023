library(tidyverse)

# Scrape the data
daylight_data <- rvest::read_html("https://www.visitnorthwest.com/sunrise-sunset-times/edinburgh/") %>%
  rvest::html_table() %>%
  tibble(.[[1]]) %>%
  # https://www.statmethods.net/input/dates.html
  mutate(date = as.Date(Date, format = "%A, %B %d, %Y"),
         length = as.numeric(lubridate::hm(`Length of Day (Hours:Minutes)`))) %>%
  filter(date < as.Date("2024-04-11"))


# Plot it
ggplot(daylight_data) +
  geom_hline(aes(yintercept = 20000),
             colour = "#dfdfdf") +
  geom_hline(aes(yintercept = 40000),
             colour = "#dfdfdf") +
  geom_hline(aes(yintercept = 60000),
             colour = "#dfdfdf") +
  geom_hline(aes(yintercept = 80000),
             colour = "#dfdfdf") +
  geom_col(aes(x = date,
               y = length),
           fill = "#9d3a3c",
           size = 0.2,
           colour = "#dfdfdf") +
  coord_polar(start = -1.22) +
  labs(title = "Daylight hours in Edinburgh through the year",
       subtitle = "From December to April, the number of daylight hours almost doubles. No wonder we're feeling brighter!",
       caption = "Data visualisation: Cara Thompson | #30DayChartChallenge<br>**____________________________________________________________________________________________________________**<br><br>Source: visitnorthwest.com/sunrise-sunset-times/edinburgh") +
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
                           label = paste0("<span style=\"font-family:\'BBC Reith Serif\'; color:#8b2327\">**", date_name, "**<br>",
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
                       colour = "#6f6f71",
                       halign = 0.5,
                       family = "BBC Reith Sans") +
  theme(plot.title = ggtext::element_textbox_simple(family = "BBC Reith Serif", size = 18, halign = 0,
                                                    margin = margin(t = 1, b = 0.2, unit = "cm"),
                                                    colour = "#1b1b1b"),
        plot.subtitle = ggtext::element_textbox_simple(family = "BBC Reith Sans", size = 14, halign = 0,
                                                       margin = margin(t = 0, b = 0.5, unit = "cm"),
                                                       colour = "#1b1b1b"),
        plot.caption = ggtext::element_textbox_simple(family = "BBC Reith Sans", size = 8, halign = 0,
                                                       margin = margin(t = 0.5, b = 0.5, unit = "cm"),
                                                      colour = "#1b1b1b",
                                                      lineheight = 0.2),
        plot.background = element_rect(fill = "#ffffff", colour = "#ffffff"),
        plot.margin = margin(t = 0.5, b = 0.5, l = 1.5, r = 1.5, unit = "cm"))

# Export
ggsave(filename = here::here("plots/day12_bbc.png"),
       dpi = 400, width = 8, height = 9)
