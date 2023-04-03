library(tidyverse)
library(gganimate)
# To get the fonts to work when rendered with anim_save
library(extrafont)

aupres_data <- readxl::read_xlsx(here::here("data/aupres.xlsx")) %>%
  pivot_longer(c("1":"13")) %>%
  rename(note = name,
         depicted = value) %>%
  mutate(depicted = factor(depicted, levels = c("j", "l", "o", "ca", "t", "p", "co", "x"))) %>%
  filter(!is.na(depicted)) %>%
  mutate(fauna_flora = factor(case_when(depicted %in% c("j", "l") ~ "Flora",
                                        depicted %in% c("o", "ca", "t", "co", "p") ~ "Fauna",
                                        TRUE ~ "Other notes"),
                              levels = c("Flora", "Fauna", "Other notes")),
         total_note = 1:nrow(.)) %>%
  group_by(fauna_flora) %>%
  mutate(grouped_rank = rank(total_note)) %>%
  arrange(depicted) %>%
  mutate(grouped_rank_ordered = sort(grouped_rank),
         x_modulo = (case_when(grouped_rank_ordered < 14 ~ grouped_rank_ordered - 1,
                               TRUE ~ (grouped_rank_ordered - 1) %% 14) + 1),
         y_modulo = (case_when(grouped_rank_ordered < 14 ~ 0,
                               TRUE ~ floor((grouped_rank_ordered - 1) / 14)) - 1),
         adapted_y_modulo = case_when(fauna_flora == "Fauna" ~ y_modulo + 3,
                                      fauna_flora == "Other notes" ~ y_modulo + 6,
                                      TRUE ~ y_modulo + 2),
         state = 1) %>%
  ungroup()

aupres_double_data <- select(aupres_data, c(text, line, note, depicted)) %>%
  mutate(note = as.numeric(note),
         line = as.numeric(line),
         state = 1) %>%
  bind_rows(select(aupres_data, c(adapted_y_modulo, x_modulo, depicted)) %>%
              rename(line = adapted_y_modulo, note = x_modulo) %>%
              mutate(state = 2))

aupres_labels <- tibble(group_label = c("", "", "", "Flora", "Fauna", "Everything else"),
                        label_x = c(11.5, 11.5, 11.5, 11.5, 6.5, 4.5),
                        label_y = c(8, 8, 8, 1, 4, 15),
                        state = c(1, 1, 1, 2, 2, 2))

song_text <- readxl::read_xlsx(here::here("data/aupres.xlsx"))

p <- ggplot(aupres_double_data) +
  geom_point(aes(x = note,
                 y = line,
                 colour = depicted),
             size = 7,
             alpha = 0.8,
             show.legend = FALSE) +
  scale_y_reverse() +
  scale_x_continuous(limits = c(0, 30)) +
  labs(title = "Flora, fauna & everything else in",
       subtitle = "Auprès de ma Blonde",
       caption = "Each dot represents a syllable in the melody of the nursery rhyme<br>Data visualisation: Cara Thompson<br>#30DayChartChallenge | Day 3 | Comparisons: Flora/Fauna") +
  scale_colour_manual(values = c("j" = "#358a7d",
                                 "l" = "#6e5083",
                                 "o" = "#948c5e",
                                 "ca" = "#cdaf8b",
                                 "t" = "#b2c4c2",
                                 "p" = "#a5a2a1",
                                 "co" = "#e3cec8",
                                 "x" = "#3E474A")) +
  ggtext::geom_textbox(data = song_text,
                       aes(x = 16, y = line,
                           label = text),
                       hjust = 0, halign = 0,
                       width = unit(20, "lines"),
                       family = "Amaranth",
                       colour = "#D9D6DB",
                       box.colour = NA,
                       fill = NA) +
  ggtext::geom_textbox(data = aupres_labels,
                       aes(x = label_x,
                           y = label_y,
                           label = group_label),
                       hjust = 0, halign = 0,
                       vjust = 0.5, valign = 0,
                       width = unit(20, "lines"),
                       family = "Amaranth",
                       lineheight = 0.8,
                       colour = "#F9F5F4",
                       box.colour = NA,
                       fill = NA) +
  transition_states(state,
                    transition_length = 2,
                    state_length = 4) +
  enter_fade() +
  exit_fade() +
  ease_aes("quadratic-in-out") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#25252A", colour = "#25252A"),
        plot.background = element_rect(fill = "#25252A", colour = "#25252A"),
        plot.title = element_text(family = "Amaranth", colour = "#D9D6DB",
                                  hjust = 0.5, size = 16,
                                  margin = margin(t = 1, b = 0.3, unit = "cm")),
        plot.caption = ggtext::element_markdown(family = "Amaranth", colour = "#A2A6A7",
                                                hjust = 0.5, size = 9, lineheight  = 1.4,
                                                margin = margin(t = 0.75, b = 1, unit = "cm")),
        plot.subtitle = element_text(family = "Amita", colour = "#F9F5F4",
                                     hjust = 0.5,
                                     size = 32,
                                     margin = margin(t = 0, b = 0.5, unit = "cm")))

anim_save(animate(p, width = 9, height = 6.5, units = "in", res = 300),
          filename = here::here("plots/day03_flora.gif"), rewind = T)
