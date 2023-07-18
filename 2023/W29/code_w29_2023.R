library(tidyverse)
library(gt)

tuesdata <- tidytuesdayR::tt_load('2023-07-18')

detectors <- tuesdata$detectors

detectors

data <- detectors %>%
  filter(model %in% c("GPT3", "GPT4")) %>%
  group_by(model, .pred_class) %>%
  tally() %>%
  #ungroup() %>%
  mutate(n = n/sum(n) * 100)

data

pl <- ggplot(data, aes(x = "", y = n, fill = .pred_class)) +
  geom_col() +
  geom_text(aes(label = paste(as.factor(round(n, 1)), "%")),
            position = position_stack(vjust = 0.5),
            size = 6) +
  scale_fill_manual(values = c("red", "green")) +
  facet_wrap(~ model) +
  coord_polar(theta = "y") +
  theme_minimal(base_size = 30) +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "#444654", colour = "#444654"),
        strip.text = element_text(colour = "white"))

ggsave("2023/W29/pl.PNG", units = "cm", width = 18, height = 12)


img <- tibble(who = c("me", "chatgpt"),
              img = c("https://github.com/AlbertMorera/TidyTuesday/blob/main/2023/W29/me_png.PNG?raw=true",
                      "https://github.com/AlbertMorera/TidyTuesday/blob/main/2023/W29/chatpgt_png.PNG?raw=true"))

emotis <- tibble(who = "chatgpt",
              emotis = "https://github.com/AlbertMorera/TidyTuesday/blob/main/2023/W29/emotis.PNG?raw=true")

chat <-
  tibble(who = c("me", "chatgpt", "me", "chatgpt"),
         sentences = c("Hi, I'm working on this week's #tidytuesday.",
                       "That's great! #TidyTuesday is a fantastic project that encourages data enthusiasts to explore and visualize 
                       interesting datasets using the principles of tidy data and the R programming language. How can I assist you with your #TidyTuesday project?",
                       "Hey, do you know how often GPT Detectors can figure out if it's actually GPT-3 or GPT-4 doing the writing instead of a real human?",
                       "GPT-3 successfully deceived the GPT Detectors in 58.4% of cases, while GPT-4 achieved an even higher success rate of 75.5%. It's a 
                       significant breakthrough! Allow me to present the information in the form of a graph."),
         plots = c(NA, NA, NA,
                "https://github.com/AlbertMorera/TidyTuesday/blob/main/2023/W29/pl.PNG?raw=true")) %>%
  left_join(img, by = "who") %>%
  left_join(emotis, by = "who") %>%
  select(img, sentences, plots, emotis)

chat

chat %>%
  gt() %>%
  gtExtras::gt_img_rows(img, height = 25) %>%
  gtExtras::gt_img_rows(emotis, height = 30) %>%
  gtExtras::gt_img_rows(plots, height = 200) %>%
  data_color(
    columns = img,
    target_columns = everything(),
    palette = c("#444654", "#343541")
  ) %>%
  cols_width(
    #extra ~ px(75),
    img ~ px(200),
    sentences ~ px(700),
    emotis ~ px(150)
  ) %>%
  tab_options(data_row.padding = px(20),
              column_labels.font.size =  px(0),
              column_labels.padding = px(0)) %>%
  cols_align(
    align = "center",
    columns = c(img, emotis)
  ) %>%
  opt_table_lines("none")


