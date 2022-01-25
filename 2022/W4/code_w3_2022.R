library(tidyverse)

sysfonts::font_add_google("Press Start 2P")
sysfonts::font_add_google("Bungee Outline")
sysfonts::font_add_google("Train One")
sysfonts::font_add_google("Metamorphous")
sysfonts::font_add_google("Macondo Swash Caps")

showtext::showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2022-01-25')

ratings <- tuesdata$ratings
details <- tuesdata$details

rating_details <- ratings %>%
  left_join(details, by = "id")


mean(rating_details$average)

data <-
  rating_details %>%
  select(id, name, year, average, boardgamecategory) %>%
  mutate(boardgamecategory = str_replace_all(boardgamecategory, "'", ""),
         boardgamecategory = str_replace_all(boardgamecategory, "\\[", ""),
         boardgamecategory = str_replace_all(boardgamecategory, "\\]", ""),
         boardgamecategory = str_replace_all(boardgamecategory, "\"", "")) %>%
  separate(boardgamecategory, sep = ", ", into = paste0("cat_", c(1:100))) %>%
  discard(~all(is.na(.x))) %>%
  gather(key = "cat", value = "category", -id, -name, -year, -average) %>%
  select(-cat) %>%
  drop_na()

rating_by_cat <- 
  data %>%
  group_by(category) %>%
  summarise(n = n(),
            q25 = quantile(average, .25),
            q75 = quantile(average, .75)) %>%
  arrange(desc(n))

rating_by_cat

data_sub <- 
  data %>%
  filter(category %in% rating_by_cat$category[1:10])

best_games_by_cat <- 
  data_sub %>%
  group_by(category) %>%
  filter(average == max(average)) %>%
  bind_cols(figure = c(paste0("https://raw.githubusercontent.com/AlbertMorera/TidyTuesday/main/2022/W4/games/", 
                              c("terrorixico", "RPGQuest"), 
                              ".png"),
                       rep(NA, 8))) %>%
  mutate()


best_games_by_cat$name


nb.cols <- 10
mycolors <- grDevices::colorRampPalette(MetBrewer::met.brewer("Nattier"))(nb.cols)


ggplot(data_sub, aes(category, average)) +
  geom_hline(yintercept = mean(rating_details$average), col="black", lty = "longdash") +
  geom_segment(data = rating_by_cat[1:10, ], 
    aes(x = category, xend = category, y = q25, yend = q75, col=category), 
    size = 0.8) +
  stat_summary(fun = median, geom = "point", size = 5, aes(col = category)) +
  ggimage::geom_image(data = best_games_by_cat, aes(category, average, image = figure), asp=2.1) +
  geom_text(data = best_games_by_cat, aes(category, average, label = name), family = "Metamorphous", nudge_y = .2, hjust = 0, size = 3)+
  scale_color_manual(values = mycolors) +
  labs(x = NULL, y = NULL,
       title = "BOARD GAMES RATING") +
  coord_flip(y = c(5,10)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.ticks.length.x = unit(.25,"cm"),
    axis.ticks.x = element_line(color="black"),
    panel.background = element_rect(fill = "tan", color = "tan"),
    plot.background = element_rect(fill = "tan"),
    plot.title = element_text(family = "Metamorphous", size = 30, color = "black", hjust = .5, margin = margin(0,0,50,0)),
    axis.text.y = element_text(family = "Metamorphous", color = "black", size = 15),
    axis.text.x = element_text(family = "Metamorphous", color = "black", margin = margin(10,0,0,0))
  )


