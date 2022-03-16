library(tidyverse)
library(ggwordcloud)

# Reading data
tuesdata <- tidytuesdayR::tt_load('2022-03-15')

# Loading fonts
sysfonts::font_add_google(name = "Kodchasan", family = "Kodchasan")
showtext::showtext_auto()


cran <- tuesdata$cran %>%
  filter(date != "0") %>%
  mutate(date = str_remove(date, "\\w{3}\\s"),
         date = case_when(str_detect(date, "\\w{3}\\s") ~ parse_datetime(date, format = "%b %d %H:%M:%S %Y"),
                               T ~ parse_datetime(date, format = "%Y-%m-%d %H:%M:%S UTC")))

# Installed packages
installed_pkg <- installed.packages() %>%
  as.tibble() %>%
  select(package = Package, version = Version)


my_pkg <- cran %>%
  filter(package %in% installed_pkg$package)

cloud <- 
  my_pkg %>%
  count(package) %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))

p <- ggplot(cloud, aes(label = package, size = n, col = n, angle = angle)) +
  geom_text_wordcloud_area(eccentricity = 1.25) +
  scale_color_distiller(palette = "Dark2") +
  scale_size_area(max_size = 30) +
  labs(title = "My most released CRAN-packages",
       subtitle = "Which of my installed CRAN packages are the most released?", 
       caption = "#TidyTuesday Week 10 - 2022 | Data from Robert Flight | Chart by @_AlbertMorera") +
  theme_minimal() +
  theme(panel.background = element_rect(color = "gray25", fill = "gray25"),
        plot.background = element_rect(fill = "gray25"),
        plot.caption = element_text(family = "mono", color = "white", size = 20),
        plot.subtitle = element_text(hjust = 0.5, color = "white", size = 30, family = "Kodchasan"),
        plot.title = element_text(hjust = 0.5, color = "white", size = 70, family = "Kodchasan", margin = margin(10,0,5,0)))
  

ggsave("2022/W11/word_cloud.png", p, width = 13, height = 10, units = "cm", dpi = 500)


