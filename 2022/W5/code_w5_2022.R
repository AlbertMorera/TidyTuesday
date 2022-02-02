library(tidyverse)



breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
breed_rank <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

sysfonts::font_add_google("Quicksand")
showtext::showtext_auto()


fp <- data.frame(cat = c("Energy", "Adaptability", "Openness", "Dog friendly"),
                 Image = paste0("C:/Users/Albert/Desktop/TidyTuesday/2022/W5/img/",c("orange", "blue", "yellow", "green"),".png"))


breed_rank_rank <- breed_rank %>%
  rename(R2013 = `2013 Rank`, 
         R2014 = `2014 Rank`, 
         R2015 = `2015 Rank`, 
         R2016 = `2016 Rank`, 
         R2017 = `2017 Rank`, 
         R2018 = `2018 Rank`, 
         R2019 = `2019 Rank`, 
         R2020 = `2020 Rank`) %>%
  mutate(Rank = rowMeans(across(where(is.numeric)), na.rm = T)) %>%
  #mutate(Breed = str_replace_all(Breed, " ", "")) %>%
  select(Breed, Rank, Image) %>%
  arrange(Breed)

data <- breed_traits %>%
  select("Breed", "Energy Level", "Adaptability Level", "Openness To Strangers", "Good With Other Dogs") %>%
  rename(Energy = "Energy Level",
         Adaptability = "Adaptability Level",
         Openness = "Openness To Strangers",
         `Dog friendly` = "Good With Other Dogs") %>%
  arrange(Breed) %>%
  mutate(Breed = breed_rank_rank$Breed) %>%
  left_join(breed_rank_rank,
            by = "Breed") %>%
  arrange(Rank)


data <- data[1:20, ] %>% 
  mutate(Breed = factor(Breed, levels = data[1:20, ]$Breed),
         position.y = c(rep(52, 5), rep(36, 5), rep(20, 5), rep(4,5)),
         position.x = c(1:5, 1:5, 1:5, 1:5))


fp_data <- data %>%
  select(-Rank, -Image) %>%
  gather(key = "cat", value = "char", -Breed, -position.x, -position.y) %>%
  left_join(fp, by = "cat")



main <- ggplot(data = data)+
  ggimage::geom_image(aes(position.x, position.y + 6, image = Image), size = .08, asp = 1.5) +
  geom_text(aes(position.x, position.y-2.5, label = Breed), family = "Quicksand", size = 12)+
  ggimage::geom_image(data = fp_data %>% filter(cat == "Energy"), aes(position.x, position.y, image = Image, size = I(char/210)), asp = 1.8, nudge_x = -0.3) +
  ggimage::geom_image(data = fp_data %>% filter(cat == "Adaptability"), aes(position.x, position.y, image = Image, size = I(char/210)), asp = 1.8, nudge_x = -0.1) +
  ggimage::geom_image(data = fp_data %>% filter(cat == "Openness"), aes(position.x, position.y, image = Image, size = I(char/210)), asp = 1.8, nudge_x = 0.1) +
  ggimage::geom_image(data = fp_data %>% filter(cat == "Dog friendly"), aes(position.x, position.y, image = Image, size = I(char/210)), asp = 1.8, nudge_x = .3) +
  labs(title = "Dog Breeds",
       subtitle = "Personality traits of the 20 most popular dog breeds")+
  coord_cartesian(x = c(0,6), y = c(0,60))+
  theme_void() +
  theme(text = element_text(family = "Quicksand", size = 100),
        plot.title = element_text(hjust = 0.5, margin = margin(5,0,5,0)),
        plot.subtitle = element_text(hjust = 0.5, size = 50, margin = margin(5,0,5,0)))


labs <- fp_data %>%
  filter(Breed == "Beagles") %>%
  mutate(position.x = position.x + c(-.3, -.1, .1, .3))

legend <- ggplot()+
  ggimage::geom_image(data = fp_data %>% filter(cat == "Energy" & Breed == "Beagles"), aes(position.x, position.y, image = Image), size = 0.05, asp = 2, nudge_x = -0.3) +
  ggimage::geom_image(data = fp_data %>% filter(cat == "Adaptability" & Breed == "Beagles"), aes(position.x, position.y, image = Image), size = 0.05, asp = 2, nudge_x = -0.1) +
  ggimage::geom_image(data = fp_data %>% filter(cat == "Openness" & Breed == "Beagles"), aes(position.x, position.y, image = Image), size = 0.05, asp = 2, nudge_x = 0.1) +
  ggimage::geom_image(data = fp_data %>% filter(cat == "Dog friendly" & Breed == "Beagles"), aes(position.x, position.y, image = Image), size = 0.05, asp = 2, nudge_x = .3) +
  geom_text(data = labs, aes(position.x, position.y-.3, label = cat), family = "Quicksand", size = 12) +
  coord_cartesian(x = c(4.5,5.5), y = c(50.75,53.25))+
  theme_void()+
  theme(plot.margin = margin(-2, -2, -1.5, -2, "cm"),
        plot.background = element_rect(color = "black")); legend

p <- cowplot::ggdraw(main) +
  cowplot::draw_plot(legend, .3, -.1, .4, .1) +
  theme(plot.margin = margin(0, 0, 2, 0, "cm"))

ggsave("2022/W5/dog_breeds.png", p, width = 22, height = 20, units = "cm", dpi = 500)

