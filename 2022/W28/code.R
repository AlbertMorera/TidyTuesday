library(tidyverse)

flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')


bcn <- flights %>%
  filter(APT_NAME == "Barcelona") %>%
  mutate(FLT_DEP_IFR_2 = - FLT_DEP_IFR_2) %>%
  mutate(year = as.numeric(format(FLT_DATE, "%Y")),
         month = as.numeric(format(FLT_DATE, "%m")),
         doy = as.numeric(format(FLT_DATE, "%d"))) %>%
  mutate(seasson = case_when((month >= 6 & month <= 9) & 
           (month > 6 | doy >= 15) & 
           (month < 9 | doy <= 15) ~ "summer",
         T ~ "no summer")) %>%
  print(n = 5, width = Inf)

to_plot <- bcn %>%
  mutate(year = as.numeric(format(FLT_DATE, "%Y")),
         month = as.numeric(format(FLT_DATE, "%m")),
         doy = as.numeric(format(FLT_DATE, "%d"))) %>%
  filter(month >= 6 & month <= 9) %>%
  filter(month > 6 | doy >= 15) %>%
  filter(month < 9 | doy <= 15) %>%
  group_by(year) %>%
  summarise(summer_arrivals = sum(FLT_ARR_IFR_2),
            date = median(FLT_DATE)) %>%
  ungroup() %>%
  mutate(img = "G:/Mi unidad/TidyTuesday/2022/W28/data/jumbo2.png") %>%
  print(n = 100, width = Inf)


BCN <- tibble(img = "G:/Mi unidad/TidyTuesday/2022/W28/data/BCN_skyline.png",
              x = mean(to_plot$date))

sysfonts::font_add_google(name = "Gloria Hallelujah", 
                          family = "Gloria Hallelujah")
showtext::showtext_auto()

ggplot(to_plot) +
  geom_line(data = bcn, aes(FLT_DATE, FLT_ARR_IFR_2), col = "gray90", size = .1) +
  geom_line(data = bcn %>% filter(seasson == "summer"), 
            aes(FLT_DATE, FLT_ARR_IFR_2, group = year), col = "gray50", size = .1) +
  ggimage::geom_image(data = BCN,
                      aes(x, 200, image = img, size = I(1)), 
                      asp = 3) +
  geom_text(aes(date, 750, label = year), family = "Gloria Hallelujah") +
  geom_text(aes(date, 625, label = paste("(", summer_arrivals, ")")), family = "Gloria Hallelujah",
            col = "darkgreen") +
  geom_text(aes(as.POSIXct("2022-01-01"), -200), label = "#TouristsGoHome", 
            family = "Gloria Hallelujah", size = 3) +
  scale_y_continuous(breaks = c(0, 300, 600)) +
  labs(title = "BARCELONA",
       subtitle = "flight arrivals in summer",
       caption = "#TidyTuesday Week 28 - 2022 | Data from Eurocontrol | Chart by @_AlbertMorera") +
  coord_cartesian(y = c(-700, 720)) +
  theme_minimal(base_size = 40,
             base_family = "Gloria Hallelujah")+
  theme(plot.title = element_text(hjust = .5, margin = margin(.1,0,-3,0)),
        plot.subtitle = element_text(hjust = .5, size = 20, margin = margin(-3,0,3,0)),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.y = element_line(size = .1),
        axis.ticks.length.y = unit(.1, "cm"),
        plot.caption = element_text(margin = margin(-5,2,0,2), family = "mono", size = 8),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.margin = margin(5,3,3,3))




ggsave("2022/W28/flights.png", width = 8, height = 4.5, units = "cm", dpi = 300)











