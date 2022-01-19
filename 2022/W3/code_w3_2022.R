library(tidyverse)
library(sf)

tuesdata <- tidytuesdayR::tt_load('2022-01-18')

world <- ne_countries(scale = 'small', returnclass = 'sf') # Download a world layer
world <- world %>% select(name_sort) # Select only the info that we will need
world <- world %>% st_transform(crs="+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0") # Transform to Robinson projection
centroids <- st_centroid(world)

# Preparing data
dark_chocolate <- 
  tuesdata$chocolate %>%
  mutate(cocoa_percent = as.numeric(str_replace(cocoa_percent, pattern = "%", replacement = ""))) %>%
  filter(cocoa_percent >=100 & country_of_bean_origin != "Blend") %>%
  mutate(country_of_bean_origin = replace(country_of_bean_origin, country_of_bean_origin == "Venezuela", "Venezuela, RB"), # Some changes to match the country names on both objects
         company_location = replace(company_location, company_location == "U.K.", "United Kingdom"),
         company_location = replace(company_location, company_location == "U.S.A.", "United States of America"))

dark_chocolate <-
  dark_chocolate %>%
  left_join(centroids %>%
              rename(country_of_bean_origin = name_sort)) %>%
  rename(geom.origin = geometry) %>%
  left_join(centroids %>%
              rename(company_location = name_sort)) %>%
  rename(geom.company = geometry) %>%
  mutate(as.data.frame(st_coordinates(geom.origin))) %>%
  rename(origin.X = X, origin.Y = Y) %>%
  mutate(as.data.frame(st_coordinates(geom.company))) %>%
  rename(company.X = X, company.Y = Y)

exporters <- world %>%
  filter(name_sort %in% dark_chocolate$country_of_bean_origin)

# Remove non-exporting countries
dark_chocolate <- dark_chocolate[is.na(match(dark_chocolate$company_location, dark_chocolate$country_of_bean_origin)), ] %>%
  filter(complete.cases(origin.X))


ggplot() +
  geom_sf(data = world, color = "white", fill = "white", size = .1)+
  geom_sf(data = world, color = "chocolate", fill = "chocolate", size = .1, alpha=.25)+
  geom_sf(data = exporters, aes(fill = name_sort), color = "chocolate", size = .1)+
  geom_curve(data = dark_chocolate, aes(x = origin.X, y = origin.Y, xend = company.X, yend = company.Y, col=country_of_bean_origin),
             arrow = arrow(type = "closed", length=unit(.1,"cm")), size=.25)+
  scale_color_brewer(palette="Set2")+
  scale_fill_brewer(palette="Set2")+
  labs(title = "Tracking cocoa beans of the real BLACK chocolate",
       subtitle = "Country of origin of the cocoa beans used to make 100% cocoa chocolate",
       caption = "#TidyTuesday Week 3 - 2022 | Data from Flavours of Cacao | Chart by @_AlbertMorera")+
  ggthemes::theme_map()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "cornsilk", color=NA),
        text = element_text(family = "Gill Sans MT"),
        plot.title = element_text(hjust = 0.5, margin = margin(10,0,5,0), family = "Gill Sans MT", size = 100),
        plot.subtitle = element_text(hjust = 0.5, family = "Gill Sans MT", size = 50),
        plot.caption = element_text(family = "mono", size = 30))

ggsave("2022/W3/chocolate.png", width = 15, height = 9.12, units = "cm", dpi = 1000)
