library(tidyverse)
library(sf)

# Rent data
rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

# San Francisco Neighbours
SF_shp <- read_sf("2022/W27/data/SF_nhoods/geo_export_6aa41e96-c37a-4b67-8fcc-13d32795dab4.shp")

# Read roads data
library(osmdata)

highway_sizes <- tibble::tribble(
  ~highway, ~highway_group, ~size,
  "motorway", "large", 0.5,
  "motorway_link", "large", 0.3,
  "primary", "large", 0.5,
  "primary_link", "large", 0.3,
  "secondary", "medium", 0.3,
  "secondary_link", "medium", 0.3,
  "tertiary", "medium", 0.3,
  "tertiary_link", "medium", 0.3,
  "residential", "small", 0.2,
  "living_street", "small", 0.2,
  "unclassified", "small", 0.2,
  "service", "small", 0.2,
  "footway", "small", 0.2
)

streets_osm <- opq("San Francisco") %>%
  add_osm_feature(key = "highway", 
                  value = highway_sizes$highway) %>%
  osmdata_sf()

streets <- streets_osm$osm_lines %>% 
  select(osm_id, name, highway, maxspeed, oneway, surface) %>% 
  mutate(length = as.numeric(st_length(.))) %>% 
  left_join(highway_sizes, by = "highway")

# Some corrections
corrections <- tribble(
  ~nhood, ~nbrhood,
  "Bernal", "Bernal Heights",
  "Castro", "Eureka Valley/Dolores Heights",
  "Ccsf", "Sunnyside",
  "Civic/Van Ness", "Van Ness/Civic Center",
  "Cole Valley", "Cole Valley/Parnassus Heights",
  "Excelsior/Outer Mission", "Excelsior",
  "Excelsior/Outer Mission", "Outer Mission",
  "Financial District", "Financial District/Barbary Coast",
  "Lakeshore", "Lake Shore",
  "Lower Heights", "Hayes Valley",
  "Lower Pac Hts", "Lower Pacific Heights",
  "Marina/Cow Hollow", "Marina",
  "Marina/Cow Hollow", "Cow Hollow",
  "Mission District", "Inner Mussion",
  "Nopa", "North Panhandle",
  "North Beach/Telegraph Hill", "North Beach",
  "North Beach/Telegraph Hill", "Telegraph Hill",
  "Presidio Hts/Laurel Hts/Lake St", "Presidio",
  "Presidio Hts/Laurel Hts/Lake St", "Jordan Park/Laurel Heights",
  "San Francisco", "Saint Francis Wood",
  "Soma/South Beach", "South Beach",
  "Usf/Anza Vista", "Anza Vista",
  "West Portal/Forest Hills", "West Portal",
  "West Portal/Forest Hills", "Forest Hill"
)

nbrhood_princes <- rent %>%
  filter(city == "san francisco" & year >= 2015) %>%
  mutate(nhood = str_to_title(nhood),
         nhood = str_replace_all(nhood, " / ", "/")) %>%
  left_join(corrections, by = "nhood") %>%
  mutate(nbrhood = case_when(is.na(nbrhood) ~ nhood,
                             T ~ nbrhood)) %>%
  group_by(nbrhood) %>%
  summarise(price = mean(price, na.rm = T))

SF_shp <- SF_shp %>%
  left_join(nbrhood_princes, by = "nbrhood")

# Streets data
streets <- st_transform(streets, st_crs(SF_shp))
sf_use_s2(FALSE)
streets_sf <- st_intersection(streets, SF_shp)
streets_sf_medium <- streets_sf %>% filter(highway_group == "medium")
streets_sf_large <- streets_sf %>% filter(highway_group == "large")


sysfonts::font_add(family = "Didot", regular = "GIL_____.TTF")
showtext::showtext_auto()

p <- ggplot() +
  geom_sf(data = SF_shp, aes(fill = price), col = NA) +
  geom_sf(data = streets_sf_large, col = "gray40", alpha = .5, size = .25) +
  geom_sf(data = streets_sf_medium, col = "gray70", alpha = .5, size = .25) +
  labs(subtitle = "The City of San Francisco",
       title = "Median rent prices by neighborhood of",
       caption = "#TidyTuesday Week 27 - 2022 | Data from Kate Pennington and data.sfgov.org | Chart by @_AlbertMorera") +
  scale_fill_stepsn(name = NULL, 
                    colors = RColorBrewer::brewer.pal(7, "YlOrRd"), labels = scales::dollar, 
                    na.value = "gray90",
                    breaks = seq(0, 6000, 1000),
                    limits = c(1000, 6000)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = .5, 
                               barwidth = unit(5, units = "cm"), 
                               barheight = unit(.1, unit = "cm"))) +
  coord_sf(xlim = c(-122.53, -122.35),
           ylim = c(37.81, 37.715)) +
  theme_bw(base_family = "Didot",
           base_size = 25) +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(color = "grey50", size = 35, 
                                  hjust = .5, face = "plain", 
                                  family = "Didot"),
        plot.subtitle = element_text(color = "grey20", size = 80, 
                                     hjust = .5, face = "plain", 
                                     family = "Didot"),
        legend.margin = margin(-20,0,20,0),
        legend.spacing.y = unit(.15, 'cm'),
        plot.caption = element_text(margin = margin(15,0,0,0), family = "mono", size = 15))


ggsave("2022/W27/SF.png", p, width = 13, height = 13, units = "cm", dpi = 300)
