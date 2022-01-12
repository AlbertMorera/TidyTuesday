# TidyTuesday week 2 - 2022
# Code by @_AlbertMorera

library(sf)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2022-01-11')

usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

USA_data <- 
  usa %>% 
  rename(state = ID) %>%
  mutate(state = stringi::stri_trans_totitle(state)) %>%
  left_join(tuesdata$stressor) %>%
  group_by(state, months, stressor) %>%
  summarise(stress_pct = mean(stress_pct, na.rm = T)) %>%
  filter(!is.na(months) & months != "2019" & !is.na(stressor)) %>%
  mutate(months = factor(months, levels = c("January-March", "April-June", "July-September", "October-December"))) %>%
  filter(stressor %in% c("Disesases", "Other pests/parasites", "Pesticides", "Varroa mites")) %>%
  mutate(stressor = factor(stressor, levels = c("Varroa mites", "Disesases", "Pesticides", "Other pests/parasites")))


months.labs <- c("January - March", "April - June", "July - September", "October - December")
names(months.labs) <- c("January-March", "April-June", "July-September", "October-December")

sysfonts::font_add(family = "Gill Sans MT", regular = "GIL_____.TTF")
showtext::showtext_auto()

pl <- 
  ggplot(USA_data) +
  geom_sf(data = usa, color = "white", fill = "gray90", size = .1) +
  geom_sf(aes(fill = stress_pct), color = "white",size = .1) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = NULL, limits = c(0, 100))+
  guides(fill = guide_colourbar(barheight = .25, barwidth = 10, title.vjust = .9, title.position = "top", title.hjust = .5)) +  
  coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
  facet_grid(months ~stressor, 
             switch = "y",
             labeller = labeller(months = months.labs)) +
  labs(title = "Bee Colonies Decline (%) Across U.S.",
       caption = "#TidyTuesday Week 2 - 2022 | Data from USDA | Chart by @_AlbertMorera")+
  ggthemes::theme_map(base_size = 18) +
  theme(legend.margin = margin(0,0,0,0),
        legend.box.margin=margin(0,0,-15,0),
        legend.position = "top", 
        legend.justification = c(.5,.5),
        legend.background = element_rect(fill = NA), 
        strip.background = element_rect(color = NA, fill = NA),
        text = element_text(family = "Gill Sans MT"),
        plot.title = element_text(hjust = 0.5, margin = margin(0,0,-5,0), family = "Gill Sans MT", size = 30),
        plot.caption = element_text(margin = margin(15,0,0,0), family = "mono", size = 10)
        )

ggsave("2022/W2/BeeColoniesDecline.png", pl, width = 15, height = 10, units = "cm", dpi = 300)
