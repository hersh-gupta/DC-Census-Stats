library(tidyverse)
library(sf)
library(sp)
library(viridis)
library(hrbrthemes)
library(tidycensus)


#Get variables
variables <- tibble(name = c("Studio",
                             "1 Bedroom",
                             "2 Bedrooms",
                             "3 Bedrooms",
                             "4 Bedrooms",
                             "5+ Bedrooms"),
                    id = c("B25031_002",
                           "B25031_003",
                           "B25031_004",
                           "B25031_005",
                           "B25031_006",
                           "B25031_007"))
#Get shapefiles
dc <- tigris::state_legislative_districts(state = "11", house = "upper", cb = T)

#Get Census data
acs <- get_acs(geography = "state legislative district (upper chamber)",
               variables = variables$id,
               geometry = FALSE, 
               state = "DC",
               cb = FALSE, 
               survey = "acs5"
)

#Cleanup Census data
acs_merged <- acs %>%
  left_join(variables, by = c("variable" = "id")) %>%
  mutate(value =(estimate*12)/.3) %>%
  select(GEOID,name,value) %>% mutate(name = factor(name, levels = variables$name))

#Merge Shapefile and Census data
dc_merged <- sp::merge(dc, acs_merged, by = "GEOID", duplicateGeoms = TRUE)

#Convert to sf
dc_sf <- st_as_sf(dc_merged, crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

#Plot
p <- dc_sf %>%
  ggplot(aes(fill = value)) +
  geom_sf(color = NA)+
  coord_sf(crs = 4326, datum = NA, clip = "off") + 
  facet_grid(~name,labeller = label_wrap_gen(width=20)) +
  labs(title = "Household Income Required to Afford Median Rent in Each Ward", fill = "", 
       caption = paste0("@gupta_hersh",str_pad("Assumes 30% Rental Burden, Source: Five Year ACS 2018", width = 200, side = "left")))+
  scale_fill_distiller(breaks = waiver(),
                       palette = "YlGn",
                       direction = 1,
                       #trans = "log",
                       guide = guide_legend(keyheight = unit(3, units = "mm"),
                                            keywidth=unit(12, units = "mm"),
                                            label.position = "bottom",
                                            title.position = "top", nrow=1),
                       labels = scales::dollar_format(accuracy = 100)) +
  theme_ipsum_pub(base_size = 14, axis = F, ticks = F, grid = F, 
                  plot_title_margin = margin(0,0,0,0)) +
  theme(legend.position = "top",
        legend.box.margin = margin(0,0,0,0),
        legend.margin = margin(0,0,0,0),
        plot.caption = element_text(hjust = 0)
  )

p
#Save
ggsave(p, filename = here::here("Output","Rents.png"), device = "png", dpi = 320, 
       width = 8, height = 4.5, scale = 1.3)




