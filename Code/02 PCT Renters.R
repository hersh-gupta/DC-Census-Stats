library(tidyverse)
library(sf)
library(sp)
library(viridis)
library(hrbrthemes)
library(tidycensus)


#Get variables
variables <- tibble(name = c("Total", "Owner Occupied", "Renter Occupied"),
                    id = c("B25008_001","B25008_002","B25008_003"))
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
  pivot_wider(id_cols = 1, names_from = 6, values_from = 4) %>% 
  mutate(`Percent Renter Occupied` = `Renter Occupied`/Total,
         `Percent Owner Occupied` = `Owner Occupied`/Total) %>%
  pivot_longer(cols = 5:6) %>% select(GEOID,name,value) 

#Merge Shapefile and Census data
dc_merged <- sp::merge(dc, acs_merged, by = "GEOID", duplicateGeoms = TRUE)

#Convert to sf
dc_sf <- st_as_sf(dc_merged, crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

#Plot
p <- dc_sf %>%
  ggplot(aes(fill = value)) +
  geom_sf(color = NA)+
  coord_sf(crs = 4326, datum = NA, clip = "off") + 
  facet_grid(~name) +
  labs(title = "Percent of Renters and Owners by Ward", fill = "", 
       caption = paste0("@gupta_hersh",str_pad("Source: Five Year ACS 2018", width = 105, side = "left")))+
  scale_fill_gradient(breaks=waiver(),
                       #option = "magma",
                       guide = guide_legend(keyheight = unit(3, units = "mm"),
                                            keywidth=unit(12, units = "mm"),
                                            label.position = "bottom",
                                            title.position = "top", nrow=1),
                       labels = scales::percent_format(accuracy = 1)) +
  theme_ipsum_pub(base_size = 14, axis = F, ticks = F, grid = F, 
                  plot_title_margin = margin(0,0,0,0)) +
  theme(legend.position = "top",
        legend.box.margin = margin(0,0,0,0),
        legend.margin = margin(0,0,0,0),
        plot.caption = element_text(hjust = 0),
        panel.spacing = unit(6, "lines"))

#Save
ggsave(p, filename = here::here("Output","PCT Renters.png"), device = "png", dpi = 320, 
       width = 8, height = 4.5, scale = 1.11)
