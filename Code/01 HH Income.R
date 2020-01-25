library(tidyverse)
library(sf)
library(sp)
library(viridis)
library(hrbrthemes)
library(tidycensus)


#Get variables
variables <- tibble(name = c("White", "Black", "Asian", "Hispanic"),
                    id = c("B19013A_001","B19013B_001","B19013D_001","B19013I_001"))
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
  mutate(name = fct_reorder(name, -estimate))

#Merge Shapefile and Census data
dc_merged <- sp::merge(dc, acs_merged, by = "GEOID", duplicateGeoms = TRUE)

#Convert to sf
dc_sf <- st_as_sf(dc_merged, crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

#Plot
dc_sf %>%
  ggplot(aes(fill = estimate)) +
  geom_sf(color = NA)+
  coord_sf(crs = 4326, datum = NA) + 
  facet_grid(~name) +
  labs(title = "Median Houshold Income by Race and Ward", fill = "", 
       caption = paste0("@gupta_hersh",str_pad("Source: Five Year ACS 2018", width = 165, side = "left")))+
  scale_fill_viridis_c(breaks=waiver(),
                       #option = "magma",
                       guide = guide_legend(keyheight = unit(3, units = "mm"),
                                            keywidth=unit(12, units = "mm"),
                                            label.position = "bottom",
                                            title.position = "top", nrow=1),
                       labels = scales::dollar_format()) +
  theme_ipsum_pub(base_size = 14, axis = F, ticks = F, grid = F, 
                  plot_title_margin = margin(0,0,0,0)) +
  theme(legend.position = "top",
        legend.box.margin = margin(0,0,0,0),
        legend.margin = margin(0,0,0,0),
        plot.caption = element_text(hjust = 0))

#Save
ggsave(filename = here::here("Output","HH Income.png"), device = "png", dpi = 320, width = 8, height = 4.5)
