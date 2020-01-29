library(tidyverse)
library(sf)
library(sp)
library(viridis)
library(hrbrthemes)
library(tidycensus)


#Get variables
variables <- tibble(name = c("Total", 
                             "Europe",
                             "Asia",
                             "Africa",
                             "Latin America"),
                    id = c("B05006_001",
                           "B05006_002",
                           "B05006_047",
                           "B05006_091",
                           "B05006_124"
                    ))
#Get shapefiles
#dc <- tigris::state_legislative_districts(state = "11", house = "upper", cb = T)

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
  pivot_wider(id_cols = 1, names_from = 6, values_from = 4, values_fn = list(estimate = sum)) %>% 
  mutate_at(.vars = 3:6, ~.x/Total) %>%
  pivot_longer(cols = 3:6) %>% select(GEOID,name,value) %>% mutate(name = factor(name, levels = variables$name[-1] %>% unique()))

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
  labs(title = "Foreign-Born Population by Ward", fill = "", 
       caption = paste0("@gupta_hersh",str_pad("Source: Five Year ACS 2018", width = 165, side = "left")))+
  scale_fill_viridis_c(#breaks=seq(0,.1,.02),
                       breaks = waiver(),
                       #option = "magma",
                       trans = "log",
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
        #panel.spacing = unit(6, "lines")
  )

p
#Save
ggsave(p, filename = here::here("Output","Foreign Born.png"), device = "png", dpi = 320, 
       width = 8, height = 4.5, scale = 1)




