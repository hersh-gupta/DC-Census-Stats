library(tidyverse)
library(sf)
library(sp)
library(viridis)
library(hrbrthemes)
library(tidycensus)


#Get variables
variables <- tibble(name = c("Total",
  "0 to 20%","0 to 20%","0 to 20%",
  "20 to 40%","20 to 40%","20 to 40%","20 to 40%",
  "40% or Greater","40% or Greater"
  #"Missing"
  ),
  id = c("B25070_001",
    "B25070_002","B25070_003","B25070_004",
    "B25070_005","B25070_006","B25070_007","B25070_008",
    "B25070_009","B25070_010"
    #"B25070_011"
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
  mutate_at(.vars = 3:ncol(.), ~.x/Total) %>%
  pivot_longer(cols = 3:ncol(.)) %>% 
  select(GEOID,name,value) %>% mutate(name = factor(name, levels = variables$name[-1]%>% unique()))

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
  labs(title = "Gross Rent as a Percentage of Gross Income by Ward", fill = "% of Renters", 
       caption = paste0("@gupta_hersh",str_pad("Source: Five Year ACS 2018", width = 160, side = "left")))+
  scale_fill_distiller(breaks=c(.1,.2,.3,.4,.5),
                       #breaks = waiver(),
                       #option = "plasma",
                       palette = "OrRd",
                       direction = 1,
                       #trans = "log",
                       guide = guide_legend(keyheight = unit(3, units = "mm"),
                                            keywidth=unit(12, units = "mm"),
                                            label.position = "bottom",
                                            title.position = "left", nrow=1),
                       labels = scales::percent_format(accuracy = 1)) +
  theme_ipsum_pub(base_size = 14, axis = F, ticks = F, grid = F, 
                  plot_title_margin = margin(20,0,0,0, unit = "mm")) +
  theme(legend.position = "none",
        legend.box.margin = margin(15,0,0,0),
        legend.margin = margin(0,0,0,0),
        plot.caption = element_text(hjust = 0),
        #panel.spacing = unit(6, "lines")
  )

p
#Save
ggsave(p, filename = here::here("Output","Cost Burden.png"), device = "png", dpi = 320, 
       width = 8, height = 4.5, scale = 1)






