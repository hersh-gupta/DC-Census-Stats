library(tidyverse)
library(sf)
library(sp)
library(viridis)
library(hrbrthemes)
library(tidycensus)
library(ggthemes)

#Get variables
variables <- tibble(name = c("White", "Black", "Asian", "Hispanic",
                             #"Studio",
                             #"1 Bedroom",
                             "median_pct"
                             #"3 Bedrooms",
                             #"4 Bedrooms",
                             #"5+ Bedrooms"
                             ),
                    id = c("B19013A_001","B19013B_001","B19013D_001","B19013I_001",
                           #"B25031_002",
                           #"B25031_003",
                           "B25071_001"
                           #"B25031_005",
                           #"B25031_006",
                           #"B25031_007"
                           ))

#Get Census Tract data
acs <- get_acs(geography = "tract",
               variables = variables$id,
               geometry = F, 
               state = "DC",
               cb = FALSE, 
               survey = "acs5"
)

#Cleanup Census Tract data
acs_merged <- acs %>%
  left_join(variables, by = c("variable" = "id")) %>%
  pivot_wider(id_cols = 1, names_from = 6, values_from = 4) %>%
  pivot_longer(cols = 2:5, names_to = "race", values_to = "med_income") %>%
  mutate(median_pct = median_pct/100)

#Get Census Ward data
acs_w <- get_acs(geography = "state legislative district (upper chamber)",
               variables = variables$id,
               geometry = F, 
               state = "DC",
               cb = FALSE, 
               survey = "acs5"
)

#Cleanup Census Ward data
acs_merged_w <- acs_w %>%
  left_join(variables, by = c("variable" = "id")) %>%
  pivot_wider(id_cols = 1, names_from = 6, values_from = 4) %>%
  pivot_longer(cols = 2:5, names_to = "race", values_to = "med_income")%>%
  mutate(median_pct = median_pct/100)


dc_tracts <- tigris::tracts(state = "11", cb = T)
dc_wards <- tigris::state_legislative_districts(state = "11", house = "upper", cb = T)

#Convert to sf
dc_tracts_sf <- st_as_sf(dc_tracts, crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
dc_wards_sf <- st_as_sf(dc_wards, crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

dc_tract_ward <- st_covered_by(dc_tracts_sf,dc_wards_sf,sparse = T)

ward_tract_merged <- dc_tracts_sf %>%
  mutate(tract = row_number()) %>% left_join(
dc_tract_ward %>% 
  as_tibble() %>% 
  select(tract = 1, ward = 2)) 

afford <- acs_merged %>% sp::merge(ward_tract_merged %>% 
                           select(GEOID, ward), by = "GEOID") %>% 
  mutate(ward = as.factor(ward) %>% fct_explicit_na(na_level = "Tract Overlaps\nMultiple Wards")) 



p <- afford %>%
    filter(median_pct < 1) %>%
  ggplot(aes(y = median_pct,
             x = med_income, 
             color = ward)) + 
  geom_hline(yintercept = .3,lty = 2, lwd = 1, alpha = .5) +
  geom_point(alpha = .5, size = 3, stroke = 1) + 
  #geom_point(data = acs_merged_w, alpha = 1, size = 3, stroke = 1, color = "grey") +
  facet_wrap(~race) + 
  labs(y = "Median Gross Rent as a Percentage of Household (%)",
       x = "Median Census Tract Household Income ($)",
       title = "Cost Burden in DC",
       subtitle = "DC Census Tracts by Median Gross Rent as a Percentage of Household Income and Race", 
       color = "Ward",
       caption = paste0("@gupta_hersh",str_pad("Source: Census Five Year ACS 2018", width = 280, side = "left"))) + 
  scale_x_continuous(label = scales::unit_format(prefix = "$", unit = "K", accuracy = 1, scale = 1/1000, big.mark = ""))+
  scale_y_continuous(label = scales::percent_format(accuracy = 1), breaks = seq(.1,1,.2))+
  # geom_text(data = tibble(med_income = 130000, cost_burden = .42, Race = "Black"), 
  #           mapping = aes(x = med_income, y = cost_burden),
  #           label = "Census Tracts where\nMedian 2-Bedroom Unit Rent\nExceeds 30% Median Income ↑", size = 4, hjust = 0, 
  #           fontface = "bold", lineheight = .8 , inherit.aes = F, alpha = .5) +
  ggthemes::scale_fill_tableau() +
  #geom_smooth(method = "lm", se = F) +
  #geom_abline(lty = 2, lwd = 1, alpha = .5) +
    theme_ipsum_pub(base_size = 12, axis_title_just = 0) +
    theme(plot.caption = element_text(hjust = 0, margin = margin(t = 15)),
          axis.title.x = element_text(margin = margin(t = 15)),
          axis.title.y = element_text(margin = margin(r = 15)),
          #legend.text = element_text(margin = margin(t = 10)),
          legend.key.height = unit(8, "mm"),
          #panel.spacing = unit(6, "lines")
    )

p

ggsave(p, filename = here::here("Output","Cost Burden.png"), device = "png", dpi = 320, 
       width = 8, height = 4.5, scale = 1.75)
