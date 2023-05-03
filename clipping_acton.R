# Loading Packages --------------------------------------------------------
library(tidyverse)
library(sf)


# I first attempted to conduct a pairwise clip in ArcGIS Pro, but
# I was having trouble 'accessing the permissions', so I performed 
# that operation here. This information is present on the demographic
# portion of my Acton, MA dashboard.




# Geographic Information --------------------------------------------------
census_2010 <- st_read("data/Census2010_tracks_joined.shp")

acton <- st_read("data/TOWNSSURVEY_POLY.shp") %>% 
  filter(TOWN == 'ACTON')


census_2010 %>% 
  view()

acton %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  theme_void()


census_2010 %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  theme_void()


acton_census <- st_intersection(census_2010, acton) %>% 
  janitor::clean_names() %>%
  st_transform(crs = 3395)
  filter(town == "ACTON")


acton_census %>% 
  view()
  
acton_census %>% 
  st_write("data/acton_census_joined.shp") 



test_shp <- st_read("data/acton_census_joined.shp")




test_shp %>% 
  ggplot() +
  geom_sf(
    data = census_2010,
    aes(geometry = geometry)
  ) +
  geom_sf(
    aes(
      fill = medn_nc,
      geometry = geometry
      )
    ) +
  theme_void()

st_read("data/acton_census_joined.shp") %>% 
  glimpse()





# Commonwealth-Wide Demographics ------------------------------------------
census_2010 %>% 
  select(
    c(median_inc, black, hispanic, pct_renter)
  ) %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  summarize(
    avg_median_inc = mean(median_inc, na.rm = TRUE),
    avg_black_pct = mean(black, na.rm = TRUE),
    avg_hispanic_pct = mean(hispanic, na.rm = TRUE),
    avg_renter_pct = mean(pct_renter, na.rm = TRUE)
  )


acton_census %>% 
  select(
    c(median_inc, black, hispanic, pct_renter)
  ) %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  summarize(
    avg_median_inc = mean(median_inc, na.rm = TRUE),
    avg_black_pct = mean(black, na.rm = TRUE),
    avg_hispanic_pct = mean(hispanic, na.rm = TRUE),
    avg_renter_pct = mean(pct_renter, na.rm = TRUE)
  )
