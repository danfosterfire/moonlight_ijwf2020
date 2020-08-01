
#### setup #####################################################################

library(here)
library(tidyverse)
library(sf)

# load data
fuels = readRDS(here::here('02-data', '01-intermediate', 'fuel_loads.rds'))
cover_long = readRDS(here::here('02-data', '02-for_analysis', 'cover_long.rds'))
patches = readRDS(here::here('02-data', '02-for_analysis', 'patches.rds'))

subtransects.sf = 
  st_read(here::here('02-data',
                     '00-source',
                     '99-gis',
                     'transects',
                     'subtransects.shp'))



#### munging cover data ########################################################

# need to aggregate the cover-long table from 0.25 chunk per row to 10m 
# subsubtransect per row. Ultimately, we want a binary measure of 
# whether tall (>= 0.5m) shrub cover >= 5m out of each 10m
cover_fuels = 
  cover_long %>%
  mutate(subsubtransect = ifelse(position_m < 10,
                                 3,
                                 ifelse(position_m < 20,
                                        13,
                                        ifelse(position_m < 30,
                                               23,
                                               NA)))) %>%
  mutate(subsubtransect = paste0(subtransect, ':', subsubtransect)) %>%
  group_by(time, treatment, planting, followup, block, plot, transect, subtransect, 
           subsubtransect, cover_type, avg_height_cm) %>%
  summarise(cover_length_m = 0.25*n()) %>%
  ungroup() %>%
  mutate(high_fuels = ifelse(avg_height_cm >= 50 & cover_type == 'shrub',
                             1,
                             0)) %>%
  group_by(time, treatment, planting, followup, block, plot, transect, subtransect, subsubtransect) %>%
  summarise(high_fuels_length_m = sum(high_fuels*cover_length_m,
                                      na.rm = TRUE),
            total_fuels_length_m = sum(cover_length_m,
                                       na.rm = TRUE),
            p_high = high_fuels_length_m / total_fuels_length_m,
            high_shrub_fuels = ifelse(p_high >= 0.5,
                                      TRUE,
                                      FALSE)) %>%
  ungroup()

cover_fuels
hist(cover_fuels$p_high)
summary(cover_fuels$p_high)

#### munging fuels data ########################################################

# we want to flag whether the fuel load for the subsubtransect is >= 10.02 tons/acre
head(fuels)

# convert threshold from tons/acre to mg/ha
anderson10 = 10.02*(0.907185)*(1/0.404686)

fuels = 
  fuels %>%
  mutate(fwd_mgha = x1h_mgha+x10h_mgha+x100h_mgha,
         litterduff_mgha = litter_mgha+duff_mgha) %>%
  select(time, treatment, planting, followup, 
         block, plot, transect, subtransect, subsubtransect,
         litterduff_mgha, fwd_mgha, cwd_mgha = x1000h_mgha, total_mgha) %>%
  mutate(high_fwd = ifelse(fwd_mgha >= anderson10, TRUE, FALSE))


#### combine fuel and shrubs ###################################################

fuel_types = 
  fuels %>%
  left_join(cover_fuels %>%
              select(time, treatment, planting, followup, 
                     block, plot, transect, subtransect, subsubtransect,
                     p_highshrubs = p_high, high_shrub_fuels,
                     highshrubs_m = high_fuels_length_m, total_m = total_fuels_length_m)) %>%
  filter(!is.na(high_shrub_fuels)) %>%
  mutate(high_intensity_fuels = ifelse(high_fwd | high_shrub_fuels, TRUE, FALSE))


#### geolocate observations ####################################################

# create a subtransect ID string for the line segements which matches 
# the formatting in the data, and select only the early seral data
subtransects.sf = 
  subtransects.sf %>%
  mutate(subtransect = 
           paste0(seral, BlockID, ':',
                  PlotTrtmt, ':',
                  trt, ':',
                  strt)) %>%
  filter(seral == 'E') %>%
  select(subtransect)


# convert subtransect line seements to sub-sub-transect midpoints
points_10m = 
  sf::st_line_sample(subtransects.sf, sample = c(5, 15, 25)/30) %>%
  sf::st_cast('POINT') %>%
  sf::st_sf()

# bind the observation ID columns back onto the points
bind_obs_id = 
  function(points.sf){
    points.sf = cbind(points.sf,
                      subtransects.sf[sf::st_nearest_feature(points.sf,
                                                          subtransects.sf),])
    points.sf$observation_id = NULL
    points.sf$geometry.1 = NULL
    return(points.sf)
  }

points_10m = 
  bind_obs_id(points_10m) %>%
  
  # and add a subsubtransect id to the points
  mutate(subsubtransect = rep(c(3, 13, 23),
                              times = length(subtransects.sf$geometry)),
         subsubtransect = paste0(subtransect, ':', subsubtransect))

# and bind the observation data onto the poinst
fuel_types.sf = 
  points_10m %>%
  left_join(.,
            y = fuel_types) %>%
  
  # drop any points without observation data
  filter(!is.na(high_intensity_fuels))

# make a dataframe version too
fuel_types = 
  fuel_types.sf %>%
  mutate(x = st_coordinates(.)[,'X'],
         y = st_coordinates(.)[,'Y']) %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

#### data cleaning #############################################################

# keep only subsubtransects with both PRE and POST data
missing_pre_or_post = 
  fuel_types %>%
  group_by(subsubtransect, time) %>%
  summarise() %>%
  ungroup() %>%
  group_by(subsubtransect) %>%
  summarise(time = length(unique(time))) %>%
  ungroup() %>%
  filter(time != 2)

to_drop = missing_pre_or_post$subsubtransect

fuel_types = 
  fuel_types %>%
  filter(!is.element(subsubtransect, to_drop))

fuel_types.sf = 
  fuel_types.sf %>%
  filter(!is.element(subsubtransect, to_drop))

#### write results #############################################################

write.csv(fuel_types,
          here::here('02-data', '02-for_analysis', 'obs_10m.csv'),
          row.names = FALSE)
st_write(fuel_types.sf,
         here::here('02-data', '02-for_analysis', 'obs_10m.shp'),
         delete_dsn = TRUE)
saveRDS(fuel_types.sf,
        here::here('02-data', '02-for_analysis', 'obs_10m.rds'))


