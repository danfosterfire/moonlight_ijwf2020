#### setup #####################################################################

# load packages
library(here)
library(tidyverse)
library(Rfuels)

# load funcitons

# load data
cwd_pre = 
  read_csv(here::here('02-data', 
                      '01-intermediate', 
                      '00-tidy_observations',
                      'cwd_pre.csv'))


fwd_pre = 
  read_csv(here::here('02-data', 
                      '01-intermediate', 
                      '00-tidy_observations',
                      'fwd_pre.csv'))


litterduff_pre = 
  read_csv(here::here('02-data', 
                      '01-intermediate', 
                      '00-tidy_observations',
                      'litterduff_pre.csv'))


cwd_post = 
  read_csv(here::here('02-data', 
                      '00-source', 
                      '02-tidy_2019',
                      '2019_early_seral_fuel_1000h.csv')) %>%
  select(-X9, -X10)


fwd_post = 
  read_csv(here::here('02-data', 
                      '00-source', 
                      '02-tidy_2019',
                      '2019_early_seral_fuel_1_to_100h.csv'))


litterduff_post = 
  read_csv(here::here('02-data', 
                      '00-source', 
                      '02-tidy_2019',
                      '2019_early_seral_fuel_depths.csv'))


#### combine pre and post ######################################################



cwd = rbind(cwd_pre %>%
              mutate(time = 'pre'),
            cwd_post %>%
              mutate(time = 'post')) %>%
  mutate(diameter_cm = as.numeric(diameter_cm)) %>%
  
  # that NA diameter was listed as 'NONE', so somebody put an empty row in:
  filter(!is.na(diameter_cm))

fwd = 
  rbind(fwd_pre %>%
          mutate(time = 'pre'),
        fwd_post %>%
          mutate(time = 'post')) %>%
  mutate(x1_hour_count = as.numeric(x1_hour_count),
         x10_hour_count = as.numeric(x10_hour_count),
         x100_hour_count = as.numeric(x100_hour_count))

litterduff = 
  rbind(litterduff_pre %>%
          mutate(time = 'pre'),
        litterduff_post %>%
          mutate(time = 'post')) %>%
  mutate(litter_cm = as.numeric(litter_cm),
         duff_cm = as.numeric(duff_cm),
         fuel_cm = as.numeric(fuel_cm))

seedlings = 
  rbind(seedlings_pre %>%
          mutate(time = 'pre',
                 height_cm = height_m * 100) %>%
          select(-height_m),
        seedlings_post %>%
          mutate(time = 'post') %>%
          filter(condition == 'LIVE') %>%
          select(-condition))



#### data munging fuels ########################################################

# aggregate to the 10m subsubtransect level; note that CWD is pseudoreplicated 
# at this level, so we'll need to aggregate again up to 30m subtransects for the 
# CWD model
fuels = 
  fwd %>%
  left_join(litterduff %>%
              mutate(startpoint_m = (floor(location_m/10)*10)+3),
            by = c('block', 'plot', 'transect', 'subtransect', 'startpoint_m', 
                   'time', 'date')) %>%
  select(block, plot, transect, subtransect, startpoint_m, location_m, date, time, 
         slope_percent = slope, litter_cm, duff_cm, fuel_cm, 
         x1h_count = x1_hour_count, 
         x10h_count = x10_hour_count,
         x100h_count = x100_hour_count) %>%
  left_join(cwd %>%
              group_by(block, plot, transect, subtransect, time, decay_class) %>%
              summarise(ssd_cm2 = sum(diameter_cm**2)) %>%
              ungroup() %>%
              pivot_wider(names_from = decay_class, values_from = ssd_cm2)) %>%
  mutate(sum_d2_1000r_cm2 = ifelse(is.na(R), 0, R),
         sum_d2_1000s_cm2 = ifelse(is.na(S), 0, S),
         subsubtransect = paste0(subtransect, ':', startpoint_m)) %>%
  select(-R, -S, startpoint_m) %>%
  group_by(block, plot, transect, subtransect, subsubtransect, time, date, slope_percent) %>%
  summarise_at(vars(litter_cm, duff_cm, fuel_cm, 
               x1h_count, x10h_count, x100h_count, 
               sum_d2_1000r_cm2, sum_d2_1000s_cm2),
               function(x){mean(x, na.rm = TRUE)}) %>%
  ungroup()

slope_test = 
  fuels %>%
  select(subtransect, time, slope_percent) %>%
  group_by(subtransect, time) %>%
  summarise(slope_percent = unique(slope_percent)) %>%
  pivot_wider(names_from = time, values_from = slope_percent) %>%
  filter(is.na(post) | is.na(pre))

# 7 locations for which we are missing slope
slope_test

# fill i with the mean slope from all observations
mean_slope = mean(fuels$slope_percent, na.rm = TRUE)
mean_slope

fuels = 
  fuels %>%
  mutate(slope_percent = ifelse(is.na(slope_percent), mean_slope, slope_percent))

head(fuels)

#### estimate fuel loads #######################################################

Rfuels_fuels = 
  fuels %>%
  select(plot_id = subsubtransect,
         inv_date = date,
         slope_percent,
         count_x1h = x1h_count,
         count_x10h = x10h_count,
         count_x100h = x100h_count,
         duff_depth_cm = duff_cm,
         litter_depth_cm = litter_cm,
         sum_d2_1000r_cm2,
         sum_d2_1000s_cm2) %>%
  mutate(azimuth = 90,
         x1h_length_m = 3,
         x10h_length_m = 3,
         x100h_length_m = 4,
         x1000h_length_m = 30) %>%
  select(plot_id, inv_date, azimuth, slope_percent, x1h_length_m, x10h_length_m,
         x100h_length_m, x1000h_length_m, count_x1h, count_x10h, count_x100h,
         duff_depth_cm, litter_depth_cm, sum_d2_1000r_cm2, sum_d2_1000s_cm2) %>%
  mutate(inv_date = as.character(inv_date, format = '%m/%d/%Y')) %>%
  as.data.frame()

head(Rfuels_fuels)

#We need to supply Rfuels a treelist so that it can assign loading coefficients 
#to the fuels. However, there are no treelists associated with the early seral 
#subtransects (there were no live trees in the early seral plots). We generate 
#a generic treelist of a single 'unknown' tree for each subtransect. The 
#'unknown' species will be assigned the 'all_species' generic coefficient for the 
#purpose of calculating fuel loads. 
treelist = 
  Rfuels_fuels %>%
  group_by(plot_id, inv_date) %>%
  summarise() %>%
  ungroup() %>%
  mutate(species = 'UNK',
         dbh_cm = 10) %>%
  as.data.frame()

head(treelist)

fuel_loads = Rfuels::estimate_fuel_loads(fuels_data = Rfuels_fuels,
                                         trees_data = treelist,
                                         results_type = 'full')

fuel_loads = 
  fuels %>%
  left_join(fuel_loads %>%
              mutate(fines_mgha = fuelload_fwd_Mgha+fuelload_litter_Mgha) %>%
              select(subsubtransect = plot_id,
                     date = inv_date,
                     litter_mgha = fuelload_litter_Mgha,
                     duff_mgha = fuelload_duff_Mgha,
                     x1h_mgha = fuelload_1h_Mgha,
                     x10h_mgha = fuelload_10h_Mgha,
                     x100h_mgha = fuelload_100h_Mgha,
                     x1000h_mgha = fuelload_1000h_Mgha,
                     fines_mgha,
                     total_mgha = fuelload_total_Mgha))

# fuels are aggregated to the subsubtransect (10m scale) observations; 
# litter and duff loads are calculated from the mean of the 2 samples per 10m
# note that CWD was sampled at the 30m scale, so it is pseudoreplicated at the 10m scale
head(fuels)

#### data cleaning #############################################################

# drop subtransects which wound up falling outside the treated area; 
# see 'Moonlight Notes.docx' with the raw 2019 data. I'm leaving in subtransects 
# with untreated 'islands', which were retained to preserve some ground cover 
# as part of the site prep Rx. Only dropping subtransects which wound up extending 
# beyond the bounds of the site prep. For simplicity, I'm going to drop the whole 
# 30m subtransect, which is a shame for some of these where >25m of the subtransect 
# was good. 
outside_treated = 
  c('E1:EG:M:3',
    'E1:EG:N:3',
    'E1:EH:N:1',
    'E2:CN:M:3',
    'E2:CN:N:3',
    'E2:CN:S:2',
    'E2:CN:S:3',
    'E2:CG:S:1',
    'E2:CG:S:3',
    'E3:EG:S:3',
    'E4:EG:S:3')
fuels = 
  fuels %>%
  filter(!is.element(subtransect, outside_treated))


# drop subtransects with NAs
missings = 
  fuels %>%
  filter(!complete.cases(fuels))

# only 2 obs, both from same subtransect
missings$subtransect

fuels = 
  fuels %>%
  filter(!is.element(subtransect, missings$subtransect))


# drop subttransets without PRE and POST data
missing_pre_or_post = 
  fuels %>%
  group_by(subtransect, time) %>%
  summarise() %>%
  ungroup() %>%
  group_by(subtransect) %>%
  summarise(time = length(unique(time))) %>%
  ungroup() %>%
  filter(time != 2)

missing_pre_or_post = missing_pre_or_post$time

fuels = 
  fuels %>%
  filter(!is.element(subtransect, missing_pre_or_post))

#### write results #############################################################

write.csv(fuels, here::here('02-data', '02-for_analysis', 'fuels.csv'))
saveRDS(fuels, here::here('02-data', '02-for_analysis', 'fuels.rds'))

