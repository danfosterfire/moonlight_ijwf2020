
#### setup #####################################################################

library(here)
library(tidyverse)

# load data
cover_pre = 
  read_csv(here::here('02-data', 
                      '01-intermediate', 
                      '00-tidy_observations',
                      'cover_pre.csv'))



cover_post = 
  read_csv(here::here('02-data', 
                      '00-source', 
                      '02-tidy_2019',
                      '2019_early_seral_cover.csv')) %>%
  select(-X13)

#### combine pre and post data #################################################


# this is a little bit of a fudge: in 2017-2018 we only recorded live shrubs 
# as cover, and VERY rarely (if ever) ran into a situation where a dead shrub 
# was the only cover. Almost always it was either live on top of ground, or 
# live on top of dead. In 2019, after the herbiciding dead shrubs were 
# frequent, so we felt like we should also include dead shrubs to better 
# capture the actual fuel conditions. For the data to be strictly comparable, 
# we should drop all the post data where condition is not live. However, 
# for the purposes of the Moore et al. IJWF paper, we're relying on cover height 
# to sort "high" fuel loads from "low" fuel loads, so IMO it's OK to lump 
# these together. Technically, the pretreatment data have bare ground in any 
# locations where there was a dead shrub and no other cover, but that rarely 
# (if ever) occurred before the site prep. 
cover = rbind(cover_pre %>% 
                mutate(condition = 'LIVE',
                       time = 'pre',
                       avg_height_cm = avg_height_m*100) %>%
                select(-avg_height_m),
              cover_post %>% mutate(time = 'post'))

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

cover = 
  cover %>%
  filter(!is.element(subtransect, outside_treated))

# drop subtransects with bad start or end data
bad_locations = 
  cover %>%
  filter(cover_start_m < 0 | cover_start_m > 30 | cover_end_m < 0 | cover_end_m > 30) %>%
  group_by(subtransect) %>%
  summarise() %>%
  ungroup()

bad_locations = bad_locations$subtransect

cover = 
  cover %>%
  filter(!is.element(subtransect, bad_locations))

# drop subttransets without PRE and POST data
missing_pre_or_post = 
  cover %>%
  group_by(subtransect, time) %>%
  summarise() %>%
  ungroup() %>%
  group_by(subtransect) %>%
  summarise(time = length(unique(time))) %>%
  ungroup() %>%
  filter(time != 2)

missing_pre_or_post

missing_pre_or_post = missing_pre_or_post$subtransect
cover = 
  cover %>%
  filter(!is.element(subtransect, missing_pre_or_post))


#### data munging ##############################################################

# right now, the transects are recorded in both directions. Cover start might 
# be the lower number or the higher number; we need to consistently have one 
# column be the lower number and another column the higher one, so make 
# some new position columns:
cover = 
  cover %>%
  mutate(location_min = ifelse(cover_start_m < cover_end_m,
                               cover_start_m,
                               ifelse(cover_end_m < cover_start_m,
                                      cover_end_m,
                                      NA)),
         location_max = ifelse(cover_start_m < cover_end_m,
                               cover_end_m,
                               ifelse(cover_start_m > cover_end_m,
                                      cover_start_m,
                                      NA)))

# map specific cover to broader cover types:
cover_types = 
  data.frame(species = 
               c('POAZ', 'POLZ', 'G',
                 'MON-', 'POLZ', 'ACMI', 'ASTZ', 'LUAL', 'WYMO', 'APAN', 'F', 
                    'GF', 'PTAQ', 'FAB',
                 'ARPA', 'CECO', 'CEPR', 'CEVE', 'PREM', 'RIBES', 'RINE', 
                      'SYMO', 'SALIX', 'SAME', 'SAL', 'RUPA', 'CICO', 'SYM', 'RIRO', 'SAM',
                 'BG', 'ROCK',
                 'DWD', 'LITTER', 'L',
                 'PIPO', 'PIJE'),
             cover_type = 
               c(rep('grass', times = 3),
                 rep('forb', times = 11),
                 rep('shrub', times = 16),
                 rep('bare', times = 2),
                 rep('fuel', times = 3),
                 rep('conifer', times = 2))) %>%
  mutate(species = as.character(species),
         cover_type = as.character(cover_type))

# map NA species to unknown; should maybe just ditch these subtransects...
cover = 
  cover %>%
  left_join(cover_types,
            na_matches = 'na') %>%
  mutate(cover_type = ifelse(is.na(cover_type), 'UNK', cover_type))

# finally, add a 'treatment' column for analysis later
cover = 
  cover %>% 
  mutate(treatment = gsub(plot, pattern = '^.*:', replacement = ''),
         planting = substr(treatment, 1, 1),
         followup = substr(treatment, 2, 2))

#### cover long ################################################################

# make a table of all the midpoints for each included subtransect

# want a table with 1 row per location, instead of 1 row per patch
# this lapply gives you a list where every item in the 
# list is a vector of numbers. Each number is the midpoint of a 0.25m segment
# along the transect:
cover$position_m = 
  lapply(X = 1:nrow(cover), 
         FUN = function(i){
           seq(from = cover$location_min[i]+(0.25/2), 
               to = cover$location_max[i]-(0.25/2), 
               by = 0.25)})

# take the list of vectors, where each element is all the locations in 
# between location_min and location_max, and make it longwise where each 
# row is a location:
cover_long = 
  cover %>%
  unnest(position_m) %>%
  select(time, treatment, planting, followup, block, plot, transect, subtransect, species,
         cover_type, avg_height_cm, position_m) %>%
  arrange(time, block, plot, transect, subtransect, position_m) %>%
  
  # add an expected fire behavior type
  mutate(fire_type = 
           ifelse(cover_type == 'shrub' & avg_height_cm > 0.5,
                  'high',
                  ifelse(is.element(cover_type, c('shrub', 'grass', 'forb', 
                                                  'fuel', 'conifer')),
                         'low',
                         ifelse(cover_type == 'bare',
                                'none',
                                NA)))) 

#### moebius ###################################################################
# this table already has 1 row per patch, but we want to aggregate contiguous 
# patches across subtransect boundaries and by doing the moebius strip thing 
# connectiong 0m to 90m on each transect


# convert location from being a by-subtransect position (0-30m) to a by-transect 
# postiion (0-90m)
cover = 
  cover %>%
  mutate(st = gsub(x = subtransect,
                   pattern = '.*:',
                   replacement = '')) %>%
  mutate(location_min = ifelse(st == '1',
                               location_min,
                               ifelse(st == '2',
                                      location_min+30,
                                      ifelse(st == '3',
                                             location_min+60,
                                             NA))),
         location_max = ifelse(st == '1',
                               location_max,
                               ifelse(st == '2',
                                      location_max+30,
                                      ifelse(st == '3',
                                             location_max+60,
                                             NA)))) %>%
  select(-st)

# first simplify the table to just the relevant columns
patches = 
  cover %>%
  select(time, treatment, planting, followup, block, plot, transect, location_min, location_max, 
         species, cover_type, cover_length, avg_height_cm) %>%
  group_by(time, transect) %>%
  arrange(time, transect, location_min) %>%
  ungroup() %>%
  mutate(obs = paste0(time, ':', transect))

head(patches)





# set the current patch ID to 1, and make an empty vector of patch IDs we'll fill
current_patch = 1
patch_id = numeric(nrow(patches))

# loop through every row except the last row of the patches table
for(i in 1:(nrow(patches)-1)){
  
  # weve ditched some subtransects, so get the min and max locations for 
  # each observation (transect:time)
  first_patch_starts = 
    min(patches$location_min[patches$obs==patches$obs[i]])
  
  # get the first patch in this obs
  first_patch = 
    patches[patches$obs==patches$obs[i]&
              patches$location_min==first_patch_starts,]
  
  
  if (
    # this patch and the next one have the same cover type AND the next patch is in the same obs
    patches$cover_type[i] == patches$cover_type[i+1] & 
    patches$obs[i] == patches$obs[i+1]){
    
    # then this patch and the next share an id, so store the current ID and 
    # proceed WITHOUT incrementing the id
    patch_id[i] = current_patch
    
  } else if (
    
    # if this is the last patch in the obs, and its the same cover type 
    # as the first patch in the obs
    patches$obs[i] != patches$obs[i+1] & 
    patches$cover_type[i] == first_patch$cover_type[1]
  ){
    
    # then this patch and the first patch in the obs share the same id, 
    # so store that one and start the id's back at 1 for the next time:transect
    patch_id[i] = patch_id[patches$obs==patches$obs[i] & 
                           patches$location_min == first_patch_starts][1]
    
    current_patch = 1
    
  } else {
    
    # if neither of the above is true, then this is a new patch, so store 
    # the current patch id and then increment to the next id
    patch_id[i] = current_patch
    current_patch = current_patch + 1
    
  }
}

# finally, assign an id to the last patch in the whole table:
# weve ditched some subtransects, so get the min and max locations for 
# each observation (transect:time)
last_first_patch_starts = 
  min(patches$location_min[patches$obs==patches$obs[nrow(patches)]])

# get the first patch in this obs
last_first_patch_type = 
  unique(patches$cover_type[patches$obs==patches$obs[nrow(patches)]&
            patches$location_min==first_patch_starts])

patch_id[nrow(patches)] = 
  if (
    # if  its the same cover type as the first patch in the obs
    patches$cover_type[nrow(patches)] == last_first_patch_type
  ){
    
    # then this patch and the first patch in the obs share the same id, 
    # so store that one and start the id's back at 1 for the next time:transect
    patch_id[patches$obs==patches$obs[nrow(patches)] & 
              patches$location_min == last_first_patch_starts][1]
    
  } else {
    
    # otherise, this last patch is a new patch, of the above is true, 
    # then this is a new patch, so store the current patch id
    current_patch
    
  }

# add the new id values, drop the obs ids, and aggregate on the patch id
patches = 
  patches %>%
  mutate(patch_id = 
           paste0(obs, 
                  ':', 
                  str_pad(patch_id, width = 5, side = 'left', pad = 0))) %>%
  select(-obs) %>%
  group_by(time, treatment, planting, followup, block, plot, transect, patch_id) %>%
  
  # take the only cover type present, 
  # a weighted-by-cover-length average height, 
  # and the total cover length
  summarise(cover_type = unique(cover_type),
            avg_height_cm = sum(avg_height_cm*(cover_length/sum(cover_length,
                                                                na.rm = TRUE)),
                                na.rm = TRUE),
            cover_length = sum(cover_length, na.rm = TRUE)) %>%
  ungroup() %>%
  
  # add an expected fire behavior type
  mutate(fire_type = 
           ifelse(cover_type == 'shrub' & avg_height_cm > 0.5,
                  'high',
                  ifelse(is.element(cover_type, c('shrub', 'grass', 'forb', 
                                                  'fuel', 'conifer')),
                         'low',
                         ifelse(cover_type == 'bare',
                                'none',
                                NA)))) 

#### checking ##################################################################

# data exploration making sure we got that right
ggplot(data = patches,
       aes(x = cover_type, y = avg_height_cm, fill = time))+
  geom_boxplot()

ggplot(data = patches,
       aes(x = cover_length, y = avg_height_cm, color = cover_type))+
  geom_point()

# make sure we don't have any too-long patches
patches[patches$cover_length>90,]

ggplot(data = patches,
       aes(x = cover_type, y = cover_length, fill = time))+
  geom_boxplot()+
  scale_y_log10()

# y axis is total length
ggplot(data = patches,
       aes(x = time, weight = cover_length, fill = cover_type))+
  geom_bar(position = position_stack())+
  scale_fill_viridis_d()+
  labs(y = 'total cover (m)')

# only keep transects with PRE and POST observations
missing_pre_or_post = 
  patches %>%
  group_by(transect, time) %>%
  summarise() %>%
  ungroup() %>%
  group_by(transect) %>%
  summarise(time = length(unique(time))) %>%
  ungroup() %>%
  filter(time != 2)

missing_pre_or_post

missing_pre_or_post = missing_pre_or_post$transect
patches = 
  patches %>%
  filter(!is.element(transect, missing_pre_or_post))

#### write results #############################################################

write.csv(patches,
          here::here('02-data', '02-for_analysis', 'patches.csv'))
saveRDS(patches,
        here::here('02-data', '02-for_analysis', 'patches.rds'))

write.csv(cover_long,
          here::here('02-data', '02-for_analysis', 'cover_long.csv'))
saveRDS(cover_long,
        here::here('02-data', '02-for_analysis', 'cover_long.rds'))

