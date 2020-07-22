## helper function to pull values from the table returned by xlsx_cells
scrape_location = function(sheet,location,type){
  value = sheet[sheet$address==location, ][[type]]
  if (length(value)>0){
    return(value)
  } else {
    return(NA)
  }
}


## scrape cover data and record it as a tidy table
scrape_cover = function(current_sheet,obs_id){
  
  # read data
  current_block = as.character(
    if (scrape_location(current_sheet,'B1','data_type')=='numeric'){
      scrape_location(current_sheet,'B1','numeric')
    } else {
      scrape_location(current_sheet,'B1','character')
    }
  )
  
  current_plot = scrape_location(current_sheet,'B2','character')
  
  current_transect = scrape_location(current_sheet,'B3','character')
  
  current_notes = scrape_location(current_sheet,'B4','character')
  
  current_date = as.character(scrape_location(current_sheet, 'D1','date'))
  
  ## get species for sub1, make it a vector
  current_sub1_species = current_sheet[current_sheet$row==10&
                                         current_sheet$col>=3,][['character']]
  
  ## get startpoints for sub1, make vector
  current_sub1_coverstart = current_sheet[current_sheet$row==11&
                                            current_sheet$col>=3,][['numeric']]
  
  ## get endpoints for sub1, make vector
  current_sub1_coverend = current_sheet[current_sheet$row==12&
                                          current_sheet$col>=3,][['numeric']]
  
  ## get heights for sub1, make vector
  current_sub1_heights = current_sheet[current_sheet$row==13&
                                         current_sheet$col>=3,][['numeric']]
  
  # get species for sub2, make it a vector
  current_sub2_species = current_sheet[current_sheet$row==35&
                                         current_sheet$col>=3,][['character']]
  
  # get startpoints for sub2, make vector
  current_sub2_coverstart = current_sheet[current_sheet$row==36&
                                            current_sheet$col>=3,][['numeric']]
  
  
  # get endpoints for sub2, make vector
  current_sub2_coverend = current_sheet[current_sheet$row==37&
                                          current_sheet$col>=3,][['numeric']]
  
  # get heights for sub2, make vector
  current_sub2_heights = current_sheet[current_sheet$row==38&
                                         current_sheet$col>=3,][['numeric']]
  
  # get species for sub3, make it a vector
  current_sub3_species = current_sheet[current_sheet$row==60&
                                         current_sheet$col>=3,][['character']]
  
  # get startpoints for sub3, make vector
  current_sub3_coverstart = current_sheet[current_sheet$row==61&
                                            current_sheet$col>=3,][['numeric']]
  
  # get endpoints for sub3, make vector
  current_sub3_coverend = current_sheet[current_sheet$row==62&
                                          current_sheet$col>=3,][['numeric']]
  
  # get heights for sub3, make vector
  current_sub3_heights = current_sheet[current_sheet$row==63&
                                         current_sheet$col>=3,][['numeric']]
  
  
  # check that cover species, start, end, and length vectors have the same length
  length(current_sub1_species)==length(current_sub1_coverstart)&
    length(current_sub1_coverstart)==length(current_sub1_coverend)&
    length(current_sub1_coverend)==length(current_sub1_heights)
  
  # check that cover species, start, end, and length vectors have the same length
  length(current_sub2_species)==length(current_sub2_coverstart)&
    length(current_sub2_coverstart)==length(current_sub2_coverend)&
    length(current_sub2_coverend)==length(current_sub2_heights)
  
  # check that cover species, start, end, and length vectors have the same length
  length(current_sub3_species)==length(current_sub3_coverstart)&
    length(current_sub3_coverstart)==length(current_sub3_coverend)&
    length(current_sub3_coverend)==length(current_sub3_heights)
  
  
  ############ write data
  
  sub1_length = length(current_sub1_species)
  sub2_length = length(current_sub2_species)
  sub3_length = length(current_sub3_species)
  total_length = sub1_length+sub2_length+sub3_length
  
  col_observation_id = rep(x = obs_id, 
                           times = total_length)
  
  col_date = rep(x = current_date,
                 times = total_length)
  
  col_block = rep(x = current_block,
                  times = total_length)
  
  col_plot = rep(x = current_plot,
                 times=total_length)
  
  col_transect = rep(x = current_transect,
                     times = total_length)
  
  col_subtransect = c(rep(1, times=sub1_length), 
                      rep(2, times=sub2_length),
                      rep(3, times=sub3_length))
  
  col_species = c(current_sub1_species,
                  current_sub2_species,
                  current_sub3_species)
  
  
  col_coverstart = c(current_sub1_coverstart,
                     current_sub2_coverstart,
                     current_sub3_coverstart)
  
  col_coverend = c(current_sub1_coverend,
                   current_sub2_coverend,
                   current_sub3_coverend)
  
  col_height = c(current_sub1_heights,
                 current_sub2_heights,
                 current_sub3_heights)
  
  tidy_results = data.frame(observation_id=col_observation_id,
                            date = col_date,
                            block = col_block,
                            plot = col_plot,
                            transect = col_transect,
                            subtransect = col_subtransect,
                            species = col_species,
                            cover_start_m = col_coverstart,
                            cover_end_m = col_coverend,
                            avg_height_m = col_height,
                            stringsAsFactors = FALSE)
  
  return(tidy_results)
}


scrape_fuel_1000h = function(current_sheet, obs_id) {
  
  # read data
  
  current_block = as.character(
    if (scrape_location(current_sheet,'B1','data_type')=='numeric'){
      scrape_location(current_sheet,'B1','numeric')
    } else {
      scrape_location(current_sheet,'B1','character')
    }
  )
  
  current_plot = scrape_location(current_sheet,'B2','character')
  
  current_transect = scrape_location(current_sheet,'B3','character')
  
  current_notes = scrape_location(current_sheet,'B4','character')
  
  current_date = as.character(scrape_location(current_sheet, 'D1','date'))
  
  current_sub1_slope = scrape_location(current_sheet, 'E8', 'numeric')
  
  current_sub1_1000h_diam = current_sheet[current_sheet$row==26&
                                            current_sheet$col>1,][['character']]
  
  current_sub1_1000h_diam = as.numeric(gsub("([0-9]+).*$",
                                            "\\1",
                                            current_sub1_1000h_diam))
  
  current_sub1_1000h_decay = current_sheet[current_sheet$row==26&
                                             current_sheet$col>1,][['character']]
  
  current_sub1_1000h_decay = gsub(".*([aA-zZ]).*$","\\1",current_sub1_1000h_decay)
  
  current_sub2_slope = scrape_location(current_sheet, 'E33', 'numeric')
  
  current_sub2_1000h_diam = current_sheet[current_sheet$row==51&
                                            current_sheet$col>1,][['character']]
  
  current_sub2_1000h_diam = as.numeric(gsub("([0-9]+).*$",
                                            "\\1",
                                            current_sub2_1000h_diam))
  
  current_sub2_1000h_decay = current_sheet[current_sheet$row==51&
                                             current_sheet$col>1,][['character']]
  
  current_sub2_1000h_decay = gsub(".*([aA-zZ]).*$","\\1",current_sub2_1000h_decay)
  
  current_sub3_slope = scrape_location(current_sheet, 'E58', 'numeric')
  
  current_sub3_1000h_diam = current_sheet[current_sheet$row==76&
                                            current_sheet$col>1,][['character']]
  
  current_sub3_1000h_diam = as.numeric(gsub("([0-9]+).*$",
                                            "\\1",
                                            current_sub3_1000h_diam))
  
  current_sub3_1000h_decay = current_sheet[current_sheet$row==76&
                                             current_sheet$col>1,][['character']]
  
  current_sub3_1000h_decay = gsub(".*([aA-zZ]).*$","\\1",current_sub3_1000h_decay)
  
  # write data to tidy tables
  
  sub1_length = length(current_sub1_1000h_diam)
  sub2_length = length(current_sub2_1000h_diam)
  sub3_length = length(current_sub3_1000h_diam)
  total_length = sub1_length+sub2_length+sub3_length
  
  col_observation_id = rep(x = obs_id, 
                           times = total_length)
  
  col_date = rep(x = current_date,
                 times = total_length)
  
  col_block = rep(x = current_block,
                  times = total_length)
  
  col_plot = rep(x = current_plot,
                 times=total_length)
  
  col_transect = rep(x = current_transect,
                     times = total_length)
  
  col_subtransect = c(rep(1, times=sub1_length), 
                      rep(2, times=sub2_length),
                      rep(3, times=sub3_length))
  
  col_slope = c(rep(current_sub1_slope, times = sub1_length),
                rep(current_sub2_slope, times = sub2_length),
                rep(current_sub3_slope, times = sub3_length))
  
  col_diam = c(current_sub1_1000h_diam,
               current_sub2_1000h_diam,
               current_sub3_1000h_diam)
  
  col_decay = c(current_sub1_1000h_decay,
                current_sub2_1000h_decay,
                current_sub3_1000h_decay)
  
  tidy_results = data.frame(observation_id=col_observation_id,
                            date = col_date,
                            block = col_block,
                            plot = col_plot,
                            transect = col_transect,
                            subtransect = col_subtransect,
                            slope = col_slope,
                            diameter_cm = col_diam,
                            decay_class = col_decay,
                            stringsAsFactors = FALSE)
  
  return(tidy_results)
  
}


scrape_fuel_1_to_100h = function(current_sheet, obs_id){
  
  # read data
  
  current_block = as.character(
    if (scrape_location(current_sheet,'B1','data_type')=='numeric'){
      scrape_location(current_sheet,'B1','numeric')
    } else {
      scrape_location(current_sheet,'B1','character')
    }
  )
  
  current_plot = scrape_location(current_sheet,'B2','character')
  
  current_transect = scrape_location(current_sheet,'B3','character')
  
  current_notes = scrape_location(current_sheet,'B4','character')
  
  current_date = as.character(scrape_location(current_sheet, 'D1','date'))
  
  current_sub1_slope = scrape_location(current_sheet, 'E8', 'numeric')
  current_sub1_1h_03 = scrape_location(current_sheet, 'B19','numeric')
  current_sub1_10h_03 = scrape_location(current_sheet, 'B20','numeric')
  current_sub1_100h_03 = scrape_location(current_sheet, 'B21','numeric')
  
  current_sub1_1h_13 = scrape_location(current_sheet, 'C19','numeric')
  current_sub1_10h_13 = scrape_location(current_sheet, 'C20','numeric')
  current_sub1_100h_13 = scrape_location(current_sheet, 'C21','numeric')
  
  current_sub1_1h_23 = scrape_location(current_sheet, 'D19','numeric')
  current_sub1_10h_23 = scrape_location(current_sheet, 'D20','numeric')
  current_sub1_100h_23 = scrape_location(current_sheet, 'D21','numeric')
  
  current_sub2_slope = scrape_location(current_sheet, 'E33', 'numeric')
  current_sub2_1h_03 = scrape_location(current_sheet, 'B44','numeric')
  current_sub2_10h_03 = scrape_location(current_sheet, 'B45','numeric')
  current_sub2_100h_03 = scrape_location(current_sheet, 'B46','numeric')
  
  current_sub2_1h_13 = scrape_location(current_sheet, 'C44','numeric')
  current_sub2_10h_13 = scrape_location(current_sheet, 'C45','numeric')
  current_sub2_100h_13 = scrape_location(current_sheet, 'C46','numeric')
  
  current_sub2_1h_23 = scrape_location(current_sheet, 'D44','numeric')
  current_sub2_10h_23 = scrape_location(current_sheet, 'D45','numeric')
  current_sub2_100h_23 = scrape_location(current_sheet, 'D46','numeric')
  
  current_sub3_slope = scrape_location(current_sheet, 'E58', 'numeric')
  current_sub3_1h_03 = scrape_location(current_sheet, 'B69','numeric')
  current_sub3_10h_03 = scrape_location(current_sheet, 'B70','numeric')
  current_sub3_100h_03 = scrape_location(current_sheet, 'B71','numeric')
  
  current_sub3_1h_13 = scrape_location(current_sheet, 'C69','numeric')
  current_sub3_10h_13 = scrape_location(current_sheet, 'C70','numeric')
  current_sub3_100h_13 = scrape_location(current_sheet, 'C71','numeric')
  
  current_sub3_1h_23 = scrape_location(current_sheet, 'D69','numeric')
  current_sub3_10h_23 = scrape_location(current_sheet, 'D70','numeric')
  current_sub3_100h_23 = scrape_location(current_sheet, 'D71','numeric')
  
  # write data
  
  col_observation_id = rep(obs_id, times = 9)
  
  col_date = rep(current_date, times = 9)
  
  col_block = rep(current_block, times = 9)
  
  col_plot = rep(current_plot, times = 9)
  
  col_transect = rep(current_transect, times = 9)
  
  col_subtransect = c(rep(1, times=3), 
                      rep(2, times=3),
                      rep(3, times=3))
  
  col_slope = c(rep(current_sub1_slope, times = 3),
                rep(current_sub2_slope, times = 3),
                rep(current_sub3_slope, times = 3))
  
  col_startpoint = rep(c(3,13,23),times=3)
  
  col_1h = c(current_sub1_1h_03,
             current_sub1_1h_13,
             current_sub1_1h_23,
             current_sub2_1h_03,
             current_sub2_1h_13,
             current_sub2_1h_23,
             current_sub3_1h_03,
             current_sub3_1h_13,
             current_sub3_1h_23)
  
  col_10h = c(current_sub1_10h_03,
              current_sub1_10h_13,
              current_sub1_10h_23,
              current_sub2_10h_03,
              current_sub2_10h_13,
              current_sub2_10h_23,
              current_sub3_10h_03,
              current_sub3_10h_13,
              current_sub3_10h_23)
  
  col_100h = c(current_sub1_100h_03,
               current_sub1_100h_13,
               current_sub1_100h_23,
               current_sub2_100h_03,
               current_sub2_100h_13,
               current_sub2_100h_23,
               current_sub3_100h_03,
               current_sub3_100h_13,
               current_sub3_100h_23)
  
  tidy_results = data.frame(observation_id = col_observation_id,
                            date = col_date,
                            block = col_block,
                            plot = col_plot,
                            transect = col_transect,
                            subtransect = col_subtransect,
                            slope = col_slope,
                            startpoint_m = col_startpoint,
                            x1_hour_count = col_1h,
                            x10_hour_count = col_10h,
                            x100_hour_count = col_100h,
                            stringsAsFactors = FALSE)
  
  return(tidy_results)
  
}


scrape_fuel_depths = function(current_sheet, obs_id){
  
  # read data
  
  current_block = as.character(
    if (scrape_location(current_sheet,'B1','data_type')=='numeric'){
      scrape_location(current_sheet,'B1','numeric')
    } else {
      scrape_location(current_sheet,'B1','character')
    }
  )
  
  current_plot = scrape_location(current_sheet,'B2','character')
  
  current_transect = scrape_location(current_sheet,'B3','character')
  
  current_notes = scrape_location(current_sheet,'B4','character')
  
  current_date = as.character(scrape_location(current_sheet, 'D1','date'))
  
  current_sub1_litter_03 = scrape_location(current_sheet, 'B16','numeric')
  current_sub1_duff_03 = scrape_location(current_sheet, 'B17','numeric')
  current_sub1_fuel_03 = scrape_location(current_sheet, 'B18','numeric')
  
  current_sub1_litter_07 = scrape_location(current_sheet, 'B22','numeric')
  current_sub1_duff_07 = scrape_location(current_sheet, 'B23','numeric')
  current_sub1_fuel_07 = scrape_location(current_sheet, 'B24','numeric')
  
  current_sub1_litter_13 = scrape_location(current_sheet, 'C16','numeric')
  current_sub1_duff_13 = scrape_location(current_sheet, 'C17','numeric')
  current_sub1_fuel_13 = scrape_location(current_sheet, 'C18','numeric')
  
  current_sub1_litter_17 = scrape_location(current_sheet, 'C22','numeric')
  current_sub1_duff_17 = scrape_location(current_sheet, 'C23','numeric')
  current_sub1_fuel_17 = scrape_location(current_sheet, 'C24','numeric')
  
  current_sub1_litter_23 = scrape_location(current_sheet, 'D16','numeric')
  current_sub1_duff_23 = scrape_location(current_sheet, 'D17','numeric')
  current_sub1_fuel_23 = scrape_location(current_sheet, 'D18','numeric')
  
  current_sub1_litter_27 = scrape_location(current_sheet, 'D22','numeric')
  current_sub1_duff_27 = scrape_location(current_sheet, 'D23','numeric')
  current_sub1_fuel_27 = scrape_location(current_sheet, 'D24','numeric')
  
  
  current_sub2_litter_03 = scrape_location(current_sheet, 'B41','numeric')
  current_sub2_duff_03 = scrape_location(current_sheet, 'B42','numeric')
  current_sub2_fuel_03 = scrape_location(current_sheet, 'B43','numeric')
  
  current_sub2_litter_07 = scrape_location(current_sheet, 'B47','numeric')
  current_sub2_duff_07 = scrape_location(current_sheet, 'B48','numeric')
  current_sub2_fuel_07 = scrape_location(current_sheet, 'B49','numeric')
  
  current_sub2_litter_13 = scrape_location(current_sheet, 'C41','numeric')
  current_sub2_duff_13 = scrape_location(current_sheet, 'C42','numeric')
  current_sub2_fuel_13 = scrape_location(current_sheet, 'C43','numeric')
  
  current_sub2_litter_17 = scrape_location(current_sheet, 'C47','numeric')
  current_sub2_duff_17 = scrape_location(current_sheet, 'C48','numeric')
  current_sub2_fuel_17 = scrape_location(current_sheet, 'C49','numeric')
  
  current_sub2_litter_23 = scrape_location(current_sheet, 'D41','numeric')
  current_sub2_duff_23 = scrape_location(current_sheet, 'D42','numeric')
  current_sub2_fuel_23 = scrape_location(current_sheet, 'D43','numeric')
  
  current_sub2_litter_27 = scrape_location(current_sheet, 'D47','numeric')
  current_sub2_duff_27 = scrape_location(current_sheet, 'D48','numeric')
  current_sub2_fuel_27 = scrape_location(current_sheet, 'D49','numeric')
  
  
  current_sub3_litter_03 = scrape_location(current_sheet, 'B66','numeric')
  current_sub3_duff_03 = scrape_location(current_sheet, 'B67','numeric')
  current_sub3_fuel_03 = scrape_location(current_sheet, 'B68','numeric')
  
  current_sub3_litter_07 = scrape_location(current_sheet, 'B72','numeric')
  current_sub3_duff_07 = scrape_location(current_sheet, 'B73','numeric')
  current_sub3_fuel_07 = scrape_location(current_sheet, 'B74','numeric')
  
  current_sub3_litter_13 = scrape_location(current_sheet, 'C66','numeric')
  current_sub3_duff_13 = scrape_location(current_sheet, 'C67','numeric')
  current_sub3_fuel_13 = scrape_location(current_sheet, 'C68','numeric')
  
  current_sub3_litter_17 = scrape_location(current_sheet, 'C72','numeric')
  current_sub3_duff_17 = scrape_location(current_sheet, 'C73','numeric')
  current_sub3_fuel_17 = scrape_location(current_sheet, 'C74','numeric')
  
  current_sub3_litter_23 = scrape_location(current_sheet, 'D66','numeric')
  current_sub3_duff_23 = scrape_location(current_sheet, 'D67','numeric')
  current_sub3_fuel_23 = scrape_location(current_sheet, 'D68','numeric')
  
  current_sub3_litter_27 = scrape_location(current_sheet, 'D72','numeric')
  current_sub3_duff_27 = scrape_location(current_sheet, 'D73','numeric')
  current_sub3_fuel_27 = scrape_location(current_sheet, 'D74','numeric')
  
  ###### Write data
  
  col_observation_id = rep(x = obs_id,
                           times = 18)
  
  col_date = rep(current_date, times = 18)
  
  col_block = rep(current_block, times = 18)
  
  col_plot = rep(current_plot, times = 18)
  
  col_transect = rep(current_transect, times = 18)
  
  col_subtransect = c(rep(1, times = 6),
                      rep(2, times = 6),
                      rep(3, times = 6))
  
  col_location = rep(c(3,7,13,17,23,27),
                     times = 3)
  
  col_litter = c(current_sub1_litter_03,
                 current_sub1_litter_07,
                 current_sub1_litter_13,
                 current_sub1_litter_17,
                 current_sub1_litter_23,
                 current_sub1_litter_27,
                 current_sub2_litter_03,
                 current_sub2_litter_07,
                 current_sub2_litter_13,
                 current_sub2_litter_17,
                 current_sub2_litter_23,
                 current_sub2_litter_27,
                 current_sub3_litter_03,
                 current_sub3_litter_07,
                 current_sub3_litter_13,
                 current_sub3_litter_17,
                 current_sub3_litter_23,
                 current_sub3_litter_27)
  
  col_duff = c(current_sub1_duff_03,
               current_sub1_duff_07,
               current_sub1_duff_13,
               current_sub1_duff_17,
               current_sub1_duff_23,
               current_sub1_duff_27,
               current_sub2_duff_03,
               current_sub2_duff_07,
               current_sub2_duff_13,
               current_sub2_duff_17,
               current_sub2_duff_23,
               current_sub2_duff_27,
               current_sub3_duff_03,
               current_sub3_duff_07,
               current_sub3_duff_13,
               current_sub3_duff_17,
               current_sub3_duff_23,
               current_sub3_duff_27)
  
  col_fuel = c(current_sub1_fuel_03,
               current_sub1_fuel_07,
               current_sub1_fuel_13,
               current_sub1_fuel_17,
               current_sub1_fuel_23,
               current_sub1_fuel_27,
               current_sub2_fuel_03,
               current_sub2_fuel_07,
               current_sub2_fuel_13,
               current_sub2_fuel_17,
               current_sub2_fuel_23,
               current_sub2_fuel_27,
               current_sub3_fuel_03,
               current_sub3_fuel_07,
               current_sub3_fuel_13,
               current_sub3_fuel_17,
               current_sub3_fuel_23,
               current_sub3_fuel_27)
  
  tidy_results = data.frame(observation_id = col_observation_id,
                            date = col_date,
                            block = col_block,
                            plot = col_plot,
                            transect = col_transect,
                            subtransect = col_subtransect,
                            location_m = col_location,
                            litter_cm = col_litter,
                            duff_cm = col_duff,
                            fuel_cm = col_fuel,
                            stringsAsFactors = FALSE)
  return(tidy_results)
}


scrape_seedlings = function(current_sheet, obs_id){
  
  # read data
  
  current_block = as.character(
    if (scrape_location(current_sheet,'B1','data_type')=='numeric'){
      scrape_location(current_sheet,'B1','numeric')
    } else {
      scrape_location(current_sheet,'B1','character')
    }
  )
  
  current_plot = scrape_location(current_sheet,'B2','character')
  
  current_transect = scrape_location(current_sheet,'B3','character')
  
  current_notes = scrape_location(current_sheet,'B4','character')
  
  current_date = as.character(scrape_location(current_sheet, 'D1','date'))
  
  
  current_sub1_seedling_species = current_sheet[
    current_sheet$row==29&
      current_sheet$col>1,][['character']]
  
  if (length(current_sub1_seedling_species)==0){
    current_sub1_seedling_species = NA
  }
  
  current_sub1_seedling_height = current_sheet[
    current_sheet$row==30&
      current_sheet$col>1,][['numeric']]
  
  if (length(current_sub1_seedling_height)==0){
    current_sub1_seedling_height = NA
  }
  
  current_sub2_seedling_species = current_sheet[
    current_sheet$row==54&
      current_sheet$col>1,][['character']]
  
  if (length(current_sub2_seedling_species)==0){
    current_sub2_seedling_species = NA
  }
  
  current_sub2_seedling_height = current_sheet[
    current_sheet$row==55&
      current_sheet$col>1,][['numeric']]
  
  if (length(current_sub2_seedling_height)==0){
    current_sub2_seedling_height = NA
  }
  
  current_sub3_seedling_species = current_sheet[
    current_sheet$row==79&
      current_sheet$col>1,][['character']]
  
  if (length(current_sub3_seedling_species)==0){
    current_sub3_seedling_species = NA
  }
  
  current_sub3_seedling_height = current_sheet[
    current_sheet$row==80&
      current_sheet$col>1,][['numeric']]
  
  if (length(current_sub3_seedling_height)==0){
    current_sub3_seedling_height = NA
  }
  
  
  # write data to tidy tables
  
  sub1_length = length(current_sub1_seedling_species)
  sub2_length = length(current_sub2_seedling_species)
  sub3_length = length(current_sub3_seedling_species)
  total_length = sub1_length+sub2_length+sub3_length
  
  col_observation_id = rep(x = obs_id, 
                           times = total_length)
  
  col_date = rep(x = current_date,
                 times = total_length)
  
  col_block = rep(x = current_block,
                  times = total_length)
  
  col_plot = rep(x = current_plot,
                 times=total_length)
  
  col_transect = rep(x = current_transect,
                     times = total_length)
  
  col_subtransect = c(rep(1, times=sub1_length), 
                      rep(2, times=sub2_length),
                      rep(3, times=sub3_length))
  
  col_height = c(current_sub1_seedling_height,
                 current_sub2_seedling_height,
                 current_sub3_seedling_height)
  
  col_species = c(current_sub1_seedling_species,
                  current_sub2_seedling_species,
                  current_sub3_seedling_species)
  
  tidy_results = data.frame(observation_id=col_observation_id,
                            date = col_date,
                            block = col_block,
                            plot = col_plot,
                            transect = col_transect,
                            subtransect = col_subtransect,
                            species = col_species,
                            height_m = col_height,
                            stringsAsFactors = FALSE)
  
  return(tidy_results)
  
}

standardize_block = function(block_vector){
  
  # strip block to just numbers, reformat as integers from character,
  # then convert to a factor and return
  
  return(factor(as.integer(gsub("[^[:digit:]., ]", 
                                "", 
                                block_vector)),
                levels = c(1,2,3,4,5)))
}


standardize_plot = function(plot_vector){
  
  # convert labels to all uppercase strings, then convert to factor with 6
  # levels
  
  return(factor(str_to_upper(plot_vector), 
                levels=c('CG','CH','CN',
                         'EG','EH','EN')
  )
  )
}
standardize_transect = function(transect_vector){
  
  # label is the first letter of the string (some variation on 'north' 'middle'
  # or 'south'), then capitalized and converted to a factor with 3 levels
  return(factor(stringr::str_sub(transect_vector, 
                                 1L, 1L),
                levels = c('N','M','S')))
}







