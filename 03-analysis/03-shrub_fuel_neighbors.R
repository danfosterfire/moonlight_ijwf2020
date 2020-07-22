#### fuel class neighborhoods ##################################################

# right now, the transects are recorded in both directions. Cover start might 
# be the lower number or the higher number; we need to consistently have one 
# column be the lower number and another column the higher one, so make 
# some new position columns:
cover.df['location_minimum'] = 
  ifelse(cover.df$cover_start_m<cover.df$cover_end_m,
         cover.df$cover_start_m,
         cover.df$cover_end_m)

cover.df['location_maximum'] = 
  ifelse(cover.df$cover_start_m<cover.df$cover_end_m,
         cover.df$cover_end_m,
         cover.df$cover_start_m)


