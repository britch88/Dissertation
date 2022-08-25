# Indicators

predset_x01 <- c(           
  "some_college_2017",    "hs_grad_2017",         "broadband_2017",  "median_household_income_2017",   "poverty_2017",  "unemployment_rate_2017",        
  
  "bankrate2017.na",      "bankrate2017.ltp25",  "bankrate2017.gtp75",   "bankrate2017.p25top75",         
  "poverty_2017.ltp25",  "poverty_2017.gtp75",   "poverty_2017.p25top75",          
  "unemployment_rate_2017.ltp25", "unemployment_rate_2017.gtp75",   "unemployment_rate_2017.p25top75",          
  "median_household_income_2017.ltp25",  "median_household_income_2017.gtp75",       "median_household_income_2017.p25top75",    
  "some_college_2017.ltp25",  "some_college_2017.gtp75",        "some_college_2017.p25top75",     
  "hs_grad_2017.ltp25",  "hs_grad_2017.gtp75",   "hs_grad_2017.p25top75",          
  "broadband_2017.ltp25",  "broadband_2017.gtp75",           "broadband_2017.p25top75",
  "disconnected.2017.na",  "disconnected.2017.ltp25",        "disconnected.2017.gtp75",        "disconnected.2017.p25top75",    
  "food.2017.na",         "food.2017.ltp25",      "food.2017.gtp75",     "food.2017.p25top75",   
  "violent.2017.na",      "violent.2017.ltp25",  "violent.2017.gtp75",   "violent.2017.p25top75",          
  "segregation.2017.na",  "segregation.2017.ltp25",         "segregation.2017.gtp75",         "segregation.2017.p25top75",     
  "pcp_2017.ltp25",       "pcp_2017.gtp75",       "pcp_2017.p25top75",   
  "lbw_2017.ltp25",       "lbw_2017.gtp75",       "lbw_2017.p25top75",   
  "income_ratio_2017.ltp25",        "income_ratio_2017.gtp75",        "income_ratio_2017.p25top75",    
  "uninsured_2017.ltp25",           "uninsured_2017.gtp75",           "uninsured_2017.p25top75",       
  "under18_2017.ltp25",   "under18_2017.gtp75",   "under18_2017.p25top75",         
  "over64_2017.ltp25",    "over64_2017.gtp75",    "over64_2017.p25top75",          
  "black_2017.ltp25",     "black_2017.gtp75",     "black_2017.p25top75",           
  "native_2017.ltp25",    "native_2017.gtp75",    "native_2017.p25top75",          
  "pacific_islander_2017.ltp25",    "pacific_islander_2017.gtp75",    "pacific_islander_2017.p25top75",          
  "hispanic_2017.ltp25",  "hispanic_2017.gtp75",  "hispanic_2017.p25top75",        
  "white_2017.ltp25",     "white_2017.gtp75",     "white_2017.p25top75",           
  "lim_eng_2017.ltp25",   "lim_eng_2017.gtp75",   "lim_eng_2017.p25top75",         
  "female_2017.ltp25",    "female_2017.gtp75",    "female_2017.p25top75",          
  "rural_2017.ltp25",     "rural_2017.gtp75",     "rural_2017.p25top75",           
  "prek_2017.ltp25",      "prek_2017.gtp75",      "prek_2017.p25top75",  
  "rentburden_2017.ltp25",          "rentburden_2017.gtp75",          "rentburden_2017.p25top75",      
  "deaths_2017.lt10",     "deaths_2017.ltp25",    "deaths_2017.gtp75", "deaths_2017.p25top75",           
  "incarcerated_2017.ltp25",        "incarcerated_2017.gtp75",       "incarcerated_2017.p25top75",     
  
  "voting_2017.ltp25",    "voting_2017.gtp75",   "voting_2017.p25top75",           
  
  "volunteer_2017.ltp25",           "volunteer_2017.gtp75",          "volunteer_2017.p25top75",        
  
  # Leaving out South which has most counties
  "region_Midwest",       "region_Northeast",  "region_other",        "region_West", 
  
  # Leaving out west north central
  "country_division_east north central",     "country_division_east south central",     "country_division_middle atlantic",       
  "country_division_mountain",     "country_division_new england",  "country_division_pacific",     
  "country_division_possessions",  "country_division_south atlantic",         "country_division_west north central",    
  "country_division_west south central",
  
  # Leaving out Texas since it is state with most counties
  "state_alabama",      "state_alaska",        "state_arizona",       "state_arkansas",      "state_california",   
  "state_colorado",     "state_connecticut",   "state_delaware",      "state_florida",       "state_georgia",       
  "state_hawaii",       "state_idaho",         "state_illinois",      "state_indiana",       "state_iowa",          
  "state_kansas",       "state_kentucky",      "state_louisiana",     "state_maine",         "state_maryland",     
  "state_massachusetts","state_michigan",      "state_minnesota",     "state_mississippi",   "state_missouri",      
  "state_montana",      "state_nebraska",      "state_nevada",        "state_new hampshire", "state_new jersey",    
  "state_new mexico",   "state_new york",      "state_north carolina","state_north dakota",  "state_ohio",         
  "state_oklahoma",     "state_oregon",        "state_pennsylvania",  "state_rhode island",  "state_south carolina",
  "state_south dakota", "state_tennessee",                            "state_utah",          "state_vermont",       
  "state_virginia",     "state_washington",    "state_west virginia", "state_wisconsin",     "state_wyoming"       )  
