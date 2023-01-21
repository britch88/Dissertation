
# 
# data2011 <- read_csv("raw/RWJ Health Data/analytic_data2011.csv")
# data2011x2 = data2011[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2011))]
# 
# 
# data2012 <- read_csv("raw/RWJ Health Data/analytic_data2012.csv")
# data2012x2 = data2012[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2012))]
# 
# 
# data2013 <- read_csv("raw/RWJ Health Data/analytic_data2013.csv")
# data2013x2 = data2013[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2013))]
# 
# 
# data2014 <- read_csv("raw/RWJ Health Data/analytic_data2014.csv")
# data2014x2 = data2014[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2014))]
# 
# 
# data2015 <- read_csv("raw/RWJ Health Data/analytic_data2015.csv")
# data2015x2 = data2015[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2015))]
# 
# 
# data2016 <- read_csv("raw/RWJ Health Data/analytic_data2016.csv")
# data2016x2 = data2016[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2016))]
# 
# 
# data2017 <- read_csv("raw/RWJ Health Data/analytic_data2017.csv")
# data2017x2 = data2017[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2017))]
# 
# 
# data2018 <- read_csv("raw/RWJ Health Data/analytic_data2018_0.csv")
# data2018x2 = data2018[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2018))]
# 
# 
# data2019 <- read_csv("raw/RWJ Health Data/analytic_data2019.csv")
# data2019x2 = data2019[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2019))] %>% 
#   mutate(fips_clean = "5-digit FIPS Code")



# data2020 <- read_csv("raw/RWJ Health Data/analytic_data2020_0.csv")
# data2020x2 = data2020[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2020))]
# 
# 
# 
# data2021 <- read_csv("raw/RWJ Health Data/analytic_data2021.csv")
# data2021x2 = data2021[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2021))]
# 
# 
# 
# data2022 <- read_csv("raw/RWJ Health Data/analytic_data2022.csv")
# data2022x2 = data2022[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2022))]

#### Listing and organizing vars for 2019, and exclude columns with missing, also exclude columns likely
#### to be highly correlated








vars2019lengthoflife <- c("Premature death raw value" ,                                          
                          "Premature age-adjusted mortality raw value", 
                          "Life expectancy raw value"  
                          #   "Child mortality raw value" ,                                           
                          #   "Infant mortality raw value" 
)



vars2019quality <- c("Poor or fair health raw value", 
                     "Poor physical health days raw value",                                 
                     "Poor mental health days raw value",
                     "Low birthweight raw value",
                     "Diabetes prevalence raw value",
                     "Frequent physical distress raw value",                                 
                     "Frequent mental distress raw value"                                 
                     #  "HIV prevalence raw value"
)  




vars2019behaviors <- c("Adult smoking raw value" ,                                             
                       "Adult obesity raw value" ,                                            
                       "Food environment index raw value" ,                                    
                       "Physical inactivity raw value"  ,                                     
                       "Access to exercise opportunities raw value"   ,                        
                       "Excessive drinking raw value"    ,                                    
                       "Alcohol-impaired driving deaths raw value" ,                           
                       "Sexually transmitted infections raw value",                           
                       "Teen births raw value",
                       "Insufficient sleep raw value"                                         
)



vars2019access <- c( "Uninsured raw value",
                     # "Uninsured adults raw value",                                          
                     # "Uninsured children raw value",
                     "Primary care physicians raw value",                                 
                     "Ratio of population to primary care physicians.",                    
                     # "Ratio of population to primary care providers other than physicians." 
                     "Dentists raw value", 
                     # "Other primary care providers raw value"
                     "Ratio of population to dentists.",                                   
                     #     "Mental health providers raw value",                                    
                     #   "Ratio of population to mental health providers.",                     
                     "Preventable hospital stays raw value",                            
                     #    "Mammography screening raw value",                                     
                     "Flu vaccinations raw value",
                     #  "Childhood immunizations raw value" 
)



vars2019ses <- c("High school graduation raw value",
                 "Some college raw value",
                 "Unemployment raw value",
                 "Children in poverty raw value",
                 #   "Poverty raw value",                                                  
                 "Income inequality raw value",
                 "Median household income raw value",
                 "Children eligible for free or reduced price lunch raw value",
                 #    "Residential segregation - non-white/white raw value",
                 #    "Homicides raw value",                                                 
                 #    "Firearm fatalities raw value",
                 "Social associations raw value",
                 #   "Reading proficiency raw value",
                 #   "Disconnected youth raw value",                                        
                 "Violent crime raw value",                                              
                 "Injury deaths raw value"
                 #   "Drug overdose deaths raw value",                                       
                 #   "Motor vehicle crash deaths raw value",                                
                 #  `Communicable disease raw value` ,                                     
                 # "Self-inflicted injury hospitalizations raw value",                   
                 #"Drug arrests raw value",                                              
                 #  "Opioid hospital visits raw value",                                     
                 #  "Alcohol-related hospitalizations raw value",                          
                 #  "On-road motor vehicle crash-related ER visits raw value",             
                 #  "Off-road motor vehicle crash-related ER visits raw value",            
                 # "Hate crimes raw value",                                                
                 # "Child abuse raw value"
                 #  "W-2 enrollment raw value"                                            
                 #"Injury hospitalizations raw value"                                
                 #   "Motor vehicle crash occupancy rate raw value"                        
)                                                                                                       


vars2019environ <- c("Air pollution - particulate matter raw value",
                     "Drinking water violations raw value",
                     "Severe housing cost burden raw value",
                     "Severe housing problems raw value" ,
                     "Percentage of households with high housing costs" ,                   
                     "Percentage of households with overcrowding"      ,                    
                     "Percentage of households with lack of kitchen or plumbing facilities",
                     "Homeownership raw value"  ,                                           
                     #"Driving alone to work raw value" ,                                     
                     "Long commute - driving alone raw value",
                     "Children in single-parent households raw value",
                     "Food insecurity raw value" ,                                           
                     "Limited access to healthy foods raw value" )


vars2019pop <- c("Population raw value",                                                
                 "% below 18 years of age raw value" ,                                   
                 "% 65 and older raw value" ,                                           
                 # "% American Indian and Alaskan Native raw value" ,                    
                 #  "% Native Hawaiian/Other Pacific Islander raw value",                  
                 # "% not proficient in English raw value",                               
                 "% Females raw value",                                                 
                 "% Rural raw value" )



#  "Older adults living alone raw value"                                 
#  "Fall fatalities 65+ raw value"                                       
#  "Year structure built raw value"                                    #   "Male population 0-17 raw value"                                      
#  "Male population 18-44 raw value"                                   #   "Male population 45-64 raw value"                                     
#  "Male population 65+ raw value"                                    #    "Total male population raw value"                                     
#  "Female population 0-17 raw value"                                  #   "Female population 18-44 raw value"                                   
# "Female population 45-64 raw value"                                  #  "Female population 65+ raw value"                                     
#  "Total female population raw value"                                 #   "Population growth raw value"  )                 
#  "Cancer incidence raw value"                                          
# "Coronary heart disease hospitalizations raw value"                  
#  "Cerebrovascular disease hospitalizations raw value"                  
#  "Smoking during pregnancy raw value"   



vars2010lengthoflife <- c( "Premature death raw value")


vars2010quality <- c(
  #"Poor or fair health raw value", 
  "Poor physical health days raw value",                                 
  "Poor mental health days raw value",
  "Low birthweight raw value",
  "Diabetes monitoring raw value"
)  


vars2010behaviors <- c(
  #"Adult smoking raw value" ,                                             
  "Adult obesity raw value" ,                                            
  "Access to healthy foods raw value",
  #   "Binge drinking raw value",
  "Teen births raw value",
  "Motor vehicle crash deaths raw value",
  # "Motor vehicle crash occupancy rate raw value",             
  # "On-road motor vehicle crash-related ER visits raw value", 
  # "Off-road motor vehicle crash-related ER visits raw value",
  # "Smoking during pregnancy raw value"
)



vars2010access <- c("Uninsured adults raw value",
                    "Ratio of population to primary care physicians"
                    # "Primary care provider rate per 100000 population",
                    # "Did not get needed health care raw value",
                    # "No recent dental visit raw value"
)


vars2010ses <- c( "High school graduation raw value",                       
                  "College degrees raw value",
                  "Unemployment raw value", 
                  "Children in poverty raw value",
                  "Income inequality raw value",
                  #  "Inadequate social support raw value", 
                  "Single-parent households raw value"
                  # "Violent crime raw value",
                  # "Homicides raw value"
)


vars2010environ <- c("Liquor store density raw value",  
                     "Air pollution-particulate matter days raw value",         
                     "Air pollution-ozone days raw value"      )




#### combine data for 2010 and 2019 ----
col2010 <- colnames(combined1)[grepl("2010",colnames(combined1))]

data2010x3 <- data2010x2 %>% filter(Name != "county") %>% 
  mutate(`Premature death raw value` = as.numeric(`Premature death raw value`)) %>% 
  mutate(`Poor or fair health raw value` = as.numeric(`Poor or fair health raw value`)) %>% 
  mutate(`Poor physical health days raw value` = as.numeric(`Poor physical health days raw value`)) %>% 
  mutate(`Poor mental health days raw value` = as.numeric(`Poor mental health days raw value`)) %>% 
  mutate(`Low birthweight raw value` = as.numeric(`Low birthweight raw value`)) %>% 
  mutate(`Adult smoking raw value` = as.numeric(`Adult smoking raw value`)) %>% 
  mutate(`Adult obesity raw value` = as.numeric(`Adult obesity raw value`)) %>% 
  mutate(`Binge drinking raw value` = as.numeric(`Binge drinking raw value`)) %>% 
  mutate(`Motor vehicle crash deaths raw value` = as.numeric(`Motor vehicle crash deaths raw value`)) %>%
  mutate(`Chlamydia raw rate value` = as.numeric(`Chlamydia raw rate value`)) %>% 
  mutate(`Teen births raw value` = as.numeric(`Teen births raw value`)) %>% 
  mutate(`Primary care provider rate per 100000 population` = as.numeric(`Primary care provider rate per 100000 population`)) %>% 
  mutate(`Preventable hospital stays raw value` = as.numeric(`Preventable hospital stays raw value`)) %>% 
  mutate(`Diabetes monitoring raw value` = as.numeric(`Diabetes monitoring raw value`)) %>% 
  mutate(`Hospice use raw value` = as.numeric(`Hospice use raw value`)) %>% 
  mutate(`High school graduation raw value` = as.numeric(`High school graduation raw value`)) %>% 
  mutate(`College degrees raw value` = as.numeric(`College degrees raw value`)) %>% 
  mutate(`Unemployment raw value` = as.numeric(`Unemployment raw value`)) %>% 
  mutate(`Children in poverty raw value` = as.numeric(`Children in poverty raw value`)) %>% 
  mutate(`Income inequality raw value` = as.numeric(`Income inequality raw value`)) %>% 
  # mutate(`Inadequate social support raw value` = as.numeric(``)) %>% 
  mutate(`Single-parent households raw value` = as.numeric(`Single-parent households raw value`)) %>% 
  mutate(`Violent crime raw value` = as.numeric(`Violent crime raw value`)) %>% 
  mutate(`Homicides raw value` = as.numeric(`Homicides raw value`)) %>% 
  mutate(`Air pollution-particulate matter days raw value` = as.numeric(`Air pollution-particulate matter days raw value`)) %>% 
  mutate(`Air pollution-ozone days raw value` = as.numeric(`Air pollution-ozone days raw value`)) %>% 
  mutate(`Access to healthy foods raw value` = as.numeric(`Access to healthy foods raw value`)) %>% 
  mutate(`Liquor store density raw value` = as.numeric(`Liquor store density raw value`)) 


data2019x3 <- data2019x2 %>% filter(Name != "county") %>% 
  mutate(`Premature death raw value` = as.numeric(`Premature death raw value`)) %>% 
  mutate(`Poor or fair health raw value` = as.numeric(`Premature death raw value`)) %>% 
  mutate(`Poor physical health days raw value` = as.numeric(`Premature death raw value`)) %>% 
  mutate(`Poor mental health days raw value` = as.numeric(`Premature death raw value`)) %>% 
  mutate(`Low birthweight raw value` = as.numeric(`Premature death raw value`)) %>% 
  mutate(`Adult smoking raw value` = as.numeric(`Premature death raw value`)) %>% 
  mutate(`Adult obesity raw value` = as.numeric(`Premature death raw value`)) %>% 
  mutate(`Binge drinking raw value` = as.numeric(`Premature death raw value`)) %>% 
  mutate(`Motor vehicle crash deaths raw value` = as.numeric(`Premature death raw value`)) %>%
  mutate(`Chlamydia raw rate value` = as.numeric(`Premature death raw value`)) %>% 
  mutate(`Teen births raw value` = as.numeric(`Premature death raw value`)) %>% 
  mutate(`Primary care physicians raw value` = as.numeric(`Primary care physicians raw value`)) %>% 
  mutate(`Preventable hospital stays raw value` = as.numeric(`Preventable hospital stays raw value`)) %>% 
  mutate(`Diabetes prevalence raw value` = as.numeric(`Diabetes prevalence raw value`)) %>% 
  # mutate(`Hospice use raw value` = as.numeric(`Hospice use raw value`)) %>% 
  mutate(`High school graduation raw value` = as.numeric(`High school graduation raw value`)) %>% 
  mutate(`Some college raw value` = as.numeric(`Some college raw value`)) %>% 
  mutate(`Unemployment raw value` = as.numeric(`Unemployment raw value`)) %>% 
  mutate(`Children in poverty raw value` = as.numeric(`Children in poverty raw value`)) %>% 
  mutate(`Income inequality raw value` = as.numeric(`Income inequality raw value`)) %>% 
  # mutate(`Inadequate social support raw value` = as.numeric(``)) %>% 
  # mutate(`Single-parent households raw value` = as.numeric(`Single-parent households raw value`)) %>% 
  mutate(`Violent crime raw value` = as.numeric(`Violent crime raw value`)) %>% 
  mutate(`Homicides raw value` = as.numeric(`Homicides raw value`)) #%>% 
#  mutate(`Air pollution-particulate matter days raw value` = as.numeric(`Air pollution-particulate matter days raw value`)) %>% 
#  mutate(`Air pollution-ozone days raw value` = as.numeric(`Air pollution-ozone days raw value`)) %>% 
# mutate(`Access to healthy foods raw value` = as.numeric(`Access to healthy foods raw value`)) %>% 
#  mutate(`Liquor store density raw value` = as.numeric(`Liquor store density raw value`)) 

c2019 <- as.data.frame(colnames(data2019x2))


combined1 <- merge(data2010x3, data2019x3, by="5-digit FIPS Code", suffixes = c("2010","2019"))

checkcombo <- as.data.frame(sapply(combined2, function(x) sum(is.na(x))))

combined2 <- combined1 %>% 
  select(-`Hate crimes raw value`,-`Alcohol-related hospitalizations raw value`,-`Fall fatalities 65+ raw value`,
         -`Self-inflicted injury hospitalizations raw value`,-`Coronary heart disease hospitalizations raw value`,
         -`Cerebrovascular disease hospitalizations raw value`,-`Off-road motor vehicle crash-related ER visits raw value2010`,
         -`No recent dental visit raw value`,-`Did not get needed health care raw value`,-`Municipal water (WI) raw value`,
         -`Contaminants in municipal water (WI) raw value`,-`Drug arrests raw value`, -`Opioid hospital visits raw value`,
         -`Off-road motor vehicle crash-related ER visits raw value2019`,-`Smoking during pregnancy raw value2010`,
         -`Motor vehicle crash occupancy rate raw value2010`,-`On-road motor vehicle crash-related ER visits raw value2010`,
         -`Lead poisoned children (WI) raw value`,-`Communicable disease raw value`,-`Cancer incidence raw value`,
         -`Smoking during pregnancy raw value2019`,-`Motor vehicle crash occupancy rate raw value2019`,
         -`On-road motor vehicle crash-related ER visits raw value2019`,
         -`Childhood immunizations raw value`,-`Reading proficiency raw value`,-`W-2 enrollment raw value`,
         -`Poverty raw value`, -`Older adults living alone raw value`,-`Child abuse raw value`,
         -`Injury hospitalizations raw value`,-`Year structure built raw value`,-`Male population 0-17 raw value`,
         -`Male population 18-44 raw value`,-`Male population 45-64 raw value`,-`Male population 65+ raw value`,
         -`Total male population raw value`,-`Female population 0-17 raw value`,-`Female population 18-44 raw value`,
         -`Female population 45-64 raw value`,-`Female population 65+ raw value`,-`Total female population raw value`,
         -`Population growth raw value`,-`Infant mortality raw value`, -`Homicides raw value2019`, - `Disconnected youth raw value`,
         -`Violent crime raw value2010`,-`Homicides raw value2010`,-`Drug overdose deaths raw value`,-`Child mortality raw value`,
         -`Inadequate social support raw value`,-`Firearm fatalities raw value`,-`HIV prevalence raw value`,-`Hospice use raw value`
  )



# Convert additional character to numeric 2010 health data ----
health.dat2010v2 <- health.dat2010 %>% 
  mutate(`Chlamydia rate raw value` = as.numeric(`Chlamydia rate raw value`),
         `Uninsured adults raw value` = as.numeric(`Uninsured adults raw value`),
         `Ratio of population to primary care physicians` = as.numeric(`Ratio of population to primary care physicians`)) %>% 
  # `Food environment index raw value` = as.numeric(`Food environment index raw value`),
  # `Physical inactivity raw value` = as.numeric(`Physical inactivity raw value`),
  # `Access to exercise opportunities raw value` = as.numeric(`Access to exercise opportunities raw value`),
  #  `Excessive drinking raw value` = as.numeric(`Excessive drinking raw value`),
  #   `Alcohol-impaired driving deaths raw value` = as.numeric(`Alcohol-impaired driving deaths raw value`),
  #   `Sexually transmitted infections raw value` = as.numeric(`Sexually transmitted infections raw value`),
  #    `Uninsured raw value` = as.numeric(`Uninsured raw value`),
  #    `Ratio of population to primary care physicians` = as.numeric(`Ratio of population to primary care physicians`),
  #   `Mental health providers raw value` = as.numeric(`Mental health providers raw value`),
  #   `Ratio of population to mental health providers.` = as.numeric(`Ratio of population to mental health providers.`),
  #  `Mammography screening raw value` = as.numeric(`Mammography screening raw value`),
#  `Flu vaccinations raw value` = as.numeric(`Flu vaccinations raw value`),
#  `Children in single-parent households raw value` = as.numeric(`Children in single-parent households raw value`),
#  `Social associations raw value` = as.numeric(`Social associations raw value`),
#  `Injury deaths raw value` = as.numeric(`Injury deaths raw value`),
#  `Air pollution - particulate matter days raw value` = as.numeric(`Air pollution - particulate matter days raw value`)
#  `Drinking water violations raw value` = as.numeric(`Drinking water violations raw value`),
#  `Severe housing problems raw value` = as.numeric(`Severe housing problems raw value`),
# `Percentage of households with overcrowding` = as.numeric(`Percentage of households with overcrowding`),
#  `Percentage of households with lack of kitchen or plumbing facilities` = as.numeric(`Percentage of households with lack of kitchen or plumbing facilities`),
# `Percentage of households with high housing costs` = as.numeric(`Percentage of households with high housing costs`),
# `Driving alone to work raw value` = as.numeric(`Driving alone to work raw value`),
# `Long commute - driving alone raw value` = as.numeric(`Long commute - driving alone raw value`),
#  `Life expectancy raw value` = as.numeric(`Life expectancy raw value`),
#  `Premature age-adjusted mortality raw value` = as.numeric(`Premature age-adjusted mortality raw value`),
#  `Frequent physical distress raw value` = as.numeric(`Frequent physical distress raw value`),
#  `Frequent mental distress raw value` = as.numeric(`Frequent mental distress raw value`),
#  `Food insecurity raw value` = as.numeric(`Food insecurity raw value`),
#  `Limited access to healthy foods raw value` = as.numeric(`Limited access to healthy foods raw value`),
#  `Insufficient sleep raw value` = as.numeric(`Insufficient sleep raw value`),
#  `Uninsured adults raw value2010` = as.numeric(`Uninsured adults raw value2010`),
#  `Uninsured children raw value` = as.numeric(`Uninsured children raw value`),
#  `Other primary care providers raw value` = as.numeric(`Other primary care providers raw value`),
#   `Ratio of population to primary care providers other than physicians.` = as.numeric(`Ratio of population to primary care providers other than physicians.`),
#  `Median household income raw value` = as.numeric(`Median household income raw value`),
#  `Children eligible for free or reduced price lunch raw value` = as.numeric(`Children eligible for free or reduced price lunch raw value`),
#  `Residential segregation - non-white/white raw value` = as.numeric(`Residential segregation - non-white/white raw value`),
#   `Homeownership raw value` = as.numeric(`Homeownership raw value`),
#   `Severe housing problems raw value` = as.numeric(`Severe housing problems raw value`),
#   `Population raw value` = as.numeric(`Population raw value`),
#   `% below 18 years of age raw value` = as.numeric(`% below 18 years of age raw value`),
#  `% 65 and older raw value` = as.numeric(`% 65 and older raw value`),
#  `% American Indian and Alaskan Native raw value` = as.numeric(`% American Indian and Alaskan Native raw value`),
# `% Native Hawaiian/Other Pacific Islander raw value` = as.numeric(`% Native Hawaiian/Other Pacific Islander raw value`),
# `% not proficient in English raw value` = as.numeric(`% not proficient in English raw value`),
# `% Females raw value` = as.numeric(`% Females raw value`),
# `% Rural raw value` = as.numeric(`% Rural raw value`))
janitor::clean_names()


# Convert additional character to numeric 2019 health data ----
health.dat2019v2 <- health.dat2019 %>% 
  mutate(#`Chlamydia rate raw value` = as.numeric(`Chlamydia rate raw value`),
    `Uninsured adults raw value` = as.numeric(`Uninsured adults raw value`),
    # `Ratio of population to primary care physicians` = as.numeric(`Ratio of population to primary care physicians`),
    `Food environment index raw value` = as.numeric(`Food environment index raw value`),
    `Physical inactivity raw value` = as.numeric(`Physical inactivity raw value`),
    `Access to exercise opportunities raw value` = as.numeric(`Access to exercise opportunities raw value`),
    `Excessive drinking raw value` = as.numeric(`Excessive drinking raw value`),
    `Alcohol-impaired driving deaths raw value` = as.numeric(`Alcohol-impaired driving deaths raw value`),
    `Sexually transmitted infections raw value` = as.numeric(`Sexually transmitted infections raw value`),
    `Uninsured raw value` = as.numeric(`Uninsured raw value`),
    # `Ratio of population to primary care physicians` = as.numeric(`Ratio of population to primary care physicians`),
    `Mental health providers raw value` = as.numeric(`Mental health providers raw value`),
    `Ratio of population to mental health providers.` = as.numeric(`Ratio of population to mental health providers.`),
    `Mammography screening raw value` = as.numeric(`Mammography screening raw value`),
    `Flu vaccinations raw value` = as.numeric(`Flu vaccinations raw value`),
    `Children in single-parent households raw value` = as.numeric(`Children in single-parent households raw value`),
    `Social associations raw value` = as.numeric(`Social associations raw value`),
    `Injury deaths raw value` = as.numeric(`Injury deaths raw value`),
    #  `Air pollution - particulate matter days raw value` = as.numeric(`Air pollution - particulate matter days raw value`),
    `Drinking water violations raw value` = as.numeric(`Drinking water violations raw value`),
    `Severe housing problems raw value` = as.numeric(`Severe housing problems raw value`),
    `Percentage of households with overcrowding` = as.numeric(`Percentage of households with overcrowding`),
    `Percentage of households with lack of kitchen or plumbing facilities` = as.numeric(`Percentage of households with lack of kitchen or plumbing facilities`),
    `Percentage of households with high housing costs` = as.numeric(`Percentage of households with high housing costs`),
    `Driving alone to work raw value` = as.numeric(`Driving alone to work raw value`),
    `Long commute - driving alone raw value` = as.numeric(`Long commute - driving alone raw value`),
    `Life expectancy raw value` = as.numeric(`Life expectancy raw value`),
    `Premature age-adjusted mortality raw value` = as.numeric(`Premature age-adjusted mortality raw value`),
    `Frequent physical distress raw value` = as.numeric(`Frequent physical distress raw value`),
    `Frequent mental distress raw value` = as.numeric(`Frequent mental distress raw value`),
    `Food insecurity raw value` = as.numeric(`Food insecurity raw value`),
    `Limited access to healthy foods raw value` = as.numeric(`Limited access to healthy foods raw value`),
    `Insufficient sleep raw value` = as.numeric(`Insufficient sleep raw value`),
    #  `Uninsured adults raw value2010` = as.numeric(`Uninsured adults raw value2010`),
    `Uninsured children raw value` = as.numeric(`Uninsured children raw value`),
    `Other primary care providers raw value` = as.numeric(`Other primary care providers raw value`),
    `Ratio of population to primary care providers other than physicians.` = as.numeric(`Ratio of population to primary care providers other than physicians.`),
    `Median household income raw value` = as.numeric(`Median household income raw value`),
    `Children eligible for free or reduced price lunch raw value` = as.numeric(`Children eligible for free or reduced price lunch raw value`),
    `Residential segregation - non-white/white raw value` = as.numeric(`Residential segregation - non-white/white raw value`),
    `Homeownership raw value` = as.numeric(`Homeownership raw value`),
    `Severe housing problems raw value` = as.numeric(`Severe housing problems raw value`),
    `Population raw value` = as.numeric(`Population raw value`),
    `% below 18 years of age raw value` = as.numeric(`% below 18 years of age raw value`),
    `% 65 and older raw value` = as.numeric(`% 65 and older raw value`),
    `% American Indian and Alaskan Native raw value` = as.numeric(`% American Indian and Alaskan Native raw value`),
    `% Native Hawaiian/Other Pacific Islander raw value` = as.numeric(`% Native Hawaiian/Other Pacific Islander raw value`),
    `% not proficient in English raw value` = as.numeric(`% not proficient in English raw value`),
    `% Females raw value` = as.numeric(`% Females raw value`),
    `% Rural raw value` = as.numeric(`% Rural raw value`)) %>% 
  janitor::clean_names()

# remove corrupted variables ----

health2010v3 <- health2010v2 %>%  
  select(-premature_death_raw_value,
         -`poor_or_fair_health_raw_value`,
         -`poor_physical_health_days_raw_value`,
         -`poor_mental_health_days_raw_value`,
         -`low_birthweight_raw_value`,
         -`adult_smoking_raw_value`,
         -`adult_smoking_raw_value`,
         -`adult_obesity_raw_value`,
         -`binge_drinking_raw_value`,
         -`chlamydia_raw_rate_value`,
         -`teen_births_raw_value`,
         -`motor_vehicle_crash_deaths_raw_value`,
         -`state_fips_code`,
         -`county_fips_code`,
         -`state_abbreviation`,
         -`release_year`,
         -name)


health2019v3 <- health2019v2 %>%  
  select(-premature_death_raw_value,
         -`poor_or_fair_health_raw_value`,
         -`poor_physical_health_days_raw_value`,
         -`poor_mental_health_days_raw_value`,
         -`low_birthweight_raw_value`,
         -`adult_smoking_raw_value`,
         -`adult_smoking_raw_value`,
         -`adult_obesity_raw_value`,
         -`binge_drinking_raw_value`,
         -`chlamydia_raw_rate_value`,
         -`teen_births_raw_value`,
         -`motor_vehicle_crash_deaths_raw_value`,
         -`state_fips_code`,
         -`county_fips_code`,
         -`state_abbreviation`,
         -`release_year`,
         -name,
         -fips_clean_y) %>% 
  mutate(
    ratio_of_population_to_primary_care_physicians = as.numeric(ratio_of_population_to_primary_care_physicians),
    dentists_raw_value = as.numeric(dentists_raw_value),
    ratio_of_population_to_dentists = as.numeric(ratio_of_population_to_dentists),
    air_pollution_particulate_matter_raw_value = as.numeric(air_pollution_particulate_matter_raw_value),
    severe_housing_cost_burden_raw_value = as.numeric(severe_housing_cost_burden_raw_value))
