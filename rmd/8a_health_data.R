# read in data
library(readr)
library(tidyverse)
data2010 <- read_csv("raw/RWJ Health Data/analytic_data2010.csv")
data2010x2 = data2010[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2010))] %>% 
  mutate(fips_clean = "5-digit FIPS Code")



data2011 <- read_csv("raw/RWJ Health Data/analytic_data2011.csv")
data2011x2 = data2011[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2011))]


data2012 <- read_csv("raw/RWJ Health Data/analytic_data2012.csv")
data2012x2 = data2012[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2012))]


data2013 <- read_csv("raw/RWJ Health Data/analytic_data2013.csv")
data2013x2 = data2013[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2013))]


data2014 <- read_csv("raw/RWJ Health Data/analytic_data2014.csv")
data2014x2 = data2014[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2014))]


data2015 <- read_csv("raw/RWJ Health Data/analytic_data2015.csv")
data2015x2 = data2015[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2015))]


data2016 <- read_csv("raw/RWJ Health Data/analytic_data2016.csv")
data2016x2 = data2016[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2016))]


data2017 <- read_csv("raw/RWJ Health Data/analytic_data2017.csv")
data2017x2 = data2017[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2017))]


data2018 <- read_csv("raw/RWJ Health Data/analytic_data2018_0.csv")
data2018x2 = data2018[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2018))]


data2019 <- read_csv("raw/RWJ Health Data/analytic_data2019.csv")
data2019x2 = data2019[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2019))] %>% 
  mutate(fips_clean = "5-digit FIPS Code")


data2020 <- read_csv("raw/RWJ Health Data/analytic_data2020_0.csv")
data2020x2 = data2020[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2020))]



data2021 <- read_csv("raw/RWJ Health Data/analytic_data2021.csv")
data2021x2 = data2021[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2021))]



data2022 <- read_csv("raw/RWJ Health Data/analytic_data2022.csv")
data2022x2 = data2022[,!grepl("CI low|CI high|numerator|denominator|AIAN|Black|Hispanic|Asian|White",names(data2022))]

#### Listing and organizing vars for 2019, and exclude columns with missing, also include columns likely
#### to be highly correlated
### check missings for 2019 
check2019 <- as.data.frame(sapply(data2019x2, function(x) sum(is.na(x))))


vars2019lengthoflife <- c("Premature death raw value" ,                                          
                      "Premature age-adjusted mortality raw value", 
                      "Life expectancy raw value"  ,
                   #   "Child mortality raw value" ,                                           
                   #   "Infant mortality raw value" 
                   )



vars2019quality <- c("Poor or fair health raw value", 
                     "Poor physical health days raw value",                                 
                     "Poor mental health days raw value",
                     "Low birthweight raw value",
                     "Diabetes prevalence raw value",
                     "Frequent physical distress raw value",                                 
                     "Frequent mental distress raw value",                                  
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
                "Injury deaths raw value",
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




#### Listing and organizing vars for 2010, and exclude columns with missing, also include columns likely
#### to be highly correlated
### check missings for 2010 
check2010 <- as.data.frame(sapply(data2010x2, function(x) sum(is.na(x))))


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
 
 
                            
#### combine data for 2010 and 2019

combined1 <- merge(data2010x2, data2019x2, by="fips_clean")

                               
                                                                                     
               