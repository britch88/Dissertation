

options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(ggcorrplot)


# read in data ----
health2010 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2010.rds")
health2019 <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2019.rds")


# looking at rows with missings ----
health2010$na_count <- apply(health2010, 1, function(x) sum(is.na(x)))
hist(health2010$na_count, breaks = 60)
check <- filter(health2010, na_count >30)
health2010v2 <- health2010 %>%  filter(na_count <= 30)


health2019$na_count <- apply(health2019, 1, function(x) sum(is.na(x)))
hist(health2019$na_count, breaks = 60)
check <- filter(health2019, na_count >55)
health2019v2 <- health2019 %>%  filter(na_count <= 55)




# Remove variables with more than 15% missing
health2010v3 <- health2010v2[,which(colMeans(!is.na(health2010))>0.15)] %>% janitor::clean_names() %>% 
  select(-homicides_raw_value, -violent_crime_raw_value, 
         -inadequate_social_support_raw_value, -hospice_use_raw_value,
         -adult_smoking_raw_value,-binge_drinking_raw_value,-poor_or_fair_health_raw_value, 
         -`release_year`,
         -state,
         -county_fips_code,
         -state_fips_code,
         -county_ranked_yes_1_no_0,
         #-name
         #,
         #-children_eligible_for_free_or_reduced_price_lunch_raw_value,
         #-primary_care_physicians_raw_value, -ratio_of_population_to_primary_care_physicians,
         #-dentists_raw_value,-ratio_of_population_to_dentists,-high_school_graduation_raw_value,
         fips_clean_y)



health2019v3 <- health2019v2[,which(colMeans(!is.na(health2019v2))>0.15)] %>% janitor::clean_names() %>% 
  select(-`infant_mortality_raw_value`,-homicides_raw_value,
         -disconnected_youth_raw_value, -drug_overdose_deaths_raw_value,
         -child_mortality_raw_value, -firearm_fatalities_raw_value,
         -hiv_prevalence_raw_value, -motor_vehicle_crash_deaths_raw_value,
         -residential_segregation_non_white_white_raw_value, -mental_health_providers_raw_value,
         -ratio_of_population_to_mental_health_providers, -violent_crime_raw_value,
         #`state_abbreviation`,
         -`release_year`,
         -state,
         -county_fips_code,
         -state_fips_code,
         -county_ranked_yes_1_no_0,
         #,
         #-children_eligible_for_free_or_reduced_price_lunch_raw_value,
         #-primary_care_physicians_raw_value, -ratio_of_population_to_primary_care_physicians,
         #-dentists_raw_value,-ratio_of_population_to_dentists,-high_school_graduation_raw_value,
         fips_clean_y)


      
# Descriptive summary table ----

summarydat2010 <- as.data.frame(skim(health2010v3)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)


summarydat2019 <- as.data.frame(skim(health2019v3)) %>% 
  select(-character.max, -character.empty,
         -character.n_unique, -character.min,
         -character.whitespace)



write_csv(summarydat2010, "/data/share/xproject/Training/Practice/henderson/Dissertation/Output/health_descriptive_table2010_230119.csv")
write_csv(summarydat2019, "/data/share/xproject/Training/Practice/henderson/Dissertation/Output/health_descriptive_table2019_230119.csv")




# Correlation plot  2010 Health Data ----

## select only numeric columns
health2010num <- health2010v3 %>% dplyr::select(where(is.numeric)) 


# Compute a correlation matrix
corr2010 <- cor(health2010num, use = "complete.obs")
corr2010focal <- corr2010[,c('rate2000','rate2010','rate2019')]


# Compute a matrix of correlation p-values
p.mat2010 <- cor_pmat(health2010num, use = "complete.obs")

# plot correlations
ggcorrplot(corr2010, 
           method = "circle", 
           type = "lower",
           outline.col = "white",
           insig = "blank",
           lab = TRUE,
           lab_size = 2.5,
           digits = 1,
           p.mat = p.mat2010,
           tl.cex = 10,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726")) +
  theme(text = element_text(size=10))


# Correlation plot 2019 Health Data ----

## select only numeric columns
health2019num <- health2019v3 %>% dplyr::select(where(is.numeric)) 


# Compute a correlation matrix
corr2019 <- cor(health2019num, use = "complete.obs") 
corr2019focal <- corr2019[c('rate2000','rate2010','rate2019'),]
corr2019focalv2 <- corr2019[,c('rate2000','rate2010','rate2019')]


# Compute a matrix of correlation p-values
p.mat2019 <- cor_pmat(health2019num, use = "complete.obs")
p.mat2019focal <- p.mat2019[c('rate2000','rate2010','rate2019'),]
p.mat2019focalv2 <- p.mat2019[,c('rate2000','rate2010','rate2019')]


# plot correlations
ggcorrplot(corr2019, 
          # method = "circle", 
           type = "lower",
           outline.col = "white",
           insig = "blank",
           lab = TRUE,
           lab_size = 1,
           p.mat = p.mat2019,
           tl.cex = 6,
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726")) +
  theme(text = element_text(size=6))


options(repr.plot.width = 15, repr.plot.height =8)

ggcorrplot(corr2019focal, 
           # method = "circle", 
           type = "upper",
           outline.col = "white",
           insig = "blank",
           lab = TRUE,
           lab_size = 2,
           p.mat = p.mat2019focal,
           tl.cex = 8,
           digits = 1,
           ggtheme = ggplot2::theme_void,
           colors = c("#6D9EC1", "white", "#E46726")) +
  scale_x_discrete( expand=c(2, 0)) +
  theme(text = element_text(size=8),
        axis.text.x = element_text(size = 8))

library(corrplot)
corrplot(cor(lpp_axis1, lpp_axis2), method=c("number"), bg = "grey10",
         addgrid.col = "gray50", tl.cex=1,
         tl.col = "black", 
         col = colorRampPalette(c("yellow","green","navyblue"))(100))
dev.new()

par(pin=c(20,9))              ##  (width, height) in inches    
par(omi=c(0,.001,.001,0.001))        ## (bottom, left, top, right)  in inches  

corrplot(corr2019focalv2, method=c("number"), bg = "grey10",
         addgrid.col = "gray50", tl.cex=.4,
         tl.col = "black", 
         col = colorRampPalette(c("yellow","green","navyblue"))(100))


# export correlation plots ----
write_csv(as.data.frame(corr2010focal) %>% rownames_to_column(), "/data/share/xproject/Training/Practice/henderson/Dissertation/Output/corr2010_230119.csv")
write_csv(as.data.frame(corr2019focalv2) %>% rownames_to_column(), "/data/share/xproject/Training/Practice/henderson/Dissertation/Output/corr2019_230119.csv")


# deprecated code ----
# select only numeric columns
#num.dat2010 <- health2010v3 %>% dplyr::select(where(is.numeric))
#cormat2010 <- round(cor(health2010num, complete.obs = TRUE),3)

