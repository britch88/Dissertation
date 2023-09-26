

options(scipen=100)

# Load packages ----
library(skimr)
library(tidyverse)
library(ggcorrplot)
library(dplyr)


# read in data ----
health.dat <- readRDS("/data/share/xproject/Training/Practice/henderson/Dissertation/rda/health_analysis_file2011_2019.rds")





# looking at rows with missings ----
health.dat$na_count <- apply(health.dat, 1, function(x) sum(is.na(x)))
hist(health.dat$na_count, breaks = 60)
check <- filter(health.dat, na_count >30)




# Remove variables with more than 15% missing
health.dat2 <- health.dat[,which(colMeans(!is.na(health.dat))>0.15)] %>% janitor::clean_names() #%>% 
  select( -state, -x5_digit_fips_code, 
         -fips_county_code,
         -fips_state_code)


# Histograms of key variables ----

### "Poor mental health days raw value" 
hist(health.dat2$poor_mental_health_days_raw_value, breaks=20)


### "Poor physical health days raw value"
hist(health.dat2$poor_physical_health_days_raw_value, breaks=20)

### "Poor or fair health raw value"   
hist(health.dat2$poor_or_fair_health_raw_value, breaks = 20)



# Change in variables over time ----

# Poor mental health days
ggplot(health.dat2, aes(x = year, y = poor_mental_health_days_raw_value)) +
  geom_point(alpha=.01) +
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019)) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  ylab("Days") +
  xlab("Year") +
  labs(title = "County Average Number of Poor Mental Health Days in Last 30 Days") +
  # Calculate the mean based on y, set geom = line 
  stat_summary(fun.y = "mean", colour = "red", size = 1.5, geom = "line") +
  theme(text = element_text(size=12),
        axis.text = element_text(size=12))

# Poor physical health days
ggplot(health.dat2, aes(x = year, y = poor_physical_health_days_raw_value)) +
  geom_point(alpha=.01) +
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019)) +
  scale_y_continuous(breaks = seq(0,10,1)) +
  ylab("Days") +
  xlab("Year") +
  labs(title = "County Average Number of Poor Physical Health Days in Last 30 Days") +
  # Calculate the mean based on y, set geom = line 
  stat_summary(fun.y = "mean", colour = "red", size = 1.5, geom = "line") +
  theme(text = element_text(size=12),
        axis.text = element_text(size=12))

# Poor or fair health 
ggplot(health.dat2, aes(x = year, y = poor_or_fair_health_raw_value*100)) +
  geom_point(alpha=.01) +
  scale_x_continuous(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019)) +
#  scale_y_continuous(breaks = seq(0,10,1)) +
  ylab("Percent") +
  xlab("Year") +
  labs(title = "Percent of Adults Reporting Poor or Fair Health") +
  # Calculate the mean based on y, set geom = line 
  stat_summary(fun.y = "mean", colour = "red", size = 1.5, geom = "line") +
  theme(text = element_text(size=12),
        axis.text = element_text(size=12))

      
# Descriptive summary table ----

summarydat<- as.data.frame(skim(ungroup(health.dat2))) %>% 
  select(-character.max, -character.empty, -numeric.p25, -numeric.p75,
         -character.n_unique, -character.min, -complete_rate,
         -character.whitespace, -numeric.hist, -skim_type)


write_csv(summarydat, "/data/share/xproject/Training/Practice/henderson/Dissertation/Output/healthsummary2011_2019.csv")



