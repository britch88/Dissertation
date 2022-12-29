library(readxl)
dat2000 <- read_excel("raw/Additional Predictors for Decomposition/percent white and correctional pop 2000.xlsx")

dat2000v2 <- dat2000 %>% 
  mutate(white.pct = (`white male` + `white female`)/ Total * 100,
         corrections.pct = `Correctional institutions`/ Total * 100,
         fips_clean = substr(`GIS Join Match Code`,3,7))

dat2000whitecorrections <- dat2000v2 %>%
  select(fips_clean,white.pct, corrections.pct)


saveRDS(dat2000whitecorrections, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/white_corrections2000.rds")
