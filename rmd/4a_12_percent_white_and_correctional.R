library(readxl)
dat2000 <- read_excel("raw/Additional Predictors for Decomposition/percent white and correctional pop 2000.xlsx")

dat2000v2 <- dat2000 %>% 
  mutate(white.pct = (`white male` + `white female`)/ Total * 100,
         corrections.pct = `Correctional institutions`/ Total * 100,
         statecode = str_pad(`State Code`,2,pad ="0"),
         countycode = str_pad(`County Code`,3,pad="0"),
         fips_clean = paste0(statecode,countycode))

dat2000whitecorrections <- dat2000v2 %>%
  select(fips_clean,white.pct, corrections.pct)


saveRDS(dat2000whitecorrections, "/data/share/xproject/Training/Practice/henderson/Dissertation/rda/white_corrections2000.rds")
