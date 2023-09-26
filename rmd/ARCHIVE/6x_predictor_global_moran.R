# Global Moran's I for all predictors ----

library(sf)
library(dplyr)
library(spdep)

# a shapefile
shape <- st_read(system.file("shape/nc.shp", package="sf")) 

# list weight object
lw <- nb2listw(neighbours = poly2nb(shape, 
                                    queen = TRUE), 
               style = "W",
               zero.policy = TRUE)

# variable names to be investigated
variables <- c("BIR74", "SID74", "NWBIR74")

# initate empty resultset
result <- tibble(NULL)

# now let's iterate!! :)
for (i in variables) {
  
  # calculate the moran's object (as list)
  res <- moran.mc(pull(shape, !!i),
                  listw = lw, 
                  nsim = 999, 
                  zero.policy = TRUE)
  
  # use the res object to create a new row in results dataset
  result <- result %>% 
    bind_rows(tibble(variable = i,
                     statistic = res$statistic,
                     pvalue = res$p.value))
  
}

# check result
result

