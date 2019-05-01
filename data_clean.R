library(readxl)
library(dplyr)
# read data from excel file
xls_shanghai <- read_excel("data/data.xlsx",sheet = "aft615",n_max=100000)
# drop out the rows with NA
xls_shanghai <- na.omit(xls_shanghai)

# extract the latitude and longitude from string
get_lat_long <- function(location,lat=TRUE)
{
  spl <- strsplit(location,"/")
  spl
}

lat_long <- lapply(X=xls_shanghai$lat_long,FUN = get_lat_long)
lat_long_mtx <- t(as.matrix(as.data.frame(lat_long)))
xls_shanghai['lat'] <- as.numeric(lat_long_mtx[,1])
xls_shanghai['long'] <- as.numeric(lat_long_mtx[,2])

# drop the the string form address and save the data
xls_shanghai <- xls_shanghai %>% select(-lat_long)
write.csv(xls_shanghai,"data/clean.csv")
