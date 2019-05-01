#***********step1: read data************************
library(dplyr)
library(lubridate)


data <- read.csv("data/clean.csv")

# format time
data <- data %>% mutate(start_time = ymd_hms(start_time)) %>%
  mutate(end_time = ymd_hms(end_time))

# compute connected time
data["con_time"] <- as.numeric(difftime(ymd_hms(data$end_time),
                                        ymd_hms(data$start_time),units = "secs"))
head(data)


#***********step2: scatter3d***********************
library(plotly)
sum_time <- data %>% group_by(long,lat) %>% summarise(sum_time = sum(con_time))
p <- plot_ly(sum_time, x =~long, y = ~lat, z = ~sum_time,type = "scatter3d")
p


#mesh3d
p <- plot_ly(sum_time, x =~long, y = ~lat, z = ~sum_time, type = 'mesh3d')
p


#***********step4: map***********************

p <- plot_mapbox(data, x = ~long, y = ~lat) %>%
  add_paths(size = I(2)) %>%
  add_segments(x = -100, xend = -50, y = 50, 75)

p




p <- data %>%
  plot_mapbox(lat = ~lat, lon = ~long,
              split = ~class, size=2,
              mode = 'scattermapbox', hoverinfo='user_id') %>%
  layout(title = 'Shanghai Network (by Laha Ale)',
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))
p



#**********************step5 Clean*****************
dev.off() 
library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(sf)
shapefile <- st_read("./data/shp/shang_dis_merged.shp")
plot(shapefile)


name_shape <- shapefile[,'Name']
plot_ly(name_shape,color = ~Name )

name_shape
data <- data %>% filter(long>=120.8544 & 
                          long<=121.974 & 
                          lat >=30.68889 & 
                          lat<=31.86765)


data <- data %>% mutate(rnd_strat = minute(round_date(start_time, "minute"))) %>%
  mutate(rnd_end = minute(round_date(end_time, "minute")))

sum_time <- data %>% group_by(long,lat,rnd_strat) %>% summarise(sum_time = sum(con_time))
num_conn <- data %>% group_by(long,lat) %>% summarise(num_con = n())





ptx <- sum_time[,1:2]
crdref <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
ptx <- SpatialPoints(ptx, proj4string=crdref)

ptsdf <- SpatialPointsDataFrame(ptx, data=data.frame(sum_time$sum_time))
ptsdf
library(raster)
plot(name_shape$geometry,border="blue", col="gray")
points(ptsdf,pch=20, cex=1, col="red")


#*********************Clean Map********************
p <- data %>%
  plot_mapbox(lat = ~lat, lon = ~long,
              split = ~class, size=2,
              mode = 'scattermapbox', hoverinfo='user_id') %>%
  layout(title = 'Shanghai Network (by Laha Ale)',
         font = list(color='white'),
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(style = 'dark'),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))
p


p <- plot_ly(sum_time, x =~long, y = ~lat, z = ~sum_time,type = "scatter3d")
p



p <- plot_ly(sum_time, x =~long, y = ~lat, z = ~sum_time, type = 'mesh3d')
p



p <- plot_ly(sum_time, x =~long,
             y = ~lat, z = ~sum_time,
             color = ~rnd_strat,
             colors = c('#CCFF00', '#0000CC'),type = "scatter3d")
p


#************Geo Stat*****************8
library(MBA)
library(fields)

coords <- as.matrix(data[,c("long","lat")])
x.res <- 200; y.res <- 200
surf <- mba.surf(cbind(coords,
                       data$con_time),
                 no.X=x.res, no.Y=y.res, h=5,
                 m=2, extend=FALSE)$xyz.est
image.plot(surf, xaxs = "r",
           yaxs = "r", xlab="long",
           ylab="lat",
           main="Connection Time" )
points(coords)
contour(surf,add = T)





