library(dplyr)
library(lubridate)


data <- read.csv("data/clean.csv")

# format time
data <- data %>% mutate(start_time = ymd_hms(start_time)) %>%
  mutate(end_time = ymd_hms(end_time))

# compute connected time
data["con_time"] <- as.numeric(difftime(ymd_hms(data$end_time),
                                        ymd_hms(data$start_time),units = "secs"))






data <- data %>% filter(long>=120.8544 & 
                          long<=121.974 & 
                          lat >=30.68889 & 
                          lat<=31.86765)


data <- data %>% mutate(rnd_strat = minute(round_date(start_time, "minute"))) %>%
  mutate(rnd_end = minute(round_date(end_time, "minute")))

num_conn <- data %>% group_by(long,lat,rnd_strat) %>% summarise(num_con = n()) %>% arrange(rnd_strat)
train_data <- data.frame(tail(num_conn,n=200))
test_data <- data.frame(head(num_conn,n=100))
#===============================================================
library(spTimer)
post.gp <- spT.Gibbs(formula = num_con ~ 1 , 
                     data = train_data, model = "GP", 
                     coords = ~ long + lat,
                     scale.transform = "SQRT",
                     spatial.decay = spT.decay(distribution = Gamm(2,1), 
                                               tuning = 0.1))




grid.pred <- predict(post.gp, newdata = test_data, newcoords = ~long + lat)

crdref <- CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
ptx <- SpatialPoints(grid.pred$pred.coords, proj4string=crdref)

pre_mean = as.numeric(grid.pred$Mean)
ptsdf <- SpatialPointsDataFrame(ptx,data.frame(pre_mean))
ptsdf
par(bg=NA)
library(raster)
plot(name_shape$geometry,border="blue", col="gray")

points(ptsdf,pch=20, cex=1, col="red")



library(MBA)
library(fields)

coords <- as.matrix(grid.pred$pred.coords)
x.res <- 200; y.res <- 200
surf <- mba.surf(cbind(coords,
                       pre_mean = as.numeric(grid.pred$Mean)),
                 no.X=x.res, no.Y=y.res, h=5,
                 m=2, extend=FALSE)$xyz.est
image.plot(surf, xaxs = "r",
           yaxs = "r", xlab="long",
           ylab="lat",
           main="Predict mean" )
points(coords)
contour(surf,add = T)



pred <- data.frame(cbind(coords,as.numeric(grid.pred$Mean)))

write.csv(pred,"pred.csv")




pred <- read.csv("pred.csv")
coords <- as.matrix(pred[,2:3])


surf <- mba.surf(cbind(coords, pred[,4]),
                 no.X=x.res, no.Y=y.res, h=5,
                 m=2, extend=FALSE)$xyz.est
image.plot(surf, xaxs = "r",
           yaxs = "r", xlab="long",
           ylab="lat",
           main="Predict mean" )
points(coords)
contour(surf,add = T)




surface3d(surf[[1]], surf[[2]], surf[[3]], col = abs(col))
axes3d()
title3d(main = "strata", xlab = "long", ylab = "lat", zlab = "conn_time")
drape.plot(surf[[1]], surf[[2]], surf[[3]],
           col = col.br(150), theta = 225, phi = 50,
           border = FALSE, add.legend = FALSE,
           xlab = "long", ylab = "lat", zlab = "predict mean")
image.plot(zlim = range(surf[[3]], na.rm = TRUE),
           legend.only = TRUE, horizontal = FALSE)

