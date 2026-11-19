#first library a few packages that we will use during the practical
#note you may need to install them first...
library(spatstat)
library(here)
library(sp)
library(tmap)
library(sf)
library(tmaptools)
here::here()

borough_file <- here::here(
  "Prac1_data", "statistical-gis-boundaries-london", "ESRI",
  "London_Borough_Excluding_MHW.shp"
)
LondonBoroughs <- sf::st_read(borough_file)
library(stringr)
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(BoroughMap)
summary(BoroughMap)
plaques_file <- file.choose()
BluePlaques <- sf::st_read(plaques_file)
BluePlaques <- st_read(here::here("prac6_data",
                                    "open-plaques-london-2018-04-08.geojson")) %>%
  st_transform(.,27700)
summary(BluePlaques)
#plot the blue plaques in the city
tmap_mode("plot")

tm_shape(BoroughMap) +
  tm_polygons(fill_alpha = 0.5)+
  tm_shape(BluePlaques) +
  tm_dots(fill = "blue", size=0.1)
#remove duplicates
library(tidyverse)

library(sf)
BluePlaques <- distinct(BluePlaques)
BluePlaquesSub <- BluePlaques[BoroughMap,]

#plot the blue plaques in the city
tmap_mode("plot")

#check to see that they've been removed
tm_shape(BoroughMap) +
  tm_polygons(fill_alpha = 0.5)+
  tm_shape(BluePlaquesSub) +
  tm_dots(fill = "blue", size=0.1)
# add sparse=false to get the complete matrix.
# this is the same as select by location in QGIS
# filter from dplyr is the same as select by attribute 
intersect_indices <-st_intersects(BoroughMap, BluePlaques)
#extract the borough

# select by attribute
Harrow <- BoroughMap %>%
  filter(., NAME=="Harrow")

#Check to see that the correct borough has been pulled out
tm_shape(Harrow) +
  tm_polygons(col = NA, fill_alpha = 0.5)
#clip the data to our single borough
BluePlaquesSub <- BluePlaques[Harrow,]



#check that it's worked
tm_shape(Harrow) +
  tm_polygons(fill_alpha = 0.5)+
  tm_shape(BluePlaquesSub) +
  tm_dots(fill = "blue", size=0.1)
#now set a window as the borough boundary
window <- as.owin(Harrow)
plot(window)
#now set a window as the borough boundary
window <- as.owin(Harrow)
plot(window)

window <- spatstat.geom::as.owin(Harrow)
plot(window)
#create a sp object
BluePlaquesSub<- BluePlaquesSub %>%
  as(., 'Spatial')
#create a ppp object
BluePlaquesSub.ppp <- ppp(x=BluePlaquesSub@coords[,1],
                          y=BluePlaquesSub@coords[,2],
                          window=window)
BluePlaquesSub.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="Blue Plaques Harrow")
BluePlaquesSub.ppp %>%
  density(., sigma=500) %>%
  plot()
plaque_density <- density(BluePlaquesSub.ppp, sigma = 500)
plot(plaque_density)
BluePlaquesSub.ppp %>%
  density(., sigma=1000) %>%
  plot()
#First plot the points
plot(BluePlaquesSub.ppp,
     pch=16,
     cex=0.5, 
     main="Blue Plaques in Harrow")

#now count the points in that fall in a 6 x 6
#grid overlaid across the windowBluePlaquesSub.ppp2<-BluePlaquesSub.ppp %>%
BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6)%>%
  plot(., add=T, col="red")
#run the quadrat count
Qcount <- BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6) %>%
  as.data.frame() %>%
  dplyr::count(Var1=Freq)%>%
  dplyr::rename(Freqquadratcount=n)
Qcount %>% 
  summarise_all(class)
sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%
  dplyr::select(-Var1) 

lambda<- Qcount%>%
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)
QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #now calculate the expected counts based on our total number of plaques
  #and save them to the table
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))

#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances",
     xaxt="n")
axis(side=1, at=1:5, labels=0:4)
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)
teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)
plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, add=T, col = "red")
K <- BluePlaquesSub.ppp %>%
  Kest(., correction="border") %>%
  plot()
# we can also extract the plot into a dataframe
Kval <- as.data.frame(Kest(BluePlaquesSub.ppp, correction = "Ripley"))
library(fpc)
#first check the coordinate reference system of the Harrow spatial polygon:
st_geometry(BoroughMap)
#first extract the points from the spatial points data frame
BluePlaquesSubPoints <- BluePlaquesSub %>%
  coordinates(.)%>%
  as.data.frame()

#now run the dbscan analysis
db <- BluePlaquesSubPoints %>%
  fpc::dbscan(.,eps = 700, MinPts = 4)

#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add=T)
#first extract the points from the spatial points data frame
BluePlaquesSubPoints <- BluePlaquesSub %>%
  coordinates(.)%>%
  as.data.frame()

#now run the dbscan analysis
db <- BluePlaquesSubPoints %>%
  fpc::dbscan(.,eps = 700, MinPts = 4)

#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add=T)
# used to find suitable eps value based on the knee in plot
# k is no of nearest neighbours used, use min points
library(dbscan)

BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=4)

# used to find suitable eps value based on the knee in plot
# k is no of nearest neighbours used, use min points
library(dbscan)

BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=4)
BluePlaquesSubPoints<- BluePlaquesSubPoints %>%
  mutate(dbcluster=db$cluster)
# convert the data frame to sf
BluePlaquesSubPoints_sf <- st_as_sf(BluePlaquesSubPoints, 
                                    coords = c("coords.x1", "coords.x2"), 
                                    crs = 27700)

# make the convex hulls around the cluster points
chull_polygons <- BluePlaquesSubPoints_sf %>%
  # remove any points not in a cluster
  filter(dbcluster>0)%>%
  group_by(dbcluster) %>%
  summarise(geometry = st_combine(geometry)) %>%  # combine points
  mutate(geometry = st_convex_hull(geometry)) %>% # convex hull
  st_as_sf()

library(ggspatial)
# Create the map
ggplot() +
  annotation_map_tile(zoom = 13) +
  geom_sf(data = BluePlaquesSubPoints_sf, aes(color=dbcluster), size = 3)+
  geom_sf(data = chull_polygons, aes(fill = dbcluster), 
          alpha = 0.8, 
          # remove polygon borders
          color = NA, 
          show.legend = FALSE) +
  
  theme_bw()



