
library(sf)
library(jsonlite)
library(tidyverse)

# PART 1: GET DATA ON CHARGING STATIONS IN SWITZERLAND

dat <- jsonlite::fromJSON("https://data.geo.admin.ch/ch.bfe.ladestellen-elektromobilitaet/data/oicp/ch.bfe.ladestellen-elektromobilitaet.json")

#empty df to fill data in
out<-data.frame()

#could be made more efficient, but loop through different brands
for(i in dat[["EVSEData"]][[1]]){
  #get geocoords of stations without any names or other
  temp<-data.frame(geocoords=as.character(i$GeoCoordinates$Google))#,names=i$EvseID)

  out<-rbind(out,temp)
}
#separate geocoords to two different columns
pnts<-separate(out,geocoords,c("Longitude","Latitude"),sep=" ",convert=TRUE)
#Long and Lat have to be switched
pnts<-pnts[,c(2,1)]


# PART 2: Get Shapefile and join both together
#now get the shapefile
#here it manually takes the last file. Would need an automatic detection of last file
#map <- read_sf("SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")

download.file("https://data.geo.admin.ch/ch.swisstopo.swissboundaries3d/swissboundaries3d_2021-07/swissboundaries3d_2021-07_2056_5728.shp.zip","data.zip")
unzip("data.zip")
unlink("data.zip")
map <- read_sf("SHAPEFILE_LV95_LN02/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp")


# create a points collection
pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(pnts),
                                     function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326)))

pnts_trans <- st_transform(pnts_sf, 2163) # apply transformation to pnts sf
map_trans <- st_transform(map, 2163)      # apply transformation to polygons sf

#remark: 2163 is a number for some conversion to metric.
#I took it from people applying it tto the US and new zealand


# intersect and extract gemeinde name
pnts$gemeindename <- apply(st_intersects(map_trans, pnts_trans, sparse = FALSE), 2,
                     function(col) {
                       map_trans[which(col), ]$NAME
                     })

#for the following I was just lazy and did same for kantonsnum and bfs_num
#can of course be merged with above to have it quicker
pnts$kantonsnum <- apply(st_intersects(map_trans, pnts_trans, sparse = FALSE), 2,
                     function(col) {
                       map_trans[which(col), ]$KANTONSNUM
                     })

pnts$bfs_num <- apply(st_intersects(map_trans, pnts_trans, sparse = FALSE), 2,
                         function(col) {
                           map_trans[which(col), ]$BFS_NUMMER
                         })

pnts$gemeindename<-as.character(pnts$gemeindename)
pnts$kantonsnum<-as.character(pnts$kantonsnum)
pnts$bfs_num<-as.character(pnts$bfs_num)
head(pnts)

write.csv(pnts,"output.csv",row.names=F)
