getwd()
library(ggmap)
library(ggplot2)
library(rgdal)
library(sp)
library(dplyr)

register_google(key = "personal KEY")

data5 <- read.table("C:/RH/data/KRI-DAC_Jeju_data5.txt", header=T, encoding = "EUC-KR", fileEncoding="UTF-8", sep = ",")
data6 <- read.table("C:/RH/data/KRI-DAC_Jeju_data6.txt", header=T, encoding = "EUC-KR", fileEncoding="UTF-8", sep = ",")
data7 <- read.table("C:/RH/data/KRI-DAC_Jeju_data7.txt", header=T, encoding = "EUC-KR", fileEncoding="UTF-8", sep = ",")
data7 <- data7[!(names(data7) %in% c("X", "Y"))]
data8 <- read.table("C:/RH/data/KRI-DAC_Jeju_data8.txt", header=T, encoding = "EUC-KR", fileEncoding="UTF-8", sep = ",")
jejudata <- rbind(data5, data6, data7, data8)

head(jejudata, 3)
str(jejudata)

#경도 위도 변경
dir <- ('C:/RH/202011_위치정보요약DB_제주');dir
file_list <- list.files(dir, pattern="entrc_"); file_list
data <- data.frame()
for (file in file_list){
  print(file)
  tmp <- read.delim(paste(dir, file, sep='\\'), sep="|", header=F)
  data <- rbind(data, tmp)
}
str(data)
head(data)
d <- data %>% filter(is.na(V17)==FALSE, is.na(V18)==FALSE)
dim(data)
dim(d)
convertCRS <- function(long, lat, from.crs, to.crs){
  xy <- data.frame(long=long, lat=lat)
  coordinates(xy) <- ~long+lat
  
  from.crs <- CRS(from.crs)
  from.coordinates <- SpatialPoints(xy, proj4string=from.crs)
  
  to.crs <- CRS(to.crs)
  changed <- as.data.frame(SpatialPoints(spTransform(from.coordinates, to.crs)))
  names(changed) <- c("long", "lat")
  
  return(changed)
}
from.crs <- "+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000 +ellps=GRS80 +units=m +no_defs"
to.crs <- "+proj=longlat +ellps=GRS80 +no_defs"

coord <- data.frame(grs.long=d[,17], grs.lat=d[,18])
coord <- cbind(coord, convertCRS(coord$grs.long, coord$grs.lat, from.crs, to.crs))
head(coord, 3)
str(coord)
# 제주도 데이터 전처리
jeju5 <- data.frame(grs.long=jejudata[,13], grs.lat=jejudata[,14])
jeju <- cbind(jeju5, convertCRS(jeju5$grs.long, jeju5$grs.lat, from.crs, to.crs))
head(jeju)
jeju <- data.frame(jejudata, jeju[,3:4])
head(jeju ,5)
# 사용할 변수선택
jeju <- data.frame(jeju$YM, jeju$FranClass, jeju$Type, jeju$Time, jeju$TotalSpent, jeju$DisSpent, jeju$long, jeju$lat)
head(jeju, 3)
jeju_not_support <- jeju %>% filter(jeju$jeju.DisSpent==0 & jeju$jeju.lat<33.6)
jeju_support <- jeju %>% filter(jeju$jeju.DisSpent>0 & jeju$jeju.lat<33.6)
dim(jeju_not_support)
head(jeju_support)
dim(jeju_support)

jejumap <- get_map("jeju", zoom=10, maptype = "roadmap")
map_support <- geom_point(data=jeju_support, aes(x=jeju_support$jeju.long, y=jeju_support$jeju.lat), color=2)
map_not_support <- geom_point(data=jeju_not_support, aes(x=jeju_not_support$jeju.long, y=jeju_not_support$jeju.lat), color=4)
ggmap(jejumap) + map_support + map_not_support

