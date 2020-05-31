#This file contains Exercise 5
library(maps)
library(fpc)
library(factoextra)
library(dplyr)
library(plyr)

DF = maps::world.cities

DF2= DF[which(DF[,3]>50000),]

DF2Subset <- DF2[, 4:5]
DF2SubsetMat <- as.matrix(DF2Subset)

DB <- fpc::dbscan(DF2Subset, eps = 0.15, MinPts = 8)

plot(DB, DF2Subset, main = "DBSCAN", frame = FALSE)

plot(DF2Subset,col=DB$cluster)

p1 <- fviz_cluster(DB, geom = "point", data = DF2Subset) + ggtitle("")

fviz_cluster(DB, DF2Subset, geom = "point")
DF3 <- cbind(DF2,DB$cluster)

colnames(DF3)[colnames(DF3) == "DB$cluster"] <- "clusters"

  DF4 <- DF3[DF3$clusters!=0 , ]

DF5 <- ddply(DF4, .(clusters), transform, num.clusters= length(unique(name)))

DF5 <- DF5 %>%
  select(country.etc,num.clusters) %>% group_by(num.clusters)

largestClusterCountries <- DF5 %>%  summarise(DF5$clusters,countries = country.etc)

