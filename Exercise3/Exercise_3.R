library(dplyr)




x <- c(2.0,2.0,2.0,2.5,2.5,3.0,4.0,4.0,4.5,4.5,4.5,4.5)
y <- c(1.0,1.5,2.0,1.0,2.0,4.0,1.0,2.5,1.0,1.5,2.5,3.0)
index <- c("p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11","p12")

DF <- data.frame(index, x, y, stringsAsFactors=FALSE)

CurrentCentroids <- (DF[c(1,2,3),c("index","x","y")])



Distance <- apply(CurrentCentroids,1,function(CurrentCentroids)
  (apply(DF,1,function(DF,CurrentCentroids)
    dist(rbind(DF,CurrentCentroids)),CurrentCentroids)))

Distance <- data.frame(Distance)

cols <- c("p1","p2","p3")
colnames(Distance) <- cols

Distance$Clusters <- colnames(Distance)[apply(Distance,1,which.min)]
Distance$index <-index
DF2 <- merge(DF, Distance, by="index")

DF2 <- DF2 %>%
  select(index,x, y, p1, p2, p3, Clusters) %>% group_by(Clusters)

ChangedCentroids <-  DF2 %>%  summarise(x = mean(x),
                                       y = mean(y))

while (CurrentCentroids != ChangedCentroids) {
  CurrentCentroids <- NULL
  CurrentCentroids <- ChangedCentroids
  ChangedCentroids <- NULL
  
  DistanceCalculation(CurrentCentroids,DF)
}


DistanceCalculation <- function(CurrentCentroids,DF) 
{
  
  
}
