#This file contains Exercise 5_2

library(factoextra)
library(dbscan)
library(fpc)

DfStud <- read.csv("clustering-student-mat.csv")

dbscan::kNNdistplot(DfStud, k =  6)
abline(h = 5, lty = 2)

DB <- fpc::dbscan(DfStud, eps = 5, MinPts = 6)

plot(DfStud,col=DB$cluster)

fviz_cluster(DB, DfStud, geom = "point")

stud_optics <- optics(DfStud,  minPts = 6)


for (i in seq(1,5,0.5)) {
  
  DB_clus = extractDBSCAN(stud_optics,eps_cl = i)
  
  plot(DB_clus)
  hullplot(DfStud,DB_clus)
  
  
}



