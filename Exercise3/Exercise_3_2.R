# Exercise 3_2 
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cluster)
library(factoextra)
library(dendextend)

DF <- read.csv("clustering-student-mat.csv")
ggplot(DF, aes(x = Exam1, y = Exam2)) +
  geom_point()
iter <- 1
plotlist = list()
par(mfrow=c(2,4))
for(i in 2:8) {
      K <- kmeans(DF, centers= i , nstart = 25)
      #fviz_cluster(K, geom = "point", data = DF)
      plot(DF,col=K$cluster)
      # plot(DF,K$cluster)
      # p <- ggplot(DF, aes(Exam1, Exam2)) +
      #   geom_point(aes(color = K$cluster))
      # plotlist[[i]] <- p
      # iter <- iter +1
      # if (iter != 1){
      #   z <- grid.arrange(p, z)
      # }
      # else{
      #   z <- p
      # }
     
}

k2 <- kmeans(DF, centers = 2, nstart = 25)
k3 <- kmeans(DF, centers = 3, nstart = 25)
k4 <- kmeans(DF, centers = 4, nstart = 25)
k5 <- kmeans(DF, centers = 5, nstart = 25)
k6 <- kmeans(DF, centers = 6, nstart = 25)
k7 <- kmeans(DF, centers = 7, nstart = 25)
k8 <- kmeans(DF, centers = 8, nstart = 25)


# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = DF) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = DF) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = DF) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = DF) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point",  data = DF) + ggtitle("k = 6")
p6 <- fviz_cluster(k7, geom = "point",  data = DF) + ggtitle("k = 7")
p7 <- fviz_cluster(k8, geom = "point",  data = DF) + ggtitle("k = 8")

library(gridExtra)
grid.arrange(p1, p2, p3, p4,p5,p6,p7 ,nrow = 2)







silhouette_score <- function(k){
  km <- kmeans(DF, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(DF))
  mean(ss[, 3])
}
k <- 2:8
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)


fviz_nbclust(DF, kmeans, method='silhouette')



dm <- tribble(~p1,~p2,~p3,~p4,~p5,
              0.00, 0.02, 0.90, 0.36, 0.53,
              0.02, 0.00, 0.65, 0.15, 0.24,
              0.90, 0.65, 0.00, 0.59, 0.45,
              0.36, 0.15, 0.59, 0.00, 0.56,
              0.53, 0.24, 0.45, 0.56, 0.00) %>% as.matrix()
rownames(dm) <- letters[1:5]
colnames(dm) <- letters[1:5]
knitr::kable(dm)

dm <- as.dist(dm, diag = TRUE)
dm
hclust_methods <- c("single", "complete", "average")

hc <- hclust(dm, method = "average")
fviz_dend(hc, k = 4,                 # Cut in four groups
          cex = 1.5,                 # label size
          #k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,  # color labels by groups
          ggtheme = theme_gray()     # Change theme
)




#[ theme_gray(), theme_bw(), theme_minimal(), theme_classic(), theme_void()] 
  