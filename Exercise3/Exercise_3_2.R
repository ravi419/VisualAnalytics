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
  