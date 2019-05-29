#This code contains Desicision Trees

library(stringr)
library(readr)
library(dplyr)
library(rpart)
library(caret)
library(e1071)
StudentDF <- read_csv("student_alc.csv")



gain.function <- function(GiniTar,GiniVar){
  
  g = GiniTar - GiniVar
  return(g)
}


Ginni.function <- function(DF){
  p <<- (apply(DF, 2, function(i) i/sum(i)))
  Gin <- 1 - (p*p)
  TotalGin <- sum(Gin)
  return(TotalGin)
}

main.function<- function(df){
  StudentDF <<- df %>% mutate(alc_prob = ifelse(Dalc + Walc >= 6, "alc_p" , "no_alc_p" ))
  names(StudentDF)[1:32] <- sprintf("Var%d", 1:32)
  colnames(StudentDF)[ncol(StudentDF)] <- "TargVar"
  TargetList <- as.data.frame(table(StudentDF$TargVar))
  TargetList <- data.frame(TargetList[,-1], row.names = TargetList[,1])
  GiniTar <- Ginni.function(TargetList)
  
  #StudentDF$Identifier <- ifelse(StudentDF$TargVar== c("alc_p"),StudentDF$Identifier <- 1, StudentDF$Identifier<-0)
  #StudentDF_Idf <- filter(StudentDF, Identifier == 1)
  
  Student_alc_prob = subset(StudentDF, select= -c(TargVar))
  
  datalist = list()
  gini_var <- 0
  for (Var in (names(Student_alc_p))) {
    var_df <-count(StudentDF,StudentDF[[Var]],StudentDF$TargVar)
    for (var_val in unique(k[[1]])) {
      temp_df <- subset(k,k[[1]]==var_val)
      a<- dim(StudentDF)
      Probablity <- sum(temp_df$n)/a[1]
      k_ = subset(temp_df, select= -c(1:2))
      gini_val <- Ginni.function(k_)*Probablity
      gini_var <- gini_var+ gini_val
    }
    gini_gain <- gain.function(GiniTar,gini_var)
    datalist[[Var]] <- gini_gain
    gini_var <- 0
    gini_val<- 0
    gini_gain<-0
    
  }
  
  TotalGinni<- do.call(rbind, datalist)
}

main.function(StudentDF)





intrain <- createDataPartition(y =StudentDF$TargVar, p= 0.7, list = FALSE)

training <- StudentDF[intrain,]
testing <- StudentDF[-intrain,]




trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(TargVar ~.-TargVar, data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

predict(dtree_fit, newdata = testing[1,])

test_pred <- predict(dtree_fit, newdata = testing)
confusionMatrix(test_pred, testing$TargVar )


