#This code contains Desicision Trees

library(stringr)
library(readr)
library(dplyr)
library(tidyverse)

StudentDF <- read_csv("student_alc.csv")



gain.function <- function(GiniTar,gini_var){
  
  g = GiniTar - gini_var
  return(g)
}


Ginni.function <- function(DF){
  p <<- (apply(DF, 2, function(i) i/sum(i)))
  Gin <- 1 - (sum((p*p))) 
  TotalGin <- (Gin)
  return(TotalGin)
}

main.function<- function(df){
  StudentDF <<- df %>% mutate(alc_prob = ifelse(Dalc + Walc >= 6, "alc_p" , "no_alc_p" ))
  names(StudentDF)[1:32] <- sprintf("Var%d", 1:32)
  colnames(StudentDF)[ncol(StudentDF)] <- "TargVar"
  TargetList <- as.data.frame(table(StudentDF$TargVar))
  TargetList <- data.frame(TargetList[,-1], row.names = TargetList[,1])
  GiniTar <- Ginni.function(TargetList)
  
  
  Student_alc_prob = subset(StudentDF, select= -c(TargVar))
  
  datalist = list()
  gini_var <- 0
  for (Var in (names(Student_alc_prob))) {
    k <-count(StudentDF,StudentDF[[Var]],StudentDF$TargVar)
    for (var_val in unique(k[[1]])) {
      temp_df <- subset(k,k[[1]]==var_val)
      a<- dim(StudentDF)
      s <- sum(temp_df$n)/a[1]
      k_ = subset(temp_df, select= -c(1:2))
      gini_val <- Ginni.function(k_)*s
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

