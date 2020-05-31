
library(tidytext)
library(dplyr)
library(tidyverse)
library(tm)
library(quanteda)
library(sqldf)

data("stop_words")

#1.



msg_data <- read_csv("spam.csv")
sms_data <- msg_data
head(msg_data)
table(msg_data$type)

msg_data <- msg_data %>% mutate(ind =row_number())
tokenized_msg_data <- msg_data %>% unnest_tokens(word, text, to_lower = TRUE)
tokenized_stopword_removed_msg_data <- tokenized_msg_data %>% anti_join(stop_words %>% select(word), by = "word")
tokenized_stopword_removed_msg_data$numeric <- as.numeric(tokenized_stopword_removed_msg_data$word) 
non_numeric_data <- tokenized_stopword_removed_msg_data %>% filter(is.na(numeric)) %>% select(-numeric())
non_numeric_data <- select(non_numeric_data, -"numeric")




#2.




freq_data <- non_numeric_data %>% group_by(type, word) %>% summarise(Freq=n()) %>%ungroup()%>% arrange(desc(Freq))
top_freq_data <-freq_data%>% group_by(type) %>% slice(1:10) %>%ungroup()
top_freq_data

#dtm_input_data <- non_numeric_data %>% group_by(ind,type, word) %>% summarise(repetations = n()) %>% ungroup()
#refined_freq_data <- freq_data %>% filter(Freq >2)
#refined_freq_data
#cVals <- data.frame(table(non_numeric_data$word))
#Rows <- non_numeric_data$word %in% cVals[cVals$Freq > 2,1]
#refined_rows <- non_numeric_data[Rows,]

sms_data$text <- as.character(sms_data$text)
sms_data$text <- sapply(sms_data$text, iconv, "ASCII", "UTF-8")
# cnversion to corpus object
sms_corpus_obj <- Corpus(VectorSource(sms_data$text))

# document term matrix

document_term_matrix_sec <- DocumentTermMatrix(sms_corpus_obj, control = list(
  tolower = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stripWhitespace = TRUE , bounds = list(global = c(3, Inf))))
inspect(document_term_matrix_sec[1:5, 1:10])


convert_values <- function(x) {
  x <- ifelse(x > 0, "TRUE", "FALSE")
}

logical_document_term_matrix <- apply(document_term_matrix_sec, MARGIN = 2,convert_values)





#3.


training_ratio = 0.7
training_features <- logical_document_term_matrix[1:(training_ratio*nrow(logical_document_term_matrix))]
testing_features <-  logical_document_term_matrix[(training_ratio*nrow(logical_document_term_matrix)):nrow(logical_document_term_matrix)]





4.



un_data <- readRDS( "UNVotes.rds")

Resolutions <- sqldf("SELECT year, COUNT(vote) res_vote 
             FROM un_data 
             WHERE vote in (1, 2, 3) 
             GROUP BY year
             ORDER BY year;")


plot(Resolutions$year, Resolutions$res_vote, type="p", #ylim = c(2,20000), xlim = c(1946, 2014),
     ylab = "Resolutions", xlab = "Year") 
lines(Resolutions$year, Resolutions$res_vote, type = "b", lwd = 2, lty = 1, 
      col = 1, pch = 10)

Highest_number_votes <- sqldf("SELECT year, MAX(res_vote) max_vot FROM Resolutions")

print(Highest_number_votes)

Un_data_country_correction <- un_data
Un_data_country_correction$country[Un_data_country_correction$country == "Federal Republic of Germany"] <- "Germany"  
Un_data_country_correction$country <- as.factor(Un_data_country_correction$country)


america <- sqldf("SELECT country, year, vote, COUNT(vote) cnt 
               FROM Un_data_country_correction
               WHERE country like 'United States of America' 
               GROUP BY year, country, vote;")

germany <-  sqldf("SELECT country, year, vote, COUNT(vote) cnt 
            FROM Un_data_country_correction
            WHERE country like '%Germany%' 
            GROUP BY year, country, vote;")


AgreementVote <- sqldf("SELECT year, total_vote, agg_vote, ((agg_vote * 100) / total_vote) percent
                        FROM (SELECT year, SUM(total_vote) total_vote, SUM(CASE
                                                                              WHEN vote == 1 THEN vote_cnt
                                                                              WHEN vote == 3 THEN vote_cnt
                                                                              ELSE 0
                                                                          END) agg_vote
                              FROM (SELECT A.year, A.vote, CASE
                                                              WHEN A.cnt < B.cnt THEN A.cnt
                                                              WHEN A.cnt > B.cnt THEN B.cnt
                                                              ELSE A.cnt
                                                            END vote_cnt, SUM(A.cnt) total_vote
                                                          
                                     FROM america A, germany B
                                     WHERE A.vote = B.vote
                                     AND A.year = B.year
                                     GROUP BY A.year, A.vote)
                              GROUP BY year);")
print(AgreementVote)




5.

linearModel <- lm(formula = agg_vote ~ year, data = AgreementVote)
print(summary(linearModel))

plot(linearModel, 1)
lines(agg_vote, year, type = "b", lwd = 1.5, lty = 3, 
      col = 3, pch = 18)


AgreementVote %>% ggplot( aes(x = year, y = agg_vote)) + 
  geom_path() + geom_point() +
stat_smooth(method = "lm", col = "red")


COUNTRIES <- sqldf("SELECT country, agg_vote --, ((agg_vote * 100) / total_vote) percent
                    FROM (SELECT country, SUM(cnt) total_vote, 
                                SUM(CASE
                                    WHEN vote == 1 THEN cnt
                                    WHEN vote == 3 THEN cnt
                                    ELSE 0
                                  END) agg_vote
                          FROM (SELECT country, vote, COUNT(vote) cnt 
                                 FROM un_data 
                                 GROUP BY country, vote)
                                 GROUP BY country)
                                 WHERE ((agg_vote * 100) / total_vote) >= 70;")

distMatrix <- as.matrix(dist(COUNTRIES, method = "manhattan", diag = TRUE, upper = FALSE))
print(summary(distMatrix))




#6.





DATASET <- sqldf("SELECT country, year, SUM(cnt) cnt, vote
                  FROM (SELECT CASE
                            WHEN country like 'Cuba' THEN 1
                            WHEN country like 'Germany' THEN 2
                            WHEN country like 'India' THEN 3
                            WHEN country like 'Israel' THEN 4
                            WHEN country like 'Italy' THEN 5
                            WHEN country like 'Netherlands' THEN 6
                            WHEN country like 'United States of America' THEN 7
                            ELSE 0
                         END country, CASE 
                                          WHEN vote == 3 THEN 0
                                          ELSE vote
                                        END vote, year, COUNT(vote) cnt
                 FROM Un_data_country_correction
                 WHERE country IN ('Italy', 'Netherlands', 'United States of America', 'Israel', 'Cuba', 'India', 'Germany')
                 AND vote <> 2
                 GROUP BY country, vote, year)
                 GROUP BY country, vote, year;")


nRows <- nrow(DATASET)
TRAIN_SET <- DATASET[1 : (0.7*nRows),]
TEST_SET <- DATASET[((0.7*nRows)+1) : nRows,] 

CL_TRAIN_LABEL <- factor(TRAIN_SET[,4])
CL_TEST_LABEL <- factor(TEST_SET[,4])


glmModel <- glm(formula = vote~., data = TRAIN_SET, family = binomial(link='logit'))

glmPredict <- predict(glmModel, newdata = TEST_SET, type = "response")

fitted.results <- ifelse(glmPredict > 0.5,1,0)
misClasificError <- mean(fitted.results != TEST_SET$vote)
print(paste('Logistic Regression Model Accuracy :: ',1-misClasificError))


TRAIN_SET$vote=NULL
TEST_SET$vote=NULL
knn3T <- knn3Train(TRAIN_SET, TEST_SET, cl=CL_TRAIN_LABEL, k=3)
confMatrix <- confusionMatrix(data = factor(knn3T), reference = CL_TEST_LABEL,
                              dnn = c("Prediction", "Actual"))

print(confMatrix)