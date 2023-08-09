library(SuperLearner)
library(dplyr)
cov_15 <- read.csv("cov_15.csv", sep = ",", header = T)
sample_size <- floor(0.5 * nrow(cov_15))
train_ind <- sample(seq_len(nrow(cov_15)), size = sample_size)

train <- cov_15[train_ind, ]
test <- cov_15[-train_ind, ]
sl_1 = SuperLearner(Y = train$gender, X = train[,-c(46,73,74)],newX = test[,-c(46,73,74)],family = binomial(),
                    SL.library = c("SL.glm","SL.ranger","SL.glmnet","SL.gam"))
pred_1 = sl_1$SL.predict
sl_2 = SuperLearner(Y = test$gender, X = test[-c(46,73,74)], newX = train[,-c(46,73,74)],family = binomial(),
                    SL.library = c("SL.glm","SL.ranger","SL.glmnet","SL.gam"))
pred_2 = sl_2$SL.predict
pred <- rbind(pred_1,pred_2)
cov_15 <- merge(cov_15, pred, by = "row.names", all = TRUE)
df <- dplyr::arrange(cov_15,as.numeric(Row.names))
saveRDS(df, file = "df_temp.RDS")