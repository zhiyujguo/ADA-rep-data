library(dplyr)
library(MatchIt)
library(cobalt)
library(randChecks)

cov_15 <-read.csv("cov_15.csv",header = T, sep = ",")
n <- colnames(cov_15[ ,!(colnames(cov_15) %in% c("ups", "num_comments"))])
f <- as.formula(paste("gender ~", paste(n[!n %in% "gender"], collapse = " + ")))

lasso2 <- matchit(f, data = cov_15,distance = "lasso",method = "nearest",ratio = 1,
                  caliper = 0.1, std.caliper = T)
cobalt::love.plot(lasso2)
matched.lasso2 = match.data(lasso2)
saveRDS(matched.lasso2, file = "matched.lasso2.RDS")
lasso2cb <- abs(getStandardizedCovMeanDiffs(matched.lasso2[-c(47,62:78)],matched.lasso2$gender))

mean(lasso2cb)
max(lasso2cb)