library(dplyr)
library(MatchIt)
library(cobalt)
library(randChecks)

cov_15 <-read.csv("cov_15.csv",header = T, sep = ",")
n <- colnames(cov_15[ ,!(colnames(cov_15) %in% c("ups", "num_comments"))])
f <- as.formula(paste("gender ~", paste(n[!n %in% "gender"], collapse = " + ")))

glm2 <- matchit(f, data = cov_15,distance = "glm",method = "nearest",ratio = 1,
                  caliper = 0.1, std.caliper = T)
cobalt::love.plot(glm2)
matched.glm2 = match.data(glm2)
saveRDS(matched.glm2, file = "matched.glm2.RDS")
glm2cb <- abs(getStandardizedCovMeanDiffs(matched.glm2[-c(47,62:78)],matched.glm2$gender))

mean(glm2cb)
max(glm2cb)