library(dplyr)
library(MatchIt)
library(cobalt)
library(randChecks)

f <- as.formula("gender ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + 
                  X12 + X13 + X14 + X15 + X16 + X17 + X18 + X19 + WPS + WC + 
                  Sixltr + Dic + anger + anticipation + disgust + fear + joy + 
                  negative + positive + sadness + surprise + trust + AllPunc + 
                  Period + Comma + Colon + SemiC + QMark + Exclam + Dash + 
                  Quote + Apostro + Parenth + OtherP + genin + huliu + LMC + 
                  LSD + NRC + SWS + pleasure + arousal + dominance + LSDdouble + 
                  Flesch + Dale.Chall + Dickes.Steiwer + Traenkle.Bailer + 
                  Jan_indic + Feb_indic + Mar_indic + Apr_indic + May_indic + 
                  Jun_indic + Jul_indic + Aug_indic + Sep_indic + Oct_indic + 
                  Nov_indic + Dec_indic")
cov_15 <-read.csv("cov_15.csv",header = T, sep = ",")
cov_15 <- mutate_all(cov_15, function(x) as.numeric(as.character(x)))
glm2 <- matchit(formula = f, data = cov_15,
                distance = "glm",link = "logit",
                discard = "both",method = "nearest",ratio = 1,
                caliper = 0.1, std.caliper = T)
matched.glm2<- match.data(glm2)
saveRDS(matched.glm2, file = "matched.glm2.RDS")
glm2cb <-abs(getStandardizedCovMeanDiffs(matched.glm2[-c(47,62:78)],matched.glm2$gender))
mean(glm2cb)
max(glm2cb)