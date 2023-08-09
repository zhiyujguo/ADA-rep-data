library(dplyr)
library(MatchIt)
library(cobalt)
library(randChecks)

matched.card <- readRDS("matched.card.RDS")
matched.card <- matched.card %>% mutate_at(c("gender","Jan_indic","Feb_indic","Mar_indic",
                                             "Apr_indic","May_indic","Jun_indic","Jul_indic",
                                             "Aug_indic","Sep_indic","Oct_indic","Nov_indic",
                                             "Dec_indic" ), as.factor)

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
cardglm <- matchit(f, data = matched.card[, !(names(matched.card) %in% c("weights"))],distance = "glm",method = "nearest",ratio = 1,
                     caliper = 0.1, std.caliper = T)

matched.cardglm = match.data(cardglm)
saveRDS(matched.cardglm, file = "matched.cardglm.RDS")
cardglm_cb <- abs(getStandardizedCovMeanDiffs(matched.cardglm[-c(46,61:77)],matched.cardglm$gender))

mean(cardglm_cb)
max(cardglm_cb)