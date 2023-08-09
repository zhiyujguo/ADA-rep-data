library(dplyr)
library(MatchIt)
library(cobalt)
library(randChecks)

matched.card <- readRDS("matched.card.RDS")
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


cardlasso <- matchit(f, data = matched.card[, !(names(matched.card) %in% c("weights"))],distance = "lasso",method = "nearest",ratio = 1,
                  caliper = 0.1, std.caliper = T)
cobalt::love.plot(cardlasso)
matched.cardlasso = match.data(cardlasso)
saveRDS(matched.cardlasso, file = "matched.cardlasso.RDS")
cardlasso_cb <- abs(getStandardizedCovMeanDiffs(matched.cardlasso[-c(47,62:78)],matched.cardlasso$gender))

mean(cardlasso_cb)
max(cardlasso_cb)