
setwd("/Users/zjbranson/Documents/CMU/Research/Reddit Project")
data = readRDS("matched.cardglm.rds")
# data = cov_15
#female and male subsets
data.f = subset(data, gender == 1)
data.m = subset(data, gender == 0)

#function for the mean difference estimator (with CI)
getMeanDiffEstCI = function(data, outcome, logTrans, alpha = 0.05){
	#which outcome to use?
  if(outcome == "ups"){
  	y = data$ups
  }
  if(outcome == "num_comments"){
  	y = data$num_comments
  }
  #log transformation?
  if(logTrans == TRUE){
  	y = log(y + 1)
  }

  #the female/male outcomes are
  y.f = y[data$gender == 1]
  y.m = y[data$gender == 0]

  #the point estimate is
  est = mean(y.f) - mean(y.m)

  #the sample sizes are
  n.f = length(y.f)
  n.m = length(y.m)

  #the sample variances are
  var.f = var(y.f)
  var.m = var(y.m)

  #the total variance for the mean difference estimator
  totalVar = var.f/n.f + var.m/n.m

  #the 95% confidence interval, then, is
  ci = c(est - qnorm(1 - alpha/2)*sqrt(totalVar), est + qnorm(1 - alpha/2)*sqrt(totalVar))

  return(c(est, ci))
}

#results with log transformation
getMeanDiffEstCI(data = data, outcome = "ups", logTrans = TRUE)
getMeanDiffEstCI(data = data, outcome = "num_comments", logTrans = TRUE)
#results without log transformation
getMeanDiffEstCI(data = data, outcome = "ups", logTrans = FALSE)
getMeanDiffEstCI(data = data, outcome = "num_comments", logTrans = FALSE)

#Now let's run a sensitivity analysis
library(sensitivitymw)
#first, create subsets of female/male
data.f = subset(data, gender == 1)
data.m = subset(data, gender == 0)
#now we need a two-column matrix where...
# 1) the first column contains female outcomes,
#    and the second column contains male outcomes.
# 2) each column is organized by subclass.

#First, organize the datasets by subclass:
data.f = data.f[order(data.f$subclass),]
data.m = data.m[order(data.m$subclass),]
#now aggregate the female/male outcomes into a two-column matrix
data.y = cbind(data.f$num_comments, data.m$num_comments)

#Note that senmw() tests the null against the alternative
#that the treatment effect is POSITIVE.
#When the treatment effect is estimated to be NEGATIVE,
#a negative sign needs to be introduced to the two-column matrix.
#(For the not-transformed num_comments, this is the case.)

#then, the sensitivity analysis with gamma = 1 is:
senmw(y = data.y, gamma = 1, method = "t")
#here is the vector of gamma values
gamma.seq = seq(1, 2, by = 0.01)
#now we'll compute the p-value for each gamma
pvalue.seq = vector(length = length(gamma.seq))
for(g in 1:length(gamma.seq)){
  pvalue.seq[g] = senmw(y = data.y,
  	gamma = gamma.seq[g], method = "t")$pval
}

plot(gamma.seq, pvalue.seq, pch = 16,xlab= "Gamma",
                                ylab = "p-value", main = "p−value v. gamma, number of comments")
abline(a=0.05, b=0, col="red")
abline(v=1.13, lty = 2, col = "gray")
legend("right", legend=c("p-value = 0.05", "Gamma = 1.13"),
       col=c("red", "gray"), lty=1:2, cex=1.2)

# original scale, num of upvotes
data.y = cbind(data.f$ups, data.m$ups)
senmw(y = data.y, gamma = 1, method = "t")
#here is the vector of gamma values
gamma.seq = seq(1, 2, by = 0.01)
#now we'll compute the p-value for each gamma
pvalue.seq = vector(length = length(gamma.seq))
for(g in 1:length(gamma.seq)){
  pvalue.seq[g] = senmw(y = data.y,
                        gamma = gamma.seq[g], method = "t")$pval
}

plot(gamma.seq, pvalue.seq, pch = 16,xlab= "Gamma",
     ylab = "p-value", main = "p−value v. gamma, number of upvotes")
abline(a=0.05, b=0, col="red")
abline(v=1.33, lty = 2, col = "gray")
legend("right", legend=c("p-value = 0.05", "Gamma = 1.33"),
       col=c("red", "gray"), lty=1:2, cex=1.2)


#Let's repeat this sensitivity analysis, 
#but for the log-transformed num_comments.
data.y = cbind(log(data.f$num_comments + 1),
	log(data.m$num_comments + 1))

#then, the sensitivity analysis with gamma = 1 is:
senmw(y = data.y, gamma = 1, method = "t")
#here is the vector of gamma values
gamma.seq = seq(1, 2, by = 0.01)
#now we'll compute the p-value for each gamma
pvalue.seq = vector(length = length(gamma.seq))
for(g in 1:length(gamma.seq)){
  pvalue.seq[g] = senmw(y = data.y,
  	gamma = gamma.seq[g], method = "t")$pval
}

plot(gamma.seq, pvalue.seq, pch = 16,xlab= "Gamma",
     ylab = "p-value", main = "p−value v. gamma, log(comments+1)")
abline(a=0.05, b=0, col="red")
abline(v=1.01, lty = 2, col = "gray")
legend("right", legend=c("p-value = 0.05", "Gamma = 1.01"),
       col=c("red", "gray"), lty=1:2, cex=1.2)

# log transformed, upvotes
data.y = cbind(log(data.f$ups + 1),
               log(data.m$ups + 1))
senmw(y = data.y, gamma = 1, method = "t")
#here is the vector of gamma values
gamma.seq = seq(1, 2, by = 0.01)
#now we'll compute the p-value for each gamma
pvalue.seq = vector(length = length(gamma.seq))
for(g in 1:length(gamma.seq)){
  pvalue.seq[g] = senmw(y = data.y,
                        gamma = gamma.seq[g], method = "t")$pval
}
plot(gamma.seq, pvalue.seq, pch = 16,xlab= "Gamma",
     ylab = "p-value", main = "p−value v. gamma, log(upvotes+1)")
abline(a=0.05, b=0, col="red")
abline(v=1.09, lty = 2, col = "gray")
legend("right", legend=c("p-value = 0.05", "Gamma = 1.09"),
       col=c("red", "gray"), lty=1:2, cex=1.2)

