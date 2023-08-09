library(remotes)
library(quanteda)
library(tm)
library(dplyr)

# pair_distances() from Reagan's textmatch pacakge
pair_distances = function (dat, Z, include = c("cosine", "euclidean", "mahalanobis"), 
                           form = "data.frame", verbose = FALSE){
  if (is.null(form)) {
    form = "data.frame"
  }
  stopifnot(form %in% c("data.frame", "list"))
  simil = c("cosine", "jaccard", "correlation", "ejaccard", 
            "dice", "edice", "hamman", "simple matching", "faith")
  dists = c("euclidean", "kullback", "manhattan", "maximum", 
            "canberra", "minkowski")
  oth = c("mahalanobis", "mahal.lite", "lps")
  all.methods = c(simil, dists, oth)
  try(if (sum(!include %in% all.methods) > 0) 
    stop("invalid distance metric"))
  if (!is.dfm(dat)) {
    dat = quanteda::as.dfm(dat)
  }
  group.names = c("index.0", "index.1")
  if (length(unique(Z)) != 2) {
    stop("treatment indicator Z must be binary")
  }
  else if (is.numeric(Z)) {
    group.names = paste("index.", unique(Z), sep = "")
    Z = as.logical(Z)
  }
  else if (is.character(Z)) {
    group.names = paste("index.", unique(Z), sep = "")
    Z = as.logical(Z == unique(Z)[1])
  }
  ind = which(Z == TRUE)
  ind2 = which(Z == FALSE)
  if (form == "data.frame") {
    tmp = as.data.frame(matrix(0, nrow = length(ind), ncol = length(ind2)))
    docnames = 1:length(Z)
    rownames(tmp) = docnames[ind]
    colnames(tmp) = docnames[ind2]
    tmp = subset(reshape2::melt(as.matrix(tmp)), select = c(Var2, 
                                                            Var1))
    names(tmp) = group.names
    tmp$index.0 = as.numeric(tmp$index.0)
    tmp$index.1 = as.numeric(tmp$index.1)
  }
  else if (form == "list") {
    tmp = list()
  }
  calc = all.methods[all.methods %in% include]
  for (j in 1:length(calc)) {
    if (verbose == TRUE) {
      print(paste("Calculating ", calc[j], " distances...", 
                  sep = ""))
    }
    if (calc[j] %in% simil) {
      d1 = quanteda.textstats::textstat_simil(dat, y = dat[c(ind2), 
      ], method = calc[j], margin = "documents")
      dist = 1 - as.matrix(d1)[c(ind), ]
      rm(d1)
    }
    if (calc[j] == "lps") {
      dat2 = quanteda::convert(dat, to = "matrix")
      fwd = glm(Z ~ dat2, family = "binomial")
      dist = optmatch::match_on(fwd, data = convert(dat, 
                                                    to = "data.frame"))
      rm(fwd, dat2)
    }
    if (calc[j] == "mahalanobis") {
      dat2 = data.frame(Z, dat)
      dist = optmatch::match_on(Z ~ ., data = dat2)
      rm(dat2)
    }
    if (calc[j] == "mahal.lite") {
      dat2 = quanteda::convert(dat, to = "matrix")
      dat2 = Rfast::standardise(dat2)
      dist = as.matrix(Rfast::Dist(dat2))[ind, ind2]
      rm(dat2)
    }
    else if (calc[j] %in% dists) {
      d1 = quanteda.textstats::textstat_dist(dat, y = dat[c(ind2), 
      ], method = calc[j], margin = "documents")
      dist = as.matrix(d1)[c(ind), ]
      rm(d1)
    }
    name = paste(calc[j], ".dist", sep = "")
    if (form == "data.frame") {
      tmp0 = subset(reshape2::melt(dist, value.name = name), 
                    select = c(name))
      tmp = cbind(tmp, abs(tmp0))
      rm(tmp0)
    }
    else if (form == "list") {
      tmp = list(dist)
      names(tmp) = name
    }
    rm(name, dist)
  }
  return(tmp)
}
textmeta_15 <- readRDS("textmeta_15.RDS")
cardlasso_match_table <- readRDS("cardlasso_match_table.RDS")
cardlasso_cosdist <- matrix(nrow = nrow(cardlasso_match_table),ncol=1)
colnames(cardlasso_cosdist)<-c("cos_dist")
gender_bin <- c(1,0)
for (i in 1:nrow(cardlasso_match_table)){
  text_tr <- rbind(textmeta_15[as.numeric(cardlasso_match_table[i,1]),],
                   textmeta_15[as.numeric(cardlasso_match_table[i,2]),])
  corp_2015 <- corpus(text_tr, text_field = "selftext")
  toks <-corp_2015 %>%
    tokens(remove_numbers = T, remove_punct = T, remove_separators = T)
  dfm_2015<-dfm(toks,tolower = TRUE,)
  cardlasso_cosdist[i] <-pair_distances(dfm_2015,gender_bin, include = "cosine",verbose = FALSE)[,3]
}
saveRDS(cardlasso_cosdist, file = "cardlasso_cosdist.RDS")