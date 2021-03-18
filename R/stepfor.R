#' @title stepfor.Rd
#' @param y dependent variable for selection
#' @param d select the independent variables
#' @description When choosing 'd', and d is a dataframe, then the entire dataframe can be used (look at example), although when this is the case you have to make sure the dependent variable is de-selected
#' @examples data(weight)
#' @examples stepfor(weight$wgt, weight[, -1], alpha = 0.2)
#' @export
stepfor<-function (y = y, d = d, alpha = 0.05)
{
  pval <- NULL
  design <- NULL
  j = 1

  #Filter strings such that more than one different type of variable can be included
  x = Filter(function(x) is.integer(x)|is.numeric(x)|is.factor(x)|is.character(x), d)
  #split the factors that appear into separate columns with values 1 in appearance and 0 elsewhere
  x = as.data.frame(Reduce(cbind, lapply(x, function(col) model.matrix(~ . -1, d = data.frame(col)))))
  #Assign names to numerical values that are lost with the function above
  names(x)[1:ncol(Filter(is.numeric, d))] <- names(Filter(is.numeric, d))
  #filter the names for the rest of the values
  d <- x
  #assign names
  setNames(d, sub(pattern = "^col", replacement = "", names(d)))


  #remove the intercept
  resul0 <- summary(lm(y ~ ., data = d))$coefficients[, 4][-1]

  #remove [-1] to include all the column names that are factored
  d <- as.data.frame(d[, names(resul0)])
  for (i in 1:ncol(d)) {
    sub <- cbind(design, d[, i])
    sub <- as.data.frame(sub)
    lm2 <- lm(y ~ ., data = sub)
    result <- summary(lm2)
    pval[i] <- result$coefficients[, 4][j + 1]
  }
  min <- min(pval)
  while (min < alpha) {
    b <- pval == min
    c <- c(1:length(pval))
    pos <- c[b]
    pos <- pos[!is.na(pos)][1]
    design <- cbind(design, d[, pos])
    design <- as.data.frame(design)
    colnames(design)[j] <- colnames(d)[pos]
    j = j + 1
    d <- as.data.frame(d[, -pos])
    pval <- NULL
    if (ncol(d) != 0) {
      for (i in 1:ncol(d)) {
        sub <- cbind(design, d[, i])
        sub <- as.data.frame(sub)
        lm2 <- lm(y ~ ., data = sub)
        result <- summary(lm2)
        pval[i] <- result$coefficients[, 4][j + 1]
      }
      min <- min(pval, na.rm = TRUE)
    }
    else min <- 1
  }
  if (is.null(design)) {
    lm1 <- lm(y ~ 1)
  }
  else {
    lm1 <- lm(y ~ ., data = design)
  }
  return(lm1)
}
