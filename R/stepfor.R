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

  d <- do.call(cbind, unname(Map(function(x, z) {
    tmp <- as.data.frame(model.matrix(~x -1))
    if(ncol(tmp) == 1 & class(tmp[[1]]) == 'numeric') {
      names(tmp) <- paste0(names(tmp), z)}
    tmp
  }, d, names(d))))
  names(d) <- sub('^x', '', names(d))


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
