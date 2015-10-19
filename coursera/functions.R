add2 <- function(x, y) {
  x + y
}

above10 <- function(x) {
  use <- x > 10 ## returns logical vector
  x[use]
}

above <- function(x, n = 10) { ## default values
  use <- x > n ## returns logical vector
  x[use]
}

columnmean <- function(y, removeNA = TRUE) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[, i], na.rm = removeNA)
  }
  means
}