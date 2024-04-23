x <- c(1, 2, 3, 4)
y <- c(1, log(2)* exp(20*3), 54, 78)

ExpReg <- function(X, Y) {
    Y <- log(Y)
    n <- length(X)	# X hossza

    # Összegzések
    osszY <- Ossz(Y)
    osszX <- Ossz(X)
    osszXY <- Ossz(X, Y)
    osszXNegyzet <- Ossz(X, X)

    # "a" kiszámolása
    osszA1 <- (osszXY * n) - (osszY * (-osszX))
    osszA2 <- (osszXNegyzet * n) - (osszX * (-osszX))
    a <- osszA1/osszA2

    # "b" kiszámolása
    osszB1 <- (osszX * osszY) - (osszX * osszXY)
    osszB2 <- (osszXNegyzet * n) - (osszX * (-osszX))
    b <- osszB1 / osszB2

    # Kiíratás
    cat("(a) =", a, "\n")
    cat("(b) =", b, "\n")

    e <- 2.71  

    # Kirajzolás
    plot(X, Y)	
    curve(exp(a* x) * exp(b), col = "purple")
}

Ossz <- function(X, Y = NULL) {
  sum <- 0
  
  # Ha csak X-et kap, összegzi az X változókat
  if (is.null(Y)) {
    for (i in seq_along(X)) {
      sum <- sum + X[i]
    }
  } else {
    # Ha X és Y is meg van adva, összegzi az X és Y szorzatát
    for (i in seq_along(X)) {
      sum <- sum + X[i] * Y[i]
    }
  }
  
  return(sum)
}

ExpReg(x, y)


# Mátrix:

# A1 - Jobb fent:
# sum(xy)	-sum(x)
# sum(y)	n

# B1 - Bal fent:
# sum(x)	sum(xy)
# sum(x)	sum(y)

# A2 - Jobb lent:
# sum(x^2)	-sum(x)
# sum(x)	n

# B2 - Bal lent:
# sum(x^2)	-sum(x)
# sum(x)	n
