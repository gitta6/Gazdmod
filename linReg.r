x <- rnorm(100)  # x tömb generálása
y <- rnorm(100)  # y tömb generálása
n <- length(x)  

LinReg <- function(X, Y) {
    plot(X, Y)  
    a <- KiszamitA(X, Y)  # a paraméter kiszámítása
    b <- KiszamitB(X, Y)  # b paraméter kiszámítása

    lines(X, a * X + b, type = "l", lty = 1, col = "purple")  # Lineáris regressziós vonal hozzáadása a plot-hoz
}

KiszamitA <- function(X, Y) {
    osszX <- Ossz(X)  # X összegének kiszámítása
    osszY <- Ossz(Y)  # Y összegének kiszámítása
    osszXY <- Ossz(X, Y)  # X és Y összegének szorzata
    osszXNegyzet <- Ossz(X, X)  # X négyzetösszegének kiszámítása

    felso <- (osszXY * n) - (osszY * (-osszX))  # a számlálója
    also <- (osszXNegyzet * n) - (osszX * (-osszX))  # a nevezője
    return(felso / also) 
}

KiszamitB <- function(X, Y) {
    osszX <- Ossz(X)  # X összegének kiszámítása
    osszY <- Ossz(Y)  # Y összegének kiszámítása
    osszXY <- Ossz(X, Y)  # X és Y összegének szorzata
    osszXNegyzet <- Ossz(X, X)  # X négyzetösszegének kiszámítása

    felso <- (osszX * osszY) - (osszX * osszXY)  # b számlálója
    also <- (osszXNegyzet * n) - (osszX * (-osszX))  # b nevezője
    return(felso / also) 
}

Ossz <- function(X, Y = NULL) {
  ossz <- 0
  
  # Ha csak X-et kap, összegzi az X változókat
  if (is.null(Y)) {
    for (i in seq_along(X)) {
      ossz <- ossz + X[i]
    }
  } else {
    # Ha X és Y is meg van adva, összegzi az X és Y szorzatát (vagy X, X esetén X^2 értékét)
    for (i in seq_along(X)) {
      ossz <- ossz + X[i] * Y[i]
    }
  }
  
  return(ossz)
}

LinReg(x, y)  

# "a":
# (sum(x, y) * n) - (sum(y) * (-sum(x)))
# ______________________________________
# (sum(x^2) * n) - (sum(x) * (-sum(x)))

# "b":
# (sum(x) * sum(y)) - (sum(x) * sum(x, y))
# ______________________________________
# (sum(x^2) * n) - (sum(x) * (-sum(x)))
