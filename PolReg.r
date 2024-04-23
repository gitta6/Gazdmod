x <- c(1, 2, 3, 4)
# A2 * X^2 + A1 * X + A0
# y = 5x^2 + 3x + 7, behelyettesítve az x értékeket:
y <- c(5 * 1^2 + 3 * 1 + 7, 5 * 2^2 + 3 * 2 + 7, 5 * 3^2 + 3 * 3 + 7, 5 * 4^2 + 3 * 4 + 7)
n <- length(x)

PolReg <- function(X, Y) {
	plot(X, Y, main = "Polinomiális regresszió", xlab = "X", ylab = "Y")

	A2 <- KiszamitA2(X, Y)
	A1 <- KiszamitA1(X, Y)
	A0 <- KiszamitA0(X, Y)

	curve_x <- seq(min(X), max(X), length.out = 100)
	curve_y <- A2 * curve_x^2 + A1 * curve_x + A0

	lines(curve_x, curve_y, col = "purple")

	# Értékek kiíratása
	cat("a2 =", round(A2, 2), "\n")
	cat("a1 =", round(A1, 2), "\n")
	cat("a0 =", round(A0, 2), "\n")
}

KiszamitA2 <- function(X, Y) {
	osszX <- Ossz(X)
	osszY <- Ossz(Y)
	osszXY <- Ossz(X,Y)
	osszX2Y <- Ossz(X^2, Y)
	osszX2 <- Ossz(X^2)
	osszX3 <- Ossz(X^3)
	osszX4 <- Ossz(X^4)
	
	# Szamlalo es nevezo kiszamitasa:

	felso <- osszX2Y * ((osszX2 * n) - (osszX * osszX)) - osszX3 * ((osszXY * n) - (osszY * osszX)) + osszX2 * ((osszXY * osszX) - (osszY * osszX2))
	also <- osszX4 * ((osszX2 * n) - (osszX * osszX)) - osszX3 * ((osszX3 * n) - (osszX2  * osszX)) + osszX2 * ((osszX3 * osszX) - (osszX2 * osszX2))
	return(felso/also)
}

KiszamitA1 <- function(X, Y) {
	osszX <- Ossz(X)
	osszY <- Ossz(Y)
	osszXY <- Ossz(X,Y)
	osszX2Y <- Ossz(X^2, Y)
	osszX2 <- Ossz(X^2)
	osszX3 <- Ossz(X^3)
	osszX4 <- Ossz(X^4)
	
	# Szamlalo es nevezo kiszamitasa
	felso <- osszX4 * ((osszXY * n) - (osszY * osszX)) - osszX2Y * ((osszX3 * n) - (osszX2 * osszX)) + osszX2 * ((osszX3 * osszY) - (osszX2 * osszXY)) 
	also <- osszX4 * ((osszX2 * n) - (osszX * osszX)) - osszX3 * ((osszX3 * n) - (osszX2 * osszX)) + osszX2 * ((osszX3 * osszX) - (osszX2 * osszX2))
	return(felso/also)
}

KiszamitA0 <- function(X, Y) {
	osszX <- Ossz(X)
	osszY <- Ossz(Y)
	osszXY <- Ossz(X,Y)
	osszX2Y <- Ossz(X^2, Y)
	osszX2 <- Ossz(X^2)
	osszX3 <- Ossz(X^3)
	osszX4 <- Ossz(X^4)
	
	# Szamlalo es nevezo kiszamitasa
	felso <- osszX4 * ((osszX2 * osszY) - (osszX * osszXY)) - osszX3 * ((osszX3 * osszY) - (osszX2 * osszXY)) + osszX2Y * ((osszX3 * osszX) - (osszX2 * osszX2)) 
	also <- osszX4 * ((osszX2 * n) - (osszX * osszX)) - osszX3 * ((osszX3 * n) - (osszX2 * osszX)) + osszX2 * ((osszX3 * osszX) - (osszX2 * osszX2))
	return(felso/also)
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

PolReg(x, y)
