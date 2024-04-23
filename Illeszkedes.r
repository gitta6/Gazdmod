k <- c(83, 91, 122, 107, 74, 123)	# Kapott szamok
p <- c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6)	# Valoszinusegek
N <- 600 #Dobasok szama
kuszobszam <- 9.236

Illeszkedes <- function(k, p, N){
	khiNegyzet <- 0
	n <- length(k)

	for (i in 1:n) {
		khiNegyzet <- khiNegyzet + ((( k[i] - N * p[i] ) ^2 ) / ( N * p[i] ))
		}

	cat("Khi négyzet: ", khiNegyzet, "\n")

	if (khiNegyzet <= kuszobszam) {
		cat("A nullhipotézist elfogadjuk.")
	} else {
		cat("A nullhipotézist elutasítjuk, mert nagyobb, mint ", kuszobszam, ".")
	}
}

Illeszkedes(k, p, N)

# khi^2 = 
# Szumma (
# (ki - N * pi)^2
# ______________
# (N * pi)
# )
