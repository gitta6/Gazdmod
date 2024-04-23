atlag = function(X) {
	s = 0
	n = length(X)
	for (i in 1:n){
		s = s + X[i]
	}
	atlag = s / n
};
cat("Átlag: ", atlag(x), "\n")

empSzoras = function(X, atl){
	s = 0
	n = length(X)
	for(i in 1:n){
		s = s + (X[i] - atl)^2
	}
	emp = s / (n - 1)

	# Gyök alatt:
		# Számláló: sum(xi - atl(x))^2
		# Nevező: n - 1
};
cat("Empirikus szórásnégyzet:", empSzoras(x, atlag(x)), "\n")
