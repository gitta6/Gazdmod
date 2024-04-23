adatMatrix <- matrix(c(42,28,3,17,89,21), nrow = 2, ncol = 3, byrow = TRUE) # Mátrix megadása

FuggetlensegVizsgalat <- function(k){
  sor = rowSums(k) # Mátrix sorainak összegzése
  oszlop = colSums(k) # Mátrix oszlopainak összegzése
  N = sum(sor) # Az összes elem összege
  r = length(sor) # Sorok száma
  s = length(oszlop) # Oszlopok száma
  osszR = 0
  for (i in 1:r){
    osszS = 0
    for (j in 1:s){
      osszS = osszS + (((k[i,j] - (sor[i] * oszlop[j]) / N)^2) / ((sor[i] * oszlop[j]) / N)) # Képlet szerint
    }
    osszR = osszR + osszS
  }
  khiNegyzet = osszR
  cat('Khi négyzet: ', khiNegyzet, '\n')

  if (khiNegyzet < 44.56) {
    cat("Elfogadjuk. \n")
  } else {
    cat("Nem fogadjuk el. \n")
  }

  cat("\n A megadott mátrix: \n")
  print(adatMatrix)
}


FuggetlensegVizsgalat(adatMatrix)
