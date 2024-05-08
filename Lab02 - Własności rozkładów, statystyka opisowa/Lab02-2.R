# 2. Wygenerować N = 1000 obserwacji z rozkładu normalnego standardowego.
# Utworzyć histogram oraz estymator jądrowy dla tej próby. Nałożyć na uzyskany
# obraz wykres gęstości teoretycznej rozkładu normalnego.

n = 1000
dane = rnorm(n)
h = hist(dane, prob=T, br=70, ylim=c(0,0.5)) # ,ylim=c(0,...)

#dens = density(dane) # wyznacza est jądrowy

lines(density(dane, bw="SJ"))
# lines(density(dane, bw=0.001))
curve(dnorm, add=T, col="red")