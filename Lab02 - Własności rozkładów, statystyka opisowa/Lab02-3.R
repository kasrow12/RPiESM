# 3. Sporządzić wykresy funkcji prawdopodobieństwa następujących rozkładów
# dwumianowych: binom(10, 0.5), binom(10, 0.25), binom(50, 0.25).
# Wyciągnąć wnioski.

px = dbinom(0:10, 10, 0.5)
py = dbinom(0:10, 10, 0.25)
pz = dbinom(0:100, 100, 0.25)

# widzimy CTG
par(mfrow=c(4,1))
barplot(px, names.arg=0:10)
barplot(py, names.arg=0:10)
barplot(pz, names.arg=0:100)
#lines(dnorm(100, 100*0.25, sqrt(100*0.25*0.75)))

# ten bardziej oddaje że dyskretne
plot(0:10, px, pch=19, type='h', lwd=2, las=1, col=10, lty=1)

