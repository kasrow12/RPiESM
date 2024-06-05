# Naukowiec chce sprawdzić czy liczba cząstek emitowanych przez pewną substancję
# promieniotwórczą w ciągu 10-ciu sekund jest zmienną losową o rozkładzie Poissona.
# W tym celu zbadał liczbę cząstek, które zostały wyemitowane przez tą substancję
# w ciągu dziesięciosekundowych odcinków czasu i zebrane dane zapisał w poniższej
# tabelce:

# liczba wyemitowanych cząstek
# w ciągu 10-ciu sekund 0 1 2 3 4 5
# liczba przypadków, kiedy
# zostało wyemitowanych tyle cząstek 140 280 235 200 100 45

# Jakie wnioski wyciągnie naukowiec na poziomie istotności 0,1?

dane = c(140,280,235,200,100,45)

# wersja głupia XD
# dane0 = c(rep(0, 140), rep(1, 280), rep(2, 235), rep(3, 200), rep(4, 100), rep(5, 45))

dane0 = rep(0:5, dane)
library(MASS)
est.lambdy = fitdistr(dane0, "Poisson")$est

prob = c(dpois(0:4, est.lambdy), ppois(4, est.lambdy, lower.tail=F))

# T ~ chi^2(k - 1 - r)
# t         6       1

# pv = P(T > t) = 1 - pchisq(t, 4)

t = chisq.test(dane, p=prob)$stat
cat('t =', t, ', p-value =', 1 - pchisq(t, 4), '< 0.1 => odrzucamy')

