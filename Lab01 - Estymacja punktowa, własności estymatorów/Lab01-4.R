# Niech X1, X2, . . . , Xn będzie prostą próbą losową z rozkładu wykładniczego
# Exp(λ), czyli rozkładu o gęstości λexp(−λx) dla x > 0, 0 wpp, gdzie λ > 0.


# a) W celu oszacowania czasu działania pewnych bateryjek, dział kontroli jakości
# zmierzył czas pracy 8 losowo wybranych bateryjek i otrzymał następujące wyniki
# (w godz.):
dane <- c(483, 705, 2623, 347, 620, 2719, 1035, 421)
# Wiadomo, że czas pracy tych bateryjek ma rozkład wykładniczy Exp(λ) z nieznaną
# λ > 0. Dla danych zebranych przez dział kontroli jakości, podać wartość
# estymatora największej wiarygodności parametru λ.


library(MASS)
# lambda daszek nw
eest <- fitdistr(dane, "exponential")$est
# albo z wzorów/przekształceń
est <- 1 / mean(dane)
cat('a) eest =', eest)
# print(eest - est) # 0


# b) Dla danych z pkt. a) wyznaczyć estymator największej wiarygodności dla
#   (i) średniego czasu działania bateryjki,
#   (ii) prawdopodobieństwa, że bateryjka będzie działać krócej niż 1000 godz.

#ENW(EX) = ENW(1/lambda) = 1/ENW(lambda) = X daszek
cat('\nb)  (i)', 1 / eest)

# daszek(P(X < 1000)) = F(1000, daszek(lambda))
# = 1 - exp(-daszek(lambda)x) =

cat('\nb) (ii)', pexp(1000, eest))
