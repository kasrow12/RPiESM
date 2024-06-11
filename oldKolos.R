set.seed(111)
x = rexp(27, 0.25)

# a) wartość estymatora największej wiarogodności lambda^falka parametru lambda
library(MASS)
est = fitdistr(x, "exponential")$est

# b) wartość estymatora największej wiarygodności prawdopodobieństwa P(X > 7),
# gdzie X ~ Exp(lambda^falka).
# P(X > 7) = 1 - P(X <= 7) =
1-pexp(7, rate=est)

# -- 2
# Na 250 losowo wybranych do badania kibiców siatkówki, 97 wytypowało Polaków
# jako potencjalnych zwycięzców. Utworzyć 99% przedział ufności dla odsetka
# kibiców podzielających ten pogląd.

binom.test(97, 250, conf.level=0.99)$conf.int


# -- 3
# H0: mi = 50
# H1: mi < 50

library(lattice);
dane = subset(barley, site == "Waseca")$yield
t.test(dane, mu=50, alt="less", conf.level=0.95)$p.value


# -- 4
# H0: sigma^2_F = sigma^2_M
# H1: sigma^2_F != sigma^2_M
var.test(cats$Bwt ~ cats$Sex, alt="two.sided", conf.level=0.95)
var.test(subset(cats, Sex=="M")$Bwt, subset(cats, Sex=="F")$Bwt, alt="two.sided", conf.level=0.95)


ks.test(c(-0.89, -0.13, -0.05, 1.18, 0.25), "pt", df=30)
