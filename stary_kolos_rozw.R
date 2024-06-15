# Zadanie 1 (4 pkt.)
# Ustawić ziarno generatora na 111.
# Wylosować 27 obserwacji z rozkładu wykładniczego z parametrem λ = 0,25.
# Następnie udać, że się nie zna wartości parametru λ i wyznaczyć:

set.seed(111)
x = rexp(27, 0.25)

# a) wartość estymatora największej wiarogodności λ^falka parametru λ

library(MASS)
est = fitdistr(x, "exponential")$est

# b) wartość estymatora największej wiarygodności prawdopodobieństwa P(X > 7),
# gdzie X ~ Exp(λ^falka).

# P(X > 7) = 1 - P(X <= 7) =
1 - pexp(7, rate=est)


# Zadanie 2 (3 pkt.)
# Na 250 losowo wybranych do badania kibiców siatkówki, 97 wytypowało Polaków
# jako potencjalnych zwycięzców. Utworzyć 99% przedział ufności dla odsetka
# kibiców podzielających ten pogląd.

binom.test(97, 250, conf.level=0.99)$conf.int


# Zadanie 3 (4 pkt.)
# Zbiór barley z biblioteki lattice zawiera dane dot. upraw jęczmienia w USA.
# Zmienna yield zawiera informację o wysokości plonów, a zmienna site o farmie,
# na której prowadzono pomiary. Interesuje nas wysokość plonów dla farmy Waseca.
# Można założyć, że badana zmienna ma rozkład normalny.
# Na poziomie istotności α = 0,05 zweryfikować hipotezę, że średni plon uzyskiwany
# na farmie Waseca jest mniejszy niż 50 jednostek.

# Test parametryczny dla jednej populacji
# H0: mi = 50
# H1: mi < 50

library(lattice)
dane = subset(barley, site == "Waseca")$yield
t.test(dane, mu=50, alt="less", conf.level=0.95)$p.value
# p-value = 0.19 > 0.05 -> brak podstaw do odrzucenia H0


# Zadanie 4 (4 pkt.)
# Zbiór cats z biblioteki MASS zawiera dane dotyczące kotów. Interesuje nas zmienna
# Bwt (masa ciała w kg) oraz Sex (płeć, F - kotka, M - kocur).
# Czy można twierdzić, że wariancja masy ciała kocurów i wariancja masy ciała
# kotek różnią się istotnie? Założyć, że w obu grupach rozkład badanej cechy jest
# normalny. Przyjąć poziom istotności testu α = 0,05.

# Test parametryczny dla dwóch populacji
# H0: sigma^2_F = sigma^2_M
# H1: sigma^2_F != sigma^2_M

var.test(cats$Bwt ~ cats$Sex, alt="two.sided", conf.level=0.95)
var.test(subset(cats, Sex=="M")$Bwt, subset(cats, Sex=="F")$Bwt, alt="two.sided", conf.level=0.95)
# p-value = 0.0001 < 0.05 => odrzucamy H0


# Zadanie 5 (4 pkt.)
# W celu sprawdzenia, czy generator liczb losowych z rozkładu t-Studenta t(30)
# działa poprawnie, wygenerowano 5-cio elementową próbę z tego rozkładu i otrzymano
# następujące wartości:
dane = c(-0.89, -0.13, -0.05, 1.18, 0.25)
# Czy można w związku z tym twierdzić, że badany generator działa poprawnie?
# Zweryfikować odpowiednią hipotezę przyjmując poziom istotności testu 0,05.

# H0: dane mają rozkład t(30)
# H1: ~H0

ks.test(dane, "pt", df=30)
# p-value = 0.85 > 0.05 => brak podstaw do odrzucenia H0


# Zadanie 6 (6 pkt.)
# Badano czas działania 4 różnych typów baterii: A, B, C i D. Badanie przeprowadzono
# na losowej próbie 20 baterii i otrzymano następujące wyniki (czas działania w h):
# typ A: 163, 205, 197, 286, 172
# typ B: 87, 106, 101, 94, 123
# typ C: 82, 153, 87, 103, 96
# typ D: 104, 136, 98, 207, 146
# Czy na podstawie danych z tabelii można twierdzić, że występują istotne różnice
# w średniej długości czasu działania baterii A, B, C i D? Zweryfikować odpowiednie
# hipotezy na poziomie istotności α = 0,05.

# ANOVA
# H0: mi_A = mi_B = mi_C = mi_D
# H1: istnieją X i Y, t. że mi_X != mi_Y
# Założenia:
# normalność - shapiro.test (git)
# równość wariancji - bartlett (git)

# anova - lm, summary
# p-val = 0.001 < 0.05 => odrzucamy H0

# TukeyHSD - baterie A trzymają dłużej

