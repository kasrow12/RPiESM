# W katalogu bazowym base znajduje się plik iris a w nim między innymi
# następujące zmienne:
# Sepal.Width - szerokość działki kielicha,   (x)
# Spieces - odmiana irysa                     (y) - musi być typu factor
# class(iris$Species), albo is.factor(iris$Species)
# wtedy z = as.factor(y)


# a) Chcemy zweryfikować czy szerokość działki kielicha zależy od odmiany.
# Jakie postawimy hipotezy do testowania? Wykonać wykresy skrzynkowe w grupach
# by wstępnie ocenić sytuację.

#srednie = tapply(iris$Sepal.Width, iris$Species, mean)
#boxplot(iris$Sepal.Width~iris$Species)
#lines(1:3, srednie, pch=20, type="p", cex=2)

par(mfrow=c(1,3))
with(iris, tapply(Sepal.Width, Species, function(x){qqnorm(x); qqline(x)}))

#tapply(iris$Sepal.Width, iris$Species, function (x) shapiro.test(x)$p.value)
tapply(iris$Sepal.Width, iris$Species, \(x) shapiro.test(x)$p.value)

simplify2array(tapply(iris$Sepal.Width, iris$Species, \(x) shapiro.test(x)[1:2]))

# b) Sprawdzić czy są spełnione założenia analizy wariancji.

# H0: sigma_1^2 = sigma_2^2 = sigma_3^2
# H1: !H0

wynik = bartlett.test(iris$Sepal.Width, iris$Species)$p.value
cat('b) (bartlett) p-value =', wynik, '> alfa = 0.01 (z wykladu) => brak podstaw do odrzucenia')


# --------------- punkt b drugim testem
# te same hipotezy
library(car)
wynik = leveneTest(iris$Sepal.Width~iris$Species, center=mean)[1,3]
# Pr(>F) to nasze p-value
cat('\n   (levene)   p-value =', wynik, '> alfa = 0.01') #?


# c) Stwierdzić, czy szerokość działki kielicha irysa zależy od jego odmiany.
# Jeśli tak, przeprowadzić testy porównań wielokrotnych.

model = lm(iris$Sepal.Width~iris$Species) # tutaj musi być y-ek typu factor
wynik = anova(model)

# stopnie swobody, SSA, mean sq error (2 kol/ 1 kol), meansq 1/meansq 2, pvalue
# liczba grup, SSE, 

# jeden z estymatorów przestaje być dobrym estymatorem i wtedy pvalue strzela

cat('\nc) p-value =', wynik[1,5], '< alfa = 0.01 => odrzucamy H0')

# versicolor-setosa
# H0: mi_v = mi_s
# H1: mi_v != mi_s
# p - v_adj = 0
pairwise.t.test(iris$Sepal.Width, iris$Species, p.adjust="bonf")

# coś innego
par(mfrow=c(1,1))
(tukey = TukeyHSD(aov(model))) #p adj
plot(tukey)
# summary(model) - tutaj też pvalue napisane itd na dole
