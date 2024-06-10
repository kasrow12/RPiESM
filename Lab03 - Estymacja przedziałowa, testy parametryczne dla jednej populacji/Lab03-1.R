# Ornitolog, badający określony gatunek, pobrał próbę losową 10 dorosłych ptaków
# i zmierzył ich wagę, otrzymując następujące wyniki (w kg):
dane = c(5.21, 5.15, 5.20, 5.48, 5.19, 5.25, 5.09, 5.17, 4.94, 5.11)
# Zakładamy, że waga ptaków badanego gatunku ma rozkład normalny.


# a) Utworzyć 95% przedział ufności dla średniej wagi ptaków badanego gatunku.
alfa = 0.05
# Metoda 2 w tablicach
cat('a) [', t.test(dane, conf.level=0.95)$conf.int, ']')


# b) Czy na poziomie istotności 0,05 można stwierdzić, że średnia waga ptaków
# badanego gatunku wynosi 5,15 kg?

# 1 - alfa
# H0 u = 5.15 = u0
# H1 u != 5.15

#                   \/ u0 - uważać (nie mi)
test = t.test(dane, mu = 5.15, alt='two.sided') # stąd t i p-value
cat('\nb) p-value =', test$p.value, ', t =', test$statistic, ', calculated p-val =',
    2*pt(-test$statistic, df=length(dane) - 1),
    '> 0.05 => nie ma podstaw do odrzucenia H0') 

# p=0.5 > alfa - nie mamy powodów do odrzucenia H0

#                               -t,    n - 1
# p-value wyjdzie też z 2*pt(-0.67144, df=9)


# c) Czy na poziomie istotności 0,05 można stwierdzić, że średnia waga ptaków
# badanego gatunku jest mniejsza niż 5,20 kg?

# H0 u = 5.20 = u0
# H1 u < 5.20

wynik = t.test(dane, mu=5.20, alt="less")
cat('\nc) p-value =', wynik$p.value, '> 0.05 => nie ma podstaw do odrzucenia H0')


# d) Z jakim prawdopodobieństwem test, przeprowadzony w pkt. c), przyjmie
# na poziomie istotniści 0,05 hipotezę, że średnia waga ptaków badanego gatunku
# jest mniejsza niż 5,20 kg, w sytuacji, gdy w rzeczywistości ta średnia waga
# wynosi 5,15 kg?

# P(odrz H0 | u0=5.15) = moc(5.15) = beta(5.15)

#                                  abs(5.15-5.20),  tutaj już trochę kłamiemy z sd
wynik = power.t.test(n=length(dane), delta=0.05, sd=sd(dane), type="one.sample",
             alternative="one.sided", sig.level=alfa)$power # 0.28

# moc = 1 - (prawdopodobienstwo bledu 2 rodzaju) = 72%
# ?(czyli zakładamy że prawdziwe mi = 5.15 => 5.15 < 5.20,
# czyli delta jaka szansa że wykryjemy)
cat('\nd) p-value =', wynik, ', czyli', (1 - wynik) * 100, '%')


# e) Ile by musiała wynosić średnia waga ptaków tego gatunku by test z pkt. c)
# z prawdopodobieństwem 0,8, na poziomie istotności 0,05, przyjmował hipotezę,
# że średnia waga jest mniejsza niż 5,20 kg?

# teraz szukamy delty, mamy moc
wynik = power.t.test(n=length(dane), power=0.8, sd=sd(dane), type="one.sample",
             alternative="one.sided", sig.level=alfa)$delta # 0.1165

# szukana waga = 5.20 - delta (uważać czy dodać czy odjąć) # 5.08
# ?jakie musi być naprawdę, żeby wyszło, że jest mniejsze od 5.20
cat('\ne) delta =', wynik, ', szukana waga =', 5.20 - wynik)


# f) Załóżmy, że rzeczywista średnia waga ptaków jest równa 5,15 kg.
# Wyznaczyć minimalną liczność próby, która zagwarantuje, że test na poziomie
# istotności 0,05, z prawdopodobieństwem nie mniejszym niż 0,8, będzie
# przyjmował hipotezę, że średnia waga jest mniejsza niż 5,20 kg.

# szukamy n, mamy power i deltę
wynik = power.t.test(power=0.8, delta=0.05, sd=sd(dane), type="one.sample",
          alternative="one.sided", sig.level=alfa)$n # 47.51 czyli 48 (mamy n=10)

cat('\nf) n =', ceiling(wynik))


# g) Utworzyć 95% przedział ufności dla wariancji wagi ptaków badanego gatunku.
library(TeachingDemos)

# zepsuteDane = c(dane, NA)
# -> sigma test nie zadziała na zepsutych danych, t.test tak
# na.omit(zepsuteDane)

wynik = sigma.test(dane, conf.level=0.95)$conf #sigma^2 należy do
cat('\ng) [', wynik, ']')


# h) Utworzyć 95% przedział ufności dla odchylenia standardowego wagi ptaków
# badanego gatunku.
wynik = sqrt(sigma.test(dane, conf.level=0.95)$conf) # sigma € (0.09; 0.25)
cat('\nh) [', wynik, ']')

# i) Czy na poziomie istotności 0,05 można stwierdzić, że odchylenie standardowe
# wagi ptaków badanego gatunku wynosi 0,20 kg?
  
# H0 sigma = 0.2
# H1 sigma != 0.2
#sigma.test(dane, sigmasq=0.04)
wynik = sigma.test(dane, sigma=0.2)$p.value #p-value = 0.204

cat('\ni) p-value =', wynik, '> 0.05 => nie ma podstaw do odrzucenia H0')


