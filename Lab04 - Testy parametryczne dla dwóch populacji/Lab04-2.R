# Pomiary dokonane niezależnie na próbach losowych dwóch gatunków papierosów
# dały następujące wyniki zawartości nikotyny (w miligramach):

daneA = c(26.4, 22.5, 24.9, 23.7, 21.5)
daneB = c(25.1, 29.0, 23.4, 27.6, 22.3)

# Przyjmujemy, że, w przypadku obu badanych gatunków papierosów, zawartość
# nikotyny ma rozkład normalny.


# a) Na poziomie istotności 0,05 zweryfikować hipotezę, że gatunek B ma wyższą
# zawartość nikotyny niż gatunek A.

# Model II?
# Co z wariancjami?
# H0: sigma_A^2 = sgima_B^2
# H1: sigma_A^2 != sgima_B^2

wynik = var.test(daneA, daneB)
# pvalue = 0.489 > alfa^falka = 0.1 // z wykładu
#cat('a) p-value =', wynik$p.value, '> 0.1 -> nie ma podstaw do odrzucenia H0')

# test dla mi
# H0: mi_A = mi_B
# H1: mi_A < mi_B
wynik = t.test(daneA, daneB, alt="less", var.equal=T) 
cat('a) p-value =', wynik$p.value, '> 0.05 -> nie ma podstaw do odrzucenia H0,',
  'czyli nie ma podstaw twierdzić, że gatunek B ma wyższą zawartość nikotyny',
  'niż gatunek A')
# df - liczba stopni swobody = n1+n2-2 = 8
# jak var.equal=F to jest inny test - Welch


# b) Zakładając, że gatunek B ma zawartość nikotyny średnio o 2 miligramy
# większą niż gatunek A, obliczyć prawdopodobieństwo, że test z pkt. a) da
# błędną odpowiedź.

# ogólnie: s^2^falka = ((n1 - 1)s1^2 + (n2 - 1)s2^2) / (n1 + n2 - 2)
# ale n1=n2 czyli
# s_1 = sqrt(  (var(x) + var(y)) / 2 )

wynik = 1 - power.t.test(n=5, delta=2,
                     alt="one.sided", sig.level=0.05,
                     sd=sqrt((var(daneA) + var(daneB))/2),
                     type="two.sample")$power
cat('\nb) wynik =', wynik)


# c) Załóżmy, że gatunek B ma zawartość nikotyny średnio o 2 miligramy większą
# niż gatunek A. Jak liczne próby losowe tych gatunków paperosów trzeba by
# pobrać, by na ich podstawie, test z pkt. a), z prawdopodobieństwem nie
# mniejszym niż 0,75, dawał poprawną odpowiedź?
wynik = power.t.test(delta=2, alt="one.sided", sig.level=0.05,
             sd=sqrt((var(daneA) + var(daneB))/2),
             type="two.sample", power=0.75)$n
cat('\nc) n =', ceiling(wynik))



