# W kolumnie WeightInitial w pliku goats.txt zapisano wagę (w kg) losowo
# wybranych młodych kóz hodowanych w Australii. Wiadomo, że rozkład badanej
# cechy jest normalny.
dane = goats$WeightInitial

# a) Utworzyć 95% przedział ufności dla wartości oczekiwanej wagi
# młodych kóz hodowanych w Australii
cat('a) [', t.test(dane, conf.level=0.95)$conf.int, ']')


# b) Na poziomie istotności 0,05 przetestować hipotezę, że średnia waga młodych
# kóz hodowanych w Australii przekracza 23 kg.

# Note: 0 zawsze mniejsze niż alfa, jak R wypluje p-value = 0, odrzucamy?

# alfa = 0.05
# H0: mi = 23
# H1: mi > 23 // większe -> pole na prawo od punktu t

test = t.test(dane, mu=23, alt='greater')

# można też                 \/ n-1
# p-value = 1-pt(t, length(dane)-1) = pt(t, n-1, lower.tail=F)
cat('\nb) p-value =', test$p.value, '> 0.05 => nie ma podstaw do odrzucenia H0,',
  't =', test$statistic) 


# c) Zakładając, że rzeczywista średnia waga młodych kóz hodowanych
# w Australii wynosi 24 kg, wyznaczyć prawdopodobieństwo, że przeprowadzając
# test na poziomie istotności 0,05 i na podstawie 40 obserwacji, błędnie uznamy,
# że średnia waga takich kóz nie przekracza 23 kg.

# P(nie odrz. H0 | H1 prawdziwe, mi = 24) = P(błąd II rodzaju) = 1 - moc()
wynik = 1 - power.t.test(n=40, delta=24-23, sd=sd(dane), type="one.sample",
                     alternative="one.sided", sig.level=0.05)$power
cat('\nc)', wynik)


# d) Załóżmy, że rzeczywista średnia waga młodych kóz hodowanych w Australii
# wynosi 24 kg. Ile trzeba by zebrać pomiarów wag takich kóz, by test
# (przeprowadzony na poziomie istotności 0,05) wykrywał, z prawdopodobieństwem
# nie mniejszym niż 0,8, że średnia waga takich kóz przekracza 23 kg?

wynik = power.t.test(power=0.8, delta=24-23, sd=sd(dane), type="one.sample",
                         alternative="one.sided", sig.level=0.05)$n
cat('\nd) n =', wynik, 'czyli', ceiling(wynik), 'kóz')


# e) Utworzyć 90% przedział ufności dla wariancji wagi młodych kóz
# hodowanych w Australii.
library(TeachingDemos)
wynik = sigma.test(dane, conf.level=0.9)$conf
cat('\ne) [', wynik, ']')


# f) Czy można przyjąć, że wariancja wagi młodych kóz hodowanych w Australii
# wynosi 20 kg2? Zweryfikować odpowiednią hipotezę na poziomie istotności 0,1.

# H0: sigmasq = 20 = sigma_0^2
# H1: sigmasq != 20
wynik = sigma.test(dane, sigmasq=20)$p.value
cat('\nf) p-value =', wynik, '< alfa=0.1 => odrzucamy H0')


# g) Na poziomie istotności 0,1 zweryfikować hipotezę, że odchylenie standardowe
# wagi młodych kóz hodowanych w Australii przekracza 3 kg.

# H0: sigma0 = 3
# H1: sigma > 3
# alfa = 0.1
wynik = sigma.test(dane, sigma=3, alt='greater')$p.value
cat('\ng) p-value =', wynik, '< alfa=0.1 => odrzucamy H0')

