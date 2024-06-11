# W losowej próbie 233 dorosłych mieszkańców Warszawy znalazło się 40 takich,
# które regularnie robią zakupy w sklepach sieci Żuczek. W Krakowie na
# 220 zapytane osoby, 31 okazało się klientami Żuczka.

# H0: p_k = p_w
# H1: p_k < p_w

# a) Czy na podstawie powyższych danych można stwierdzić, że odsetek regularnych
# klientów Żuczka w Warszawie jest większy niż w Krakowie? Przyjąć poziom
# istotności α = 0,05.
# kolejność w wektorze jak wyżej
wynik = prop.test(x=c(31, 40), n=c(220, 233), alt='less', conf.level=0.95)$p.value
cat('a) p-value =', wynik, '> 0.05 -> nie ma podstaw do odrzucenia H0')


# b) Przypuszczamy, że odsetek regularnych klientów Żuczka w Warszawie
# wynosi 17%, a w Krakowie - 14%.
#   (i) Jakie jest prawdopodobieństwo, że test z pkt. a) potwierdzi, że odsetek
#       regularnych klientów Żuczka jest większy w Warszawie niż w Krakowie?

# p_k = p1
# p_w = p2
wynik = power.prop.test(n=c(220, 233), p1=0.14, p2=0.17, alt='one.sided')$power
cat('\nb) (i) power € [', wynik, ']')


#   (ii) Ilu mieszkańców Warszawy i ilu mieszkańców Krakowa trzeba by wylosować
#       do próby by, z prawdopodobieństwem nie mniejszym niż 0,8, jednostronny
#       test o poziomie istotności 0,05 porównujący odsetek regularnych klientów
#       Żuczka potwierdził, że odsetek ten jest większy w Warszawie niż w Krakowie?
wynik = power.prop.test(power=0.8, p1=0.14, p2=0.17, alt='one.sided')$n
cat('\n   (ii) n =', ceiling(wynik))

  

