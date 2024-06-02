# Losową próbę studentów spytano o ich ulubiony przedmiot. Otrzymano następujące
# odpowiedzi:
#                                     Przedmiot Fizyka WF Mechanika Statystyka
# Liczba studentów, którzy najbardziej lubią ten przedmiot 380 340 380 500

# Na poziomie istotności 0,05 sprawdzić hipotezę że rozkład preferencji jest
# równomierny.


dane = c(380, 340, 380, 500)
n = sum(dane)

# H0: p1 = p2 = p3 = p4 = 1/4 (rozkład jest równomierny)
# H1: ~H0

# Założenie: npi >= 10, czyli 1600*1/4 >= 10 => git, więc dobre

wynik = chisq.test(dane)$p.value
cat('pvalue =', wynik, '< 0.05 => odrzucamy H0 (rozkład nie jest równomierny)')




