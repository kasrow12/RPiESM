# Aby zaliczyć programowanie Maciek musi napisać program generujący liczby
# losowe z rozkładu dwumianowego o parametrach 3 i 0,5. Co więcej, Maciek musi
# wykazać, że jego program pracuje prawidłowo. W tym celu nasz bohater
# wygenerował 200 liczb i otrzymał następujące wyniki:

# Wygenerowana liczba losowa 0 1 2 3
# Liczba uzyskanych wyników 24 73 77 26

# Czy na poziomie istotności 0,05 można stwierdzić, że generator Maćka działa
# prawidłowo?

# H0: próba pochodzi z binom(n=3, p=0.5)
# H1: ~H0

dane = c(24,73,77,26)
prob = dbinom(0:3, 3, 0.5)

wynik = chisq.test(dane, p=prob)$p.value
cat('pvalue =', wynik, '> 0.05 => nie ma podstaw do odrzucenia H0')
