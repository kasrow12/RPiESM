# W katalogu bazowym base znajduje się plik InsectSprays a w nim następujące
# zmienne:
# count - liczba insektów w wydzielonych obszarach eksperymentalnych,
# spray - rodzaj stosowanego w danym obszarze środka owadobójczego.


# a) Chcemy zweryfikować czy przeżywalność insektów zależy od rodzaju stosowanego
# środka owadobójczego. Jakie postawimy hipotezy do testowania? Wykonać wykres
# średnich w grupach by wstępnie ocenić sytuację.

# H0: mi_A = mi_B = mi_C = mi_D = mi_E = mi_F
# H1: !H0


# b) Sprawdzić czy są spełnione założenia analizy wariancji.

bartlett.test(InsectSprays$count, InsectSprays$spray)$p.value

leveneTest(InsectSprays$count~InsectSprays$spray, center=mean)$Pr[1]


# c) Jeśli założenia nie są spełnione, to zaproponować odpowiednie przekształcenie
# zmiennej count, tak by w nowym modelu przynajmniej w przybliżeniu założenia
# analizy wariancji były spełnione. WSKAZÓWKA: Zalecane przekształcenie to
# pierwiastkowanie, bo zmienna count jest typu zliczającego.

bartlett.test(sqrt(InsectSprays$count), InsectSprays$spray)$p.value
leveneTest(sqrt(InsectSprays$count)~InsectSprays$spray, center=mean)$Pr[1]


# d) Na podstawie nowego modelu stwierdzić czy liczba insektów zależy od rodzaju
# stosowanego środka owadobójczego (opisać dokładnie używany model). Jeśli zależy,
# to przeprowadzić testy porównań wielokrotnych.

model = lm(sqrt(InsectSprays$count)~InsectSprays$spray)
TukeyHSD(aov(model))
