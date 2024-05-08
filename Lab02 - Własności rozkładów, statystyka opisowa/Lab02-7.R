# 7. Wygenerować 10000 prób 10-elementowych z rozkładu normalnego. Następnie zakładając, iż o
# próbach wiemy tylko tyle, że pochodzą one z rozkładu normalnego o nieznanych parametrach,
# wyznaczyć dla każdej próby przedział ufności dla wartości oczekiwanej na poziomie ufności 0,95.
# Porównać frakcję pokryć przez przedział ufności faktycznej wartości oczekiwanej z założonym
# poziomem ufności.

N = 10000
n = 10
mi = 0.6
sigma = 1.5

# liczba pokryć
ile_wpada = replicate(N, {
  x = rnorm(n, mi, sigma)
  przedz = t.test(x, conf.level=0.95)$conf
  mi >= przedz[1] & mi <= przedz[2]
})

#ile_wpada[1:10]
print(sum(ile_wpada)/N)



