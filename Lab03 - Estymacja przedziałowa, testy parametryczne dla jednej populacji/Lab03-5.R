# a) Napisać funkcję, która dla dużej próby losowej (n >= 100) z dowolnego rozkładu, zwraca
# przedział ufności dla średniej na zadanym poziomie ufności. Zadbać by funkcja zwracała
# błąd w przypadku jej użycia do próby o liczności mniejszej niż 100.

lsfv <- function(x, conf.level = 0.95) {
  if(class(x) != "numeric" && class(x) != "integer"){
    stop("x is not numeric nor an integer!")
  }
  if(class(conf.level) != "numeric" && class(conf.level) != "integer"){
    stop("Conf.level is not numeric no ran integer!")
  }
  if(length(x) < 100){
    warning("Sample might be too small!")
  }
  if(conf.level < 0 || conf.level > 1){
    stop("Conf.level must be in range (0, 1)")
  }
  
  # wyznaczamy alpha z poziomu ufności
  alpha = 1 - conf.level
  
  # stosujemy wzór dla modelu III z tablicy wzorów
  temp1 = mean(x)
  S = sqrt(sum((x - temp1)^2)/(length(x)-1))
  temp2 = qnorm(1 - alpha/2) * S / sqrt(length(x))
  
  # wyznaczamy górną i dolną granicę przedziału ufności
  lo = temp1 - temp2
  hi = temp1 + temp2
  
  return(c(lo, hi))
  
  # zamiast oddzielnie obliczać lo i hi, wystarczyłoby napisać
  # return(temp1 + c(-1, 1)*temp2)
}


# b) W pakiecie MASS znajduje się zbiór danych geyser zawierający kolumnę duration
# z czasami trwania (w min) wybuchów gejzeru Old Faithful w Parku Narodowym Yellowstone
# w # USA. Na poziomie ufności 0,95 wyznaczyć przedział ufności dla średniego czasu trwania
# wybuchu tego gejzera.

library(MASS)

dane = MASS::geyser$duration

przedz = lsfv(dane)
cat(sprintf(
"
Przedział ufności 0.95 dla czasu trwania
wybuchu gejzeru Old Faithful w minutach:
(%f, %f)

", przedz[1], przedz[2]))