# 8. Wybrać µ ∈ R, σ > 0. Wygenerować N = 50 prób n-elementowych (n = 10) z rozkładu N(µ, σ)
# i dla każdej z nich utworzyć przedział ufności dla µ na poziomie ufności 0,95. Przedstawić na
# jednym wykresie uzyskane przedziały ufności. Ile z nich powinno zawierać µ?
  
N = 50
n = 10
mi = 0.3
sigma = 1.25

wyniki = replicate(N, {
  x = rnorm(n, mi, sigma)
  t.test(x, conf.level = 0.95)$conf
})
#dim(wyniki)

matplot(wyniki, rbind(1:N, 1:N), type='l', lty=1, col=c('black','red'),
        las=1, xlab='granice przedziału', ylab='nr przedziału')
abline(v=mi, col='green')
