#install.packages("mosaicCalc")
library(mosaicCalc)

#########
# 5/a.) linearize f(x)=3*x^2-5 at x = 2
f1 <- slice_plot(3 * x^2 - 5 ~ x, domain(x = range(-5, 5)))$data
t1 <- slice_plot(12*x - 17 ~ x, domain(x = range(1, 5)))$data

png(filename = "results_5a.png", width = 800, height = 800,
    units = 'px', res = 120)
par(pty = "s")
plot(f1, type = 'l', 
     xlab = "x", ylab = "f(x)",
     main = expression(paste("Linear approximation of f(x)=",3%*%x^2-5,
                  " at x=2")))
lines(t1, type = 'l', 
      lwd = 2, col = 'red')
dev.off()


#########
# 5/b.) linearize f(x)=exp(2x) at x = 0
f2 <- slice_plot(exp(2*x) ~ x, domain(x = range(-1, 1)))$data
t2 <- slice_plot(2*x + 1 ~ x, domain(x = range(-1, 1)))$data

png(filename = "results_5b.png", width = 800, height = 800,
    units = 'px', res = 120)
par(pty = "s")
plot(f2, type = 'l', 
     xlab = "x", ylab = "f(x)",
     main = expression(paste("Linear approximation of f(x)=",e^(2%*%x),
                             " at x=0")))
lines(t2, type = 'l', 
      lwd = 2, col = 'red')
dev.off()


#########
# 5/c.) linearize f(x)=ln(x + 5) at x = -1
f3 <- slice_plot(log(x + 5) ~ x, domain(x = range(-5, 5)))$data
t3 <- slice_plot(1/4*x + 1/4 + log(4) ~ x, domain(x = range(-5, 5)))$data

png(filename = "results_5c.png", width = 800, height = 800,
    units = 'px', res = 120)
par(pty = "s")
plot(f3, type = 'l', 
     xlab = "x", ylab = "f(x)",
     main = expression(paste("Linear approximation of f(x)=",log(x+5),
                             " at x=-1")))
lines(t3, lwd = 2, col = 'red')
dev.off()
#########
# 5/d.) linearize f(x)=sin(-x) - 2x at x = 0
f4 <- slice_plot(sin(-x) - 2*x ~ x, domain(x = range(-5, 5)))$data
t4 <- slice_plot(-3*x ~ x, domain(x = range(-5, 5)))$data

png(filename = "results_5d.png", width = 800, height = 800,
    units = 'px', res = 120)
par(pty = "s")
plot(f4, type = 'l', 
     xlab = "x", ylab = "f(x)",
     main = expression(paste("Linear approximation of f(x)=",sin(-x)-2*x,
                             " at x=0")))
lines(t4, lwd = 2, col = 'red')
dev.off()