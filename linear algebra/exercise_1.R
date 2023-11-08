#1.) Exercise - rats and owls
t0 <- matrix(c(2000, 1000), 2, 1) # initial number of owls and rats

# relation defining change in owl and rat numbers 
R <- matrix(c(0.5, 0.3, -0.2, 1.2), 2, 2, byrow = T) 
# number of owls and rats in the next year
t1 <- R %*% t0
# calculate population changes for the next 10 years
populations <- t0
for (i in 1:10){
  populations <- cbind(populations, 
                       R %*% populations[,ncol(populations)])
  
}
#set row and column names
populations <- cbind(t(populations), as.matrix(0:10))
dimnames(populations) <-  list(c(as.character(1:11)),
                               c('owls','rats', 'year'))

plot(populations[,"year"], populations[,"rats"],
     type = "l", col = "green", lwd = 2, las = 1,
     ylab = "Population size", xlab = "Year",
     ylim = c(0, max(populations)),
     main = stringr::str_wrap("Spotted owl and wood rat populations 
                              in the California redwood forest", 40))
lines(populations[,"year"], populations[,"owls"],
      col = "red", lwd = 2,)
legend('topright',legend=c('rats','owls'),
       lty='solid', col=c('green','red'))
