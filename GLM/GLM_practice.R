#####
##### Logistic regression
#####

#Data exercise: seed dormancy 
setwd('./GLM')
library(curl)
download.file("https://raw.githubusercontent.com/MartonHorvath98/BIOS14_QuantitativeAnalysis/main/Lecture%20notes/datasets/dormancy/dormancy.csv", 
              destfile = 'germination.csv',
              method = "curl")

#Functions for exercises
logit <- function(x) log(x/(1-x))
invlogit <- function(x) 1/(1+exp(-x))

germination <- read.csv(file = 'germination.csv', stringsAsFactors = T)
summary(germination)
names(germination)

germinationCC <- germination[which(germination$pop == 'CC'),]

germ <- germinationCC$germ2 * germinationCC$nseed # successes
nogerm <- germinationCC$nseed - germ

mod1 <- glm(cbind(germ, nogerm) ~ timetosowing, data = germinationCC, 
            family = 'binomial')
mod2 <- glm(germ2 ~ timetosowing, data = germinationCC, 
            weights = nseed, family = 'binomial')

logLik(mod1) == logLik(mod2)

#model to ask how the propability of germination depends on the duration of 
#after-ripening, and whether this further depends on the seed size. 
mod3 <- glm(germ2 ~ timetosowing + MCseed, data = germinationCC,
            weights = nseed, family = 'binomial') 
# model includes both after-ripening and seed size as predictors
summary(mod3)

#summary of the model suggests detectable effects, plot of the results
par(pty = 's')
plot(x = germinationCC$timetosowing, y = germinationCC$germ2,
     xlab = 'Duration of after-ripening (days)',
     ylab = 'Germination rate', las = 1)
xvals = seq(min(germinationCC$timetosowing, na.rm = T),
            max(germinationCC$timetosowing, na.rm = T), 0.01)

coefs = summary(mod3)$coef
y_hat = coefs[1,1] + coefs[2,1] * xvals

lines(xvals, invlogit(y_hat), lwd = 2)

y_hat2 = coefs[1,1] + coefs[2,1] * xvals + coefs[3,1] * sd(germinationCC$MCseed)
lines(xvals, invlogit(y_hat2), lty = 2)

y_hat3 = coefs[1,1] + coefs[2,1] * xvals - coefs[3,1] * sd(germinationCC$MCseed)
lines(xvals, invlogit(y_hat3), lty = 2)

#calculate the duration of after-ripening for 50% seed germination?
-coefs[1,1]/coefs[2,1]
## [1] 106.7274

#how does it change for seeds that are one standard deviation heavier than the 
#mean?
-(coefs[1,1] + coefs[3,1]*sd(germinationCC$MCseed))/coefs[2,1]
## [1] 129.424

#...and for seeds that are one standard deviation smaller?
-(coefs[1,1] - coefs[3,1]*sd(germinationCC$MCseed))/coefs[2,1]
## [1] 84.03079


#####
##### Poisson and negative-binomial regression
#####
n <- rpois(n = 200, lambda = 3)
hist(n, las = 1, main = 'Histogram of data with Poission distribution',
     sub = 'n = 200, lambda = 3',
     xlab = expression(paste(p(n),'=',frac(lambda^n%*%e^(-lambda),'n!'))))

# for poisson distribution the lambda parameter determines both the mean and the
# variance E(X) = V(X) = λ, thus the data has density: the variance increases 
# linearly with the mean
x <- seq(0,20,1)
par(pty = 's')
plot(x, dpois(x, lambda = 1), type = 'b', las = 1, xlab = 'k', ylab = 'P(x=k)',
     pch = 16, col = 1)
points(x, dpois(x, lambda = 3), type = 'b', pch = 16, col = 2)
points(x, dpois(x, lambda = 10), type = 'b', pch = 16, col = 3)
legend('topright', col=1:3, pch = 16,
       legend=c(expression(paste(lambda, ' = 1')),
                expression(paste(lambda, ' = 3')),
                expression(paste(lambda, ' = 10'))))

# for count data log-transformation can be used to normalize the data, then using 
# a GLM model framework with poisson distributed errors (using log link function)
set.seed(42)
x <- rnorm(200, 10, 3) 
eta <- -2 + 0.2*x
y <- ceiling(exp(eta + rpois(200, 0.3)))

par(mfrow=c(1,2), pty = 's')
plot(x, eta, las = 1)
plot(x, y, las = 1)

m <- glm(y ~ x, family = 'poisson') 
summary(m)

par(pty = 's', mfrow=c(1,1))
plot(x, y, las = 1, col = 'darkgrey',
     pch = 16)
xx = seq(min(x), max(x), 0.01)
y_hat = predict(m, newdata = list(x = xx),
                type = 'response', se.fit = T)

lines(xx, y_hat$fit)
polygon(c(xx, rev(xx)), 
        c(y_hat$fit + 1.96*y_hat$se.fit,
          rev(y_hat$fit - 1.96*y_hat$se.fit)),
        col=rgb(0,1,0.5, alpha = 0.5), border=F)

library(MuMIn)
r.squaredGLMM(m)
1-(m$deviance/m$null.deviance)

#####
##### Data with overdispersion
#####
rm(list = ls())
set.seed(1)
x = rnorm(n = 200, mean = 10, sd = 3)
eta = -2 + 0.2*x
y = floor(exp(eta + rnbinom(200, 1, mu = .8)))

#exploratory plots
par(mfrow=c(1,2))
plot(x, eta, las = 1)
plot(x, y, las = 1)

#fit generalized liner model
m <- glm(y~x, family = 'poisson')
summary(m)
#Residual deviance: >8000, degrees of freedom: 198 - overdispersed data!!!

#correction of overdispersion
library(MASS)
m <- glm.nb(y ~ x)
summary(m)
