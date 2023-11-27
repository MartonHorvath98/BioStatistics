#####
##### Bee distribution
#####

#Build a GLM modell to explain the distribution patterns of the bee (Eulaema nigrita)
setwd('C:/Users/Marton Horvath/Documents/Lund/BIOS14/BioStatistics/GLM')
library(curl)
download.file("https://raw.githubusercontent.com/MartonHorvath98/BIOS14_QuantitativeAnalysis/main/Lecture%20notes/datasets/Eulaema.csv", 
              destfile = 'eulaema.csv',
              method = "curl")
rm(list = ls())

#data file info
file_info <- file("eulaema.csv", open = "rt")
encoding <- Encoding(readLines(file_info, n = 1)) #encoding is unknown!!
close(file_info)


data <- read.csv('eulaema.csv', fileEncoding = 'latin1',
                 stringsAsFactors = T)
head(data)
summary(data)
names(data)
# "Eulaema_nigrita" - count of observed bees
# "SA" - data source
# "SU" - source region
# "method" - methods of catching the bee 
# "effort" - log hours of collecting
# "altitude" - altitude of source
# "MAT" - mean annual temperature
# "MAP" - mean annual precipitation
# "Tseason" - temperature per season
# "Pseason" - precipitation per season
# "forest." - forest coverage
# "lu_het" - lumination of the location

#Functions for exercises
logit <- function(x) log(x/(1-x))
invlogit <- function(x) 1/(1+exp(-x))


library(MASS)
library(MuMIn)
#GLM with negative binomial distribution (to account for over-dispersion)
m_glm <- glm(Eulaema_nigrita ~ MAP + forest., data = data, family = 'poisson')
summary(m_glm)
# Residual deviance = 17881; DOF = 175 - significant over-dispersion is present 
# in the data... binomial distribution is needed instead of poisson link function
m <- glm.nb(Eulaema_nigrita ~ MAP + forest., data = data)
summary(m)
# Theta: 0.7545, the dispersion parameter is very high suggesting strong over-
# dispersion in the original data set

# 1 - m_glm$deviance/m_glm$null.deviance

1 - m$deviance/m$null.deviance
# = 0.2216 (a tiny bit smaller than the original fit - comparable - but the standard
#           error is now correct)
# Null deviance: 271.10  on 177  degrees of freedom
# Residual deviance: 211.01  on 175  degrees of freedom
# (lost degrees of freedom are the slope and theta)

# generate dummy forest cover - uniformly distributed between the observed extremes
pred_forest = seq(min(data$forest.), max(data$forest.), length.out = 200)
# fixed value of precipitation: observed mean - observed standard variance
minSD_meanMAP = rep(mean(data$MAP, na.rm = T) - sd(data$MAP, na.rm = T),
                    length(pred_forest))
# fixed value of precipitation: observed mean
meanMAP = rep(mean(data$MAP, na.rm = T),
                    length(pred_forest))
# fixed value of precipitation: observed mean + observed standard variance
maxSD_meanMAP = rep(mean(data$MAP, na.rm = T) + sd(data$MAP, na.rm = T),
                    length(pred_forest))

# plot the prediction of bee abundance variation in response to changes rainfall
# with three different, fixed values of mean annual precipitation
plot(data$forest., data$Eulaema_nigrita, col = 'grey', las = 1,
     xlab = 'Forest cover (%)',
     ylab ='E. nigrita abundance')
# predict the response of E. nigrita
y_pred1 <- predict(m, newdata = list(MAP=minSD_meanMAP, forest.=pred_forest),
                   type = 'response',se.fit = T)
y_pred2 <- predict(m, newdata = list(MAP=meanMAP, forest.=pred_forest),
                   type = 'response',se.fit = T)
y_pred3 <- predict(m, newdata = list(MAP=maxSD_meanMAP, forest.=pred_forest),
                   type = 'response',se.fit = T)
# add the lines 
lines(pred_forest, y_pred1$fit, lwd = 2)
lines(pred_forest, y_pred2$fit, lwd = 2, col = 2)
lines(pred_forest, y_pred3$fit, lwd = 2, col = 3)
legend('topright', lty = 1, lwd = 2, col=1:3, bty = 'n',
       legend=c('MAP = mean - SD',
                'MAP = mean',
                'MAP = mean + SD'))

#####
##### Hurdle model
#####
# To deal with 0 observations we can split the model into two parts and combine 
# the resuls later
# hist(data$Eulaema_nigrita) - peaks at 0


y_binom <- ((data$Eulaema_nigrita > 0) * 1) # change the observations to a binomial 
                                          # data
m_binom <- glm(y_binom ~ forest., data = data, 
             family = 'binomial' (link = 'logit'))
summary(m_binom)

y_na <- data$Eulaema_nigrita
y_na[which(y_na == 0)] <- NA #replace all 0 values with NA
m_na <- glm(y_na ~ forest., data = data, 
             family = 'poisson', na = na.exclude)
summary(m_na)

coefs_binom <- summary(m_binom)$coef
y_hat1 <- coefs_binom[1,1] + coefs_binom[2,1]*data$forest.

coefs_na <- summary(m_na)$coef
y_hat2 <- coefs_na[1,1] + coefs_na[2,1]*data$forest.

y_pred <- invlogit(y_hat1)*exp(y_hat2)

par(mfrow = c(1,3))
plot(data$forest., invlogit(y_hat1),
     xlab = 'Forest coverage (%)')
plot(data$forest., exp(y_hat2), las = 1,
     xlab = 'Forest coverage (%)')
plot(data$forest., y_pred, las = 1,
     xlab = 'Forest coverage (%)')
