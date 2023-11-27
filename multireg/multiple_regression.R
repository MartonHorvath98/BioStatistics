# clear global environment
rm(list = ls())

# set working directory
setwd('./multireg')
library(curl)
download.file("https://raw.githubusercontent.com/oysteiop/BIOS14_QuantitativeAnalysis/main/Lecture%20notes/datasets/alpineplants.csv", 
             destfile = 'alpineplants.csv',
             method = "curl")

plants <- read.csv(file = 'alpineplants.csv')

# exploration
#1. mean winter temp
plot(plants$mean_T_winter, plants$Carex.bigelowii, 
     ylab = 'Occurences', xlab = 'Mean winter temperature (°C)')
points(plants$mean_T_winter, plants$Thalictrum.alpinum, col = 'red')
legend('topleft',legend=c('Carex bigelowii','Thalictrum alpinum'),
       pch=21, col=c('black','red'))

#2. mean summer temp
plot(plants$mean_T_summer, plants$Carex.bigelowii, 
     ylab = 'Occurences', xlab = 'Mean summer temperature (°C)')
points(plants$mean_T_summer, plants$Thalictrum.alpinum, col = 'red')
legend('topleft',legend=c('Carex bigelowii','Thalictrum alpinum'),
       pch=21, col=c('black','red'))


#3. amount of light
plot(plants$light, plants$Carex.bigelowii, 
     ylab = 'Occurences', xlab = 'Light (lx)')
points(plants$light, plants$Thalictrum.alpinum, col = 'red')
legend('topright',legend=c('Carex bigelowii','Thalictrum alpinum'),
       pch=21, col=c('black','red'))


#4. Snowfall
plot(plants$snow, plants$Carex.bigelowii, 
     ylab = 'Occurences', xlab = 'Average snowfall (cm)')
points(plants$snow, plants$Thalictrum.alpinum, col = 'red')
legend('topright',legend=c('Carex bigelowii','Thalictrum alpinum'),
       pch=21, col=c('black','red'))

#5. Soil moisture
plot(plants$soil_moist, plants$Carex.bigelowii, 
     ylab = 'Occurences', xlab = 'Soil moisture (%)')
points(plants$soil_moist, plants$Thalictrum.alpinum, col = 'red')
legend('topright',legend=c('Carex bigelowii','Thalictrum alpinum'),
       pch=21, col=c('black','red'))

#6. Altitude
plot(plants$altitude, plants$Carex.bigelowii, 
     ylab = 'Occurences', xlab = 'Altitude (m)')
points(plants$altitude, plants$Thalictrum.alpinum, col = 'red')
legend('topleft',legend=c('Carex bigelowii','Thalictrum alpinum'),
       pch=21, col=c('black','red'))

# 2.) fit multiple linear regression
library(dplyr)
corr_plants <- plants %>% 
  select(!Carex.bigelowii:Thalictrum.alpinum) %>%
  filter(complete.cases(.)) %>%
  cor(.)

# Carex.bigelowii
lm1 <- lm(Carex.bigelowii ~ .,
          data = plants[,-2])
summary(lm1)

lm1_corr <- lm(Carex.bigelowii ~ mean_T_summer + altitude,
               data = plants)
summary(lm1_corr)

# Thalictrum.alpinum
lm2 <- lm(Thalictrum.alpinum ~ .,
          data = plants[,-1])
summary(lm2)

lm2_corr <- lm(Thalictrum.alpinum ~ mean_T_summer + snow,
          data = plants[,-1])
summary(lm2_corr)
