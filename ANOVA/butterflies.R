#####
##### 1.) set up the working environment
#####
library(here)
setwd(here("ANOVA")) #set working directory

#create a data folder
data_dir <- "resources"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}

#create a folder for figures
plots_dir <- "figures"
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

#####
##### 2.) Download the resource file and load it into the environment 
#####

# For some reason the file cannot be downloaded directly from github, seems 
# corrupted! 
# librare(curl)
# download.file("https://github.com/oysteiop/BIOS14_QuantitativeAnalysis/blob/f58d993bd24e8b315e78a7371b7101900d55ea98/Lecture%20notes/datasets/butterflies.csv", 
#               destfile = paste(data_dir,'butterflies.csv',sep = '/'),
#               method = "curl")

# Had to download it manually! :( 
data <- read.csv(file = paste(data_dir,'butterflies.csv',sep = '/'),
                 stringsAsFactors = T) 
#####
##### 3.) exploratory plotting of data
#####
library(ggplot2)
library(gridExtra)
library(dplyr)
library(e1071)
library(car)
library(cowplot)
library(stats)
library(scales)
library(report)
source("functions.R")
 

#histogram of development time
(hist1 <- create_hist(data = data,var = "DevelopmentTime",
                      title = "Development time", "days"))
#histogram of adult weight
(hist2 <- create_hist(data = data,var = "AdultWeight",
                      title = "Adult weight", "mg"))
#histogram of growth rate
(hist3 <- create_hist(data = data,var = "GrowthRate",
                      title = "Growth rate", "mg/day"))
#compound figure of the 3 histograms
fig1 <- plot_grid(hist1, hist2, hist3, 
                  align = 'hv', nrow = 1, ncol = 3)
#save figure 1
ggsave(paste(plots_dir,"Fig1_data_distribution.png",sep = '/'),
       fig1, width = 16, height = 8, units = 'in',
       dpi = 300, bg = "white")

# Test data normality: normally distributed residuals are necessary for ANOVA
# analysis. The histograms of the data already predict skewed distribution for
# growth rate and development time, hence I use Q-Q plot and statistical ana-
# lysis to confirm the data's eligibility for further analysis.

# Q-Q plots
#create linear regression on development time
lm1 <- lm(DevelopmentTime ~ LarvalHost * MaternalHost, 
          data, na.action = na.exclude)

# run Shapiro-Wilk normality test
norm1 <- shapiro.test(rstandard(lm1)) #failed!!
# run Levene-test to check the homogenity of the variances across groups
var1 <- leveneTest(lm1) #failed!!

#create Q-Q plot
qq1 <- make_QQplot(lm1, norm1$p.value, "Development Time")
table1 <- make_table(lm1)
qq1 <- grid.arrange(qq1,table1, heights=c(0.7,0.3), nrow=2)

#create linear regression on adult weight
lm2 <- lm(AdultWeight ~ LarvalHost * MaternalHost,
          data, na.action = na.exclude)

#run Shapiro-Wilk normality test
norm2 <- shapiro.test(rstandard(lm2)) # passed!!
# run Levene-test 
var2 <- leveneTest(lm2) # passed!!

#create Q-Q plot
qq2 <- make_QQplot(lm2, norm2$p.value, "Adult Weight")
table2 <- make_table(lm2)
qq2 <- grid.arrange(qq2,table2,heights=c(0.7,0.3), nrow=2)

#create linear regression on growth rate
lm3 <- lm(GrowthRate ~ LarvalHost * MaternalHost,
          data, na.action = na.exclude)

#run Shapiro-Wilk normality test
norm3 <- shapiro.test(rstandard(lm3)) # failed!!
#run Levene-test
var3 <- leveneTest(lm3) # failed!!

#create Q-Q plot
qq3 <- make_QQplot(lm3, norm3$p.value, "Growth Rate")
table3 <- make_table(lm3)
qq3 <- grid.arrange(qq3,table3,heights=c(0.7,0.3), nrow=2)

#compound figure of the 3 qq plot
fig2 <- grid.arrange(qq1, qq2, qq3, nrow = 1, ncol = 3, 
                     name = 'Normality test')
#save figure 2
ggsave(paste(plots_dir, "Fig2_normality_test.png",sep = '/'),
       fig2, width = 16, height = 8, units = 'in', 
       dpi = 300, bg = "white")

#####
##### 3.) ANOVA analysis
#####

#violin plot of data
grouped_data <- data %>% 
  group_by(LarvalHost,MaternalHost)

my_comparisons <- list(c("Barbarea", "Berteroa"))


fig3 <- make_violin(data = grouped_data,
                    x = "MaternalHost", y = "AdultWeight",
                    group = "LarvalHost", comparisons = my_comparisons)
#save figure 3
ggsave(paste(plots_dir, "Fig3_adult_weight_violin.png",sep = '/'),
       fig3, width = 16, height = 8, units = 'in', 
       dpi = 300, bg = "white")

# run ANOVA analysis
adult_weight_anova <- aov(AdultWeight ~ MaternalHost*LarvalHost, data = data)
# run post hoc test on anova
posthoc <- TukeyHSD(adult_weight_anova)
posthoc <- do.call(rbind, posthoc) %>%
  ftable(.)

#create a report
report <- report(adult_weight_anova)
# The ANOVA (formula: AdultWeight ~ MaternalHost * LarvalHost) suggests that:
# - The main effect of MaternalHost is statistically not significant and very 
#   small (F(1, 283) = 0.82, p = 0.366, Eta2 (partial) = 2.89e-03)
# - The main effect of LarvalHost is statistically significant and large 
#   (F(1, 283) = 144.89, p < .001, Eta2 (partial) = 0.34)
# - The interaction between MaternalHost and LarvalHost is statistically not 
#   significant and small (F(1, 283) = 3.75, p = 0.054, Eta2 (partial) = 0.01)

# Effect sizes were labelled following Field's (2013) recommendations.

R2_larvalHost <- summary(lm(AdultWeight ~ LarvalHost, data = data))$r.squared  
R2_total <- summary(lm2)$r.squared 

ETA_LarvalHost <- round(R2_larvalHost/R2_total*100,2)

#####
##### 4.) Create markdown report
#####
input_markdown <- "report.Rmd"
output_file <- "report.html"

rmarkdown::render(
  input = input_markdown,
  output_file = output_file,
  envir = parent.frame()
)


