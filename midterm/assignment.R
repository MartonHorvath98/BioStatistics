#####
##### 1.) set up the working directory
#####
setwd('C:/Users/Marton Horvath/Documents/Lund/BIOS14/BioStatistics/midterm')
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

file_path <- paste(data_dir,'tephritis.csv',sep = '/')
if(!file.exists(file_path)){
  download.file("https://raw.githubusercontent.com/MartonHorvath98/BIOS14_QuantitativeAnalysis/main/Lecture%20notes/datasets/tephritis/tephritis.txt", 
                destfile = file_path,
                method = "curl")
}

#####
##### Input data
#####
fly_data <- read.csv(file_path, sep = '\t',
                     stringsAsFactors = T)
summary(fly_data)
# Patry             Hostplant       Sex            BL              OL      
# Allopatry:301   Heterophyllum:297   Female:295   Min.   :3.430   Min.   :1.32  
# Sympatry :282   Oleraceum    :286   Male  :288   1st Qu.:4.240   1st Qu.:1.61  
#                                                  Median :4.490   Median :1.71  
#                                                  Mean   :4.467   Mean   :1.71  
#                                                  3rd Qu.:4.700   3rd Qu.:1.82  
#                                                  Max.   :5.350   Max.   :2.15 
#                                                  NA's   :298
# 
# Wing_length      Wing_width      Wing_area      Melanized_area  Melanization_ratio
# Min.   :3.739   Min.   :1.690   Min.   : 5.293   Min.   :2.834   Min.   :44.23     
# 1st Qu.:4.553   1st Qu.:2.083   1st Qu.: 7.657   1st Qu.:4.339   1st Qu.:55.10     
# Median :4.781   Median :2.192   Median : 8.386   Median :4.966   Median :59.38     
# Mean   :4.771   Mean   :2.185   Mean   : 8.387   Mean   :5.007   Mean   :59.59     
#  3rd Qu.:4.980   3rd Qu.:2.293   3rd Qu.: 9.096   3rd Qu.:5.627   3rd Qu.:64.23     
#  Max.   :6.037   Max.   :2.859   Max.   :13.436   Max.   :8.687   Max.   :74.87     
#                                                                                     
# Baltic   
# East:311  
# West:272  
           
names(fly_data)
# Patry - whether individual is from a sympatric or allopatric population.
# Hostplant - Whether the individual is a C. heterophyllum specialist or a C. oleraceum
#             specialist.
# Sex - Individual sex.
# BL - Measurements of body length in millimeter.
# OL - Measurements of ovipositor length in millimeter.
# Wing_length - Measurements of wing length in millimeter.
# Wing_width - Measurements of wing width in millimeter.
# Wing_area - Wing length multiplied with wing width for an estimation of wing area.
# Melanized_area - Area of the wing which is melanised, measured with an automated script.
# Melanized_ratio - The ratio of dark and white area of the wing, measured with an automated
#                   script.
# Baltic - Whether the population of the individual is East or West of the Baltic sea

#####
##### 2.) exploratory analysis of the data
#####
source('./functions.R')
library(dplyr)
library(pheatmap)
library(factoextra)
library(ggrepel)
library(jtools)
library(report)

fly_matrix <- fly_data %>% 
  na.omit(.) %>% 
  dplyr::select(BL:Melanization_ratio) %>% 
  dplyr::mutate(relativ_OL = OL/BL)
fly_heatmap <- corr_heatmap(fly_matrix)


png(filename = paste(plots_dir, 'Fig1_heatmap.png',sep = '/'),
    width = 10, height = 8, units = 'in', res = 300)
fly_heatmap
dev.off()

################################################################################
library(ggplot2)
library(viridis)
library(ggpubr)
BL_plot <- make_boxplot(fly_data, predictors = 'population', group = 'groups', 
                        response = 'BL',y_label = 'Body length (mm)',
                        design = 'Sex ~ Baltic')

OL_data <- fly_data %>% 
  dplyr::filter(complete.cases(.)) %>% 
  droplevels(.)

OL_plot <- make_boxplot(OL_data, predictors = 'population', group = 'groups', 
                        response = 'OL',y_label = 'Ovipository length (mm)',
                        design = 'Sex ~ Baltic')

relOL_plot <- make_boxplot(OL_data %>% dplyr::mutate(relativ_OL = 100*relativ_OL), 
                           predictors = 'population', group = 'groups', 
                           response = 'relativ_OL',
                           y_label = 'Body-proportionate length of ovipository (%)',
                           design = 'Sex ~ Baltic')

library(cowplot)
fig1 <- plot_grid(BL_plot + 
                    ggtitle('Body length') + 
                    theme(legend.position = 'none'),
                  OL_plot + 
                    ggtitle('Ovipository length') +
                    theme(legend.position = 'none'),
                  relOL_plot + 
                    ggtitle('Ovipository body-proportionate length'),
                  ncol = 3, scale = .9, labels = 'AUTO')

# save the image file
ggsave(paste(plots_dir,"Fig1_exploratory.png",sep = '/'),
       fig1, width = 20, height = 10, units = 'in',
       dpi = 300, bg = "white")

################################################################################
fly_data <- fly_data %>%  
  dplyr::mutate( 
    groups = as.factor(paste(Patry, Hostplant)),
    population = as.factor(paste(Baltic, Patry, Hostplant)),
    relativ_OL = OL/BL) %>% 
  dplyr::relocate(relativ_OL, .before = groups)


OL_lm <- lm(OL ~ Hostplant*Patry,fly_data)
summary(OL_lm)

OL_anova <- aov(OL ~ Hostplant*Patry,fly_data)
OL_posthoc <- TukeyHSD(OL_anova) # run post hoc test on anova

(OL_posthoc <- do.call(rbind, OL_posthoc) %>%
  as.data.frame(.) %>% 
    dplyr::arrange(diff))

OL_lm_coef <- as.data.frame(summ(OL_lm)$coeftable)
report(OL_lm)
report(OL_anova)


BL_lm <- lm(BL ~ Hostplant*Patry,fly_data)
jtools::summ(BL_lm)

relOL_lm <- lm(relativ_OL*100 ~ Hostplant*Patry,fly_data)
jtools::summ(relOL_lm)

library(jtools)
library(lme4)
library(mixedup)

lmer_model <- lmer(OL ~ Hostplant*Patry + (1|population), data = fly_data, REML = F)
report(lmer_model)
extract_random_effects(lmer_model)
extract_fixed_effects(lmer_model)


extract_VarCorr(lmer_model, ci_level = .95, ci_scale = 'var')

#log-likelihood ratio test
null_model <- lmer(OL ~ 1 + (1|population), data = fly_data, REML = F)
lrt_results <- anova(null_model, lmer_model)
print(lrt_results)
# the two models are not significantly different

#Variation
VarAmongGroups = attr(VarCorr(lmer_model)$population, "stddev")^2
VarWithinGroups = attr(VarCorr(lmer_model), "sc")^2
CV_Among = VarAmongGroups/mean(fly_data$OL, na.rm = T)
CV_Within = VarWithinGroups/mean(fly_data$OL, na.rm = T)

variance = data.frame(Mean = mean(fly_data$OL, na.rm = T), SD = sd(fly_data$OL, na.rm = T),
                Among = VarAmongGroups/(VarAmongGroups+VarWithinGroups)*100,
                Within = VarWithinGroups/(VarAmongGroups+VarWithinGroups)*100,
                CV_Among, CV_Within)
(variance = apply(variance, MARGIN=2, FUN=round, digits=4))



OL_means <- get_means(na.omit(fly_data), 'OL', list('Hostplant','Patry'))
OL_trend <- make_dotplot(data = fly_data, response = 'OL',
             pred = 'Patry', group = 'Hostplant',
             means = OL_means, 
             var = 'Ovipository length', un = 'mm')

BL_means <- get_means(fly_data, 'BL', list('Hostplant','Patry'))  
BL_trend <- make_dotplot(data = fly_data, response = 'BL',
             pred = 'Patry', group = 'Hostplant',
             means = BL_means, 
             var = 'Body length', un = 'mm')  

relOL_means <- get_means(na.omit(fly_data), 'relativ_OL', list('Hostplant','Patry'))
relOL_trend <- make_dotplot(data = fly_data, response = 'relativ_OL',
             pred = 'Patry', group = 'Hostplant',
             means = relOL_means, 
             var = 'Body-proportionate length of the ovipository tube', un = '%')  

fig2 <- plot_grid(BL_trend + 
                    ggtitle('Body length'),
                  OL_trend + 
                    ggtitle('Ovipository length'),
                  relOL_trend + 
                    ggtitle('Ovipository body-proportionate length'),
                  ncol = 3, scale = .9, labels = 'AUTO')

# save the image file
ggsave(paste(plots_dir,"Fig2_interactions.png",sep = '/'),
       fig2, width = 18, height = 6, units = 'in',
       dpi = 300, bg = "white")


input_markdown <- "report.Rmd"
output_file <- "report.html"

rmarkdown::render(
  input = input_markdown,
  output_file = output_file,
  envir = parent.frame()
)
