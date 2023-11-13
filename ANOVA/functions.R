library(ggplot2)
library(cowplot)
library(gridExtra)
library(dplyr)
library(e1071)
library(car)
# function to create a histogram with density line and a theoretical normal 
# distribution line overlayed on top  
create_hist <- function(data, var, title = character(0), unit = character(0)){
  return(ggplot(data, aes(x = data[[var]])) + 
           #create histogram of the input data, on the input column (filled white)
           geom_histogram(aes(y = ..density..),
                          colour = 1, fill = "white", bins = 20) +
           cowplot::theme_cowplot() + 
           #create a density line of the data, with a semitransparent blue fill
           #of the area under the curve
           geom_density(color = "black", fill = "steelblue",
                        alpha = .5, size = .5) + 
           #create a red line showing the theoretical normal distribution for the
           #given data
           stat_function(fun = dnorm, 
                         args = list(mean = mean(data[[var]]),
                                     sd = sd(data[[var]])),
                         color = "darkred", size = 1) + 
           labs(title = paste("Histogram of", title),
                x = paste(title,"(", unit, ")")) + 
           theme(plot.title = element_text(hjust = 0.5)))
}
# function to create a Q-Q plot from a regression result and highlights it 
# according to whether or not the sample fails the Shapiro-Wilk normality test
make_QQplot <- function(data, pval, title = character(0)){
  #create a Q-Q plot using ggplot
  plot = ggplot() +
    geom_qq(aes(sample = rstandard(data))) +
    geom_abline(color = "red", linewidth = 1) +
    coord_fixed(ratio = 1) + 
    scale_x_continuous(name = "Theoretical Quantiles",
                       limits = c(-3, 3),
                       n.breaks = 6) + 
    scale_y_continuous(name = "Sample Quantiles",
                       limits = c(-3, 3), 
                       n.breaks = 6) + 
    labs(title = paste("Normal Q-Q plot of",title)) + 
    theme(title = element_text(hjust = 0.5))
  #if the normality test fails color the background of the plot red
  if(pval < 0.05){
    plot = plot +
      theme(panel.background = element_rect(fill = alpha('red', 0.2)))
  }
  return(plot)
}

make_table <- function(reg){
  residuals <- reg[['residuals']]
  # calculate parameters
  mean_ <- round(mean(residuals),3)
  SE_ <- round(sd(residuals)/length(residuals), 3)
  median_ <- round(median(residuals),3)
  skew_ <- round(skewness(residuals),3)
  kurt_ <- round(kurtosis(residuals),3)
  # create data frame
  df <- data.frame(
    'Mean' = mean_,
    'SE' = SE_,
    'Median' = median_,
    'Skew' = skew_,
    'Kurtosis' = kurt_
  )
  #create table
  table <- tableGrob(df,rows = '',theme = ttheme_minimal())
  return(table)
}

#
make_violin <- function(data, x, y, group){
  return(ggplot(data, #data
                aes(x = data[[x]], #x-axis
                    y = data[[y]], #y-axis
                    colour = data[[group]])) + #group 
           
           # add violin plots plotted by the maternal host and grouped by the 
           # larval host plant
           geom_violin(position = position_dodge(0.9), # separate by group
                       scale = "width", trim = F, # shape configuration
                       linewidth = 1, fill = "white", # shape settings
                       show.legend = F) + # legend settings
           
           # add the individual measurement points inside the violin plots
           geom_dotplot(aes(fill = data[[group]]), #fill by group
                        position = position_dodge(0.9), # separate by group
                        binaxis='y', stackdir='center', # position settings
                        show.legend = T) + # legend settings
           
           # add mean and 1-times the standard deviation statistics to the 
           # violin plots
           stat_summary(aes(group = data[[group]]), # separate by group
                        position = position_dodge(0.9),
                        fun = "mean", geom = "point", # function
                        color = "black", size = 2, # shape settings
                        show.legend = F) + # legend settings
           stat_summary(aes(group = data[[group]]), # separate by group
                        position = position_dodge(0.9), 
                        fun.data="mean_sdl", fun.args = list(mult=1), # function 
                        geom = "errorbar", 
                        color = "black", width = 0.15, size = 1, # shape settings
                        show.legend = F) + # legend settings
           cowplot::theme_cowplot() +
           
           # set the color schemes for the conditions
           scale_color_manual(name = "Host plant (during larval stage):",
                              values = c("Barbarea" = "steelblue",
                                         "Berteroa" = "orange")) + 
           scale_fill_manual(name = "Host plant (during larval stage):",
                             values = c("Barbarea" = "steelblue",
                                        "Berteroa" = "orange")) + 
           
           # figure configuration: axis labels and legend position
           labs(x = "Maternal host plant",
                y = "Adult weight (mg)") +
           theme(legend.position = 'bottom',
                 legend.justification = 'center'))  
}

