#####
##### 1.) set up the working environment
#####
setwd('C:/Users/Marton Horvath/Documents/Lund/BIOS14/BioStatistics/GLM/assignment')
#set working directory

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

library(curl)
library(sf)
library(stringdist)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(pheatmap)
library(MASS)
library(MuMIn)
library(report)
library(cowplot)
#####
##### 2.) Download the resource file and load it into the environment 
#####
download.file("https://raw.githubusercontent.com/MartonHorvath98/BIOS14_QuantitativeAnalysis/main/Lecture%20notes/datasets/Eulaema.csv", 
              destfile = paste(data_dir,'eulaema.csv',sep = '/'),
              method = "curl")

#data file info
file_path <- paste(data_dir,'eulaema.csv',sep = '/')
file_info <- file(file_path, open = "rt")
encoding <- Encoding(readLines(file_info, n = 1)) #encoding is unknown!!
close(file_info)

# load data file with manually set encoding to get the strings properly
bee_file <- read.csv(file_path, fileEncoding = 'latin1',
                 stringsAsFactors = T)
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

#####
##### 3.) exploratory plotting of data
#####

# load the maps of the region Mata Atlantica and biologically interesting units
# (e.g. natural reserves) within the region
brazil_map <- geobr::read_country()
region_map <- sf::st_read(paste(data_dir, 'biome_border.shp',sep = '/'))
units <- sf::st_read(paste(data_dir, 'conservation_units2.shp',sep = '/'))
# preprocessing of map to prepare for merging with the bee data
unit_names <- units %>% 
  dplyr::mutate(name = stringr::str_remove(tolower(nome), tolower(categoria))) %>%
  dplyr::pull(id, name = name)

# preprocessing of bee data locations for merging with the map of Brazil
bee_data <- bee_file %>% 
  #make the location name comparable
  dplyr::mutate(SA = tolower(stringr::str_replace_all(SA,'_',' '))) %>% 
  dplyr::mutate(SA = as.factor(SA)) #factorize location names



# bee data locations and map locations tend to have slightly different names for the
# same locations, thus mapping of the data was carried out by a distance measure on 
# the names - using longest common sub-string distance (lcs)
distance_matrix <- stringdistmatrix(unique(bee_file$SA), unique(names(unit_names)), useNames = T,
                                    method = 'lcs', nthread = 4)
# function to find the closest match
get_min <- function(row){
  index <- which.min(row)
  return(names(row)[index])
}
# extract the closest matching locations
matches <- apply(distance_matrix, 1, get_min)
# create a data frame to merge with the bee data and location ids 
matches <- data.frame(SA = names(matches), 
                      name = matches, 
                      id = unit_names[match(matches, names(unit_names))])
# merging the bee data and location ids
bee_data <- bee_data %>% 
  merge(., matches, by = 'SA', all.x = T) %>%  #merge bee data with ids
  dplyr::left_join(., units, by = c('id' = 'id')) #join bee data a map data on ids
  
bee_map <- bee_data %>% 
  dplyr::rowwise(.) %>% 
  dplyr::mutate(
         # extract the x coordinate from the centroid of the MOLTIPOLYGON object
         longitude = st_coordinates(st_centroid(geometry))[1], 
         # extract the y coordinate from the centroid of the MOLTIPOLYGON object
         latitude = st_coordinates(st_centroid(geometry))[2]) %>%
  dplyr::group_by(method, SA) %>% # group data frame on capturing method and location
  dplyr::summarise(n_bee = sum(Eulaema_nigrita), # sum up captured bees for each location 
                                          # using each capturing method
            longitude = longitude, latitude = latitude, # keep x- and y-coors for mapping
            .groups = 'drop') %>% # drop grouping of the data frame
  dplyr::distinct(.,.keep_all = T) # keep unique rows

# plot the map of mata atlantica with the number of captured bees plotted on the 
# spot of capture
(bee_map_plot <- ggplot() + # initialize empty plot
  geom_sf(data = brazil_map, color = 'darkgrey', fill = 'white') +
  geom_sf(data = region_map, color = 'darkgrey', fill = 'lightgrey') + # add the outline of the region to map
  geom_sf(data = units, fill = 'darkgreen') +  # add the outline of the ecological units to the map
  coord_sf(xlim = c(-60, -35), ylim = c(-30, -5), clip = 'on') + 
  geom_point(data = bee_map %>% filter(n_bee > 0), # add the bees to the map as points
             aes(x = longitude, # plot on the corresponding x- and y coords
                 y = latitude, 
                 size = n_bee, # size of the points is determined by the number
                               # of bees caputred
                 color = method), # colour of the points is determined by
             alpha = .6) +
  geom_text(data = brazil_map,
             aes(x = st_coordinates(st_centroid(brazil_map))[1],
                 y = st_coordinates(st_centroid(brazil_map))[2]),
            label = 'BRAZIL', size = 24, color = 'darkgrey') + 
  scale_size_continuous( # legend specification for size
    range = c(5, 20),
    name = 'Nr. of captured bees') +  
  scale_color_manual( # legend specification for color
    values = c(
      'Net' = '#4169E1',
      'NetTraps' = '#DC143C',
      'Traps' = '#228B22'
    ),
    name = 'Capturing methods') + 
  # legend title
  labs(title = expression(paste('The abundance of the bee species (',italic('E. nigrita'),
                                 ') in Mata Atlântica (Brazil)', sep = '')),
       xlab = 'Longitude',
       ylab = 'Latitude') +
  # override legend point size for color guide
  guides(color = guide_legend(override.aes = list(size = 5))) +
  # centralize legend title
  theme(plot.title = element_text(hjust = 0.5)) + 
  # select a pretty theme
  theme_minimal())

# save the image file
ggsave(paste(plots_dir,"Fig1_bee_map.png",sep = '/'),
       bee_map_plot, width = 9, height = 9, units = 'in',
       dpi = 300, bg = "white")

#####
##### 4.) select meaningful model
#####

# figure correlating descriptors
bee_data <- bee_data %>% 
  dplyr::mutate(ano_cria = as.numeric(ano_cria)) %>% 
  dplyr::select(!c('name','nome','grupo','esfera','SU','id')) %>% 
  dplyr::relocate(c('SA','categoria','method'), .before = everything()) %>% 
  dplyr::mutate(bee_norm = Eulaema_nigrita/exp(effort)) %>%
  dplyr::relocate(bee_norm, .after = ano_cria)


corr_bee <- bee_data %>% 
  dplyr::select(effort:ano_cria) %>%
  dplyr::filter(complete.cases(.)) %>%
  cor(.)
pheatmap(mat = corr_bee,
         display_numbers = T)

# Full model 
glm_full <- glm.nb(Eulaema_nigrita ~ .,
          data = bee_data[,4:13])
summary(glm_full)
report(glm_full)

# Optimal model 
glm_opt <- glm.nb(Eulaema_nigrita ~ effort + MAP + Tseason + Pseason + forest.,
                   data = bee_data)
summary(glm_opt)
report(glm_opt)

(effort_plot <- ggplot(bee_data, aes(x = effort, y = Eulaema_nigrita)) + 
    geom_point(aes(color = method), size = 3, alpha = .7) + 
    scale_color_manual( # legend specification for color
      values = c(
        'Net' = '#4169E1',
        'NetTraps' = '#DC143C',
        'Traps' = '#228B22'
      ),
      name = 'Capturing methods') +
    stat_smooth(color = 'black', method = "glm.nb",
                show.legend = F) +
    stat_regline_equation(
      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))) +
    stat_smooth(aes(color = method, fill = method), method = "glm.nb",
                show.legend = F, se = F) +
    xlab('Log hours of collecting') + 
    ylab(expression(paste('Abundance of ',italic('E. nigrita'),sep = ''))) +
    theme_bw())

(precip_plot <- ggplot(bee_data, aes(x = MAP, y = Eulaema_nigrita)) + 
    geom_point(color = '#DC143C',size = 3, alpha = .7) + 
    xlab('Mean annual precipitation (mm)') + 
    stat_smooth(color = 'black', method = "glm.nb",
                show.legend = F) +
    stat_regline_equation(
      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))) +
    ylab(expression(paste('Abundance of ',italic('E. nigrita'),sep = ''))) +
    theme_bw())


(tseason_plot <- ggplot(bee_data, aes(x = Tseason, y = Eulaema_nigrita)) + 
    geom_point(color = '#DC143C',size = 3, alpha = .7) + 
    stat_smooth(color = 'black', method = "glm.nb",
                show.legend = F) +
    stat_regline_equation(
      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))) +
    xlab('Temperature seasonality') + 
    ylab(expression(paste('Abundance of ',italic('E. nigrita'),sep = ''))) +
    theme_bw())

(pseason_plot <- ggplot(bee_data, aes(x = Pseason, y = Eulaema_nigrita)) + 
    geom_point(color = '#DC143C',size = 3, alpha = .7) + 
    stat_smooth(color = 'black', method = "glm.nb",
                show.legend = F) +
    stat_regline_equation(
      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))) +
    xlab('Rainfall seasonality (%)') + 
    ylab(expression(paste('Abundance of ',italic('E. nigrita'),sep = ''))) +
    theme_bw())

(forest_plot <- ggplot(bee_data, aes(x = forest. * 100, y = Eulaema_nigrita)) + 
    geom_point(color = '#DC143C',size = 3, alpha = .7) +
    stat_smooth(color = 'black', method = "glm.nb",
                show.legend = F) +
    stat_regline_equation(
      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))) +
    xlab('Forest coverage (%)') + 
    ylab(expression(paste('Abundance of ',italic('E. nigrita'),sep = ''))) +
    theme_bw())

(fig2 <- cowplot::plot_grid(tseason_plot, pseason_plot, forest_plot, precip_plot,
                           nrow = 2, ncol = 2, scale = .9,
                           labels = c('A.)','B.)','C.)','D.)'), label_size = 14,
                           align='hv'))
ggsave(paste(plots_dir,"Fig2_contributors.png",sep = '/'),
       fig2, width = 16, height = 9, units = 'in',
       dpi = 300, bg = "white")


ggsave(paste(plots_dir,"Fig3_effort.png",sep = '/'),
       effort_plot, width = 10, height = 6, units = 'in',
       dpi = 300, bg = "white")



# Full model 
glm_norm_full <- glm.nb(bee_norm ~ .,
                   data = bee_data[,6:14])
summary(glm_norm_full)

# Optimal model 
glm_norm_opt <- glm.nb(bee_norm ~ MAP + Tseason + forest.,
                  data = bee_data)
summary(glm_norm_opt)
report(glm_norm_opt)

(precip_norm_plot <- ggplot(bee_data, aes(x = MAP, y = bee_norm)) + 
    geom_point(color = '#4169E1',size = 3, alpha = .7) + 
    xlab('Mean annual precipitation (mm)') + 
    stat_smooth(color = 'black', method = "glm.nb",
                show.legend = F) +
    stat_regline_equation(
      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))) +
    ylab(expression(paste('Relative abundance of ',italic('E. nigrita'),
                          ' (per hour)',sep = ''))) +
    theme_bw())

(tseason_norm_plot <- ggplot(bee_data, aes(x = Tseason, y = bee_norm)) + 
    geom_point(color = '#4169E1',size = 3, alpha = .7) + 
    stat_smooth(color = 'black', method = "glm.nb",
                show.legend = F) +
    stat_regline_equation(
      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))) +
    xlab('Temperature seasonality') + 
    ylab(expression(paste('Relative abundance of ',italic('E. nigrita'),
                          ' (per hour)',sep = ''))) +
    theme_bw())

(forest_norm_plot <- ggplot(bee_data, aes(x = forest. * 100, y = bee_norm)) + 
    geom_point(color = '#4169E1',size = 3, alpha = .7) +
    stat_smooth(color = 'black', method = "glm.nb",
                show.legend = F) +
    stat_regline_equation(
      aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~"))) +
    xlab('Forest coverage (%)') + 
    ylab(expression(paste('Relative abundance of ',italic('E. nigrita'),
                          ' (per hour)',sep = ''))) +
    theme_bw())

(fig4 <- cowplot::plot_grid(tseason_norm_plot, forest_norm_plot, precip_norm_plot,
                            nrow = 1, ncol = 3, scale = .9,
                            labels = c('A.)','B.)','C.)'), label_size = 14,
                            align='hv'))
ggsave(paste(plots_dir,"Fig4_contributors_norm_effort.png",sep = '/'),
       fig4, width = 16, height = 9, units = 'in',
       dpi = 300, bg = "white")

cols <- c("Mean" = 'black', "Mean + SD" = 'red', "Mean - SD" = 'green')
# Create a ggplot with the original data points
(fig5 <- ggplot(bee_data, aes(x = MAP, y = Eulaema_nigrita)) +
  geom_point(color = "darkgrey", stroke = 1,
             shape = 21, size = 2) +
  geom_point(color = "grey", stroke = 1,
             alpha = .5, size = 2) +
  labs(
    main = 'Effects of annual rainfall and its distribution',
    x = "Mean annual precipitation",
    y = expression(paste(italic("El. nigrita"), "abundance"))) +
  
  # Add the predicted values from the model
  geom_line(data = data.frame(MAP = newMAP, Pseason = mean(bee_data$Pseason)),
            aes(x = newMAP, 
                y = predict(m, newdata = data.frame(MAP = newMAP,
                                                    Pseason = mean(bee_data$Pseason)), 
                                        type = "response"),
                color = 'Mean'),
            linewidth = 1) +
  # Add lines for Mean + SD and Mean - SD
  geom_line(data = data.frame(MAP = newMAP, Pseason = mean(bee_data$Pseason) + sd(bee_data$Pseason)),
            aes(x = newMAP, 
                y = predict(m, newdata = data.frame(MAP = newMAP, 
                                                    Pseason = mean(bee_data$Pseason) + sd(bee_data$Pseason)),
                                        type = "response"),
                color = 'Mean + SD'),
            linewidth = 1) +
  geom_line(data = data.frame(MAP = newMAP, Pseason = mean(bee_data$Pseason) - sd(bee_data$Pseason)),
            aes(x = newMAP, 
                y = predict(m, newdata = data.frame(MAP = newMAP, 
                                                    Pseason = mean(bee_data$Pseason) - sd(bee_data$Pseason)),
                            type = "response"),
                color = 'Mean - SD'),
            linewidth = 1) +
  # Add legend
  scale_color_manual(values = cols, name = "Rainfall seasonality") + 
  theme_bw())

ggsave(paste(plots_dir,"Fig5_rainfall_seasonality.png",sep = '/'),
       fig5, width = 10, height = 6, units = 'in',
       dpi = 300, bg = "white")

input_markdown <- "report.Rmd"
output_file <- "report.html"

rmarkdown::render(
  input = input_markdown,
  output_file = output_file,
  envir = parent.frame()
)

  