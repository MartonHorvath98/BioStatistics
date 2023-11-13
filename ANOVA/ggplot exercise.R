blossoms <- read.csv("blossoms.csv")
library(ggplot2)
library(RColorBrewer)
palette <- brewer.pal(9, "Set1")

ggplot(data=blossoms, aes(x=log(LBL), y=log(UBL))) +
  geom_point(aes(color = pop), #shape=pop),             #add points and specify grouping factor
             size = 5, shape = 19,                               #modify point size
             alpha = 0.5) +                           #make points transparent
  #scale_shape_manual(values=c(23,22,21,20,19,18,17,16,15)) +   #manually specify which symbols to use for the groups
  scale_color_manual(values=palette) +
  labs(y="Upper brace length",                        #define axes labels
       x="Lower bract length (log mm)") +
  scale_x_continuous(expand = c(.1, .1))+          #specify length of x-axis
  scale_y_continuous(expand = c(.1, .1))+          #specify length of y-axis 
  theme_gray() +                                      #specify the overall aesthetic of the plot
  theme(aspect.ratio=0.5,                             #modify the ratio of x- and y-axis
        text = element_text(size=14),                 #modify size of axis label text
        legend.text = element_text(size=3))  +        #modify size of legend text
  labs(shape="Population", color="Population") +      #modify legend title
  geom_smooth(method='lm',                            #add a regression line
              se=TRUE,                                #add or remove the standard error
              color = "lightpink") + 
  geom_segment(x=1,y=1,xend=30,yend=30,               #manually add a line to the plot 
               linetype="dashed",                     #modify line type
               size=.5, colour = "grey20") #+
  # facet_grid(cols=vars(pop))                         #splits up graphs by a grouping variable

