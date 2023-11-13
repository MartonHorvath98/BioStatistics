#Analysis of variances

#1.) generate data to modell ANOVA on:
set.seed(100)
groups <- factor(rep(c("Low","Medium","High"), each = 50),
                 levels = c("Low","Medium","High"))

x <- c(rnorm(50, 10, 3), rnorm(50, 13, 3), rnorm(50, 14, 3))

library(ggplot2)
df <- data.frame(
  groups = groups,
  size = x
)

(plot <- ggplot(df, aes(x = groups, y = x)) +
  geom_boxplot(aes(fill = groups), 
               outlier.size = 0, alpha = 0.5) + 
  geom_jitter(width = 0.15, size = 3, shape = 21) + 
  geom_point(aes(x = groups, y = median(x)),
             fill = "black", size = 3, shape = 21) + 
  scale_x_discrete(name = "") + 
  scale_y_continuous(name = "Body size (g)",
                     limits = c(0,25)) + 
  theme(legend.background = element_rect(fill = alpha(colour = "white", 0.5)),
        legend.position = "none", 
        axis.title.y = element_text(hjust = 1, margin = margin(10, 5, 0, 0, unit = 'mm'))))

m <- lm(x ~ groups)
print(anova(m))
print(summary(m))
