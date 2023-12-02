corr_heatmap <- function(data, filter, columns){
  return(data %>%
           na.omit(.) %>% 
           cor(.) %>% 
           pheatmap(., display_numbers = T, angle_col = 45,
                    clustering_distance_cols = 'euclidean'))
}

linear_regression <- function(response, pred1, pred2){
  .design = model.matrix(~ pred1 * pred2, data = data.frame(pred1, pred2))
  .design = cbind(Intercept = 1, .design)
  
  linreg = lm(response ~ .design)
  
  return(list(
    'lm' = linreg,
    'summary' = jtools::summ(linreg),
    'regression report' = report::report(linreg)
  ))
}

simple_anova <- function(response, pred1, pred2){
  .design = model.matrix(~ pred1 * pred2, data = data.frame(pred1, pred2))
  .design = cbind(Intercept = 1, .design)
  
  .anova = aov(response ~ .design)
  .posthoc = TukeyHSD(.anova)
  .df = do.call(rbind, .posthoc) %>% 
    as.data.frame(.) %>% 
    dplyr::arrange(diff)
  
  return(list(
    'anova' = .anova,
    'posthoc' = .posthoc,
    'anova report' = report::report(.anova)
  ))
}

get_means <- function(data, response, predictors=list()){
  pred1 = predictors[[1]]
  pred2 = predictors[[2]]
  
  return(df = tapply(data[[response]], list(data[[pred1]], data[[pred2]]), mean) %>% 
           as.data.frame(.) %>% 
           tibble::rownames_to_column(pred1) %>% 
           tidyr::pivot_longer(!{{pred1}}, names_to = {{pred2}}, values_to = 'mean') %>% 
           dplyr:: mutate_if(is.character,as.factor))
}

make_boxplot <- function(data, predictors, group, response, y_label, design){
  .design = reformulate(design)
  
  return(ggplot(data, aes(x =  data[[predictors]], y = data[[response]])) + 
           geom_boxplot(aes(fill = data[[group]])) +
           scale_fill_viridis(discrete = T) + 
           labs(x = '', y = y_label, fill = 'Groups') +
           facet_grid(design, scales = 'free_x', drop = T) +
           theme_bw() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold'),
                 strip.text = element_text(face = 'bold', size = 14)))
}

get_means <- function(data, response, predictors=list()){
  pred1 = predictors[[1]]
  pred2 = predictors[[2]]
  
  return(df = tapply(data[[response]], list(data[[pred1]], data[[pred2]]), mean) %>% 
           as.data.frame(.) %>% 
           tibble::rownames_to_column(pred1) %>% 
           tidyr::pivot_longer(!{{pred1}}, names_to = {{pred2}}, values_to = 'mean') %>% 
           dplyr:: mutate_if(is.character,as.factor))
}

make_dotplot<- function(data, response, pred, group, means, var, un){
  return(ggplot(data, aes(x = .data[[pred]], y = .data[[response]])) +
           geom_jitter(aes(color = .data[[group]]), 
                       width = .1,
                       size = 3, alpha = .5) +
           geom_line(
             data = means, show.legend = F, na.rm = T,
             position = position_dodge(0), linewidth = .8,
             aes(y = mean, group = .data[[group]], linetype = .data[[group]])) + 
           stat_summary(aes(group = .data[[group]]), # separate by group
                        position = position_dodge(.05), 
                        fun.data="mean_sdl", fun.args = list(mult=1), # function 
                        geom = "errorbar", 
                        color = "black", width = 0.15, size = .8, # shape settings
                        show.legend = F) +
           stat_summary(aes(group = .data[[group]], fill = .data[[group]]), # separate by group
                        position = position_dodge(.05),
                        fun = "mean", geom = "point",
                        shape = 21, color = 'black', stroke = .8, size = 3, # shape settings
                        show.legend = F) + 
           theme_bw() +
           labs(y = stringr::str_wrap(paste(var, ' (', un,')',sep = ''),30), x = '') +
           theme(legend.position = 'bottom', legend.justification = 'center',
                 axis.text = element_text(size = 12))
  )
}

