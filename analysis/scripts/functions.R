library(tidyverse)
library(patchwork)
library(ggrepel)
library(tictoc)
library(crayon)
library(mixtools)
#library(parallel)

# Scatter plot with optional trendline and marginal plots

scatter_stats <- function(input_table,
                          x,
                          y,
                          color = NULL,
                          xlab = x,
                          ylab = y,
                          log_transform = F,
                          pseudocount = 1,
                          log_scale = F,
                          marginal = T,
                          marginal_type = 'density',
                          marginal_fill = 'grey80',
                          marginal_color = 'black',
                          marginal_alpha = 0.8,
                          title = '',
                          point_size = 0.5,
                          point_alpha = 'auto',
                          point_color = '#1D6996',
                          trendline = T,
                          trendline_se = F,
                          line_size = 1,
                          line_type = 'solid',
                          line_color = '#EDAD08',
                          xlims = NULL,
                          ylims = NULL,
                          xbreaks = NULL,
                          ybreaks = NULL,
                          correlation = T,
                          coord_equal = T) {
  
  table_subset <- input_table %>% select(one_of(x, y, color))
  
  if(is.null(color)) {
    colnames(table_subset) <- c('X', 'Y')
  } else {
    colnames(table_subset) <- c('X', 'Y', 'Color')
  }
  
  
  if(log_transform) {
    table_subset <- table_subset %>% mutate_all(function(i) {log10(i + pseudocount)}) %>% filter(is.finite(X), is.finite((Y)))
  }
  
  if (point_alpha == 'auto') {
    point_alpha <- 100*sqrt(point_size)/sqrt(nrow(table_subset)) 
  }
  
  if(correlation) {
    if(log_scale) {
      correlation_subset <- table_subset %>% mutate_all(function(i) {log10(i + pseudocount)}) %>% filter(is.finite(X), is.finite((Y)))
    } else {
      correlation_subset <- table_subset %>% drop_na()
    }
    r <- round(cor(correlation_subset$X, correlation_subset$Y, use = 'complete.obs'), 2)
    
  }
  
  if(is.null(color)) {
    p <- table_subset %>% 
      ggplot(aes(x = X,
                 y = Y)) + 
      geom_point(size = point_size,
                 alpha = point_alpha,
                 color = point_color) 
    
  } else {
    p <- table_subset %>% 
      ggplot(aes(x = X,
                 y = Y,
                 color = Color)) +
      geom_point(size = point_size,
                 alpha = point_alpha) 
  }
  
  
  if(coord_equal) {
    p <- p + coord_equal()  
  }
  
  if(log_scale) {
    if(is.null(xbreaks)) {
      p <- p + 
        scale_x_log10(limits = xlims) 
    } else {
      p <- p + 
        scale_x_log10(limits = xlims, breaks = xbreaks) 
    }
    if(is.null(ybreaks)) {
      p <- p + 
        scale_y_log10(limits = ylims) 
    } else {
      p <- p + 
        scale_y_log10(limits = ylims, breaks = ybreaks) 
    }
  } else {
    if(is.null(xbreaks)) {
      p <- p + 
        scale_x_continuous(limits = xlims) 
    } else {
      p <- p + 
        scale_x_continuous(limits = xlims, breaks = xbreaks) 
    }
    if(is.null(ybreaks)) {
      p <- p + 
        scale_y_continuous(limits = ylims) 
    } else {
      p <- p + 
        scale_y_continuous(limits = ylims, breaks = ybreaks) 
    }
  }
  if(trendline) {
    p <- p + geom_smooth(method = 'lm', color = line_color, size = line_size, se = trendline_se)
  }
  
  p <- p + theme_publication(grid = F) +
    labs(title = title,
         subtitle = paste('Pearson r =', r),
         x = xlab,
         y = ylab) +
    theme(plot.title = element_text(face = 'plain', size = rel(1.1)),
          plot.subtitle = element_text(hjust = 0.5))
  
  if(marginal) {
    ggExtra::ggMarginal(p, 
                        type = marginal_type, 
                        fill = marginal_fill, 
                        color = marginal_color,
                        alpha = marginal_alpha)
    
  } else {
    p
  }
}

# run an Fisher exact test by setting up a 2 x 2 contingency table
# Enrichment testing
enrichment_test <- function(list1, # list of positives in category 1
                            list2, # list of positives in category 2
                            cat1 = c(TRUE, FALSE), # category 1 names
                            cat2 = c(TRUE, FALSE), # category 2 names
                            background, # total background list of all elements
                            print = TRUE) {
  
  # Make sure lists are within the full background set
  list1 <- intersect(list1, background)
  list2 <- intersect(list2, background)
  full_list <- unique(c(list1, list2, background))
  
  # contingency table
  df <- tibble(element = full_list) %>% 
    mutate(list1 = ifelse(element %in% list1, cat1[1], cat1[2]) %>% factor(levels = cat1), # assign list1 to category 1 and set as ordered factor
           list2 = ifelse(element %in% list2, cat2[1], cat2[2]) %>% factor(levels = cat2)) # assign list2 to category 2 and set as ordered factor
  
  contingency_table <- table(df$list1, df$list2)
  
  # Test
  fisher <- fisher.test(contingency_table)
  chisq <- chisq.test(contingency_table)
  
  if(print) {
    print(contingency_table)
    print(fisher)  
    print(chisq)
  }
  
  # Return fisher results as well as table
  list('contingency' = contingency_table,
       'table' = df,
       'chisq' = chisq,
       'fisher' = fisher)
}

# color scales

generate_colors <- function(n, palette = 'default', ...) {
  
  colors <- dplyr::case_when(
    palette == 'default' ~ c(0, 360, 0, 180, 0, 100),
    palette == 'pastel' ~ c(0, 360, 0, 54, 67, 100),
    palette == 'pimp' ~ c(0, 360, 54, 180, 27, 67),
    palette == 'intense' ~ c(0, 360, 36, 180, 13, 73),
    palette == 'custom1' ~ c(0, 360, 30, 100, 25, 100),
    palette == 'custom2' ~ c(0, 360, 30, 70, 60, 100))
  
  unname(hues::iwanthue(n = n,
                        hmin = colors[1],
                        hmax = colors[2],
                        cmin = colors[3],
                        cmax = colors[4],
                        lmin = colors[5],
                        lmax = colors[6],
                        ...))
}

### mixture model
plot_mix_comps <- function(x, mu, sigma, lam) {
  lam * dnorm(x, mu, sigma)
}

plot_mix_model <- function(mixmdl,
                           alpha = 0.5,
                           bins = 1000,
                           colors = c('orangered', 'dodgerblue4'),
                           xlab = '',
                           ylab = 'Density',
                           intercept = NULL) {
  
  data.frame(x = mixmdl$x) %>%
    ggplot() +
    geom_histogram(aes(x, ..density..),
                   alpha = alpha,
                   bins = bins) +
    stat_function(geom = "line", fun = plot_mix_comps,
                  args = list(mixmdl$mu[1], mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                  color = colors[1], lwd = 1.5) +
    stat_function(geom = "line", fun = plot_mix_comps,
                  args = list(mixmdl$mu[2], mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                  color = colors[2], lwd = 1.5) +
    ylab(ylab) +
    xlab(xlab) +
    geom_vline(xintercept = intercept, linetype = 'dashed') +
    theme_publication()
  
}

mixture_model <- function(input_vector, 
                          log_transform = FALSE,
                          k = 2,
                          alpha = 0.5,
                          bins = 1000,
                          colors = c('orangered', 'dodgerblue4'),
                          xlab = '',
                          ylab = 'Density',
                          intercept = 'auto') {
  
  mixmdl <- normalmixEM(input_vector, k = k)
  
  if(intercept == 'auto') {
    intercept <- mixture_cutoff(mixmdl)
  }
  
  # plot
  mix_plot <- plot_mix_model(mixmdl,
                             alpha = alpha,
                             bins = bins,
                             colors = colors,
                             xlab = xlab,
                             ylab = ylab,
                             intercept = intercept)
  
  list('model' = mixmdl,
       'plot' = mix_plot,
       'cutoff' = intercept)
}

mixture_cutoff <- function(mixmod) {
  uniroot(f = function(x) 
    plot_mix_comps(x,
                   mixmod$mu[1],
                   mixmod$sigma[1],
                   mixmod$lambda[1]) -
      plot_mix_comps(x,
                     mixmod$mu[2],
                     mixmod$sigma[2],
                     mixmod$lambda[2]), mixmod$mu)$root
}