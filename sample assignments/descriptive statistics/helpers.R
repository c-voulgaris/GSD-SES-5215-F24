descriptive_histogram <- function(this_vector, 
                                  label_x,
                                  label_y,
                                  is_log = FALSE,
                                  decimal_places){
  this_mean <- mean(this_vector)
  this_quartiles <- quantile(this_vector, c(0.25, 0.50, 0.75))
  this_stdev <- sd(this_vector)
  
  this_ci <- t.test(this_vector)$conf.int
  
  label_x <- ifelse(is_log, paste0(label_x, " (log-scale axis)"), label_x)
  
  this_iqr <- this_quartiles["75%"] - this_quartiles["25%"]
  
  this_df <- as_tibble(this_vector)
  
  bins <- min(length(unique(this_vector)), 40)
  
  hist_plot_initial <- ggplot(this_df) +
    geom_histogram(aes(x = value),
                   fill = "gray60",
                   color = "gray90",
                   bins = bins) +
    
    scale_y_continuous(name = label_y) +
    scale_x_continuous(name = label_x,
                       transform = ifelse(is_log, "log10", "identity")) +
    theme_minimal()
    
  this_y_max <- layer_scales(hist_plot_initial)$y$range$range[2]
  this_x_breaks <- layer_scales(hist_plot_initial)$x$break_positions()[
    !is.na(layer_scales(hist_plot_initial)$x$break_positions())
  ]
  
  if(is_log) {
    this_x_breaks <- 10^(this_x_breaks)
  }
  
  stdev_pts <- tibble(x = c(this_mean, this_mean + this_stdev),
                         y = c(this_y_max/2, this_y_max/2))
  
  iqr_pts <- tibble(x = c(this_quartiles["25%"], this_quartiles["75%"]),
                      y = c(this_y_max/4, this_y_max/4))
  
  ci_pts <- tibble(x = c(this_ci[1], this_ci[2]),
                    y = c(this_y_max * 0.9, this_y_max * 0.9))
  
  hist_plot_labels <- hist_plot_initial +
    annotate(geom = "linerange",
             x = this_mean,
             ymin = 0, 
             ymax = this_y_max * 1.2) +
    annotate(geom = "text", 
             hjust = 0.5, 
             vjust = 0,
             x = this_mean,
             y = this_y_max * 1.21,
             label = paste0("Average = ", 
                            formatC(this_mean,
                                    format = "f",
                                    big.mark = ",",
                                    digits = decimal_places))) +
    annotate(geom = "linerange",
             x = this_quartiles["50%"],
             ymin = 0,
             ymax = this_y_max * 1.1) +
    annotate(geom = "text", 
             hjust = ifelse(this_mean < this_quartiles["50%"], 0, 1), 
             vjust = 0,
             x = this_quartiles["50%"],
             y = this_y_max * 1.11,
             label = paste0(" Median = ", 
                            formatC(this_quartiles["50%"],
                                    format = "f",
                                    big.mark = ",",
                                    digits = decimal_places))) +
    geom_line(data = stdev_pts,
              aes(x = x, y = y),
              arrow = arrow(ends = "both",
                            type = "closed",
                            length = unit(0.25, "cm"))) +
    annotate(geom = "text",
             hjust = 0,
             vjust = 0,
             x = this_mean * 1.01,
             y = this_y_max * 0.52,
             label = paste0("Standard\ndeviation =\n",
                            formatC(this_stdev,
                                    format = "f",
                                    big.mark = ",",
                                    digits = decimal_places))) +
    geom_line(data = iqr_pts,
              aes(x = x, y = y),
              arrow = arrow(ends = "both",
                            type = "closed",
                            length = unit(0.25, "cm"))) +
    annotate(geom = "text",
             hjust = ifelse(this_mean < this_quartiles["50%"], 0, 1),
             vjust = 0,
             x = ifelse(this_mean < this_quartiles["50%"],
                        this_quartiles["50%"] * 1.01,
                        this_quartiles["50%"] * 0.99),
             y = this_y_max * 0.26,
             label = paste0("Interquartile\nrange =\n",
                            formatC(this_iqr,
                                    format = "f",
                                    big.mark = ",",
                                    digits = decimal_places))) +
    geom_line(data = ci_pts,
              aes(x = x, y = y),
              size = 4) +
    annotate(geom = "text",
             hjust = ifelse(this_mean < this_quartiles["50%"], 1, 0),
             vjust = 0,
             x = ifelse(this_mean < this_quartiles["50%"],
                        this_mean * 0.99,
                        this_mean * 1.01),
             y = this_y_max * 0.91,
             label = paste0("95-percent confidence\ninterval for mean =\n",
                            formatC(this_ci[1],
                                    format = "f",
                                    big.mark = ",",
                                    digits = decimal_places),
                            " - ",
                            formatC(this_ci[2],
                                    format = "f",
                                    big.mark = ",",
                                    digits = decimal_places))) +
    scale_x_continuous(name = label_x,
                       breaks = this_x_breaks,
                       labels = formatC(this_x_breaks,
                                        format = "f",
                                        big.mark = ",",
                                        digits = decimal_places),
                       transform = ifelse(is_log, "log10", "identity")) 
  
}

descriptive_cat_bars <- function(this_vector,
                                 x_label,
                                 y_label) {
  this_df <- as_tibble(this_vector)
  
  this_summary <- this_df |>
    group_by(value) |>
    summarise(num = n()) 
  
  this_summary <- this_summary |>
    mutate(proportion = num/sum(num)) 
  
  ci_low <- prop.test(this_summary$num[1], 
                      sum(this_summary$num))$conf.int[1]
  
  ci_hi <- prop.test(this_summary$num[1], 
                      sum(this_summary$num))$conf.int[2]
  
  for(i in 2:nrow(this_summary)) {
    this_ci_low <- prop.test(this_summary$num[i], 
                             sum(this_summary$num))$conf.int[1]
    
    this_ci_hi <- prop.test(this_summary$num[i], 
                       sum(this_summary$num))$conf.int[2]
    
    ci_low <- c(ci_low, this_ci_low)
    ci_hi <- c(ci_hi, this_ci_hi)
  }
  
  this_summary$ci_low <- ci_low
  this_summary$ci_hi <- ci_hi
  
  this_summary <- this_summary |>
    mutate(label_pos_x = 1:nrow(this_summary),
           label_pos_y = ifelse(ci_low > 0.2,  0.02, ci_hi + 0.02),
           label = paste0(round(proportion * 100, digits = 1), 
                          "%\n95-percent confidence:\n(",
                          round(ci_low * 100, digits = 1),
                          "% - ",
                          round(ci_hi * 100, digits = 1),
                          "%)")) 
  
  ggplot(this_summary) +
    geom_bar(aes(x = value,
                 y = proportion),
             stat = "identity",
             fill = "gray75") +
    geom_errorbar(aes(x = value,
                      ymin = ci_low,
                      ymax = ci_hi),
                  width = 0.5) +
    geom_text(aes(x = label_pos_x,
                  y = label_pos_y,
                  label = label),
              vjust = 0) +
    scale_x_discrete(name = x_label) +
    scale_y_continuous(name = y_label,
                       limits = c(0, 1),
                       breaks = breaks<- seq(0, 1, by=0.25),
                       labels = paste0(breaks*100, "%")) +
    theme_minimal()
}


