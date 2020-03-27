#' Theme Template
#'
#'#'@importFrom ggplot2 "element_blank" "element_rect" "element_text"
#'@noRd

theme_comland <- function(...){
  ggplot2::theme(
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    panel.spacing.x = unit(10, "points"),
    panel.spacing.y = unit(10, "points"),
    legend.key = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(r = 10, l = 10)
  )
}

#'plots of comlands data
#'
#'@param data data frame. Output from \code{comland.r}
#'@param by The variable within \code{data} that you want to plot by
#'@param type Plot either 'landings' or 'value'.
#'@param range Range of years to plot.  Default value plots all years.
#'@param free.y Allow the y-axis to rescale per "By" group.
#'
#'@importFrom ggplot2 "aes" "vars" "margin"
#'@importFrom data.table "setnames"
#'
#'@export


plot_comland <- function(data, by, type = 'landings', range = NA, free.y = T) {
  
  #Sum landings/value and assign "By" category
  comland.sum <- data[, .(landings = sum(SPPLIVMT), value = sum(SPPVALUE)), 
                      by = c('YEAR', by, 'EPU')]
  if(!is.na(range[1])) comland.sum <- comland.sum[YEAR %in% range, ]
  data.table::setnames(comland.sum, c(by, type), c('By', 'yvar'))
  
  #Remove scientific notation from axis
  format.axis <- function(x) x / 10e3
  
  #Plot
  if(type == 'landings') ylab <- expression('Landings, metric tons 10'^3)
  if(type == 'value') ylab <- expression('Value, US dollars 10'^3)
  
  if(free.y == T){
    g <- ggplot2::ggplot(comland.sum) +
      ggplot2::geom_line(aes(x = YEAR,
                             y = yvar)) +
      ggplot2::facet_grid(By ~ EPU, scale = 'free_y') +
      ggplot2::scale_y_continuous(labels = format.axis) +
      ggplot2::labs(y = ylab, x = 'Year') +
      theme_comland()
  }
  
  if(free.y == F){
    g <- ggplot2::ggplot(comland.sum) +
      ggplot2::geom_line(aes(x = YEAR,
                             y = yvar)) +
      ggplot2::facet_grid(By ~ EPU) +
      ggplot2::scale_y_continuous(labels = format.axis) +
      ggplot2::labs(y = expression('Landings, metric tons 10'^3), x = 'Year') +
      theme_comland()
  }
  
  #plot figure
  plot(g)
}




