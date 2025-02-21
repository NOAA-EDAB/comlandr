#' Theme Template
#'
#'
#'@importFrom ggplot2 "element_blank" "element_rect" "element_text"
#'
#'@noRd

theme_comland <- function(...){
  ggplot2::theme(
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    panel.spacing.x = ggplot2::unit(10, "points"),
    panel.spacing.y = ggplot2::unit(10, "points"),
    legend.key = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = ggplot2::margin(r = 10, l = 10)
  )
}

#'Plots comlands data
#'
#'Comland data landings or the value of landings are plotted for each EPU over time by either \code{GEAR} type, Quarter Year (\code{QY}), vessel \code{SIZE}, or location of landings (\code{US}/nonUS)
#'
#'@param data Data frame. Output from \code{comland.r}
#'@param by Character string. The variable within \code{data} that you want to plot by (GEAR, QY, SIZE, US)
#'@param type Character string. Plot either 'landings' (default) or 'value'.
#'@param range Numeric vector. Range of years to plot.  Default value plots all years.
#'@param free.y Boolean. Allow the y-axis to rescale per "By" group (Default = T)
#'
#'@importFrom ggplot2 "aes" "vars" "margin"
#'@importFrom data.table "setnames"
#'
#'
#'@noRd


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




