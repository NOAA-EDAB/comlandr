#' Theme template
#'
#'@importFrom ggplot2 "element_blank" "element_rect" "element_text"
#'@noRd

#library(ggplot2); library(data.table)


theme_ts <- function(...){
  ggplot2::theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    legend.key = element_blank(),
    axis.title = element_text(size = 10)
  )
}

#' Theme Template
#'
#'#'@importFrom ggplot2 "element_blank" "element_rect" "element_text"
#'@noRd

theme_facet <- function(...){
  ggplot2::theme(
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    legend.key = element_blank(),
    axis.title = element_text(size = 10)
  )
}

#'plots of comlands data
#'
#'@param data data frame. Output from \code{comland.r}
#'
#'@importFrom ggplot2 "aes" "vars" "margin"
#'
#'@export


plot_comland <- function(data) {
  
  comland.gear <- data[, .(landings = sum(SPPLIVMT)), by = c('YEAR', 'GEAR', 'EPU')]
  
  format.axis <- function(x) x / 10e3
  
  g <- ggplot2::ggplot(comland.gear) +
    ggplot2::geom_line(aes(x = YEAR,
                           y = landings)) +
    ggplot2::facet_grid(GEAR ~ EPU, scale = 'free_y') +
    ggplot2::scale_y_continuous(labels = format.axis) +
    ggplot2::labs(y = expression('Landings, metric tons 10'^3), x = 'Year') +
    theme_facet() +
    ggplot2::theme(plot.margin = margin(r = 10, l = 10))
  
  plot(g)
}




