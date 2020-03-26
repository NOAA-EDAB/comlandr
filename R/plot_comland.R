#plots of comlands data
library(ggplot2); library(data.table)

comland.gear <- comland[, .(landings = sum(SPPLIVMT)), by = c('YEAR', 'GEAR')]

theme_ts <- function(...){
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    legend.key = element_blank(),
    axis.title = element_text(size = 10)
  )
}

theme_facet <- function(...){
  theme(
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    legend.key = element_blank(),
    axis.title = element_text(size = 10)
  )
}

ggplot(comland.gear) +
  geom_line(aes(x = YEAR,
                y = landings)) +
  facet_wrap(vars(GEAR), scales = 'free') + 
  labs(y = 'Landings, metric tons',
       x = 'Year') +
  theme_facet() +
  theme(plot.margin = margin(r = 10, l = 10))
