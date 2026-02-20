## Creates lazy data for EPUs

create_comland_gears <- function() {
  otter <- 50:59
  dredge.sc <- 131:132
  pot <- c(180:190, 200:219, 300, 301)
  longline <- c(10, 40)
  seine <- c(70:79, 120:129, 360)
  gillnet <- c(100:119, 500, 510, 520)
  midwater <- c(170, 370)
  dredge.o <- c(281, 282, 380:400)

  GEARs <- list(
    otter = otter,
    dredge.sc = dredge.sc,
    pot = pot,
    longline = longline,
    seine = seine,
    gillnet = gillnet,
    midwater = midwater,
    dredge.o = dredge.o
  )

  usethis::use_data(GEARs)
}
