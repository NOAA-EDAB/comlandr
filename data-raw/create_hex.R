#' create hex sticker
#'
#'
#'
#'
#'

create_hex <- function() {
  imgurl <- here::here("data-raw", "5.FISHING.png")
  hexSticker::sticker(
    imgurl,
    package = "comlandr",
    p_size = 24,
    s_x = 1,
    s_y = 1,
    s_width = 1.28,
    p_x = 1,
    p_y = 1.2,
    h_size = 1,
    h_fill = "#FFFFFF",
    h_color = "#bc4700",
    #angle = 30,
    spotlight = T,
    l_x = 1.5,
    l_y = 1.1,
    l_width = 3,
    l_height = 3,
    l_alpha = 0.5,
    u_x = .99,
    u_y = .05,
    url = "noaa-edab.github.io/comlandr",
    u_size = 5.5,
    white_around_sticker = T,
    #filename = here::here("man/figures", "logo.png"),
    filename = here::here("data-raw", "logo.png")
  )
}
