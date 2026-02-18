#' Compare the new FRED dataset with the old
#' determine wha changes

compare_fred_data <- function(old, new) {
  #######################################################
  #######################################################
  ## Checks on dimension
  #######################################################
  #######################################################

  output <- list()
  if (isTRUE(all.equal(old, new))) {
    # No change
    output$isEqual <- T
  } else {
    # Changes found
    output$isEqual <- F
    # check number of rows
    output$rowNum$new <- nrow(new)
    output$rowNum$old <- nrow(old)
    # remove new rows and compare
    if (nrow(new) > nrow(old)) {
      newnew <- new |>
        head(-(nrow(new) - nrow(old)))
      if (isTRUE(all.equal(newnew, old))) {
        # just new rows added
        output$newRows <- T
      } else {
        output$newRows <- F
        # create plots
        d <- new |>
          dplyr::left_join(old, by = c("YEAR", "MONTH")) |>
          dplyr::rename(old = value.y, new = value.x) |>
          tidyr::pivot_longer(
            cols = -c(MONTH, YEAR),
            names_to = "source",
            values_to = "value"
          ) |>
          dplyr::mutate(
            date = as.Date(paste0(YEAR, "-", MONTH, "-01", sep = ""))
          )

        p <- ggplot2::ggplot(data = d) +
          ggplot2::geom_line(
            ggplot2::aes(x = date, y = value, color = source),
            na.rm = T
          ) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(
              angle = 90,
              hjust = 1,
              vjust = 0.5
            )
          )
        output$ggplot <- p
      }
    }
  }

  return(output)
}
