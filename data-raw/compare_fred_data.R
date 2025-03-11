#' Compare the new FRED dataset with the old
#' determine wha changes

compare_fred_data <- function(current,new) {


  #######################################################
  #######################################################
  ## Checks on dimension
  #######################################################
  #######################################################

  output <- list()
  if (all.equal(current,new)) {
    # No change
    output$isEqual <- T
  } else {
    # Changes found
    output$isEqual <- F
    # check number of rows
    output$rowNum <- c(nrow(new),nrow(current))
    # remove new rows and compare
    if(nrow(new) > nrow(current)) {
      newnew <- new |>
        head(-(nrow(new)-nrow(current)))
      if(all.equal(newnew,current)) {
        # just new rows added
        output$newRows <- T
      } else {
        # create plots
        # new |>
        #   dplyr::left_join(current, by = c("YEAR","MONTH"))
        #
        # p <- ggplot2::ggplot(data=current)
        #
        # output$ggplot <- p

      }
    }
  }


  return(output)

}
