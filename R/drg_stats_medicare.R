#' DRG Code Medicare Statistics
#'
#' This function takes a data frame containing Average Medicare Payments for different medical procedures and their
#' DRG codes at various hospitals, and it calculates a either the standard deviation, mean, or median for each DRG
#' code and returns a data frame for further manipulation.
#'
#' @param data Data frame containing data from data.cms.gov. The data can be loaded from the package using the
#' command "data(DRG_data)
#' @param stat Desired calculation statistic as a string: "mean", "median", or "sd"
#'
#' @return A data frame containing the statistics for medical procedures by DRG code for Average Medicare Payments
#' @export
#'
#' @import dplyr
#' @import stringr
#'
#'
#' @examples
#' data(DRG_data)
#'
#' mean_medicare <- drg_stats_medicare(data = DRG_data,
#'                                 stat = "mean")
#'
drg_stats_medicare <- function(data, stat) {
  statVec <- ## Create a vector containing valid statistics to use for comparison of input
    c(
      "mean",
      "median",
      "sd")
  FUN <- get(stat) ## Get the object from the string
  colname <- paste0(str_to_title(stat),".Average.Medicare.Payments") ## Create the new column name adding the stat calculated
  if (stat %in% statVec) { ## Check to see if the statistics calculation is valid
    drg_stat <- data %>%
      group_by(DRG.Definition) %>% ## Group data by DRG Definitions
      summarize(!!colname := FUN(Average.Medicare.Payments)) ## Calculate the statistic and store in new variable
    return(drg_stat) ## Return the new data frame
  } else{
    stop("Not a valid option for statistics calculation") ## Throw an error for invalid statistics
  }
}
