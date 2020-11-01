#' DRG Code Medicare Statistics
#'
#' This function takes a data frame containing Average Medicare Payments, Average Total Payments, and Average Covered
#' Charges for various hospital charges organized by DRG medical procedure codes, and it calculates the mean,
#' standard deviation, or median for the Average Medicare Payments by DRG code
#'
#' @param data Data frame containing data from data.cms.gov
#' @param stat Desired calculation statistic as a string: "mean", "median", or "sd"
#'
#' @return A data frame containing the statistics by DRG code for Average Medicare Payments
#' @export
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#'
#'
#' @examples
#' data(drg_data)
#'
#' mean_medicare <- drg_stats(data = drg_data,
#'                                 stat = "mean")
#'
drg_stats_medicare <- function(data, stat) {
  stat_opt <- ## Create a vector containing valid statistics to use for comparison of inpit
    c(
      "mean",
      "median",
      "sd")
  FUN <- get(stat) ## Get the object from the string
  colname <- paste0(str_to_title(stat),".Average.Medicare.Payments") ## Create the new column name adding the stat calculated
  if (stat %in% stat_opt) { ## Check to see if the statistics calculation is valid
    drg_stat <- data %>%
      group_by(DRG.Definition) %>% ## Group data by DRG Definitions
      summarize(!!colname := FUN(Average.Medicare.Payments)) ## Calculate the statistic and store in new variable
    return(drg_stat) ## Return the new data frame
  } else{
    stop("Not a valid option for statistics calculation") ## Throw an error for invalid statistics
  }
}
