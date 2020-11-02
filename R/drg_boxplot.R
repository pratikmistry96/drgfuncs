#' Create DRG Boxplot
#'
#' This function takes a data frame containing Average Medicare Payments, Average Total Payments, and Average Covered
#' Charges for various hospital charges organized by DRG medical procedure codes, and it creates and returns a plot of
#' boxplots for each DRG code for one of the data columns.
#'
#' @param data  Data frame containing data from data.cms.gov. The data can be loaded from the package
#' using the command "data(DRG_data)"
#' @param colOpt Desired column for calculation a a string: "Average.Medicare.Payments", "Average.Covered.Charges",
#' or "Average.Total.Payments"
#'
#' @return A ggplot object of a boxplot of medical procedure payments by DRG code
#' @export
#'
#' @import ggplot2
#' @import stringr
#' @import dplyr
#'
#' @examples
#' ~ Load Data ~
#' data(DRG_data)
#'
#' drg_boxplot(data = DRG_data,
#'             colOpt = "Average.Covered.Charges")
#'
drg_boxplot <- function(data, colOpt) {
  ## Create a vector containing valid column names, if the user doesn't select the correct column. It will throw an
  ## error
  colVec <- c(
    "Average.Covered.Charges",
    "Average.Total.Payments",
    "Average.Medicare.Payments"
  )
  ## The if-statement will check to see if the user has input an appropriate column name
  if (colOpt %in% colVec) {
    plt <- data %>% ## Store the plot in a variable plt
      mutate(DRG = str_extract(DRG.Definition, "[0-9]*")) %>% ## Create a DRG column containing the DRG code without
      ## the definition
      group_by(DRG) %>% ## Group by DRG code
      ## Enter the values from the plot x = DRG code, and y = the payment option selected by the user
      ggplot(aes_string(x = "DRG",
                        y = colOpt)) +
      geom_boxplot(outlier.size = 0.1) + ## Adjust the size of the outlier points
      ## Create the title, add an x and y label
      labs(
        title = paste0(gsub(".", " ", colOpt, fixed = TRUE), " by DRG Code"), ## Add the plot title
        x = "Medical Procedure (DRG)", ## Add x-label
        y = "Log - Payment ($)" ## Add y-label
      ) +
      theme(
        axis.text.x = element_text(size = 5, ## Adjust the axis text
                                   angle = 90,
                                   hjust = 1)
      ) +
      scale_y_log10(labels = function(x) format(x, scientific = FALSE)) ## Log Scale the y-axis for better visualization
    return(plt) ## Return the plot
  } else{
    stop("Invalid Payment Column") ## Error message is the user inputs the wrong
  }
}
