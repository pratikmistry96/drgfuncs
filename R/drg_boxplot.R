#' Create DRG Plot
#'
#' This function takes a data frame containing Average Medicare Payments, Average Total Payments, and Average Covered
#' Charges for various hospital charges organized by DRG medical procedure codes, and it creates and returns a plot of
#' boxplots for each DRG code for one of the data columns.
#'
#' @param data  Data frame containing data from data.cms.gov
#' @param option Desired column for calculation a a string: "Average.Medicare.Payments", "Average.Covered.Charges", or "Average.Total.Payments"
#'
#' @return A ggplot object of a boxplot of payments by DRG code
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom stringr str_extract
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#'
#' @examples
#' data(drg_data)
#'
#' drg_plot(drg_data, "Average.Covered.Charges")
#'
drg_boxplot <- function(data, option) {
  ## Create a vector containing valid column names, if the user doesn't select the correct column. It will throw an
  ## error
  opt <- c(
    "Average.Covered.Charges",
    "Average.Total.Payments",
    "Average.Medicare.Payments"
  )
  ## The if-statement will check to see if the user has input an appropriate column name
  if (option %in% opt) {
    plt <- data %>% ## Store the plot in a variable plt
      mutate(DRG = str_extract(DRG.Definition, "[0-9]*")) %>% ## Create a DRG column containing the DRG code without
      ## the definition
      group_by(DRG) %>% ## Group by DRG code
      ## Enter the values from the plot x = DRG code, and y = the payment option selected by the user
      ggplot(aes_string(x = "DRG",
                        y = option)) +
      geom_boxplot(outlier.size = 0.1) + ## Adjust the size of the outlier points
      ## Create the title, add an x and y label
      labs(
        title = paste0(gsub(".", " ", option, fixed = TRUE), " by DRG Code"),
        x = "Medical Procedure (DRG)",
        y = "Log - Payment ($)"
      ) +
      theme(
        axis.text.x = element_text(size = 6, ## Adjust the axis text
                                   angle = 90,
                                   hjust = 1)
      ) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      scale_y_log10()
    return(plt) ## Return the plot
  } else{
    stop("Invalid Payment Column") ## Error message is the user inputs the wrong
  }
}
