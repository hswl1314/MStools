#' Check PCA Data Range
#'
#' This function checks the range of PC1 and PC2 in PCA analysis to help set appropriate plot limits.
#'
#' @param df A data frame containing the variables for PCA analysis
#' @param var_names Optional character vector specifying the names of variables to be used in PCA. 
#'                 If NULL, all numeric columns except 'Group' and 'SampleID' will be used.
#' @param digits Number of decimal places to round the results (default: 2)
#'
#' @return A list containing the ranges of PC1 and PC2, and prints the ranges to console
#' @export
#'
#' @examples
#' \dontrun{
#' # Create example data
#' df_example <- data.frame(
#'   SampleID = paste0("Sample_", 1:10),
#'   Group = rep(c("A", "B"), each = 5),
#'   Var1 = rnorm(10),
#'   Var2 = rnorm(10)
#' )
#'
#' # Check PCA ranges
#' ranges <- pca_check(df_example)
#' }
pca_check <- function(df, var_names = NULL, digits = 2) {
  # Automatically get variable names if not specified
  if (is.null(var_names)) {
    var_names <- setdiff(colnames(df), c("Group", "SampleID"))
  }
  
  # Validate that selected columns are numeric
  non_numeric_cols <- names(which(sapply(df[var_names], function(x) !is.numeric(x))))
  if (length(non_numeric_cols) > 0) {
    stop("The following variables are not numeric: ", paste(non_numeric_cols, collapse = ", "))
  }
  
  # Perform PCA analysis
  pca_test <- summary(vegan::rda(dplyr::select(df, dplyr::all_of(var_names)), scale = TRUE))
  sites_test <- data.frame(pca_test$sites)
  
  # Calculate ranges
  x_range <- range(sites_test$PC1)
  y_range <- range(sites_test$PC2)
  
  # Round the values
  x_range <- round(x_range, digits)
  y_range <- round(y_range, digits)
  
  # Print results
  cat("PCA Ranges:\n")
  cat(sprintf("X range (PC1): %g to %g\n", x_range[1], x_range[2]))
  cat(sprintf("Y range (PC2): %g to %g\n", y_range[1], y_range[2]))
  
  # Calculate explained variance
  explained_var <- pca_test$cont$importance[2, 1:2] * 100
  cat(sprintf("\nExplained variance:\n"))
  cat(sprintf("PC1: %.2f%%\n", explained_var[1]))
  cat(sprintf("PC2: %.2f%%\n", explained_var[2]))
  cat(sprintf("Total: %.2f%%\n", sum(explained_var)))
  
  # Return results as a list
  return(list(
    x_range = x_range,
    y_range = y_range,
    explained_variance = explained_var,
    suggested_limits = list(
      x_limits = c(floor(x_range[1]), ceiling(x_range[2])),
      y_limits = c(floor(y_range[1]), ceiling(y_range[2]))
    )
  ))
}