#' Calculate Row-wise Relative Standard Deviation (RSD)
#'
#' @param df A dataframe containing numeric values
#' @param decimal_places Integer specifying the number of decimal places (default = 2)
#' @param as_percentage Logical indicating whether to format output as percentage (default = TRUE)
#' @param cols Numeric vector specifying which columns to use (e.g., c(1,3,5) or 2:6)
#' @param col_pattern Character string specifying pattern to match in column names (default = NULL)
#' @param exclude_pattern Character string specifying pattern to exclude in column names (default = NULL)
#' @param rsd_name Character string specifying the name of the RSD column (default = "RSD")
#'
#' @return A dataframe with an additional column named by rsd_name containing the calculated values
#'
#' @examples
#' # Create example data
#' test_df <- data.frame(
#'   Sample1_val = c(10.2, 12.4, 15.6, 11.3, 13.8, 12.1),
#'   Sample1_sd = c(1.1, 1.2, 1.5, 1.1, 1.3, 1.2),
#'   Sample2_val = c(20.5, 22.7, 19.9, 21.2, 23.4, 21.8),
#'   Sample2_sd = c(2.1, 2.2, 1.9, 2.0, 2.3, 2.1),
#'   Control_val = c(30.1, 33.3, 31.5, 32.8, 34.2, 32.5),
#'   Control_sd = c(3.0, 3.3, 2.8, 3.1, 3.4, 3.2),
#'   Test_A = c(40.5, 43.2, 41.8, 42.6, 44.1, 42.9),
#'   Test_B = c(50.3, 53.7, 51.9, 52.4, 54.8, 52.9),
#'   QC_1 = c(60.1, 62.4, 61.8, 61.5, 63.2, 61.9),
#'   QC_2 = c(70.2, 73.1, 71.5, 72.3, 74.1, 72.8),
#'   Notes = c("a", "b", "c", "d", "e", "f")
#' )
#'
#' # Example 1: Basic usage with specific columns
#' result1 <- add_rsd(test_df, cols = c(1,3,5))
#'
#' # Example 2: Use column range
#' result2 <- add_rsd(test_df, cols = 7:10)  # Using Test_A, Test_B, QC_1, QC_2
#'
#' # Example 3: Include only columns with "val" pattern
#' result3 <- add_rsd(test_df, col_pattern = "val")
#'
#' # Example 4: Exclude columns containing "sd" or "Notes"
#' result4 <- add_rsd(test_df, exclude_pattern = "_sd|Notes")
#'
#' # Example 5: Include columns starting with "Test" or "QC"
#' result5 <- add_rsd(test_df, col_pattern = "^(Test|QC)")
#'
#' # Example 6: More complex pattern matching (columns ending with "val" or starting with "QC")
#' result6 <- add_rsd(test_df, col_pattern = "(_val$|^QC)")
#'
#' # Example 7: Custom decimal places and non-percentage format
#' result7 <- add_rsd(test_df, cols = c(1,3,5), decimal_places = 3, as_percentage = FALSE)
#'
#' # Example 8: Custom RSD column name
#' result8 <- add_rsd(test_df, cols = c(1,3,5), rsd_name = "CV%")
#'
#' # Example 9: Combine include and exclude patterns
#' result9 <- add_rsd(test_df, col_pattern = "Sample", exclude_pattern = "_sd")
#'
#' # Example 10: Use all numeric columns (excluding Notes)
#' result10 <- add_rsd(test_df, exclude_pattern = "Notes")
#'
#' @export
add_rsd <- function(df, decimal_places = 2, as_percentage = TRUE, 
                   cols = NULL, col_pattern = NULL, exclude_pattern = NULL,
                   rsd_name = "RSD") {
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  if (ncol(df) < 2) {
    stop("Dataframe must contain at least 2 columns to calculate RSD")
  }
  
  # Select columns for calculation first
  if (!is.null(cols)) {
    if (length(cols) < 2) {
      stop("At least 2 columns required for RSD calculation")
    }
    selected_cols <- df[, cols, drop = FALSE]
  } else if (!is.null(col_pattern) || !is.null(exclude_pattern)) {
    # Get all column names
    all_cols <- names(df)
    
    if (!is.null(col_pattern)) {
      # Keep columns matching the pattern
      matching_cols <- grep(col_pattern, all_cols, value = TRUE)
    } else {
      matching_cols <- all_cols
    }
    
    if (!is.null(exclude_pattern)) {
      # Remove columns matching the exclude pattern
      matching_cols <- matching_cols[!grepl(exclude_pattern, matching_cols)]
    }
    
    if (length(matching_cols) < 2) {
      stop("Selected pattern must match at least 2 columns")
    }
    selected_cols <- df[, matching_cols, drop = FALSE]
  } else {
    selected_cols <- df  # Use all columns if nothing specified
  }
  
  # Check for "NF" values and non-numeric data in selected columns only
  has_nf <- any(selected_cols == "NF", na.rm = TRUE)
  is_not_numeric <- !all(sapply(selected_cols, function(x) is.numeric(x) || all(is.na(x))))
  
  if (has_nf || is_not_numeric) {
    message('Please run the following code to process your data first:\n',
           'df_numeric <- data.frame(lapply(df, function(x) {\n',
           '    x[x == "NF"] <- NA\n',
           '    as.numeric(as.character(x))\n',
           '}))')
    stop("Selected columns contain 'NF' values or non-numeric data. Please convert data types first.")
  }
  
  # Internal RSD calculation function
  calculate_rsd <- function(x) {
    if (all(is.na(x))) return(NA)
    (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100
  }
  
  # Calculate RSD for all rows using selected columns
  rsd_values <- apply(selected_cols, 1, calculate_rsd)
  
  # Format output based on parameters
  if (as_percentage) {
    df[[rsd_name]] <- sprintf(paste0("%.", decimal_places, "f%%"), rsd_values)
  } else {
    df[[rsd_name]] <- round(rsd_values, decimal_places)
  }
  
  return(df)
}
