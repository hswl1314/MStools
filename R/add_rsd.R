#' Calculate Row-wise Relative Standard Deviation (RSD)
#'
#' @param df A dataframe containing numeric values or character values that can be converted to numeric
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
#'   QC1 = c("10.2", "NF", "15.6"),
#'   QC2 = c("11.3", "12.4", "NF"),
#'   QC3 = c("13.8", "14.2", "15.1"),
#'   row.names = c("Sample1", "Sample2", "Sample3")
#' )
#'
#' # Example 1: Basic usage with all columns
#' result1 <- add_rsd(test_df)
#'
#' # Example 2: Use specific columns
#' result2 <- add_rsd(test_df, cols = c(1,3))
#'
#' # Example 3: Custom decimal places and non-percentage format
#' result3 <- add_rsd(test_df, decimal_places = 3, as_percentage = FALSE)
#'
#' # Example 4: Custom RSD column name
#' result4 <- add_rsd(test_df, rsd_name = "CV%")
#'
#' # Example 5: Use pattern matching
#' result5 <- add_rsd(test_df, col_pattern = "^QC")
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
  
  # Internal RSD calculation function
  calculate_rsd <- function(x) {
    # Convert to numeric if needed
    if (!is.numeric(x)) {
      x[x == "NF"] <- NA  # Replace "NF" with NA
      x <- as.numeric(as.character(x))
    }
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
