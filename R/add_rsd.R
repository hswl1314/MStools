#' Calculate Row-wise Relative Standard Deviation (RSD)
#'
#' @param df A dataframe containing numeric values
#' @param decimal_places Integer specifying the number of decimal places (default = 2)
#' @param as_percentage Logical indicating whether to format output as percentage (default = TRUE)
#' @param cols Numeric vector specifying which columns to use (e.g., c(1,3,5) or 2:6)
#' @param col_pattern Character string specifying pattern to match in column names (default = NULL)
#'
#' @return A dataframe with an additional column named 'RSD' containing the calculated values
#'
#' @examples
#' test_df <- data.frame(
#'   A = c(10, 12, 15, 11, 13),
#'   B = c(20, 22, 19, 21, 23),
#'   C = c(30, 33, 31, 32, 34)
#' )
#' # Basic usage with specific columns
#' result1 <- add_rsd(test_df, cols = c(1,3))
#' # Use a range of columns
#' result2 <- add_rsd(test_df, cols = 2:3)
#' # Use columns matching pattern
#' result3 <- add_rsd(test_df, col_pattern = "^[AB]")
#'
#' @export
add_rsd <- function(df, decimal_places = 2, as_percentage = TRUE, 
                   cols = NULL, col_pattern = NULL) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  if (ncol(df) < 2) {
    stop("Dataframe must contain at least 2 columns to calculate RSD")
  }
  
  # Select columns for calculation
  if (!is.null(cols)) {
    if (length(cols) < 2) {
      stop("At least 2 columns required for RSD calculation")
    }
    selected_cols <- df[, cols, drop = FALSE]
  } else if (!is.null(col_pattern)) {
    matching_cols <- grep(col_pattern, names(df), value = TRUE)
    if (length(matching_cols) < 2) {
      stop("Pattern must match at least 2 columns")
    }
    selected_cols <- df[, matching_cols, drop = FALSE]
  } else {
    selected_cols <- df  # Use all columns if nothing specified
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
    df$RSD <- sprintf(paste0("%.", decimal_places, "f%%"), rsd_values)
  } else {
    df$RSD <- round(rsd_values, decimal_places)
  }
  
  return(df)
}

# 使用示例:
# test_df <- data.frame(
#   A = c(10, 12, 15, 11, 13),
#   B = c(20, 22, 19, 21, 23),
#   C = c(30, 33, 31, 32, 34),
#   D = c(40, 43, 41, 42, 44),
#   E = c(50, 53, 51, 52, 54)
# )

# 使用特定列(1,3,5列)
# result1 <- add_rsd(test_df, cols = c(1,3,5))

# 使用列范围(2至4列)
# result2 <- add_rsd(test_df, cols = 2:4)

# 使用列名模式匹配(匹配A和B开头的列)
# result3 <- add_rsd(test_df, col_pattern = "^[AB]")

# 指定小数位数和非百分比格式
# result4 <- add_rsd(test_df, cols = c(1,3,5), decimal_places = 3, as_percentage = FALSE)