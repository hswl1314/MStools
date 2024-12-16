#' Calculate When Your LC-MS Analysis Will End
#'
#' This function helps you calculate when your LC-MS analysis will finish based on 
#' the number of samples and time per sample. It supports both automatic (current system time) 
#' and manual time input.
#'
#' @param sample_count Numeric. The total number of samples to be analyzed.
#' @param minutes_per_sample Numeric. The time needed for each sample analysis in minutes.
#' @param start_time Character or NULL. The start time for analysis. Can be in format
#'        "YYYY-MM-DD HH:MM" or "HH:MM". If NULL, current system time will be used.
#'
#' @return A list containing:
#' \itemize{
#'   \item start_time: POSIXct object of the analysis start time
#'   \item end_time: POSIXct object of the analysis end time
#'   \item total_hours: Numeric value of total hours needed
#'   \item sample_count: Number of samples
#'   \item minutes_per_sample: Minutes per sample
#' }
#'
#' @examples
#' # Using current system time
#' when_you_add_samples(sample_count = 75, minutes_per_sample = 13)
#'
#' # Using specific date and time
#' when_you_add_samples(
#'   sample_count = 75,
#'   minutes_per_sample = 13,
#'   start_time = "2024-01-20 09:00"
#' )
#'
#' # Using only time (will use today's date)
#' when_you_add_samples(
#'   sample_count = 75,
#'   minutes_per_sample = 13,
#'   start_time = "09:00"
#' )
#'
#' @export
when_you_add_samples <- function(sample_count, 
                                minutes_per_sample, 
                                start_time = NULL) {
  # Handle start time
  if (is.null(start_time)) {
    start_time <- Sys.time()
  } else {
    # Handle time-only input
    if (grepl("^\\d{1,2}:\\d{2}$", start_time)) {
      today_date <- format(Sys.Date(), "%Y-%m-%d")
      start_time <- paste(today_date, start_time)
    }
    
    # Convert to POSIXct
    tryCatch({
      start_time <- as.POSIXct(start_time)
      if (is.na(start_time)) {
        stop("Unable to parse time format")
      }
    }, error = function(e) {
      stop("Invalid time format! Please use 'YYYY-MM-DD HH:MM' or 'HH:MM' format")
    })
  }
  
  # Calculate total minutes and end time
  total_minutes <- sample_count * minutes_per_sample
  end_time <- start_time + total_minutes * 60
  total_hours <- round(total_minutes/60, 2)
  
  # Print formatted output
  cat("\n=== LC-MS Run Time Calculation ===\n")
  cat("Number of samples:", sample_count, "\n")
  cat("Time per sample:", minutes_per_sample, "minutes\n")
  cat("Start time:", format(start_time, "%Y-%m-%d %H:%M"), "\n")
  cat("End time:", format(end_time, "%Y-%m-%d %H:%M"), "\n")
  cat("Total time needed:", total_hours, "hours\n")
  cat("==============================\n")
  
  # Return results invisibly
  invisible(list(
    start_time = start_time,
    end_time = end_time,
    total_hours = total_hours,
    sample_count = sample_count,
    minutes_per_sample = minutes_per_sample
  ))
}