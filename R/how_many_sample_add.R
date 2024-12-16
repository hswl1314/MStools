#' Calculate Maximum Number of Samples That Can Be Added
#'
#' This function calculates how many samples can be added before a deadline based on 
#' the time needed per sample. It supports flexible time input formats and automatically 
#' handles date calculations.
#'
#' @param end_time Character. The deadline time in either:
#'        - "HH:MM" format (e.g., "17:00", will use system date)
#'        - "YYYY-MM-DD HH:MM" format (e.g., "2024-01-21 17:00")
#' @param minutes_per_sample Numeric. The time needed for each sample analysis in minutes.
#' @param start_time Character, POSIXct, or NULL. The start time in either:
#'        - "HH:MM" format (e.g., "09:00", will use system date)
#'        - "YYYY-MM-DD HH:MM" format (e.g., "2024-01-20 09:00")
#'        - Sys.time() for current system time
#'        - NULL (will use current system time)
#'
#' @return A list containing:
#' \itemize{
#'   \item start_time: POSIXct object of the analysis start time
#'   \item end_time: POSIXct object of the deadline
#'   \item available_hours: Numeric value of available hours
#'   \item max_samples: Integer number of maximum samples possible
#'   \item minutes_per_sample: Minutes per sample
#' }
#'
#' @examples
#' # Example 1: Using current system time as start time
#' how_many_sample_add(
#'   end_time = "17:00",
#'   minutes_per_sample = 13
#' )
#'
#' # Example 2: Using time-only format (will use system date)
#' how_many_sample_add(
#'   end_time = "17:00",
#'   minutes_per_sample = 13,
#'   start_time = "09:00"
#' )
#'
#' # Example 3: Using full datetime format
#' how_many_sample_add(
#'   end_time = "2024-01-21 17:00",
#'   minutes_per_sample = 13,
#'   start_time = "2024-01-20 09:00"
#' )
#'
#' # Example 4: Using system time explicitly
#' how_many_sample_add(
#'   end_time = "17:00",
#'   minutes_per_sample = 13,
#'   start_time = Sys.time()
#' )
#'
#' @export
how_many_sample_add <- function(end_time,
                               minutes_per_sample,
                               start_time = NULL) {
  
  # Function to process time input
  process_time <- function(time_str, time_type = "start") {
    # Handle NULL or POSIXct input for start time
    if (time_type == "start") {
      if (is.null(time_str)) {
        return(Sys.time())
      }
      if (inherits(time_str, "POSIXct")) {
        return(time_str)
      }
    } else if (is.null(time_str)) {
      stop("End time cannot be NULL")
    }
    
    # Get current system date
    current_date <- format(Sys.Date(), "%Y-%m-%d")
    
    # Process time string
    if (grepl("^\\d{1,2}:\\d{2}$", time_str)) {
      # For HH:MM format, combine with current system date
      time_str <- paste(current_date, time_str)
    } else if (!grepl("^\\d{4}-\\d{2}-\\d{2} \\d{1,2}:\\d{2}$", time_str)) {
      stop(paste("Invalid", time_type, 
                "time format. Use either 'HH:MM' or 'YYYY-MM-DD HH:MM'"))
    }
    
    # Convert to POSIXct
    result <- as.POSIXct(time_str)
    if (is.na(result)) {
      stop(paste("Invalid", time_type, "time value"))
    }
    
    return(result)
  }
  
  # Process times
  start_time_processed <- process_time(start_time, "start")
  end_time_processed <- process_time(end_time, "end")
  
  # Check if end time is after start time
  if (end_time_processed <= start_time_processed) {
    # If using time-only format and end time is earlier, add one day to end time
    if (grepl("^\\d{1,2}:\\d{2}$", end_time)) {
      end_time_processed <- end_time_processed + 24*60*60
    } else {
      stop("End time must be later than start time!\n",
           "Start time: ", format(start_time_processed, "%Y-%m-%d %H:%M"), "\n",
           "End time: ", format(end_time_processed, "%Y-%m-%d %H:%M"))
    }
  }
  
  # Calculate available time and maximum samples
  available_minutes <- as.numeric(difftime(end_time_processed, 
                                         start_time_processed, 
                                         units = "mins"))
  max_samples <- floor(available_minutes / minutes_per_sample)
  available_hours <- round(available_minutes / 60, 2)
  
  # Print formatted output
  cat("\n=== Sample Capacity Calculation ===\n")
  cat("Start time:", format(start_time_processed, "%Y-%m-%d %H:%M"), "\n")
  cat("End time:", format(end_time_processed, "%Y-%m-%d %H:%M"), "\n")
  cat("Time per sample:", minutes_per_sample, "minutes\n")
  cat("Available time:", available_hours, "hours\n")
  cat("Maximum samples possible:", max_samples, "\n")
  cat("================================\n")
  
  # Return results invisibly
  invisible(list(
    start_time = start_time_processed,
    end_time = end_time_processed,
    available_hours = available_hours,
    max_samples = max_samples,
    minutes_per_sample = minutes_per_sample
  ))
}