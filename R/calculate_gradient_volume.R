#' Calculate Mobile Phase Volumes for Gradient Elution
#'
#' This function calculates the required volumes of mobile phases A and B
#' for gradient HPLC/LC-MS analysis based on the gradient program.
#'
#' @param data A data frame containing the gradient program with columns:
#'   \itemize{
#'     \item Time: Time points in minutes
#'     \item Flow: Flow rate in mL/min
#'     \item B: Percentage of mobile phase B
#'   }
#' @param sample_number Numeric. Number of samples to be analyzed (default = 1)
#' @param extra_volume Numeric. Factor for additional volume preparation (default = 1.2)
#'
#' @return A list containing three elements:
#'   \itemize{
#'     \item detailed_data: Data frame with detailed calculations
#'     \item single_sample: Data frame with volumes required for one sample
#'     \item all_samples: Data frame with total volumes required for all samples
#'   }
#'
#' @examples
#' df <- data.frame(
#'   Time = c(0.000, 0.500, 7.500, 8.000, 9.000, 9.100, 12.000),
#'   Flow = c(0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50),
#'   B = c(95.0, 95.0, 65.0, 40.0, 40.0, 95.0, 95.0)
#' )
#' result <- calculate_gradient_volume(df, sample_number = 10)
#'
#' @export
calculate_gradient_volume <- function(data, sample_number = 1, extra_volume = 1.2) {
  # Input validation
  if (!is.numeric(sample_number) || sample_number <= 0) {
    stop("Sample number must be a positive number")
  }
  
  required_cols <- c("Time", "Flow", "B")
  if (!all(required_cols %in% colnames(data))) {
    stop("Data frame must contain columns: Time, Flow, B")
  }
  
  # Calculate time intervals
  data$Time_diff <- c(0, diff(data$Time))
  
  # Calculate average percentages for each time interval
  data$B_avg <- c(0, (data$B[-1] + data$B[-nrow(data)])/2)
  data$A_avg <- 100 - data$B_avg
  
  # Calculate volumes
  data$Volume <- data$Flow * data$Time_diff
  data$Volume_A <- data$Volume * data$A_avg / 100
  data$Volume_B <- data$Volume * data$B_avg / 100
  
  # Calculate total volumes for single sample
  total_volume_A <- sum(data$Volume_A)
  total_volume_B <- sum(data$Volume_B)
  total_volume <- total_volume_A + total_volume_B
  
  # Calculate required volumes for all samples
  required_volume_A <- total_volume_A * sample_number * extra_volume
  required_volume_B <- total_volume_B * sample_number * extra_volume
  required_total <- total_volume * sample_number * extra_volume
  
  # Prepare results
  results <- list(
    detailed_data = data,
    single_sample = data.frame(
      Phase = c("A", "B", "Total"),
      Volume = c(total_volume_A, total_volume_B, total_volume)
    ),
    all_samples = data.frame(
      Phase = c("A", "B", "Total"),
      Volume = c(required_volume_A, required_volume_B, required_total),
      Note = c(
        paste("Including", extra_volume, "x extra volume"),
        paste("Including", extra_volume, "x extra volume"),
        paste("Including", extra_volume, "x extra volume")
      )
    )
  )
  
  return(results)
}