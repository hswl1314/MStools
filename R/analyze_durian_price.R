#' Durian Price Analysis Function
#'
#' This function analyzes durian prices and flesh weights based on different input parameters.
#' It can work in three modes:
#' 1. Calculate from durian weight: Given the weight of a whole durian, calculate the expected flesh weight and total price
#' 2. Calculate from flesh weight only: Given the flesh weight, calculate the possible durian weight range and price range
#' 3. Calculate from flesh parameters: Given any two of flesh weight, flesh price, and total flesh price, calculate the original durian price range
#'
#' @param price_per_500g Numeric. Price per 500g of durian (default: 29.9)
#' @param flesh_rate Numeric vector. Flesh rate range, can be a single value or a range (default: c(0.25, 0.40))
#' @param weight_jin Numeric. Durian weight in jin (default: 5)
#' @param flesh_weight_jin Numeric. Flesh weight in jin (default: NULL)
#' @param flesh_price Numeric. Flesh price per jin (default: NULL)
#' @param total_flesh_price Numeric. Total flesh price (default: NULL)
#'
#' @return A data frame containing the analysis results with the following columns:
#' \itemize{
#'   \item Parameter: The name of the parameter
#'   \item Value: The calculated value or range
#' }
#'
#' @export
#'
#' @examples
#' # Mode 1: Calculate from durian weight
#' # Calculate expected flesh weight and total price for a 5-jin durian
#' analyze_durian_price()
#'
#' # Calculate for a 8-jin durian with custom flesh rate
#' analyze_durian_price(weight_jin = 8, flesh_rate = c(0.30, 0.45))
#'
#' # Mode 2: Calculate from flesh weight
#' # Calculate possible durian weight range for 2 jin of flesh
#' analyze_durian_price(flesh_weight_jin = 2)
#'
#' # Mode 3: Calculate from flesh parameters
#' # Example 1: 2 jin of flesh at 60 yuan per jin
#' # This will calculate the total price and original durian price range
#' analyze_durian_price(flesh_weight_jin = 2, flesh_price = 60)
#'
#' # Example 2: 2 jin of flesh with total price of 120 yuan
#' # This will calculate the price per jin and original durian price range
#' analyze_durian_price(flesh_weight_jin = 2, total_flesh_price = 120)
#'
#' # Example 3: Flesh price of 60 yuan per jin with total price of 120 yuan
#' # This will calculate the flesh weight and original durian price range
#' analyze_durian_price(flesh_price = 60, total_flesh_price = 120)
#'
#' # Note: All weights are in jin (1 jin = 0.5 kg)
#' # The function will automatically handle unit conversions internally
analyze_durian_price <- function(
    price_per_500g = 29.9,    # Price per 500g
    flesh_rate = c(0.25, 0.40), # Flesh rate range
    weight_jin = 5,           # Durian weight in jin
    flesh_weight_jin = NULL,  # Flesh weight in jin
    flesh_price = NULL,       # Flesh price per jin
    total_flesh_price = NULL  # Total flesh price
) {
  # Process flesh rate parameter
  if (length(flesh_rate) == 1) {
    min_flesh_rate <- max_flesh_rate <- flesh_rate
  } else if (length(flesh_rate) == 2) {
    min_flesh_rate <- min(flesh_rate)
    max_flesh_rate <- max(flesh_rate)
  } else {
    stop("Flesh rate must be a single value or a vector of two values")
  }

  # Check flesh-related parameters
  flesh_params <- c(!is.null(flesh_weight_jin), !is.null(flesh_price), !is.null(total_flesh_price))

  if (sum(flesh_params) >= 2) {
    # Mode 3: Calculate from flesh parameters
    # Calculate missing parameter
    if (!is.null(flesh_weight_jin) && !is.null(flesh_price)) {
      # Calculate total price from weight and price
      total_flesh_price <- flesh_weight_jin * flesh_price
    } else if (!is.null(flesh_weight_jin) && !is.null(total_flesh_price)) {
      # Calculate price from weight and total price
      flesh_price <- total_flesh_price / flesh_weight_jin
    } else if (!is.null(flesh_price) && !is.null(total_flesh_price)) {
      # Calculate weight from price and total price
      flesh_weight_jin <- total_flesh_price / flesh_price
    }

    # Convert flesh weight to kg for calculations
    flesh_weight_kg <- flesh_weight_jin * 0.5

    # Calculate durian weight range (jin)
    min_durian_weight_jin <- (flesh_weight_kg / max_flesh_rate) * 2
    max_durian_weight_jin <- (flesh_weight_kg / min_flesh_rate) * 2

    # Calculate original durian price range (per 500g)
    min_durian_price <- (total_flesh_price / (max_durian_weight_jin * 0.5)) * 0.5
    max_durian_price <- (total_flesh_price / (min_durian_weight_jin * 0.5)) * 0.5

    # Create result data frame
    result <- data.frame(
      Parameter = c("Flesh Weight(jin)", "Flesh Price(yuan/jin)", "Total Flesh Price(yuan)",
                    "Flesh Rate", "Durian Weight Range(jin)", "Original Price Range(yuan/500g)"),
      Value = c(
        round(flesh_weight_jin, 2),
        round(flesh_price, 2),
        round(total_flesh_price, 2),
        paste(min_flesh_rate * 100, "%-", max_flesh_rate * 100, "%"),
        paste(round(min_durian_weight_jin, 2), "-", round(max_durian_weight_jin, 2)),
        paste(round(min_durian_price, 2), "-", round(max_durian_price, 2))
      )
    )
  } else if (!is.null(flesh_weight_jin)) {
    # Mode 2: Calculate from flesh weight only
    # Convert flesh weight to kg
    flesh_weight_kg <- flesh_weight_jin * 0.5

    # Calculate durian weight range (jin)
    min_durian_weight_jin <- (flesh_weight_kg / max_flesh_rate) * 2
    max_durian_weight_jin <- (flesh_weight_kg / min_flesh_rate) * 2

    # Calculate price range using durian price
    min_price <- (min_durian_weight_jin * 0.5 * 1000 / 500) * price_per_500g
    max_price <- (max_durian_weight_jin * 0.5 * 1000 / 500) * price_per_500g
    price_range <- c(min_price, max_price)

    # Create result data frame
    result <- data.frame(
      Parameter = c("Flesh Weight(jin)", "Flesh Rate", "Durian Weight Range(jin)", "Price Range(yuan)"),
      Value = c(
        flesh_weight_jin,
        paste(min_flesh_rate * 100, "%-", max_flesh_rate * 100, "%"),
        paste(round(min_durian_weight_jin, 2), "-", round(max_durian_weight_jin, 2)),
        paste(round(price_range[1], 2), "-", round(price_range[2], 2))
      )
    )
  } else {
    # Mode 1: Calculate from durian weight
    # Convert durian weight to kg
    weight_kg <- weight_jin * 0.5

    # Calculate total price
    total_price <- (weight_kg * 1000 / 500) * price_per_500g

    # Calculate flesh weight range (jin)
    min_flesh_weight_jin <- (weight_kg * min_flesh_rate) * 2
    max_flesh_weight_jin <- (weight_kg * max_flesh_rate) * 2

    # Create result data frame
    result <- data.frame(
      Parameter = c("Price(yuan/500g)", "Flesh Rate", "Durian Weight(jin)", "Total Price(yuan)", "Flesh Weight(jin)"),
      Value = c(
        price_per_500g,
        if(min_flesh_rate == max_flesh_rate) {
          paste(min_flesh_rate * 100, "%")
        } else {
          paste(min_flesh_rate * 100, "%-", max_flesh_rate * 100, "%")
        },
        weight_jin,
        round(total_price, 2),
        if(min_flesh_weight_jin == max_flesh_weight_jin) {
          round(min_flesh_weight_jin, 2)
        } else {
          paste(round(min_flesh_weight_jin, 2), "-", round(max_flesh_weight_jin, 2))
        }
      )
    )
  }

  # Return result data frame
  return(result)
}
