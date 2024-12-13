#' @importFrom stats sd
#' @importFrom utils read.csv write.csv
NULL

#' Plot Standard Curves for LC-MS Analysis
#'
#' This function creates standard curves for LC-MS compounds analysis. It supports
#' both individual and combined plots with options for log transformation.
#'
#' @param data A data frame containing concentration and peak area data from LC-MS analysis
#' @param compounds Character vector of compound names or "all" for all compounds
#' @param log Logical, whether to use log transformation for better visualization
#' @param split Logical, whether to save plots separately for each compound
#'
#' @return A list of linear regression results for each compound
#'
#' @importFrom graphics plot abline legend par
#' @importFrom stats lm
#' @importFrom grDevices png dev.off
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create example data from LC-MS analysis
#' example_data <- data.frame(
#'   Conc = c(5, 2.5, 1.25, 0.625, 0.3125, 0.15625, 0.078125),
#'   AA = c(68619120, 29206615, 16247479, 7586211, 4235692, 1846262, 1121494),
#'   DHA = c(58619120, 25206615, 14247479, 6586211, 3235692, 1646262, 921494),
#'   EPA = c(48619120, 22206615, 12247479, 5586211, 2935692, 1446262, 821494),
#'   ALA = c(38619120, 19206615, 10247479, 4586211, 2535692, 1246262, 721494),
#'   LA = c(28619120, 16206615, 8247479, 3586211, 2135692, 1046262, 621494),
#'   GLA = c(18619120, 13206615, 6247479, 2586211, 1735692, 846262, 521494)
#' )
#' # Plot standard curves for LC-MS compounds
#' plot_standard_curves(example_data, compounds = c("AA", "DHA"), log = TRUE, split = FALSE)
#' }
plot_standard_curves <- function(data, compounds = "all", log = FALSE, split = FALSE) {
  # Process compounds parameter
  if(compounds[1] == "all") {
    compounds <- names(data)[names(data) != "Conc"]
  }
  
  # Check if compounds exist in data
  if(!all(compounds %in% names(data))) {
    stop("Some compounds not found in data")
  }
  
  # Store results
  results <- list()
  
  if(split) {
    # Save plots separately
    for(compound in compounds) {
      filename <- paste0("Standard_Curve_of_", compound, ".png")
      png(filename, width = 5, height = 4, units = "in", res = 300)
      par(mar = c(4, 4, 3, 1))
      
      y_data <- data[[compound]]
      
      if(log) {
        x <- log10(data$Conc)
        y <- log10(y_data)
        xlab <- "log(Concentration)"
        ylab <- paste("log(Peak Area)")
      } else {
        x <- data$Conc
        y <- y_data
        xlab <- "Concentration"
        ylab <- paste("Peak Area")
      }
      
      fit <- lm(y ~ x)
      results[[compound]] <- fit
      
      plot(x, y,
           xlab = xlab,
           ylab = ylab,
           pch = 16,
           main = paste("Standard Curve of", compound))
      
      abline(fit, col = "red")
      
      r2 <- round(summary(fit)$r.squared, 4)
      coef <- coef(fit)
      eq <- paste0("y = ", round(coef[2], 2), "x + ", round(coef[1], 2))
      r2_text <- paste0("R2 = ", r2)
      
      legend("topleft", 
             c(eq, r2_text),
             bty = "n")
      
      dev.off()
      message("Plot saved as: ", filename)
    }
  } else {
    n_compounds <- length(compounds)
    n_rows <- ceiling(n_compounds/4)
    n_cols <- min(4, n_compounds)
    
    filename <- "Standard_Curve_of_compounds.png"
    width <- 5 * n_cols
    height <- 4 * n_rows
    
    png(filename, width = width, height = height, units = "in", res = 300)
    par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))
    
    for(compound in compounds) {
      y_data <- data[[compound]]
      
      if(log) {
        x <- log10(data$Conc)
        y <- log10(y_data)
        xlab <- "log(Concentration)"
        ylab <- paste("log(Peak Area)")
      } else {
        x <- data$Conc
        y <- y_data
        xlab <- "Concentration"
        ylab <- paste("Peak Area")
      }
      
      fit <- lm(y ~ x)
      results[[compound]] <- fit
      
      plot(x, y,
           xlab = xlab,
           ylab = ylab,
           pch = 16,
           main = paste("Standard Curve of", compound))
      
      abline(fit, col = "red")
      
      r2 <- round(summary(fit)$r.squared, 4)
      coef <- coef(fit)
      eq <- paste0("y = ", round(coef[2], 2), "x + ", round(coef[1], 2))
      r2_text <- paste0("R2 = ", r2)
      
      legend("topleft", 
             c(eq, r2_text),
             bty = "n")
    }
    
    dev.off()
    message("Plot saved as: ", filename)
  }
  
  return(results)
}