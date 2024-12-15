# Define global variables to avoid R CMD check notes
utils::globalVariables(c(
  ".data", "Area (Max.)", "Checked",
  "contrib", "func", "PC1", "PC2", "group"
))

.onLoad <- function(libname, pkgname) {
  # Attach ggplot2 namespace
  if (!("ggplot2" %in% .packages())) {
    attachNamespace("ggplot2")
  }
}

.onAttach <- function(libname, pkgname) {
  # Load required packages
  packages <- c("ggplot2", "dplyr", "magrittr", "vegan", 
               "ggpubr", "ggrepel", "patchwork", "ellipse")
  
  # Check and load each package
  for(pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      warning(sprintf("Package '%s' is needed for this package to work. Please install it.", pkg))
    }
  }
  
  # Welcome message
  packageStartupMessage(sprintf("MStools version %s loaded successfully!", 
                              utils::packageVersion("MStools")))
}