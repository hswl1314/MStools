utils::globalVariables(c(
  ".data", "Area (Max.)", "Checked",
  "contrib", "func", "PC1", "PC2", "group"
))

.onLoad <- function(libname, pkgname) {
  # Any initialization code if needed
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
  
  packageStartupMessage("MStoolsR loaded successfully!")
}