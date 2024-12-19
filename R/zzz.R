# Define global variables to avoid R CMD check notes
utils::globalVariables(c(
  ".data", "Area (Max.)", "Checked",
  "contrib", "func", "PC1", "PC2", "group"
))

.onAttach <- function(libname, pkgname) {
    # Load and check required packages
    pkgs <- c("readxl", "dplyr", "purrr", "cellranger")
    for(pkg in pkgs) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            warning(sprintf("Package '%s' is needed for MStools to work properly", pkg))
        }
    }
    
    # Display welcome message
    packageStartupMessage(paste0(
        "Welcome to MStools v", utils::packageVersion("MStools"), "\n",
        "If you have any questions, you can contact the author through GitHub"
    ))
}

# Function executed when unloading the package (optional)
.onDetach <- function(libpath) {
    packageStartupMessage("Unloading MStools...")
}

# Function executed when loading the package (optional, used for initialization)
.onLoad <- function(libname, pkgname) {
    # Add initialization settings here
    # Typically used for package options
    invisible()
}
