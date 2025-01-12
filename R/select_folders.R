#' Select Folders Based on Pattern Matching
#'
#' This function selects folders from a specified directory based on pattern matching.
#' It can match multiple patterns using either AND or OR logic.
#'
#' @param base_path Character string specifying the directory path to search in. 
#'        Default is current directory (".").
#' @param pattern Character vector of patterns to match folder names against. 
#'        Default is NULL (returns all folders).
#' @param match_all Logical value indicating whether all patterns must match (TRUE for AND logic) 
#'        or any pattern can match (FALSE for OR logic). Default is FALSE.
#' @param full_path Logical value indicating whether to return full paths (TRUE) 
#'        or just folder names (FALSE). Default is FALSE.
#' @param ignore_case Logical value indicating whether to ignore case when matching patterns.
#'        Default is TRUE.
#'
#' @return A character vector of folder names or full paths, or NULL if no matches found.
#'
#' @examples
#' \dontrun{
#' # Get all folders in current directory
#' select_folders()
#'
#' # Get folders containing "data" in current directory
#' select_folders(pattern = "data")
#'
#' # Get folders containing either "data" or "test", case sensitive
#' select_folders(pattern = c("data", "test"), ignore_case = FALSE)
#'
#' # Get folders containing both "DATA" and "Test", case insensitive
#' select_folders(pattern = c("DATA", "Test"), match_all = TRUE)
#'
#' # Get folders from specific path
#' select_folders(base_path = "D:/workspace", pattern = "data")
#'
#' # Get full paths instead of just folder names
#' select_folders(pattern = "data", full_path = TRUE)
#'
#' # Using regular expressions with case sensitivity
#' select_folders(pattern = "^Data[0-9]", ignore_case = FALSE)
#' }
#'
#' @export
select_folders <- function(base_path = ".", pattern = NULL, match_all = FALSE, 
                         full_path = FALSE, ignore_case = TRUE) {
  # Check if base_path exists
  if (!dir.exists(base_path)) {
    stop("Specified path does not exist")
  }
  
  # Get all folders
  folders <- list.dirs(base_path, full.names = TRUE, recursive = FALSE)
  folder_names <- basename(folders)
  
  # Apply pattern filtering if specified
  if (!is.null(pattern)) {
    if (length(pattern) > 1) {
      if (match_all) {
        # AND logic: all patterns must match
        matched <- sapply(folder_names, function(x) {
          all(sapply(pattern, function(p) 
              grepl(p, x, ignore.case = ignore_case)))
        })
      } else {
        # OR logic: any pattern can match
        pattern <- paste(pattern, collapse = "|")
        matched <- grepl(pattern, folder_names, ignore.case = ignore_case)
      }
      folder_names <- folder_names[matched]
      folders <- folders[matched]
    } else {
      # Single pattern case
      matched <- grepl(pattern, folder_names, ignore.case = ignore_case)
      folder_names <- folder_names[matched]
      folders <- folders[matched]
    }
  }
  
  # Return NULL if no folders found
  if (length(folder_names) == 0) {
    message("No matching folders found")
    return(NULL)
  }
  
  # Return appropriate format based on full_path parameter
  return(if(full_path) folders else folder_names)
} 