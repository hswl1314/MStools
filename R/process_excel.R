#' Process Excel File for MS Data
#'
#' This function reads an Excel file containing MS data and extracts either Area or RT values
#' for a specified number of samples across multiple worksheets.
#'
#' @param file_path Character string specifying the path to the Excel file
#' @param Sample_num Numeric value specifying the number of samples to process
#' @param what_you_want Character string specifying the type of data to extract ("Area" or "RT")
#'
#' @return A data frame containing:
#'   \item{SampleID}{Sample identifiers from the first column}
#'   \item{...}{Additional columns named by component names, containing either Area or RT values}
#'
#' @export
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom cellranger cell_limits
#'
#' @examples
#' \dontrun{
#' # Read Area data for 10 samples
#' result_area <- process_excel("path/to/file.xlsx", Sample_num = 10, what_you_want = "Area")
#'
#' # Read RT data for 10 samples
#' result_rt <- process_excel("path/to/file.xlsx", Sample_num = 10, what_you_want = "RT")
#' }
process_excel <- function(file_path, Sample_num, what_you_want = "Area") {
  # Input validation
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  if (!is.numeric(Sample_num) || Sample_num < 1) {
    stop("Sample_num must be a positive number")
  }
  if (!what_you_want %in% c("Area", "RT")) {
    stop("what_you_want must be either 'Area' or 'RT'")
  }

  # Calculate end row
  end_row <- 5 + Sample_num

  # Determine column number based on what_you_want
  col_number <- if(what_you_want == "Area") 5 else 15

  # Get sheet names and exclude specific sheets
  sheets <- setdiff(excel_sheets(file_path), c("Component", "mdlCalcs"))
  if (length(sheets) == 0) {
    stop("No valid sheets found in the Excel file")
  }

  # Read first sheet for SampleID
  first_sheet <- read_excel(file_path,
                          sheet = sheets[1],
                          col_names = FALSE,
                          range = cell_limits(c(1, 1), c(end_row, col_number)))

  # Initialize result dataframe with SampleID
  result_df <- data.frame(SampleID = first_sheet[[1]][6:end_row])

  # Process each sheet
  for(sheet in sheets) {
    data <- read_excel(file_path,
                      sheet = sheet,
                      col_names = FALSE,
                      range = cell_limits(c(1, 1), c(end_row, col_number)))

    component_name <- data[[1]][3]
    result_df[[component_name]] <- data[[col_number]][6:end_row]
  }

  return(result_df)
}