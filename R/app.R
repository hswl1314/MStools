#' MSdial To Jupyter UI
#' @noRd
app_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("MSdial to Jupyter"),
    shiny::h5("This app was made by Dr. Liang"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("file", "Upload CSV File", accept = c(".csv")),
        shiny::helpText("Max file size: 500MB"),
        shiny::numericInput("num", "Number of Samples:", value = 13, min = 1),
        shiny::actionButton("process", "Start Processing"),
        shiny::downloadButton("downloadInput1", "Download Input1 CSV"),
        shiny::downloadButton("downloadInput2", "Download Input2 CSV")
      ),
      shiny::mainPanel(
        shiny::textOutput("status")
      )
    )
  )
}

#' MSdial To Jupyter Server
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 500 * 1024^2)
  data <- shiny::reactiveVal(NULL)
  
  shiny::observeEvent(input$file, {
    shiny::req(input$file)
    tryCatch({
      data(read.csv(input$file$datapath))
      output$status <- shiny::renderText("File uploaded successfully!")
    }, error = function(e) {
      output$status <- shiny::renderText(paste("Error reading file:", e$message))
    })
  })
  
  shiny::observeEvent(input$process, {
    shiny::req(data())
    shiny::withProgress(message = 'Processing data...', {
      tryCatch({
        # 更新进度
        shiny::incProgress(0.2, detail = "Preparing data...")
        www <- data()
        www <- www[, -c(5:28, 31)]
        
        # Use the input num value
        num <- input$num
        
        shiny::incProgress(0.3, detail = "Creating input2...")
        # Create input2
        input2 <- www[c(1:4), ]
        input2 <- input2[, 7:(num + 7)]
        input2 <- input2[c(1, 4), ]
        input2 <- as.data.frame(t(input2))
        rownames(input2) <- NULL
        colnames(input2) <- input2[1, ]
        input2 <- input2[-1, ]
        colnames(input2)[1] <- "Type"
        colnames(input2)[2] <- "Input_column_name"
        input2$Input_column_name <- paste0(input2$Input_column_name, ".raw")
        input2$Sample_name <- input2$Input_column_name
        input2 <- input2[c("Input_column_name", "Sample_name", "Type")]
        input2$Group <- "1"
        input2$Treatment <- "1"
        input2$Model <- "WT"
        input2$Bio_replication <- "1"
        input2$Tech_replication <- "1"
        input2$Phosphorus_in_vial <- "20"
        blank_column <- rep("", nrow(input2))
        input2$Protein_in_vial <- blank_column
        input2$Cell_number_in_vial <- "10000"
        
        # Save input2
        write.csv(input2, "Input2_Sample_information.csv", row.names = FALSE)
        
        shiny::incProgress(0.3, detail = "Creating input1...")
        # Create input1
        input <- www[-c(1:3), ]
        NMR <- input[, c(4, 3, 2)]
        rownames(NMR) <- NULL
        colnames(NMR) <- NMR[1, ]
        NMR <- NMR[-1, ]
        NMR$Checked <- "TRUE"
        NMR$`Error 1` <- "1"
        blank_column <- rep("", nrow(NMR))
        NMR$`Name 2` <- blank_column
        NMR$`Error 2` <- blank_column
        NMR$`Name 3` <- blank_column
        NMR$`Error 3` <- blank_column
        NMR$Name <- blank_column
        colnames(NMR)[1] <- "Name 1"
        colnames(NMR)[2] <- "avg_mz"
        colnames(NMR)[3] <- "RT"
        
        # Convert to numeric
        NMR$av_mz_numeric <- as.numeric(as.character(NMR$avg_mz))
        NMR$`Molecular Weight` <- NMR$av_mz_numeric + 1.007825
        NMR$avg_mz <- NULL
        NMR$av_mz_numeric <- NULL
        
        NMR <- dplyr::select(NMR, Checked, dplyr::everything())
        NMR$`RT [min]` <- NMR$RT
        NMR <- NMR[, -3]
        
        matrix <- input[, 8:(num + 7)]
        matrix[1, ] <- paste0(matrix[1, ], ".raw")
        colnames(matrix) <- matrix[1, ]
        matrix <- matrix[-1, ]
        rownames(matrix) <- NULL
        matrix$`Area (Max.)` <- apply(matrix, 1, max, na.rm = TRUE)
        matrix <- dplyr::select(matrix, `Area (Max.)`, dplyr::everything())
        input1 <- cbind(NMR, matrix)
        
        shiny::incProgress(0.2, detail = "Saving files...")
        # Save input1
        write.csv(input1, "Input1_Sample_dataset.csv", row.names = FALSE)
        
        output$status <- shiny::renderText("Processing completed successfully!")
      }, error = function(e) {
        output$status <- shiny::renderText(paste("Error during processing:", e$message))
      })
    })
  })
  
  output$downloadInput1 <- shiny::downloadHandler(
    filename = function() {
      "Input1_Sample_dataset.csv"
    },
    content = function(file) {
      file.copy("Input1_Sample_dataset.csv", file)
    }
  )
  
  output$downloadInput2 <- shiny::downloadHandler(
    filename = function() {
      "Input2_Sample_information.csv"
    },
    content = function(file) {
      file.copy("Input2_Sample_information.csv", file)
    }
  )
}
