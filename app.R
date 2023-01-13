library(shiny)
library(shinyFeedback)
library(dplyr)
library(readxl)

rvals <- reactiveValues()

# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyFeedback(),
  titlePanel("Examiner"),
  fluidRow(
    column(
      width = 12,
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Select the exam .xlsx file"),
          numericInput("extra_limit", "Extra time group threshold", 10, 1),
          width = 4
        ),
        mainPanel(
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Results",
              headerPanel(""),
              textOutput("test"),
              DT::dataTableOutput("exam")
            ),
            tabPanel(
              "Export",
              headerPanel("")
            )
          ),
          width = 8
        )
      )
    )
  )
)

server <- function(input, output) {
  # File input occurs
  raw_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if (ext != "xlsx") {
      showFeedbackWarning("file", "Please upload an .xlsx file.")
    } else {
      hideFeedback("file")
      try(read_xlsx(input$file$datapath))
    }
  })

  # Uploaded file is .xlsx
  observeEvent(raw_data(), {
    valid <- TRUE
    if (inherits(raw_data(), "try-error")) {
      showFeedbackWarning("file", "Unable to parse the input file.")
      valid <- FALSE
    }
    req(valid)
    e <- raw_data()
    e_names <- tolower(names(e))
    names(e) <- e_names
    req_names <- c(
      "opiskelijanumero",
      "sukunimi", 
      "etunimet", 
      "ensisijainen sähköposti", 
      "tentti", 
      "lisätietokysymykset"
    )
    mis <- !req_names %in% e_names
    if (any(mis)) {
      showFeedbackWarning(
        "file",
        paste0(
          "Invalid .xlsx file, missing columns: ", 
          paste0(toupper(req_names[mis]), collapse = ", ")
        )
      )
      valid <- FALSE
    }
    req(valid)
    hideFeedback("file")
    rvals$test <- "Success!"
    rvals$exam <- e
  })
  output$test <- renderText(rvals$test)
  output$exam <- DT::renderDataTable({rvals$exam})
}

shinyApp(ui = ui, server = server)
