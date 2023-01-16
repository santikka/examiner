library(shiny)
library(shinyFeedback)
library(dplyr)
library(readxl)
library(sortable)

rvals <- reactiveValues(
  exam = NULL
)

room_list <- list(
  `MaA 102` = "maa_102",
  `MaA 103` = "maa_103",
  `MaD 202` = "mad_202"
)

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
  ),
  #useShinyjs(),
  useShinyFeedback(),
  titlePanel("Examiner"),
  fluidRow(
    column(
      width = 12,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fileInput("file", "Select the exam .xlsx file"),
          selectInput(
            "special_groups",
            "Special groups",
            choices = c(),
            multiple = TRUE
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            type = "tabs",
            id = "nav_tabs",
            tabPanel(
              "Summary",
              br(),
              strong("Students"),
              DT::dataTableOutput("exam_standard", width = "66%"),
              br(),
              strong("Multiple exams"),
              DT::dataTableOutput("exam_multi", width = "66%"),
              br(),
              strong("Special arrangements"),
              DT::dataTableOutput("exam_special", width = "66%"),
              br()
            ),
            tabPanel(
              "Design",
              br(),
              selectInput(
                "exam_rooms",
                "Exam rooms",
                choices = names(room_list),
                multiple = TRUE
              ),
              strong("Partition exams"),
              DT::dataTableOutput("exam_controls", width = "66%"),
              br(),
              uiOutput("exam_buckets")
            ),
            tabPanel(
              "MaA 102",
              value = "maa_102",
              br()
            ),
            tabPanel(
              "MaA 103",
              value = "maa_103",
              br()
            ),
            tabPanel(
              "MaD 202",
              value = "mad_202",
              br()
            )
          )
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
    rvals$exam <- e |>
      rename(
        id = opiskelijanumero,
        last = sukunimi,
        first = etunimet,
        email = `ensisijainen sähköposti`,
        exam = tentti,
        special = lisätietokysymykset
      ) |>
      mutate(special = gsub("\r\n", " ", special))
    updateSelectInput(
      inputId = "special_groups",
      choices = unique(rvals$exam$special)
    )
  })

  # Summarise exams
  observe({
    exam <- rvals$exam
    if (!is.null(exam)) {
      # Students with special arrangements
      special <- exam |>
        filter(special %in% input$special_groups)

      # Students with multiple exams
      multi_id <- exam |>
        filter(!special %in% input$special_groups) |>
        select(id) |>
        summarise(id = id, dupe = duplicated(id)) |>
        filter(dupe == TRUE) |>
        pull(id)
      rvals$multi_id <- multi_id
      multi <- exam |>
        filter(!special %in% input$special_groups) |>
        filter(id %in% multi_id)

      # Others
      exam <- exam |>
        filter(!special %in% input$special_groups) |>
        filter(!id %in% multi_id) |>
        arrange(last, first)
      rvals$exam_standard <- exam

      # Summaries
      output$exam_standard <- DT::renderDataTable(
        rbind(
          exam |>
            group_by(exam) |>
            count(),
          data.frame(exam = "Multiple exams", n = length(unique(multi$id))),
          data.frame(exam = "Special arrangements", n = length(unique(special$id))),
          data.frame(exam = "Total", n = n_distinct(rvals$exam$id))
        ),
        options = list(dom = "t", ordering = FALSE, pageLength = 10000)
      )
      output$exam_multi <- DT::renderDataTable(
        multi |> select(first, last, exam),
        options = list(dom = "t", ordering = FALSE, pageLength = 10000)
      )
      output$exam_special <- DT::renderDataTable(
        special |> select(first, last, exam, special),
        options = list(dom = "t", ordering = FALSE, pageLength = 10000)
      )
    }
  })

  # Exam partitions
  observe({
    exam <- rvals$exam_standard
    if (!is.null(exam)) {
      rvals$design <- exam |>
        group_by(exam) |>
        count() |>
        rbind(
          data.frame(
            exam = "Multiple exams",
            n = length(unique(rvals$multi_id))
          )
        ) |>
        mutate(
          ok = 1L,
          `part 1` = n,
          `part 2` = 0L
        ) |>
        relocate(ok, .after = exam)
      output$exam_controls <- DT::renderDT(
        datatable(
          rvals$design,
          options = list(dom = "t", ordering = FALSE),
          selection = list(mode = "single", target = "cell"),
          editable = list(target = "cell", disable = list(columns = 0:2))
        ) |> formatStyle(
          "ok",
          target = "row",
          backgroundColor = styleEqual(c(0, 1), c("orange", "white"))
        )
      )
    }
  })

  # Persistent partition edits
  observeEvent(input$exam_controls_cell_edit, {
    row <- input$exam_controls_cell_edit$row
    col <- input$exam_controls_cell_edit$col
    rvals$design[row, col] <- input$exam_controls_cell_edit$value
    cols <- rvals$design[, c("part 1", "part 2")]
    rows_ok <- (rowSums(cols) == rvals$design$n) & (apply(cols, 1, min) >= 0)
    rvals$design[rows_ok, "ok"] <- 1L
    rvals$design[!rows_ok, "ok"] <- 0L
    for (i in seq_len(nrow(rvals$design))) {
      exam <- rvals$exam_standard |> filter(exam == rvals$design$exam[i])
      if (rvals$design[i, "ok"] == 0L || rvals$design[i, "part 2"] == 0) {
        next
      }
      a <- rvals$design[i, "part 1"]
      b <- a + 1
      if (identical(exam[a, "last"] == exam[b, "last"])) {
        showNotification(
          paste0(
            "Family name conflict for exam: ",
            rvals$design$exam[i]
          )
        )
      }
    }
  })

  # Exam rooms tabs
  observeEvent(
    input$exam_rooms,
    {
      nm <- names(room_list)
      for (i in seq_along(room_list)) {
        if (nm[i] %in% input$exam_rooms) {
          showTab(inputId = "nav_tabs", target = room_list[[i]])
        } else {
          hideTab(inputId = "nav_tabs", target = room_list[[i]])
        }
      }
    },
    ignoreNULL = FALSE
  )

  # Exam room buckets
  observe({
    design <- rvals$design
    rooms <- input$exam_rooms
    exam <- rvals$exam_standard
    if (!is.null(design)) {
      exams <- character(0L)
      for (i in seq_len(nrow(design))) {
        if (any(design[i,-c(1:4)]) > 0) {
          exams <- c(
            exams,
            paste0(
              design$exam[i],
              " (part ",
              which(design[i, -c(1:3)] > 0),
              ")"
            )
          )
        } else {
          exams <- c(exams, design$exam[i])
        }
      }
      args <- c(
        list(
          add_rank_list(
            text = "Exams",
            labels = as.list(exams),
            input_id = "exam_list"
          )
        ),
        lapply(seq_along(rooms), function(i) {
          add_rank_list(
            text = rooms[i],
            labels = NULL,
            input_id = paste0("room_list_", i)
          )
        })
      )
      args$header <- "Drag the exams to the desired room"
      args$group_name = "bucket_list_group"
      args$orientation = "horizontal"
      output$exam_buckets <- renderUI({
        fluidRow(
          column(
            tags$b("Exam rooms"),
            width = 12,
            do.call("bucket_list", args = args)
          )
        )
      })
    }
  })
}

shinyApp(ui = ui, server = server)
