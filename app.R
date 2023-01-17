# Requires the following packages
# install.packages(c("dplyr", "DT", "readxl", "shiny", "shinyFeedback", "sortable))

library(dplyr)
library(shiny)
library(shinyFeedback)
library(sortable)

options(shiny.fullstacktrace = TRUE)

source("utils.R")

rvals <- reactiveValues(
  exam = NULL,
)

rooms <- rjson::fromJSON(file = "rooms.json") |>
  process_layout()

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
  ),
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
                choices = names(rooms),
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
              br(),
              uiOutput("maa_102_output")
            ),
            tabPanel(
              "MaA 103",
              value = "maa_103",
              br(),
              uiOutput("maa_103_output")
            ),
            tabPanel(
              "MaD 202",
              value = "mad_202",
              br(),
              uiOutput("mad_202_output")
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
      try(readxl::read_xlsx(input$file$datapath))
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
            count() |>
            arrange(desc(n)),
          data.frame(exam = "Multiple exams", n = length(unique(multi$id))),
          data.frame(exam = "Special arrangements", n = length(unique(special$id))),
          data.frame(exam = "Total", n = n_distinct(rvals$exam$id))
        ),
        class = "compact",
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE, pageLength = 10000)
      )
      output$exam_multi <- DT::renderDataTable(
        multi |> select(first, last, exam),
        class = "compact",
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE, pageLength = 10000)
      )
      output$exam_special <- DT::renderDataTable(
        special |> select(first, last, exam, special),
        class = "compact",
        rownames = FALSE,
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
        arrange(desc(n)) |>
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
        DT::datatable(
          rvals$design,
          class = "compact",
          options = list(dom = "t", ordering = FALSE),
          selection = list(mode = "single", target = "cell"),
          editable = list(target = "cell", disable = list(columns = 0:2))
        ) |> DT::formatStyle(
          "ok",
          target = "row",
          backgroundColor = DT::styleEqual(c(0, 1), c("orange", "white"))
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
      a <- rvals$design$`part 1`[i]
      b <- a + 1
      if (identical(exam$last[a], exam$last[b])) {
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
      nm <- names(rooms)
      for (i in seq_along(rooms)) {
        if (nm[i] %in% input$exam_rooms) {
          showTab(inputId = "nav_tabs", target = rooms[[i]]$value)
        } else {
          hideTab(inputId = "nav_tabs", target = rooms[[i]]$value)
        }
      }
    },
    ignoreNULL = FALSE
  )

  # Exam room buckets
  observe({
    design <- rvals$design
    rooms_input <- input$exam_rooms
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
        lapply(seq_along(rooms_input), function(i) {
          add_rank_list(
            text = rooms_input[i],
            labels = NULL,
            input_id = paste0(rooms[[rooms_input[i]]]$value, "_exams")
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
      lapply(seq_along(rooms_input), function(i) {
        j <- rooms_input[i]
        in_ <- paste0(rooms[[j]]$value, "_exams")
        out_ <- paste0(rooms[[j]]$value, "_output")
        observeEvent(input[[in_]], {
          sel <- input[[in_]]
          if (length(sel) > 0L) {
            part1 <- grepl("(part 1)", sel)
            part2 <- grepl("(part 2)", sel)
            sel <- gsub("\\s\\(part [1-2]\\)", "", sel)
            e <- rvals$design |>
              filter(exam %in% sel)
            e$part <- 1L
            e[e$exam %in% sel[part1], ]$part <- 1L
            e[e$exam %in% sel[part2], ]$part <- 2L
            e <- e[match(e$exam, sel), ]
            room <- try(process_seating(e, rooms[[j]]), silent = TRUE)
            assigned <- room$layout |> filter(exam != "")
            if (inherits(room, "try-error")) {
              showNotification(
                paste0("Room ", j, " does not have enough space!")
              )
            } else {
              output[[out_]] <- renderUI({
                renderPlot({
                  ggplot(room$layout, aes(x = x, y = y, width = 1, height = 1)) +
                    geom_tile(
                      data = assigned,
                      aes(x = x, y = y, width = 1, height = 1, fill = exam)
                    ) +
                    geom_tile(fill = "transparent", color = "black") +
                    coord_equal() +
                    theme_classic(base_size = 20) +
                    guides(color = "none") +
                    theme(
                      axis.text = element_blank(),
                      axis.line = element_blank(),
                      axis.ticks = element_blank(),
                      axis.title = element_blank(),
                      panel.grid = element_blank(),
                      legend.direction = "vertical",
                      legend.position = "bottom",
                      legend.title = element_blank()
                    )
                }, height = 450 + 18 * nrow(e))
              })
            }
          }
        })
      })
    }
  })
}

shinyApp(ui = ui, server = server)
