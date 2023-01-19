# Requires the following packages
# install.packages(c("data.table", "dplyr", "DT", "grid", "gtable", "gridExtra", "readxl", "shiny", "shinyjs", "shinyFeedback", "sortable", "stringr"))

library(dplyr)
library(shiny)
library(shinyjs)
library(shinyFeedback)
library(sortable)
library(ggplot2)
library(grid)
library(gtable)
library(gridExtra)

options(shiny.fullstacktrace = TRUE)

source("utils.R")

rvals <- reactiveValues(
  exam = NULL,
  start = TRUE,
  plots = list(),
  rooms_ok = list()
)

rooms <- rjson::fromJSON(file = "rooms.json") |> process_layout()
nm <- names(rooms)

cleanup_strings <- c(
  "Lisätietoja, kuten suositellut yksilölliset järjestelyt",
  "Onko sinulle tehty suositus yksilöllisistä järjestelyistä tentteihin liittyen? Jos, niin kuvaile tarpeesi. Varaudu esittämään suositus tenttitilaisuudessa."
)

js_code <- "
  shinyjs.copyToClipboard = function(params) {
    const textCopy = copyText(params);
    document.addEventListener('copy', textCopy);
    document.execCommand('copy');
    document.removeEventListener('copy', textCopy);
  }
  function copyText(text) {
    return function handleCopyEvent(evt) {
      evt.clipboardData.setData('text', text);
      evt.preventDefault();
    }
  }
"

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
  ),
  useShinyjs(),
  extendShinyjs(text = js_code, functions = c("copyToClipboard")),
  useShinyFeedback(),
  titlePanel("Examiner"),
  fluidRow(
    column(
      width = 12,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          fileInput("file", "Select the exam .csv/.xlsx file"),
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
              value = "summary",
              br(),
              DT::dataTableOutput("exam_standard", width = "75%"),
              br(),
              strong("Multiple exams"),
              DT::dataTableOutput("exam_multi", width = "75%"),
              br(),
              strong("Special arrangements"),
              DT::dataTableOutput("exam_special", width = "75%"),
              br()
            ),
            tabPanel(
              "Design",
              value = "design",
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
              "Output",
              value = "output",
              br(),
              fluidRow(
                column(
                  width = 3,
                  actionButton(
                    "clip_standard",
                    label = "Email addresses",
                    icon = icon("clipboard")
                  ),
                  actionButton(
                    "clip_special",
                    label = "Email addresses (special arrangements)",
                    icon = icon("clipboard")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {

  # Add room specific elements
  observe({
    if (rvals$start) {
      rvals$start <- FALSE
      hideTab(inputId = "nav_tabs", target = "summary")
      hideTab(inputId = "nav_tabs", target = "design")
      hideTab(inputId = "nav_tabs", target = "output")
      lapply(rev(seq_along(rooms)), function(i) {
        val <- rooms[[i]]$value
        rvals$rooms_ok[[val]] <- FALSE
        # Room tabs
        insertTab(
          inputId = "nav_tabs",
          tabPanel(
            nm[i],
            value = val,
            br(),
            uiOutput(paste0(val, "_output"))
          ),
          target = "design",
          position = "after"
        )
        hideTab(inputId = "nav_tabs", target = val)
        # Seating arrangement download links
        insertUI(
          selector = "#clip_special",
          where = "afterEnd",
          ui = hidden(
            downloadButton(
              paste0(val, "_seating"),
              paste0("Export ", nm[i], " seating arrangement")
            )
          )
        )
        # Student list download links
        insertUI(
          selector = "#clip_special",
          where = "afterEnd",
          ui = hidden(
            downloadButton(
              paste0(val, "_students"),
              paste0("Export ", nm[i], " list of students")
            )
          )
        )

      })
      rvals$start <- FALSE
    }
  })

  # File input occurs
  raw_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    if (ext %in% c("xlsx", "csv")) {
      hideFeedback("file")
      if (ext == "xlsx") {
        try(readxl::read_xlsx(input$file$datapath))
      } else {
        try({
          data.table::fread(file = input$file$datapath) |>
            data.table::setDT() |>
            as_tibble()
        })
      }
    } else {
      showFeedbackWarning("file", "Please upload a .csv or an .xlsx file.")
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
        paste0(
          "Invalid .xlsx file, missing columns: ",
          paste0(toupper(req_names[mis]), collapse = ", ")
        )
      )
      valid <- FALSE
    }
    req(valid)
    hideFeedback("file")
    showTab(inputId = "nav_tabs", target = "summary")
    showTab(inputId = "nav_tabs", target = "design")
    showTab(inputId = "nav_tabs", target = "output")
    e <- e |>
      rename(
        id = opiskelijanumero,
        last = sukunimi,
        first = etunimet,
        email = `ensisijainen sähköposti`,
        exam = tentti,
        special = lisätietokysymykset
      ) |>
      mutate(special = gsub("\r\n", " ", special)) |>
      mutate(special = gsub(cleanup_strings[1L], "", special, fixed = TRUE)) |>
      mutate(special = gsub(cleanup_strings[2L], "", special, fixed = TRUE))
    # Need to use str_order as dplyr::arrange
    # can result in incorrect order with scandinavian letters
    e <- e[stringr::str_order(e$last, locale = "fi_FI"), ]
    rvals$exam <- e
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
        filter(!id %in% multi_id)
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
        multi |> select(last, first, exam),
        class = "compact",
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE, pageLength = 10000)
      )
      output$exam_special <- DT::renderDataTable(
        special |> select(last, first, exam, special),
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
          editable = list(target = "cell", disable = list(columns = 0L:3L))
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

  # Exam room buckets
  observe({
    design <- rvals$design
    rooms_input <- input$exam_rooms
    exam <- rvals$exam_standard
    if (!is.null(design)) {
      exams <- character(0L)
      exams_orig <- character(0L)
      n <- integer(0L)
      start <- integer(0L)
      end <- integer(0L)
      for (i in seq_len(nrow(design))) {
        if (any(design[i,-c(1L:4L)]) > 0) {
          idx <- which(design[i, -c(1L:3L)] > 0)
          exams <- c(
            exams,
            paste0(design$exam[i], " (part ", idx, ")")
          )
          exams_orig <- c(exams_orig, rep(design$exam[i], length(idx)))
          parts_n <- as.integer(design[i, -c(1L:3L)])[idx]
          n <- c(n, parts_n)
          start <- c(start, cumsum(c(1L, parts_n[-length(idx)])))
          end <- c(end, cumsum(parts_n))
        } else {
          exams <- c(exams, design$exam[i])
          exams_orig <- c(exams_orig, design$exam[i])
          n <- c(n, design$n[i])
          start <- c(start, 1L)
          end <- c(end, design$n[i])
        }
      }
      rvals$design_partition <- data.frame(
        exam = exams,
        exam_orig = exams_orig,
        n = n,
        start = start,
        end = end
      )
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
    }
  })

  # Exam to room allocation
  lapply(seq_along(rooms), function(i) {
    j <- nm[i]
    val <- rooms[[j]]$value
    in_ <- paste0(val, "_exams")
    out_ <- paste0(val, "_output")
    observeEvent(input[[in_]], {
      sel <- input[[in_]]
      if (length(sel) > 0L) {
        e <- rvals$design_partition |>
          filter(exam %in% sel)
        e <- e[match(sel, e$exam), ]
        room <- try(process_seating(e, sel, rooms[[j]]), silent = TRUE)
        if (inherits(room, "try-error")) {
          showNotification(
            paste0("Room ", j, " does not have enough space!"),
            type = "error"
          )
          rvals$plots[[val]] <- NULL
          rvals$rooms_ok[[val]] <- FALSE
          output[[out_]] <- renderUI({
            br()
          })
          hideTab(inputId = "nav_tabs", target = val)
          hideElement(id = paste0(val, "_seating"))
          hideElement(id = paste0(val, "_students"))
        } else {
          assigned <- room$layout |> filter(exam != "")
          p <- ggplot(room$layout, aes(x = x, y = y, width = 1, height = 1)) +
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
          rvals$rooms_ok[[val]] <- TRUE
          rvals$plots[[val]] <- p
          output[[out_]] <- renderUI({
            renderPlot(p, height = 450 + 18 * nrow(e))
          })
          showTab(inputId = "nav_tabs", target = val)
          showElement(id = paste0(val, "_students"))
          showElement(id = paste0(val, "_seating"))
        }
      } else {
        rvals$rooms_ok[[val]] <- FALSE
        rvals$plots[[val]] <- NULL
        output[[out_]] <- renderUI({
          br()
        })
        hideTab(inputId = "nav_tabs", target = val)
        hideElement(id = paste0(val, "_seating"))
        hideElement(id = paste0(val, "_students"))
      }
    })
  })

  # Copy email addresses to clipboard
  observeEvent(input$clip_standard, {
    if (!is.null(rvals$exam)) {
      emails <- rvals$exam |>
        filter(!special %in% input$special_groups) |>
        pull(email)
      clip_str <- paste0(unique(emails), collapse = ",")
      js$copyToClipboard(clip_str)
    } else {
      js$copyToClipboard("")
    }
  })

  # Copy email addresses to clipboard (special arrangements)
  observeEvent(input$clip_special, {
    if (!is.null(rvals$exam)) {
      emails <- rvals$exam |>
        filter(special %in% input$special_groups) |>
        pull(email)
      clip_str <- paste0(unique(emails), collapse = ",")
      js$copyToClipboard(clip_str)
    } else {
      js$copyToClipboard("")
    }
  })

  # Seating arrangement download links
  lapply(seq_along(rooms), function(i) {
    val <- rooms[[i]]$value
    output[[paste0(val, "_seating")]] <- downloadHandler(
      filename = function() {paste0(val, "_seating.pdf")},
      content = function(file) {
        pdf(file, paper = "a4", width = 8.5, height = 11)
        plot(rvals$plots[[val]])
        dev.off()
      }
    )
  })

  # Student list download links
  lapply(seq_along(rooms), function(i) {
    val <- rooms[[i]]$value
    in_ <- paste0(val, "_exams")
    output[[paste0(val, "_students")]] <- downloadHandler(
      filename = function() {paste0(val, "_students.pdf")},
      content = function(file) {
        if (rvals$rooms_ok[[val]]) {
          design <- rvals$design_partition
          sel <- input[[in_]]
          students <- rvals$exam_standard
          parts <- vector(mode = "list", length = length(sel))
          for (j in seq_along(sel)) {
            if (sel[j] == "Multiple exams") {
              parts[[j]] <- students |> filter(id %in% rvals$multi_id)
            } else {
              d <- design |> filter(exam == sel[j])
              parts[[j]] <- students |>
                filter(exam == d$exam_orig) |>
                slice(seq.int(d$start, d$end))
            }
          }
          out <- dplyr::bind_rows(parts)
          out <- out[stringr::str_order(out$last, locale = "fi_FI"), ] |>
            rename(
              Sukunimi = last,
              Etunimet = first
            ) |>
            mutate(
              Läsnä = ""
            )
          pdf(file, paper = "a4", width = 8.5, height = 11)
          student_list(out, main = nm[i])
          dev.off()
        }
      }
    )
  })

}

shinyApp(ui = ui, server = server)
