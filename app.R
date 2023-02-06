# Requires the following packages
# install.packages(c(
#   "data.table", "dplyr", "DT", "ggplot2", "grid", "gtable",
#   "gridExtra", "readxl", "rjson", "shiny", "shinyjs", "shinyFeedback", "sortable",
#   "stringr"
# ))

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
    tags$style(
      HTML("
        .bucket-list-container {
          min-height: 350px;
        }
        .default-sortable.bucket-list-container {
          margin: 0px;
          padding: 0px;
        }
        .rank-list-item {
          font-size: 12px!important;
        }
        #special_groups {
          font-size: 10px;
        }
        div.red {
          border: 1px solid #d22!important;
        }
      ")
    )
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
            "Select which answers constitute as special arrangements",
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
              DT::dataTableOutput("exam_standard", width = "500px"),
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
              value = "design",
              br(),
              selectInput(
                "exam_rooms",
                "Select rooms for the exam",
                choices = names(rooms),
                multiple = TRUE
              ),
              strong("Partition exams"),
              span("(edit part 2 column to split the exam)"),
              DT::dataTableOutput("exam_controls", width = "66%"),
              br(),
              uiOutput("exam_buckets")
            ),
            tabPanel(
              "Output",
              value = "output",
              br(),
              actionButton(
                "clip_standard",
                label = "Email addresses",
                icon = icon("clipboard")
              ),
              br(),
              actionButton(
                "clip_special",
                label = "Email addresses (special arrangements)",
                icon = icon("clipboard")
              ),
              br(),
              downloadButton(
                "seating",
                "Export seating arrangements"
              ),
              br(),
              downloadButton(
                "students_rooms",
                "Export student lists by room"
              ),
              br(),
              downloadButton(
                "students_exams",
                "Export student lists by exam"
              ),
              br(),
              br(),
              textInput(
                "exam_list_title",
                label = "Title for list of exams",
                value = ""
              ),
              checkboxInput(
                "exam_list_landscape",
                label = "Landscape layout",
                value = TRUE
              ),
              downloadButton(
                "exam_list",
                "Export a list of exams by room"
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  hide_tabs <- function() {
    hideTab(inputId = "nav_tabs", target = "summary")
    hideTab(inputId = "nav_tabs", target = "design")
    hideTab(inputId = "nav_tabs", target = "output")
    hide_room_tabs()
  }

  hide_room_tabs <- function() {
    lapply(seq_along(rooms), function(i) {
      hideTab(inputId = "nav_tabs", target = rooms[[i]]$value)
    })
  }

  show_tabs <- function() {
    showTab(inputId = "nav_tabs", target = "summary")
    showTab(inputId = "nav_tabs", target = "design")
    showTab(inputId = "nav_tabs", target = "output")
  }

  rvals <- reactiveValues(
    exam = NULL,
    start = TRUE,
    plots = list(),
    rooms_ok = list()
  )

  # Add room specific elements
  observe({
    if (rvals$start) {
      rvals$start <- FALSE
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
            fluidRow(
              column(
                width = 2,
                numericInput(
                  paste0(val, "_first_col"),
                  "Starting column",
                  value = 1L,
                  min = 1L,
                  max = 2L,
                  step = 1L
                )
              ),
              column(
                width = 2,
                numericInput(
                  paste0(val, "_row_dist"),
                  "Row distance",
                  value = 0L,
                  min = 0L,
                  step = 1L
                )
              ),
              column(
                width = 2,
                numericInput(
                  paste0(val, "_col_dist"),
                  "Column distance",
                  value = 1L,
                  min = 0L,
                  step = 1L
                )
              ),
              column(
                width = 2,
                strong("Interlaced exams"),
                br(),
                checkboxInput(
                  paste0(val, "_interlace"),
                  label = "Allow",
                  value = TRUE
                )
              ),
              column(
                width = 2,
                strong("Use colors"),
                br(),
                checkboxInput(
                  paste0(val, "_use_colors"),
                  label = "Use",
                  value = FALSE
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                uiOutput(paste0(val, "_output"))
              )
            )
          ),
          target = "design",
          position = "after"
        )
      })
      hide_tabs()
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
        try(readxl::read_xlsx(input$file$datapath), silent = TRUE)
      } else {
        updateTextInput(
          inputId = "exam_list_title",
          value = paste0(
            "Matematiikan ja tilastotieteen tentti ",
            extract_date(basename(input$file$name))
          )
        )
        try(
          read.delim(
              input$file$datapath,
              skipNul = TRUE,
              fileEncoding = "UTF-16LE"
          ),
          silent = TRUE
        )
      }
    } else {
      showFeedbackWarning("file", "Please upload a .csv or an .xlsx file.")
    }
  })

  # Uploaded file is .xlsx
  observeEvent(raw_data(), {
    hide_tabs()
    valid <- TRUE
    if (inherits(raw_data(), "try-error")) {
      showFeedbackDanger("file", "Unable to parse the input file.")
      valid <- FALSE
    }
    req(valid)
    e <- raw_data()
    e_names <- tolower(names(e))
    e_names <- gsub("\\s", "\\.", e_names)
    names(e) <- e_names
    req_names <- c(
      "opiskelijanumero",
      "sukunimi",
      "etunimet",
      "ensisijainen.sähköposti",
      "tentti",
      "lisätietokysymykset"
    )
    mis <- !req_names %in% e_names
    if (any(mis)) {
      showFeedbackDanger(
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
    show_tabs()
    e <- e |>
      rename(
        id = opiskelijanumero,
        last = sukunimi,
        first = etunimet,
        email = ensisijainen.sähköposti,
        exam = tentti,
        special = lisätietokysymykset
      ) |>
      mutate(special = gsub("\r\n", " ", special)) |>
      mutate(special = gsub(cleanup_strings[1L], "", special, fixed = TRUE)) |>
      mutate(special = gsub(cleanup_strings[2L], "", special, fixed = TRUE)) |>
      mutate(id = as.character(id))
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
          rownames = FALSE,
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
    new <- input$exam_controls_cell_edit$value
    row <- input$exam_controls_cell_edit$row
    col <- input$exam_controls_cell_edit$col + 1L
    if (new < 0) {
      new <- 0L
    }
    if (new >= rvals$design$n[row]) {
      new <- rvals$design$n[row] - 1L
    }
    rvals$design[row, col] <- new
    rvals$design[row, col - 1L] <- rvals$design[row, "n"] - rvals$design[row, col]
    rvals$design$ok <- 1L
    for (i in seq_len(nrow(rvals$design))) {
      exam <- rvals$exam_standard |>
        filter(exam == rvals$design$exam[i])
      if (rvals$design$ok[i] == 0L || rvals$design$`part 2`[i] == 0) {
        next
      }
      a <- rvals$design$`part 1`[i]
      b <- a + 1
      sub_last_a <- substr(exam$last[a], 1L, 3L)
      sub_last_b <- substr(exam$last[b], 1L, 3L)
      if (identical(sub_last_a, sub_last_b)) {
        rvals$design[row, "ok"] <- 0L
        showNotification(
          paste0(
            "Family name conflict for exam: ",
            rvals$design$exam[i]
          ),
          type = "warning"
        )
      }
    }
  })

  # Exam room buckets
  observe({
    hide_room_tabs()
    design <- rvals$design
    rooms_input <- input$exam_rooms
    #exam <- rvals$exam_standard
    if (!is.null(design)) {
      exams <- character(0L)
      exams_orig <- character(0L)
      n <- integer(0L)
      start <- integer(0L)
      end <- integer(0L)
      for (i in seq_len(nrow(design))) {
        if (design$`part 2`[i] > 0L) {
          exam <- rvals$exam_standard |>
            filter(exam == design$exam[i])
          a <- design$`part 1`[i]
          b <- a + 1L
          last <- character(2L)
          last[1L] <- paste0(
            " (",
            substr(exam$last[1L], 1L, 3L),
            " - ",
            substr(exam$last[a], 1L, 3L),
            ")"
          )
          last[2L] <- paste0(
            " (",
            substr(exam$last[b], 1L, 3L),
            " - ",
            substr(exam$last[nrow(exam)], 1L, 3L),
            ")"
          )
          exams <- c(
            exams,
            paste0(design$exam[i], last)
          )
          exams_orig <- c(exams_orig, rep(design$exam[i], 2L))
          parts_n <- as.integer(design[i, c("part 1", "part 2")])
          n <- c(n, parts_n)
          start <- c(start, cumsum(c(1L, parts_n[1L])))
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
            input_id = paste0(rooms[[rooms_input[i]]]$value, "_exams"),
            css_id = paste0(rooms[[rooms_input[i]]]$value, "_rank_list")
          )
        })
      )
      args$header <- ""
      args$group_name = "bucket_list_group"
      args$orientation = "horizontal"
      n_rooms <- length(rooms_input)
      if (n_rooms > 0) {
        output$exam_buckets <- renderUI({
          fluidRow(
            column(
              tags$b("Drag exams to the desired rooms"),
              width = min(12L, (n_rooms + 1L) * 3L),
              do.call("bucket_list", args = args)
            )
          )
        })
      } else {
        output$exam_buckets <- renderUI(br())
      }
    }
  })

  # Exam to room allocation
  lapply(seq_along(rooms), function(i) {
    j <- nm[i]
    val <- rooms[[j]]$value
    out_ <- paste0(val, "_output")
    observe({
      sel <- input[[paste0(val, "_exams")]]
      ilc <- input[[paste0(val, "_interlace")]]
      first <- input[[paste0(val, "_first_col")]]
      row <- input[[paste0(val, "_row_dist")]]
      col <- input[[paste0(val, "_col_dist")]]
      use_colors <- input[[paste0(val, "_use_colors")]]
      if (length(sel) > 0L) {
        showTab(inputId = "nav_tabs", target = val)
        e <- isolate(rvals$design_partition) |>
          filter(exam %in% sel)
        e <- e[match(sel, e$exam), ]
        room <- try(
          process_seating(e, ilc, first, row, col, sel, rooms[[j]]),
          silent = TRUE
        )
        if (inherits(room, "try-error")) {
          # Add red border to rank_list container when out of space
          jss_str <- paste0("$('#", val, "_rank_list').parent().addClass('red')")
          runjs(jss_str)
          showNotification(
            paste0("Room ", j, " does not have enough space!"),
            type = "error"
          )
          isolate({
            rvals$plots[[val]] <- NULL
            rvals$rooms_ok[[val]] <- FALSE
          })
          output[[out_]] <- renderUI({
            br()
          })
          hideElement(id = paste0(val, "_seating"))
          hideElement(id = paste0(val, "_students"))
        } else {
          assigned <- room$layout |> filter(exam != "")
          p <- ggplot(room$layout, aes(x = x, y = y, width = 1, height = 1)) +
            coord_equal() +
            theme_classic(base_size = 12) +
            guides(color = "none") +
            scale_x_continuous(breaks = seq.int(1L, max(room$layout$x))) +
            scale_y_continuous(breaks = seq.int(1L, max(room$layout$y))) +
            theme(
              axis.text.x = element_text(
                hjust = 0.5,
                size = 12,
                vjust = 0.5,
                margin = margin(-10, 0, 0, 0)
              ),
              axis.text.y =  element_text(
                hjust = 0.5,
                size = 12,
                vjust = 0.5,
                margin = margin(0, -15, 0, 0)
              ),
              axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              legend.direction = "vertical",
              legend.key.size = unit(1, "cm"),
              legend.position = "bottom",
              legend.title = element_blank()
            )
          if (use_colors) {
            p <- p +
              geom_tile(
                data = assigned,
                aes(x = x, y = y, width = 1, height = 1, fill = exam)
              ) +
              geom_tile(fill = "transparent", color = "black")
          } else {
            p <- p +
              geom_point(data = assigned, aes(shape = exam), color = "black", size = 4.5) +
              scale_shape_manual(values = seq_len(nlevels(assigned$exam))) +
              geom_tile(fill = "transparent", color = "black") +
              guides(fill = "none")
          }
          isolate({
            rvals$rooms_ok[[val]] <- TRUE
            rvals$plots[[val]] <- p
          })
          output[[out_]] <- renderUI({
            renderPlot(p, height = 450 + 18 * nrow(e))
          })
          showElement(id = paste0(val, "_students"))
          showElement(id = paste0(val, "_seating"))
          jss_str <- paste0("$('#", val, "_rank_list').parent().removeClass('red')")
          runjs(jss_str)
        }
      } else {
        isolate({
          rvals$rooms_ok[[val]] <- FALSE
          rvals$plots[[val]] <- NULL
        })
        output[[out_]] <- renderUI({
          br()
        })
        hideTab(inputId = "nav_tabs", target = val)
        hideElement(id = paste0(val, "_seating"))
        hideElement(id = paste0(val, "_students"))
        jss_str <- paste0("$('#", val, "_rank_list').parent().removeClass('red')")
        runjs(jss_str)
      }
    })
  })

  # Copy email addresses to clipboard
  observeEvent(input$clip_standard, {
    if (!is.null(rvals$exam)) {
      emails <- rvals$exam |>
        filter(!special %in% input$special_groups) |>
        pull(email)
      clip_str <- paste0(unique(emails), collapse = ";")
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
      clip_str <- paste0(unique(emails), collapse = ";")
      js$copyToClipboard(clip_str)
    } else {
      js$copyToClipboard("")
    }
  })

  # Seating arrangement download links
  output$seating <- downloadHandler(
    filename = "seating.pdf",
    content = function(file) {
      pdf(file, paper = "a4", width = 8.5, height = 11)
      for (i in seq_along(rooms)) {
        val <- rooms[[i]]$value
        if (rvals$rooms_ok[[val]]) {
          plot(
            rvals$plots[[val]] +
              theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "in"))
          )
        }
      }
      dev.off()
    }
  )

  # Student list download links by exam
  output$students_exams <- downloadHandler(
    filename = "exams.pdf",
    content = function(file) {
      e <- rvals$exam
      ue <- unique(e$exam)
      pdf(file, paper = "a4", width = 8.5, height = 11)
      for (i in seq_along(ue)) {
        out <- e |>
          filter(!special %in% input$special_groups) |>
          filter(exam == ue[i]) |>
          student_list(main = ue[i])
      }
      out <- e |>
        filter(special %in% input$special_groups) |>
        student_list(main = "Lisäajalliset")
      dev.off()
    }
  )

  # Student list download links by room
  output$students_rooms <- downloadHandler(
    filename = "rooms.pdf",
    content = function(file) {
      pdf(file, paper = "a4", width = 8.5, height = 11)
      for (i in seq_along(rooms)) {
        val <- rooms[[i]]$value
        if (rvals$rooms_ok[[val]]) {
          design <- rvals$design_partition
          sel <- input[[paste0(val, "_exams")]]
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
          out <- out[stringr::str_order(out$last, locale = "fi_FI"), ]
          student_list(out, main = nm[i])
        }
      }
      dev.off()
    }
  )

  # List of exams by room
  output$exam_list <- downloadHandler(
    filename = "exam_list.pdf",
    content = function(file) {
      rooms_exams <- vector(mode = "list", length = length(rooms))
      if (input$exam_list_landscape) {
        pdf(file, paper = "a4r", width = 11, height = 8.5)
      } else {
        pdf(file, paper = "a4", width = 8.5, height = 11)
      }
      grid.text(
        input$exam_list_title,
        x = 0.08,
        y = 0.90,
        just = "left",
        gp = gpar(fontsize = 24, fontface = "bold")
      )
      for (i in seq_along(rooms)) {
        val <- rooms[[i]]$value
        rooms_exams[[i]] <- gsub(
          pattern = "Multiple exams",
          replacement = "Kahden tai useamman tentin tekijät",
          x = input[[paste0(val, "_exams")]],
          fixed = TRUE
        )
      }
      assigned <- which(lengths(rooms_exams) > 0L)
      if (length(assigned) == 1L) {
        grid.text(
          paste0("Kaikki tentit salissa ", nm[assigned]),
          x = 0.08,
          y = 0.80,
          just = "left",
          gp = gpar(fontsize = 18)
        )
      } else {
        y <- 0.85
        for (i in assigned) {
          y <- y - 0.06
          grid.text(
            nm[i],
            x = 0.08,
            y = y,
            just = "left",
            gp = gpar(fontsize = 20, fontface = "bold")
          )
          for (exam in rooms_exams[[i]]) {
            y <- y - 0.045
            grid.text(
              exam,
              x = 0.08,
              y = y,
              just = "left",
              gp = gpar(fontsize = 16)
            )
          }
        }
      }
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)
