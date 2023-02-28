extract_date <- function(x) {
  date_pattern <- "[^0-9]*([1-3]{0,1}[0-9]{1}\\.[1]{0,1}[1-9]{1}\\.).*"
  m <- regexec(date_pattern, x)
  date <- try({regmatches(x, m)[[1L]][2L]}, silent = TRUE)
  if (inherits(date, "try-error")) {
    ""
  } else {
    date
  }
}

process_layout <- function(x) {
  for (i in seq_along(x)) {
    l <- x[[i]]$layout
    o <- x[[i]]$offset
    nc <- length(l)
    cols <- vector(mode = "list", length = nc)
    for (j in seq_len(nc)) {
      cols[[j]] <- data.frame(x = j, y = o[[j]] + seq_len(l[[j]]))
    }
    x[[i]]$layout <- dplyr::bind_rows(cols)
    x[[i]]$offset <- NULL
  }
  x
}

process_seating <- function(e, ilc, cont, first, row, col, sel, room) {
  room$layout$exam <- ""
  lo <- room$layout
  max_col <- max(lo$x)
  row_counts <- lo |>
    group_by(y) |>
    summarise(
      idx = list(x[x %in% seq.int(first, max_col, by = col + 1L)]),
      n = lengths(idx)
    )
  nr <- nrow(row_counts)
  rows <- seq_len(nr)
  reserved <- logical(nr)
  free_row <- 0L
  for (i in seq_len(nrow(e))) {
    if (all(reserved)) {
      stop("Room is full")
    }
    if (ilc) {
      free_row <- min(rows[!reserved])
    } else {
      if (i > 1L) {
        free_row <- max(rows[reserved]) + row + 1L
      } else {
        free_row <- 1L
      }
    }
    idx <- seq.int(free_row, nr, by = row + 1L)
    space <- cumsum(row_counts$n[idx])
    m <- e$n[i]
    fit_idx <- which(space >= m)
    if (length(fit_idx) == 0) {
      stop("Not enough space for exam", e$exam)
    }
    idx_last <- idx[fit_idx[1L]]
    idx_used <- idx[seq_len(which(idx == idx_last))]
    for (j in idx_used) {
      row_idx <- row_counts$idx[[j]]
      row_offset <- length(row_idx) - row_counts$n[j]
      fill_x <- row_idx[row_offset + seq_len(min(m, row_counts$n[j]))]
      fill_n <- length(fill_x)
      room$layout[lo$x %in% fill_x & lo$y == j, "exam"] <- e$exam[i]
      m <- m - fill_n
      if (cont) {
        row_counts$n[j] <- row_counts$n[j] - fill_n
      } else {
        row_counts$n[j] <- 0L
      }
      if (row_counts$n[j] == 0L) {
        reserved[j] <- TRUE
      }
    }
  }
  room$layout$exam <- factor(room$layout$exam, levels = sel)
  room
}

student_list <- function(x, main) {
  title <- textGrob(
    main, x = unit(0.025, "npc"),
    gp = gpar(fontsize = 10, fontface = "bold"),
    just = "left"
  )
  cols <- c("id", "last", "first")
  y <- x |>
    select(all_of(cols)) |>
    distinct() |>
    rename(
      `Op. nro.` = id,
      Sukunimi = last,
      Etunimet = first
    ) |>
    mutate(
      L. = ""
    )
  n <- nrow(y)
  tables <- list()
  lim <- 60L
  n_parts <- ceiling(n / lim)
  y_list <- lapply(seq_len(n_parts), function(p) {
    y |> slice(seq.int(lim * (p - 1L) + 1L, lim * p))
  })
  for (i in seq_along(y_list)) {
    tables[[i]] <- tableGrob(
      y_list[[i]],
      rows = NULL,
      theme = ttheme_minimal(
        base_size = 8,
        padding = unit(c(2, 2), "mm"),
        core = list(
          bg_params = list(fill = "white", col = "black"),
          fg_params = list(hjust = 0, x = 0.03)
        ),
        rowhead = list(
          fg_params = list(fontface = "plain", hjust = 0, x = 0)
        )
      )
    )
  }
  n_pages <- ceiling(n_parts / 2)
  maingrob <- textGrob(
    main,
    x = 0.5,
    y = -0.25,
    gp = gpar(fontsize = 14, fontface = "bold")
  )
  for (i in seq_len(n_pages)) {
    if (2L * i > length(tables)) {
      grid.arrange(tables[[2L * (i - 1L) + 1L]], ncol = 1L, top = maingrob)
    } else {
      grid.arrange(
        gtable_combine(tables[[2L * (i - 1L) + 1L]], tables[[2L * i]], along = 1L),
        ncol = 1L,
        top = maingrob
      )
    }
  }
}
