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

process_seating <- function(e, sel, room) {
  room$layout$exam <- ""
  lo <- room$layout
  row_counts <- lo |>
    group_by(y) |>
    summarise(
      idx = list(x[x %% 2 == 1]),
      n = lengths(idx)
    )
  nr <- nrow(row_counts)
  rows <- seq_len(nr)
  reserved <- logical(nr)
  for (i in seq_len(nrow(e))) {
    if (all(reserved)) {
      stop("Room is full")
    }
    free_row <- min(rows[!reserved])
    idx <- seq.int(free_row, nr, by = 2)
    space <- cumsum(row_counts$n[idx])
    m <- e$n[i]
    fit_idx <- which(space >= m)
    if (length(fit_idx) == 0) {
      stop("Not enough space for exam", e$exam)
    }
    last <- idx[fit_idx[1L]]
    idx_used <- idx[seq_len(which(idx == last))]
    reserved[idx_used] <- TRUE
    for (j in idx_used) {
      fill_x <- row_counts$idx[[j]]
      fill_x <- fill_x[seq_len(min(m, length(fill_x)))]
      room$layout[lo$x %in% fill_x & lo$y == j, "exam"] <- e$exam[i]
      m <- m - row_counts$n[j]
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
  cols <- c("Sukunimi", "Etunimet", "Läsnä")
  y <- x %>% select(all_of(cols)) %>% distinct()
  n <- nrow(y)
  y_list <- list()
  tables <- list()
  lim <- 60L
  if (n > lim) {
    y_list[[1L]] <- y |> slice(seq.int(1L, lim))
    y_list[[2L]] <- y |> slice(seq.int(lim + 1L, n))
  } else {
    y_list[[1]] <- y
  }
  for (i in seq_along(y_list)) {
    tables[[i]] <- tableGrob(
      y_list[[i]],
      rows = NULL,
      theme = ttheme_minimal(
        base_size = 9,
        padding = unit(c(3, 2), "mm"),
        core = list(
          bg_params = list(fill = "white", col = "black"),
          fg_params = list(hjust = 0, x = 0.05)
        ),
        rowhead = list(
          fg_params = list(fontface = "plain", hjust = 0, x = 0)
        )
      )
    )
  }
  if (length(y_list) > 1L) {
    grid.arrange(
      gtable_combine(tables[[1L]], tables[[2L]], along = 1),
      ncol = 1,
      top = textGrob(main)
    )
  } else {
    grid.arrange(tables[[1L]], ncol = 1, top = textGrob(main))
  }
}
