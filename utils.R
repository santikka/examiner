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

process_seating <- function(e, room) {
  room$layout$exam <- ""
  lo <- room$layout
  free_row <- 1L
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
    idx <- seq.int(free_row, nr, by = 2)
    space <- cumsum(row_counts$n[idx])
    m <- e$n[i]
    last <- idx[which(space >= m)[1L]]
    idx_used <- idx[seq_len(which(idx == last))]
    reserved[idx_used] <- TRUE
    free_row <- min(rows[!reserved])
    for (j in idx_used) {
      fill_x <- row_counts$idx[[j]]
      fill_x <- fill_x[seq_len(min(m, length(fill_x)))]
      room$layout[lo$x %in% fill_x & lo$y == j, "exam"] <- e$exam[i]
      m <- m - row_counts$n[j]
    }
  }
  room
}
