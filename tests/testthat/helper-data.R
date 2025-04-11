dummy_pct <- function(nrow = 3, offset = 0) {
  x <- LETTERS[seq_len(nrow)]
  y <- seq(offset, 1, length.out = nrow)
  data.frame(name = x, value = y)
}

dummy_int <- function(nrow = 3, offset = 1) {
  x <- LETTERS[seq_len(nrow)]
  y <- seq_len(nrow) + offset
  data.frame(name = x, value = y)
}

dummy_stack <- function(nrow = 3, times = 2) {
  g <- rep(letters[seq_len(times)], each = nrow)
  x <- rep(LETTERS[seq_len(nrow)], times = times)
  y <- 1
  data.frame(group = g, name = x, value = y)
}

dummy_dodge <- function(nrow = 5, ngrps = 2, jitter = 0.05) {
  grps <- sprintf("grp%s", 1:ngrps)
  set.seed(1)
  data <- replicate(ngrps, 
                    dummy_pct(nrow) |> 
                      dplyr::mutate(value = value + runif(nrow, -jitter, jitter)), 
                    simplify = FALSE)
  data <- setNames(data, grps)
  dplyr::bind_rows(data, .id = "group") |>
    dplyr::arrange(name)
}
