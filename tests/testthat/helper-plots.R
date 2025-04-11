test_plot <- function(type = "point", vertical = TRUE) {
  if (type == "point") {
    ggplot2::ggplot(iris, ggplot2::aes(x = Sepal.Width, y = Sepal.Length)) +
        ggplot2::geom_point()
  } else {
    # df <- data.frame(name = LETTERS[1:3], value = 4:6)
    df <- dummy_int(offset = 3)
    # lowest ~ 5.0, highest ~ 6.6
    if (vertical) {
      ggplot2::ggplot(df, ggplot2::aes(x = name, y = value)) +
        ggplot2::geom_col()
    } else {
      ggplot2::ggplot(df, ggplot2::aes(y = name, x = value)) +
        ggplot2::geom_col()
    }
  }
}

plot_info <- function(path) {
    img <- magick::image_read(path)
    magick::image_info(img)
}

make_barcont_plots <- function() {
  params <- expand.grid(vertical = c(TRUE, FALSE), cont = c(TRUE, FALSE)) |>
    dplyr::mutate(id = c("vert_scale", "horiz_scale", "vert_unscale", "horiz_unscale"))
  
  plots <- dplyr::mutate(params, plot = purrr::pmap(list(vertical, cont), function(vertical, cont) {
    p <- test_plot(type = "bar", vertical = vertical)
    if (cont) {
      if (vertical) {
        p <- p + scale_y_barcontinuous(top = 0.5) # exaggerate for testing
      } else {
        p <- p + scale_x_barcontinuous(top = 0.5)
      }
    }
    p
  })) |>
    dplyr::mutate(range = purrr::map2(plot, vertical, function(p, vert) {
      if (vert) {
        ggplot2::get_guide_data(p, "y")
      } else {
        ggplot2::get_guide_data(p, "x")
      }
    }))
  plots
}
