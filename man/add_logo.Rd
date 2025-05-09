% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_logo.R
\name{add_logo}
\alias{add_logo}
\title{Easily add a logo to a ggplot}
\usage{
add_logo(
  plot,
  image = NULL,
  position = c("right", "left"),
  height = 0.05,
  width = NULL,
  place_inside = TRUE
)
}
\arguments{
\item{plot}{A \code{ggplot} object onto which the logo will be placed.}

\item{image}{Either a string giving the path or URL to an image file to be
read by \code{magick::image_read}; the results of reading a file or manipulating
an image already with \code{magick::image_read} or other \code{magick} functions;
a \code{ggplot} object / grob; or \code{NULL}, the default. If \code{NULL}, the image will
come from the file at \code{system.file("extdata/logo.svg", package = "stylehaven")}.
As built, this is a logo for DataHaven, but that file can be replaced for
repackaging this library for other organizations or projects.}

\item{position}{String, either "left" or "right", giving the side on which
the logo should be aligned. Default: "right"}

\item{height}{Numeric: the height of the logo, as a percentage of the height
of the image given in \code{plot}. Adjust as necessary based on the dimensions
of the logo. Default: 0.05}

\item{width}{Numeric: the width of the logo, as a percentage of the image given in \code{plot}. If \code{NULL}, the default, the width will be calculated based on the aspect ratio of the image. Must be given explicitly for \code{ggplot} objects, as they don't inherently have their own dimensions.}

\item{place_inside}{Logical: if \code{TRUE} (default), logo will be drawn within
the plotting area; otherwise, an additional grob is built below the plot,
meaning the logo could later be cropped away, which may be undesirable.}
}
\value{
A \code{ggplot} object.
}
\description{
This function wraps around a few functions from \code{patchwork} and
\code{magick} to add a logo (or other annotation) to the bottom of a \code{ggplot} plot,
an otherwise tedious and easy to forget process. It's meant to be flexible in
the types of objects it can place; as a result, it's less flexible in their
placement and customization. For more specific needs, the source of this
function should be easy to build upon. Previously it used \code{cowplot}, but that
package is a bit too buggy.
}
\examples{
library(ggplot2)
p <- ggplot(iris, ggplot2::aes(x = Sepal.Length)) +
    geom_density() +
    labs(title = "Test chart", caption = "Source: 2019 ACS 5-year estimates") +
    theme(plot.caption = element_text(hjust = 0))

add_logo(p)
# this one is too tall--place it outside so it doesn't block axis
add_logo(p,
    magick::image_read(system.file("extdata/25th_logo.png", package = "stylehaven")),
    height = 0.1, place_inside = FALSE
)

# This example logo is not all that attractive, but shows how you might
# attach a ggplot grob as a dynamically-created logo
dummy_data <- data.frame(
    town = letters[1:4],
    pop = c(21000, 40000, 81000, 36000)
)

gg_logo <- ggplot(dummy_data, aes(x = town, y = pop)) +
    geom_col(width = 0.8, fill = "skyblue") +
    annotate(
        geom = "text", label = "DataHaven", x = 0.6, y = 6e4, hjust = 0,
        family = "mono", size = 5
    ) +
    theme_void()

add_logo(p, gg_logo, width = 0.2, height = 0.1)
}
\seealso{
\code{\link[magick:editing]{magick::image_read()}}, \code{\link[patchwork:plot_layout]{patchwork::plot_layout()}}
}
\keyword{exporting}
