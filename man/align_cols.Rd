% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/align_cols.R
\name{align_cols}
\alias{align_cols}
\title{Create column alignment strings for \code{knitr::kable}}
\usage{
align_cols(l = NULL, r = NULL, total = NULL)
}
\arguments{
\item{l}{Number of left-aligned columns. Default: NULL}

\item{r}{Number of right-aligned columns. Default: NULL}

\item{total}{Total number of columns. Default: NULL}
}
\value{
A string that can be passed to \code{knitr::kable}, or any other function
that takes alignments in the same format.
}
\description{
\code{knitr::kable} takes column alignments as a single string, such as
\code{"llrrrr"} for a table with 2 left-aligned columns followed by 4 right-aligned
ones. This function is a quick utility for creating these strings programmatically
by the number of left- or right-aligned columns, and/or by calculating column
counts on the fly. It makes some assumptions, such as that all left-aligned
columns come first, then all right-aligned ones; that's a very common case I
developed this for, but your mileage may vary. Out of the 3 arguments (left, right,
or total), you need to supply 2.
}
\examples{
# self_rated_health has 3 categorical columns and one numeric column,
# so traditionally this would be formatted in a table with 3 left-aligned
# columns and 1 right-aligned column.
align_cols(l = 3, r = 1)

# I might also want to do this on the fly in a number of ways:
align_cols(l = 3, total = ncol(self_rated_health))
# maybe a little risky if you don't know the data
align_cols(
    r = sum(sapply(self_rated_health, is.numeric)),
    total = ncol(self_rated_health)
)
# For programming this can be useful after reshaping
n_responses <- length(levels(self_rated_health$response))
health_wide <- tidyr::pivot_wider(self_rated_health,
    id_cols = group, names_from = response
)
alignment <- align_cols(r = n_responses, total = ncol(health_wide))
if (require("knitr")) {
    knitr::kable(health_wide, align = alignment)
}

}
\seealso{
\code{\link[knitr:kable]{knitr::kable()}}
}
\keyword{tables}
\keyword{viz-utils}
