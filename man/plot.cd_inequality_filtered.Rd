% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.inequality.R
\name{plot.cd_inequality_filtered}
\alias{plot.cd_inequality_filtered}
\title{Plot Subnational Health Coverage Analysis}
\usage{
\method{plot}{cd_inequality_filtered}(x, ...)
}
\arguments{
\item{x}{A \code{cd_inequality_filtered} object returned by \code{\link[=filter_inequality]{filter_inequality()}}.}

\item{...}{Additional arguments passed to the plotting function.}
}
\value{
A \code{ggplot} object displaying the subnational health coverage plot.
}
\description{
Generates a plot to visualize health coverage data across subnational units,
distinguishing between the national mean and subnational coverage. The Mean
Absolute Difference to the Mean (MADM) is displayed as an indicator on the
y-axis.
}
\examples{
\dontrun{
data <- filter_inequality(.data, "Kenya",
  admin_level = "district",
  indicator = "measles1", denominator = "penta1"
)
plot(data)
}

}
