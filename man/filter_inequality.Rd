% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/4a_equity_calculate_inequality.R
\name{filter_inequality}
\alias{filter_inequality}
\title{Filter Subnational Inequality Metrics}
\usage{
filter_inequality(
  .data,
  indicator,
  denominator = c("dhis2", "anc1", "penta1", "penta1derived")
)
}
\arguments{
\item{.data}{A \code{cd_inequality} tibble created by \code{calculate_inequality}.}

\item{indicator}{A character vector of health indicators to include (e.g., \code{"penta3"}, \code{"measles1"}).}

\item{denominator}{A character vector of denominators to filter by (e.g., \code{"dhis2"}, \code{"anc1"}).}

\item{...}{Not used}
}
\value{
A tibble containing filtered subnational inequality metrics for
the specified indicators and denominators.
}
\description{
\code{filter_inequality} refines the output of \code{calculate_inequality} by selecting
specific indicators and denominators for analysis. It extracts and renames relevant
columns to streamline further analyses.
}
\examples{
\dontrun{
# Filter for Penta-3 coverage using DHIS-2 denominator
filtered_data <- inequality_metrics \%>\%
  filter_inequality(indicator = "penta3", denominator = "dhis2")
}

}
