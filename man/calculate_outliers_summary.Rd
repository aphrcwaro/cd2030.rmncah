% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1a_checks_outlier.R
\name{calculate_outliers_summary}
\alias{calculate_outliers_summary}
\title{Annual Summary of Outlier-Free Reporting Rates}
\usage{
calculate_outliers_summary(
  .data,
  admin_level = c("national", "adminlevel_1", "district"),
  region = NULL
)
}
\arguments{
\item{.data}{A \code{cd_data} object with \verb{_outlier5std} flag columns (0 = valid, 1 = outlier).}

\item{admin_level}{Character. Aggregation level. One of:
\itemize{
\item \code{"national"}
\item \code{"adminlevel_1"}
\item \code{"district"}
}}

\item{region}{Optional. Restrict to a specific region (only when \code{admin_level = "adminlevel_1"}).}
}
\description{
\code{calculate_outliers_summary} computes yearly percentages of non-outlier values
for each health indicator, based on precomputed 5-MAD outlier flags. Summarizes
data quality by administrative level and year.
}
\details{
Outliers are defined using a Hampel filter with a 5-MAD threshold.

This function:
\itemize{
\item Aggregates outlier flags by year and administrative level
\item Computes the share of valid (non-outlier) values per indicator
\item Adds overall summary metrics:
\itemize{
\item \code{mean_out_all}: average non-outlier rate across all indicators
\item \code{mean_out_four}: average across a subset of key indicators (excluding IPD)
}
}

Values are expressed as percentages (0–100).

@return A tibble of class \code{cd_outlier}.
}
\examples{
\dontrun{
  calculate_outliers_summary(data, admin_level = "district")
}

}
