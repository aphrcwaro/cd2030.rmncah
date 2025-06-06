% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/6_service_utilization.R
\name{get_excel_version}
\alias{get_excel_version}
\title{Format Service Utilization Data for Excel Export}
\usage{
get_excel_version(.data)
}
\arguments{
\item{.data}{A \code{cd_service_utilization} object produced by \code{\link[=compute_service_utilization]{compute_service_utilization()}}.}
}
\value{
A tibble in wide format with:
\itemize{
\item One row per indicator per admin unit
\item One column per year
\item A human-readable label for each indicator (\code{indiclabel})
}
}
\description{
Transforms a \code{cd_service_utilization} object into a wide-format table
with labeled indicators, suitable for export to Excel.
}
\details{
The output table includes population, OPD/IPD service utilization,
under-5 service metrics, reporting completeness, and case fatality rates,
reshaped to facilitate Excel review or reporting.
}
\examples{
\dontrun{
  x <- compute_service_utilization(dat, admin_level = "adminlevel_1")
  get_excel_version(x)
}

}
