% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.health_system.R
\name{plot.cd_health_system_metric}
\alias{plot.cd_health_system_metric}
\title{Plot Health Metrics for Admin 1 Units}
\usage{
\method{plot}{cd_health_system_metric}(
  x,
  indicator = c("ratio_fac_pop", "ratio_hos_pop", "ratio_hstaff_pop", "ratio_bed_pop"),
  national_score = NULL,
  target = NULL
)
}
\arguments{
\item{x}{Data frame containing admin 1 level indicators.}

\item{indicator}{A single metric to plot. Options include score_* and ratio_*.}

\item{national_score}{National benchmark for dashed comparison line.}

\item{target}{Optional target value for additional line.}
}
\value{
A ggplot object.
}
\description{
Plots individual health system indicators by admin 1 level and compares with national value or target.
}
