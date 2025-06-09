#' Compare District vs Admin 1 Health Metrics
#'
#' Generates scatterplots with linear fit and identity line comparing district to admin 1 metrics or coverage vs density.
#'
#' @param x A data frame from `calculate_health_system_comparison()`.
#' @param indicator One of ratio_* or cov_instdeliveries_* (requires `denominator`).
#' @param denominator Optional for cov_ indicators. Must be one of 'dhis2', 'anc1', 'penta1'.
#'
#' @return A ggplot object.
#'
#' @export
plot.cd_health_system_comparison <- function(x,
                                              indicator = c(
                                                'cov_instdeliveries_hstaff',
                                                'ratio_opd_u5_hstaff',
                                                'ratio_opd_u5_hos',
                                                'ratio_ipd_u5_bed'),
                                              denominator = NULL
) {

  indicator <- arg_match(indicator)

  if (str_detect(indicator, 'cov_')) {
    denominator <- arg_match(denominator, c('dhis2', 'anc1', 'penta1'))
  }
  labels <- list(
    cov_instdeliveries_hstaff = list(
      x = paste0('ad1_cov_instdeliveries_', denominator),
      y = 'ad1_ratio_hstaff_pop',
      x_label = 'Institutional delivery coverage rate (%)',
      y_label = "Number of core health workforce per 10,000 population",
      title = 'Institutional delivery coverage rate (%) by health workforce density by admin level 1',
      caption = paste0('Denominator derived from', denominator, 'data')
    ),
    ratio_opd_u5_hstaff = list(
      x = 'ad1_ratio_opd_u5_pop',
      y = 'ad1_ratio_hstaff_pop',
      x_label = 'Number of under-5 OPD visits per child per year',
      y_label = "Number of core health workforce per 10,000 population",
      title = 'Under-5 OPD Visits by health workforce density by admin level 1',
      caption = NULL
    ),
    ratio_opd_u5_hos = list(
      x = 'ad1_ratio_opd_u5_pop',
      y = 'ad1_ratio_hos_pop',
      x_label = 'Number of under-5 OPD visits per child per year',
      y_label = "Number of Hospitals per 100,000 population",
      title = 'Under-5 OPD Visits by hospital density by admin level 1',
      caption = NULL
    ),
    ratio_ipd_u5_bed = list(
      x = 'ad1_ratio_ipd_u5_pop',
      y = 'ad1_ratio_bed_pop',
      x_label = 'Number of Under-5 IPD Admission per 100 children per year',
      y_label = "Number of hospital beds per 10,000 population",
      title = 'Under-5 IPD Admissiong by hospital beds density by admin level 1',
      caption = NULL
    )
  )

  labels_prop <- labels[[indicator]]

  r2 <- summary(lm(as.formula(paste0(labels_prop$y, ' ~ ', labels_prop$x)), data = x))$r.squared

  x %>%
    ggplot(aes(x = !!sym(labels_prop$x), y = !!sym(labels_prop$y))) +
      geom_point(aes(colour = 'District'), size = 2) +
      geom_smooth(aes(colour = 'Linear fit'), method = "lm", se = FALSE, formula = y ~ x) +
      # geom_abline(aes(slope = 1, intercept = 0, colour = 'Diagonale'), linetype = "dashed", show.legend = TRUE) +
      # geom_text(aes(label = !!sym(label), size = 1, color = "#045a8d", hjust = -0.1, vjust = -0.1)) +
      scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
      # scale_colour_manual(values = c('District' = '#045a8d', 'Linear fit' = 'black', 'Diagonale' = 'red')) +
      scale_colour_manual(values = c('District' = '#045a8d', 'Linear fit' = 'black')) +
      labs(
        title = labels_prop$title,
        x = labels_prop$x_label,
        y = labels_prop$y_label,
        caption = paste0(
          paste0("R-squared = ", round(r2, 4)),
          if (!is.null(labels_prop$caption)) paste0("\n", labels_prop$caption) else ""
        )
      ) +
      # theme_minimal(base_size = 12) +
      cd_plot_theme() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 9, color = "gray40"),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        axis.text = element_text(size = 10)
      )
}

#' Plot Health Metrics for Admin 1 Units
#'
#' Plots individual health system indicators by admin 1 level and compares with national value or target.
#'
#' @param x Data frame containing admin 1 level indicators.
#' @param indicator A single metric to plot. Options include score_* and ratio_*.
#' @param national_score National benchmark for dashed comparison line.
#' @param target Optional target value for additional line.
#'
#' @return A ggplot object.
#'
#' @export
plot.cd_health_system_metric <- function(x,
                                         indicator = c(
                                           'ratio_fac_pop',
                                           'ratio_hos_pop',
                                           'ratio_hstaff_pop',
                                           'ratio_bed_pop'
                                         ),
                                         national_score = NULL,
                                         target = NULL) {

  indicator <- arg_match(indicator)

  labels <- list(
    ratio_fac_pop = list(
      title = "Health facility density per 10,000 population (all facilities) by admin 1 level",
      ylab = NULL,
      ylim = c(0, 10),
      breaks = seq(0, 10, 2)
    ),
    ratio_hos_pop = list(
      title = "Hospital density per 100,000 population by admin 1 level",
      ylab = NULL,
      ylim = c(0, 10),
      breaks = seq(0, 10, 2)
    ),
    ratio_hstaff_pop = list(
      title = "Health workforce* density per 10,000 population by admin 1 level",
      caption = paste0(
        "* physicians, non-clinique physicians, nurses & midwives\n"
      ),
      ylab = "Number of core health workforce",
      ylim = c(0, 30),
      breaks = seq(0, 30, 5)
    ),
    ratio_bed_pop = list(
      title = 'Hospital beds density by admin 1 level',
      ylab = NULL,
      ylim = c(0, 30),
      breaks = seq(0, 30, 5)
    )
  )

  lab_options <- labels[[indicator]]

  nat_density <- paste0('National level density: ', round(national_score, 1))
  target_label <- paste0('Benchmark: ', round(as.numeric(target, 1)))

  p <- x %>%
    ggplot(aes(x = reorder(adminlevel_1, !!sym(indicator)), y = !!sym(indicator))) +
    geom_col(aes(colour = 'Admin 1evel 1 Density'), fill = "#2c7fb8", width = 0.6) +
    geom_hline(aes(colour = nat_density, yintercept = national_score), linetype = "dashed", linewidth = 0.5) +
    geom_text(aes(label = round(!!sym(indicator), 1)), hjust = -0.1, size = 3)

  if (!is.null(target)) {
    p <- p + geom_hline(aes(colour = target_label, yintercept = target), linetype = "solid", linewidth = 0.5)
  }

  p +
    coord_flip() +
    scale_y_continuous(limits = lab_options$ylim, breaks = lab_options$breaks) +
    scale_colour_manual(values = set_names(
      c('#2c7fb8', 'maroon', 'green4'),
      c('Admin 1evel 1 Density', nat_density, target_label)
    )) +
    labs(
      title = lab_options$title,
      x = NULL,
      y = lab_options$ylab,
      caption = lab_options$caption
    ) +
    cd_plot_theme() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 9, color = "gray40"),
      axis.text.y = element_text(size = 9)
    )
}

#' Plot National Health System Metrics
#'
#' Creates a horizontal bar plot for either health system performance or density
#' metrics at national level.
#'
#' @param .data A one-row data frame containing national-level metrics.
#' @param metric Either 'performance' or 'density'.
#'
#' @return A ggplot object.
#'
#' @export
plot_national_health_metric <- function(.data, metric = c('performance', 'density')) {

  metric <- arg_match(metric)

  metric_labels <- list(
    performance = list(
      columns = c('score_total', 'score_infrastructure', 'score_workforce', 'score_utilization'),
      labels = c("Total score", "Score infrastructure*", "Score workforce**", "Score service utilization***"),
      title = 'Health system performance at national level',
      caption = paste0(
        "* Score facility density & bed density\n",
        "** Score health workforce density (physicians, non-clinique physicians, nurses & midwives)\n",
        "*** Score outpatient service utilization & inpatient service utilization"
      )
    ),
    density = list(
      # columns = c('ratio_fac_pop', 'ratio_hstaff_pop', 'ratio_bed_pop', 'ratio_opd_pop', 'ratio_ipd_pop'),
      # labels = c("Density of Facilities *", "Density of core health workforce *", "Density of beds *", "Ratio OPD/Population **", "Ratio IPD/Population ***"),
      columns = c('ratio_fac_pop', 'ratio_hstaff_pop', 'ratio_bed_pop'),
      labels = c("Health Facility Density *", "Health workforce density (Core health professionals) *", "Hospital Beds Density *"),
      title = 'Health system density at national level',
      # caption = '* per 10,000 population\n** per person per year\n*** per 100 population per year'
      caption = '* per 10,000 population'
    )
  )

  labels <- metric_labels[[metric]]

  .data %>%
    select(all_of(labels$columns)) %>%
    pivot_longer(cols = everything(), names_to = 'indicator', values_to = 'value') %>%
    mutate(indicator = factor(indicator, levels = rev(labels$columns), labels = rev(labels$labels))) %>%
    ggplot(aes(x = indicator, y =value)) +
    geom_col(fill = '#2c7fb8', width = 0.6) +
    coord_flip() +
    geom_text(aes(label = sprintf("%.1f", value)), hjust = -0.1, size = 4) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), expand = expansion(mult = c(0, 0.1))) +
    labs(title = labels$title, y = NULL, x = NULL, caption = labels$caption) +
    cd_plot_theme()
}
