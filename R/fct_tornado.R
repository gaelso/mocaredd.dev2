#' Tornado chart of activity data sources
#'
#' @description Plot the AD tornado of \code{fct_sensitivity2()} for one output:
#'              each bar is the output value when one AD source goes from
#'              \eqn{mean - z \cdot se} to \eqn{mean + z \cdot se}, all other inputs
#'              at their mean. Bars are sorted by swing; labels give the share of
#'              the output variance (first-order).
#'
#' @param .ad_tornado The \code{ad_tornado} table of \code{fct_sensitivity2()}.
#' @param .output Output to plot, e.g. "ER-MON1" (see \code{fct_sensitivity2()}).
#' @param .top_n Number of AD sources shown.
#'
#' @return A ggplot object.
#'
#' @importFrom rlang .data
#'
#' @examples
#' path <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2")
#' checked <- fct_checkinput(.path = path)
#'
#' sa <- fct_sensitivity2(.checked_data = checked)
#' fct_tornado(sa$ad_tornado, .output = "ER-MON1", .top_n = 10)
#'
#' @export
fct_tornado <- function(.ad_tornado, .output, .top_n = 20){

  dat_all <- .ad_tornado |> dplyr::filter(.data$output == .output)
  n_all   <- nrow(dat_all)

  dat <- dat_all |>
    dplyr::arrange(dplyr::desc(.data$swing)) |>
    dplyr::slice_head(n = .top_n) |>
    dplyr::mutate(
      pos   = rev(dplyr::row_number()),
      lo    = pmin(.data$value_low, .data$value_high),
      hi    = pmax(.data$value_low, .data$value_high),
      label = paste0(.data$trans_id, " (", .data$period_type, ")")
    )

  ## Fixed colour per REDD+ activity, DF and DG first
  pal  <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
  acts <- unique(c(intersect(c("DF", "DG"), .ad_tornado$redd_activity), sort(unique(.ad_tornado$redd_activity))))
  acts <- acts[seq_len(min(length(acts), length(pal)))]
  act_cols <- stats::setNames(pal[seq_along(acts)], acts)
  act_labs <- dplyr::recode(acts, DF = "Deforestation", DG = "Degradation")

  share_shown <- sum(dat$contribution_pct, na.rm = TRUE)

  ggplot2::ggplot(dat) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$lo, xmax = .data$hi, ymin = .data$pos - 0.35, ymax = .data$pos + 0.35,
                   fill = .data$redd_activity)
    ) +
    ggplot2::geom_vline(xintercept = dat$value[1], colour = "#52514e", linewidth = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(x = .data$hi, y = .data$pos, label = paste0(round(.data$contribution_pct, 1), "%")),
      hjust = -0.15, size = 3.5, colour = "#52514e"
    ) +
    ggplot2::scale_fill_manual(values = act_cols, labels = act_labs, name = NULL) +
    ggplot2::scale_y_continuous(breaks = dat$pos, labels = dat$label, expand = ggplot2::expansion(add = 0.6)) +
    ggplot2::scale_x_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::labs(
      x       = paste0(.output, " (tCO2e/yr)"),
      y       = NULL,
      caption = paste0(
        "Top ", nrow(dat), " of ", n_all, " AD sources, ", round(share_shown, 1), "% of the variance. ",
        "Vertical line: value at mean inputs."
      )
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      legend.position    = "bottom",
      plot.caption       = ggplot2::element_text(colour = "#52514e", hjust = 0)
    )
}
