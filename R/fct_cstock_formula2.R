#' Carbon stock formulas and IPCC Approach 1 uncertainty per land use
#'
#' @description Builds, for each period and land use of the carbon table, the
#'              carbon stock formula and its variance and relative uncertainty
#'              formulas (IPCC 2006, Vol. 1, Ch. 3, Eq. 3.1 and 3.2).
#'
#'              Each stock is written as \code{C = CF * S_DM + S_C}, with
#'              \code{S_DM} the sum of pools in dry matter and \code{S_C} the sum
#'              of pools in carbon. CF is factored once, so its uncertainty is
#'              not counted as independent across pools:
#'
#'              \code{Var(C) = CF^2 * V_DM + (CF * CF_U * S_DM)^2 + V_C}
#'
#'              with \code{V_S = sum((X * X_U)^2)} over the pools of \code{S}.
#'              With RS, AGB becomes \code{AGB * (1 + RS)} with variance
#'              \code{(AGB * (1 + RS) * AGB_U)^2 + (AGB * RS * RS_U)^2}, and BGB is
#'              ignored. If \code{ALL} is given, it replaces the other pools.
#'
#'              \code{c_tot_U = sqrt(Var(C)) / C}, simplified with the product
#'              rule when the stock is a product (one pool, or DM pools only),
#'              e.g. \code{sqrt(AGB_U^2 + (RS / (1 + RS) * RS_U)^2 + CF_U^2)}.
#'
#'              Evaluate with each element and its relative uncertainty as
#'              \code{<element>} and \code{<element>_U} (se / mean). Land uses
#'              without pools (e.g. DG_ratio only) return NA.
#'
#' @param .carbon Carbon table, from \code{fct_checkinput()$data$carbon}.
#'
#' @return A tibble with \code{c_period}, \code{c_lu}, \code{c_tot} (stock),
#'         \code{c_tot_V} (variance) and \code{c_tot_U} (relative uncertainty).
#'
#' @examples
#' path    <- system.file("extdata/mocaredd-templatev2-simple.xlsx", package = "mocaredd.dev2")
#' checked <- fct_checkinput(.path = path)
#' fct_cstock_formula2(checked$data$carbon)
#'
#' @export
fct_cstock_formula2 <- function(.carbon){

  pools <- c("ALL", "AGB", "BGB", "DW", "LI", "SOC")

  tt <- .carbon |>
    dplyr::summarise(
      form = list(cstock_formula_lu(.data$c_element, .data$c_unit, pools)),
      .by = c("c_period", "c_lu")
    ) |>
    tidyr::unnest_wider("form")

}

## Formulas of one land use
cstock_formula_lu <- function(.c_el, .c_unit, .pools){

  el   <- .c_el[.c_el %in% .pools]
  unit <- .c_unit[.c_el %in% .pools]

  if ("ALL" %in% el) {
    unit <- unit[el == "ALL"]
    el   <- "ALL"
  }

  has_RS <- "RS" %in% .c_el
  if (has_RS) {
    unit <- unit[el != "BGB"]
    el   <- el[el != "BGB"]
  }

  if (length(el) == 0) {
    return(list(c_tot = NA_character_, c_tot_V = NA_character_, c_tot_U = NA_character_))
  }

  val <- ifelse(has_RS & el == "AGB", "AGB * (1 + RS)", el)
  var <- ifelse(
    has_RS & el == "AGB",
    "(AGB * (1 + RS) * AGB_U)^2 + (AGB * RS * RS_U)^2",
    paste0("(", el, " * ", el, "_U)^2")
  )

  is_dm <- unit == "DM"
  S_DM  <- paste(val[is_dm],  collapse = " + ")
  S_C   <- paste(val[!is_dm], collapse = " + ")
  V_DM  <- paste(var[is_dm],  collapse = " + ")
  V_C   <- paste(var[!is_dm], collapse = " + ")

  c_tot   <- character(0)
  c_tot_V <- character(0)

  if (any(is_dm)) {
    c_tot   <- paste0("CF * ", cstock_wrap(S_DM))
    c_tot_V <- paste0("CF^2 * ", cstock_wrap(V_DM), " + (CF * CF_U * ", cstock_wrap(S_DM), ")^2")
  }
  if (any(!is_dm)) {
    c_tot   <- c(c_tot,   S_C)
    c_tot_V <- c(c_tot_V, V_C)
  }

  c_tot   <- paste(c_tot,   collapse = " + ")
  c_tot_V <- paste(c_tot_V, collapse = " + ")

  ## Relative uncertainty, simplified when the stock is a product
  if (length(el) == 1) {
    ## Product rule: sum of squared relative uncertainties
    u2 <- ifelse(has_RS & el == "AGB", "AGB_U^2 + (RS / (1 + RS) * RS_U)^2", paste0(el, "_U^2"))
    if (is_dm) u2 <- paste0(u2, " + CF_U^2")
    c_tot_U <- if (u2 == paste0(el, "_U^2")) paste0(el, "_U") else paste0("sqrt(", u2, ")")
  } else if (all(is_dm)) {
    ## CF * S_DM: relative uncertainty of the sum, then product rule
    c_tot_U <- paste0("sqrt(", cstock_wrap(V_DM), " / ", cstock_wrap(S_DM), "^2 + CF_U^2)")
  } else {
    c_tot_U <- paste0("sqrt(", c_tot_V, ") / ", cstock_wrap(c_tot))
  }

  list(
    c_tot   = c_tot,
    c_tot_V = c_tot_V,
    c_tot_U = c_tot_U
  )
}

## Parentheses around sums only
cstock_wrap <- function(x) ifelse(stringr::str_detect(x, stringr::fixed(" + ")), paste0("(", x, ")"), x)

