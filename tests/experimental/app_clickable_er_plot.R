# -------------------------------------------------------------------------- #
# Minimal reproducible example: clickable emission / ER plot linked to the
# density plot of the corresponding simulations.
#
# Click a dot (REF, MON1, MON2) or an ER bar -> the density of the matching
# simulations is drawn in the second card.
#
# install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "ggiraph"))
# -------------------------------------------------------------------------- #

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(ggiraph)

## 1. Fake simulations --------------------------------------------------------
## One row per Monte Carlo draw, one "key" per clickable element.

set.seed(42)
n_sim <- 5000

sims <- bind_rows(
  tibble::tibble(key = "REF",  label = "Reference level", value = rnorm(n_sim, 4.93, 0.63)),
  tibble::tibble(key = "MON1", label = "Monitoring 2019", value = rnorm(n_sim, 2.40, 0.50)),
  tibble::tibble(key = "MON2", label = "Monitoring 2021", value = rnorm(n_sim, 3.58, 0.78))
)

## ER draws: REF - MON, drawn pairwise
sims <- bind_rows(
  sims,
  tibble::tibble(
    key   = "ER1",
    label = "Emission reductions 2019",
    value = sims$value[sims$key == "REF"] - sims$value[sims$key == "MON1"]
  ),
  tibble::tibble(
    key   = "ER2",
    label = "Emission reductions 2021",
    value = sims$value[sims$key == "REF"] - sims$value[sims$key == "MON2"]
  )
)

## 2. Summary table, one row per clickable element ----------------------------

summ <- sims |>
  summarise(
    mean  = mean(value),
    lower = quantile(value, 0.05),
    upper = quantile(value, 0.95),
    .by   = c(key, label)
  )

get <- function(k, col) summ[[col]][summ$key == k]

## Points: REF repeated over the reference period, MON at monitoring years
pts <- bind_rows(
  tibble::tibble(year = 2005:2014, key = "REF"),
  tibble::tibble(year = 2019,      key = "MON1"),
  tibble::tibble(year = 2021,      key = "MON2")
) |>
  left_join(summ, by = "key")

## ER bars: dark = point estimate, light = confidence interval of the ER
bars <- tibble::tibble(
  year = c(2019, 2021),
  key  = c("ER1", "ER2")
) |>
  left_join(summ, by = "key") |>
  mutate(
    xmin      = year - 0.4,
    xmax      = year + 0.4,
    ref       = get("REF", "mean"),
    ymin_dark = ref - mean,       # top of the MON estimate
    ymax_dark = ref,              # reference level
    ymin_ci   = ref - upper,      # CI of the emission reductions
    ymax_ci   = ref - lower
  )

## 3. UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "Clickable emission reductions plot",
  sidebar = sidebar(
    width = 300,
    markdown(
      "Click a **dot** or an **ER bar** in the left card.
       The density of the corresponding simulations appears on the right.
       Click the background to clear the selection."
    ),
    verbatimTextOutput("selected_key")
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header("Emissions and emission reductions"),
      girafeOutput("plot_er", height = "500px")
    ),
    card(
      card_header(textOutput("dens_title", inline = TRUE)),
      plotOutput("plot_dens", height = "500px")
    )
  )
)

## 4. Server ------------------------------------------------------------------

server <- function(input, output, session) {

  ## --- the interactive plot: *_interactive geoms + data_id = the key -------
  output$plot_er <- renderGirafe({

    gg <- ggplot() +
      ## light bar: CI of the emission reductions
      geom_rect_interactive(
        data = bars,
        aes(xmin = xmin, xmax = xmax, ymin = ymin_ci, ymax = ymax_ci,
            data_id = key, tooltip = paste0(label, "\n90% CI")),
        fill = "#66c2a5", alpha = 0.25
      ) +
      ## dark bar: ER point estimate
      geom_rect_interactive(
        data = bars,
        aes(xmin = xmin, xmax = xmax, ymin = ymin_dark, ymax = ymax_dark,
            data_id = key,
            tooltip = paste0(label, "\n", round(mean, 2), " MtCO2e/y")),
        fill = "#66c2a5", alpha = 0.9
      ) +
      ## error bars and dots
      geom_errorbar_interactive(
        data = pts,
        aes(x = year, ymin = lower, ymax = upper, colour = key, data_id = key),
        width = 0.25, linewidth = 0.8
      ) +
      geom_point_interactive(
        data = pts,
        aes(x = year, y = mean, colour = key, data_id = key,
            tooltip = paste0(label, "\n", round(mean, 2), " MtCO2e/y")),
        size = 4
      ) +
      scale_colour_manual(
        values = c(REF = "#1f78b4", MON1 = "#e8a33d", MON2 = "#d6480f"),
        name = NULL
      ) +
      scale_x_continuous(breaks = 2005:2021) +
      labs(x = "Years", y = "Emissions (MtCO2e/y)") +
      theme_bw() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )

    girafe(
      ggobj = gg,
      options = list(
        opts_selection(type = "single", css = "stroke:black;stroke-width:2px;"),
        opts_hover(css = "cursor:pointer;opacity:0.7;"),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })

  ## --- the selection: input$<outputId>_selected ----------------------------
  selected <- reactive({
    sel <- input$plot_er_selected
    if (is.null(sel) || !nzchar(sel)) NULL else sel[1]
  })

  output$selected_key <- renderText({
    if (is.null(selected())) "No selection" else paste("Selected:", selected())
  })

  output$dens_title <- renderText({
    if (is.null(selected())) {
      "Simulations — click an element"
    } else {
      summ$label[summ$key == selected()]
    }
  })

  output$plot_dens <- renderPlot({
    req(selected())

    dat <- sims |> filter(key == selected())
    sm  <- summ  |> filter(key == selected())

    ggplot(dat, aes(x = value)) +
      geom_density(fill = "#66c2a5", colour = NA, alpha = 0.6) +
      geom_vline(xintercept = sm$mean, linewidth = 0.8) +
      geom_vline(xintercept = c(sm$lower, sm$upper),
                 linetype = "dotted", linewidth = 0.8) +
      labs(
        x = "MtCO2e/y", y = "Density",
        subtitle = sprintf(
          "mean = %.2f | 90%% CI = [%.2f ; %.2f] | U = %.1f%%",
          sm$mean, sm$lower, sm$upper,
          100 * (sm$upper - sm$lower) / 2 / sm$mean
        )
      ) +
      theme_bw()
  })
}

shinyApp(ui, server)
