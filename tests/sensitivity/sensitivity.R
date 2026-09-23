## mocaredd -- Sensitivity analysis of REDD+ Emission Reductions (ERRs)
## ============================================================================
## One script, two complementary methods on the same simulation run:
##   A. Global, variance-based: grouped Shapley effects of Var(ERR) over five
##      groups (AD_DF_REF, AD_DF_MON, AD_DG_REF, AD_DG_MON, EF), with an
##      independence test, a donut, and an experimental knn given-data check.
##   B. Local, one-at-a-time: tornado of every individual AD source.
## Method, assumptions and references: CLAUDE.md, section "Sensitivity analysis"
##
## Run from the PACKAGE ROOT with mocaredd loaded:
##   devtools::load_all()
##   source("tests/sensitivity/sensitivity.R")

suppressWarnings(suppressMessages({
    library(tidyverse)
    library(sensitivity) # shapleyPermEx, sobolshap_knn; needs `gtools`
    library(tictoc)
}))

## ------------------------------------------------------------------ CONFIG ----
cfg <- list(
    xlsx_path = "data-raw/UGA_mocareddv2_20260718.xlsx",
    out_dir   = "tests/sensitivity",
    n_iter    = 10000L,  # MC iterations forced onto the template `setup`
    seed      = 3980L,   # same seed as the template `setup`
    No_act    = 300L,    # shapleyPermEx outer loop (5 groups)
    Ni        = 3L,      # shapleyPermEx inner loop (Song et al. 2016 default)
    top_n     = 25L      # AD sources drawn in the tornado (table keeps all)
)
set.seed(cfg$seed)
if (!dir.exists(cfg$out_dir)) dir.create(cfg$out_dir, recursive = TRUE)

surface <- "#fcfcfb"; ink <- "#0b0b0b"; ink2 <- "#52514e"

################################################################################
## 1. READ TEMPLATE AND RUN THE SIMULATOR ONCE
################################################################################

data_check <- cfg$xlsx_path |> fct_checkinput()
data_check$data$setup$n_iter <- cfg$n_iter

setup <- data_check$data$setup
time  <- data_check$data$time
area  <- data_check$data$area

tic("fct_combine_mcs_E")
big <- data_check |> fct_combine_mcs_E()
toc()

################################################################################
## 2. PERIOD WEIGHTS PER TRANSITION
################################################################################
## ERR = sum_t wt_t * AD_t * EF_t, with +1/n_ref per REF year, -1/n_mon per MON
## year, divided by period length. Untyped gap periods get weight 0.

ptype <- time |> transmute(across(c(period_no, period_type), as.character)) |> deframe()
n_ref <- sum(ptype == "REF", na.rm = TRUE)
n_mon <- sum(str_starts(replace_na(ptype, ""), "MON"))

trans <- big |>
    distinct(trans_id, redd_activity, time_period, period_type, period_length) |>
    arrange(trans_id) |>
    mutate(
        period_class = if_else(str_starts(replace_na(period_type, ""), "MON"), "MON", "REF"),
        w    = case_when(period_type == "REF"           ~  1 / n_ref,
                         str_starts(period_type, "MON") ~ -1 / n_mon,
                         TRUE                           ~  0),
        wt   = w / period_length,
        cell = str_c("AD_", redd_activity, "_", period_class)
    )
trans_ids <- trans$trans_id
NT <- nrow(trans)

################################################################################
## A1. DRIVER BANK, GROUPS AND ERR OUTPUT
################################################################################
## `big` holds realised AD and EF per transition x simulation: pivot to wide.

ad_wide <- big |>
    select(sim_no, trans_id, AD) |>
    pivot_wider(names_from = trans_id, values_from = AD) |>
    arrange(sim_no) |>
    select(all_of(trans_ids)) |>
    as.matrix()
ef_wide <- big |>
    select(sim_no, trans_id, EF) |>
    pivot_wider(names_from = trans_id, values_from = EF) |>
    arrange(sim_no) |>
    select(all_of(trans_ids)) |>
    as.matrix()

bank <- cbind(ad_wide, ef_wide)                            # n_sim x (2 * NT)
colnames(bank) <- c(str_c("AD:", trans_ids), str_c("EF:", trans_ids))
n_sim  <- nrow(bank)
p      <- ncol(bank)
ad_idx <- seq_len(NT)
ef_idx <- NT + seq_len(NT)

## five groups: four disjoint AD cells + EF (kept whole: shared carbon draws)
ad_groups <- trans |>
    mutate(col = row_number()) |>
    summarise(cols = list(col), .by = cell) |>
    deframe()
groups_5 <- c(ad_groups, list(EF = ef_idx))
gk       <- names(groups_5)
gcols    <- unname(groups_5)
d        <- length(gk)

Y <- as.numeric((bank[, ad_idx, drop = FALSE] * bank[, ef_idx, drop = FALSE]) %*% trans$wt)

################################################################################
## A2. INDEPENDENCE TEST -- correlation of the five group drivers
################################################################################

grp_aggr <- groups_5 |> map(\(cols) rowMeans(bank[, cols, drop = FALSE])) |> as_tibble()
cormat   <- cor(grp_aggr)
max_off  <- cormat
diag(max_off) <- NA
max_off  <- max(abs(max_off), na.rm = TRUE)

cat("\n=== INDEPENDENCE TEST -- correlation of the 5 group drivers ===\n")
print(round(cormat, 2))
cat(str_glue("  max |off-diagonal| = {round(max_off, 3)}  ",
             "({if (max_off < 0.15) 'independent -> grouped Shapley valid' else 'dependence present'})"), "\n")

################################################################################
## A3. MODEL-BASED GROUPED SHAPLEY -- sensitivity::shapleyPermEx
################################################################################
## Each group is one estimator "input" via a bank-row index, so a group is
## frozen/resampled as a whole block. model/Xall/Xset are required by the API.
## Cost: Nv + d! * (d - 1) * No * Ni model evaluations.

cat("\n=== FIVE-GROUP DECOMPOSITION (model-based Shapley) ===\n")
set.seed(cfg$seed)
ref <- bank[1, ]
tic("shapleyPermEx")
x <- shapleyPermEx(
    model = function(idx) {
        idx <- matrix(as.integer(idx), ncol = d)
        n   <- nrow(idx)
        X   <- matrix(ref, n, p, byrow = TRUE)
        for (j in seq_len(d)) X[, gcols[[j]]] <- bank[idx[, j], gcols[[j]], drop = FALSE]
        as.numeric((X[, ad_idx, drop = FALSE] * X[, ef_idx, drop = FALSE]) %*% trans$wt)
    },
    Xall = function(n) matrix(sample.int(n_sim, n * d, replace = TRUE), n, d),
    Xset = function(n, Sj, Sjc, xjc) matrix(sample.int(n_sim, n * length(Sj), replace = TRUE), n, length(Sj)),
    d = d, Nv = n_sim, No = cfg$No_act, Ni = cfg$Ni, colnames = gk
)
toc()

res_bank <- tibble(
    group       = gk,
    Sobol_first = x$SobolS[, 1],
    Sobol_total = x$SobolT[, 1],
    Shapley     = x$Shapley[, 1],
    Shapley_lo  = x$Shapley[, 3],
    Shapley_hi  = x$Shapley[, 4]
) |> arrange(desc(Shapley))

print(res_bank)
cat(str_glue("  sum = {round(sum(res_bank$Shapley), 3)}   ",
             "AD = {round(sum(res_bank$Shapley[str_starts(res_bank$group, 'AD_')]), 3)}   ",
             "EF = {round(res_bank$Shapley[res_bank$group == 'EF'], 3)}"), "\n")

write_csv(res_bank, file.path(cfg$out_dir, "shapley_5groups.csv"))

################################################################################
## A4. DONUT OF THE FIVE SHAPLEY SHARES
################################################################################

lab <- c(
    AD_DF_MON = "Deforestation AD — MON", AD_DF_REF = "Deforestation AD — REF",
    EF = "Emission factors (all carbon)", AD_DG_MON = "Degradation AD — MON",
    AD_DG_REF = "Degradation AD — REF"
)
ord      <- c("AD_DF_MON", "AD_DF_REF", "EF", "AD_DG_MON", "AD_DG_REF")
cols_vec <- set_names(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4"), ord)

dd <- res_bank |>
    mutate(group = factor(group, levels = ord), share = pmax(Shapley, 0)) |>
    mutate(share = share / sum(share)) |>
    arrange(group) |>
    mutate(leg = str_glue("{lab[as.character(group)]}  ·  {sprintf('%.1f%%', 100 * share)}"))
dd$leg <- factor(dd$leg, levels = dd$leg)
dd <- dd |>
    mutate(
        ymax  = cumsum(share), ymin = lag(ymax, default = 0), mid = (ymax + ymin) / 2,
        inlab = if_else(share >= 0.06, sprintf("%.0f%%", 100 * share), "")
    )
legcols <- set_names(cols_vec[as.character(dd$group)], dd$leg)
ad_tot  <- sum(dd$share[str_starts(as.character(dd$group), "AD_")]) * 100

p_donut <- ggplot(dd) +
    geom_rect(aes(ymin = ymin, ymax = ymax, xmin = 3, xmax = 4, fill = leg),
              color = surface, linewidth = 1.1) +
    geom_text(aes(x = 3.5, y = mid, label = inlab),
              color = "white", fontface = "bold", size = 4.2) +
    annotate("text", x = 0, y = 0, label = sprintf("%.0f%%", ad_tot),
             size = 13, fontface = "bold", color = ink) +
    annotate("text", x = 0, y = 0, label = "of Var(ERR) from\nActivity Data (AD)",
             vjust = 2.4, size = 3.3, color = ink2, lineheight = 0.95) +
    scale_fill_manual(values = legcols, name = NULL) +
    coord_polar(theta = "y") + xlim(c(0, 4)) +
    guides(fill = guide_legend(ncol = 1, keyheight = unit(11, "pt"))) +
    labs(title = "Contribution to REDD+ Emission-Reduction uncertainty",
         subtitle = "Shapley decomposition of Var(ERR) into 5 groups") +
    theme_void(base_family = "sans") +
    theme(
        plot.background = element_rect(fill = surface, color = NA),
        legend.position = "right", legend.text = element_text(size = 9.5, color = ink),
        plot.title    = element_text(face = "bold", size = 14, color = ink, hjust = 0),
        plot.subtitle = element_text(size = 9, color = ink2, hjust = 0, margin = margin(b = 4)),
        plot.margin   = margin(12, 16, 12, 16)
    )
print(p_donut)

ggsave(file.path(cfg$out_dir, "shapley_donut.png"),
       p_donut, width = 9, height = 5.4, dpi = 150, bg = surface)

################################################################################
## A5. KNN GIVEN-DATA CROSS-CHECK (experimental) -- sensitivity::sobolshap_knn
################################################################################
## Closed effect of each group subset by nearest neighbours, no model re-runs.
## Biased low for high-dimensional groups (EF): shares usually do NOT sum to 1.

cat("\n=== KNN GIVEN-DATA Sobol' & Shapley (experimental) ===\n")
tic("sobolshap_knn")

subsets <- seq_len(d) |> map(\(r) combn(gk, r, simplify = FALSE)) |> reduce(c)
Umat <- subsets |>
    map(\(S) { row <- integer(p); row[unlist(groups_5[S])] <- 1L; row }) |>
    reduce(rbind)

x_knn <- sobolshap_knn(model = NULL, X = as.data.frame(bank), U = Umat, n.knn = 3)
x_knn <- tell(x_knn, Y)

Scl <- set_names(as.numeric(x_knn$S), map_chr(subsets, \(S) str_c(sort(S), collapse = ",")))

perms <- gtools::permutations(d, d, gk)
sh    <- set_names(numeric(d), gk)
for (i in seq_len(nrow(perms))) {
    Sp <- character(0); prev <- 0
    for (g in perms[i, ]) {
        Sp  <- c(Sp, g)
        cur <- Scl[[str_c(sort(Sp), collapse = ",")]]
        sh[g] <- sh[g] + (cur - prev)
        prev  <- cur
    }
}

res_knn <- tibble(
    group       = gk,
    Sobol_first = map_dbl(gk, \(g) Scl[[g]]),
    Sobol_total = map_dbl(gk, \(g) 1 - Scl[[str_c(sort(setdiff(gk, g)), collapse = ",")]]),
    Shapley     = as.numeric(sh[gk] / factorial(d))
) |> arrange(desc(Shapley))
toc()

print(res_knn)
cat(str_glue("  sum(Shapley) = {round(sum(res_knn$Shapley), 3)}   ",
             "(model-based sum = {round(sum(res_bank$Shapley), 3)})"), "\n")

################################################################################
## B1. TORNADO -- one row per AD source (baseline area, SE, EF, weight)
################################################################################
## Each AD source is swung across mean +/- z*SE, all else at baseline. ERR is
## linear in each AD, so swing_t = wt_t * EF_bar_t * 2 * z * SE_t.

z <- qnorm(1 - (1 - setup$conf_level) / 2)

ef_bar <- big |> summarise(EF_bar = mean(EF), AD_bar = mean(AD), .by = trans_id)

ad <- area |>
    transmute(trans_id,
              redd_activity,
              trans_period,
              lu_initial = if ("lu_initial_id" %in% names(area)) lu_initial_id else lu_initial,
              lu_final   = if ("lu_final_id"   %in% names(area)) lu_final_id   else lu_final,
              trans_area, trans_se) |>
    left_join(select(trans, trans_id, period_type, period_class, wt), by = "trans_id") |>
    left_join(ef_bar, by = "trans_id") |>
    mutate(
        source = str_c(redd_activity, " · ", lu_initial, "->", lu_final, " [", trans_period, "]"),
        rel_se = trans_se / trans_area,
        contribution = wt * trans_area * EF_bar
    )
ERR0 <- sum(ad$contribution, na.rm = TRUE)

ad <- ad |>
    mutate(
        delta         = wt * EF_bar * z * trans_se,
        ERR_area_low  = ERR0 - delta,
        ERR_area_high = ERR0 + delta,
        swing         = ERR_area_high - ERR_area_low,
        abs_swing     = abs(swing)
    ) |>
    arrange(desc(abs_swing)) |>
    mutate(
        rank           = row_number(),
        pct_of_total   = 100 * abs_swing / sum(abs_swing, na.rm = TRUE),
        swing_pct_ERR0 = 100 * abs_swing / abs(ERR0)
    )

cat(str_glue("\nBaseline ERR0 = {scales::comma(round(ERR0))} tCO2e/yr   ",
             "({round(ERR0 / 1e6, 3)} MtCO2e/yr)"), "\n")
cat(str_glue("AD sources: {nrow(ad)}   active (non-zero swing): {sum(ad$abs_swing > 0)}   ",
             "top {cfg$top_n} shown in the tornado"), "\n\n")

tornado_table <- ad |>
    transmute(rank, source, trans_id, redd_activity, period_type,
              trans_area, trans_se, rel_se, EF_bar, wt, contribution,
              ERR_area_low, ERR_area_high, swing, abs_swing,
              pct_of_total, swing_pct_ERR0)

print(tornado_table |> slice_head(n = 15), width = Inf)
write_csv(tornado_table, file.path(cfg$out_dir, "tornado_ad_table.csv"))

################################################################################
## B2. TORNADO DIAGRAM (top_n sources)
################################################################################

plot_df <- ad |>
    slice_head(n = cfg$top_n) |>
    mutate(
        source = fct_reorder(source, abs_swing),
        lo_M   = ERR_area_low  / 1e6,
        hi_M   = ERR_area_high / 1e6,
        lab    = str_c(sprintf("%.2f", abs_swing / 1e6), " Mt")
    )

act_cols <- c(DF = "#B0564C", DG = "#4991B0")

p_tornado <- ggplot(plot_df, aes(y = source)) +
    geom_vline(xintercept = ERR0 / 1e6, linetype = 2, color = ink2, linewidth = 0.5) +
    geom_linerange(aes(xmin = pmin(lo_M, hi_M), xmax = pmax(lo_M, hi_M),
                       color = redd_activity), linewidth = 6) +
    geom_text(aes(x = pmax(lo_M, hi_M), label = lab),
              hjust = -0.15, size = 2.9, color = ink2) +
    scale_color_manual(values = act_cols, name = "REDD+ activity",
                       labels = c(DF = "Deforestation", DG = "Degradation")) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.12))) +
    labs(
        title    = "Tornado — sensitivity of ERR to Activity-Data sources",
        subtitle = str_glue("Each area swung across mean +/- {round(z, 3)}*SE ",
                            "(conf {100 * setup$conf_level}%); dashed line = baseline ERR ",
                            "({round(ERR0 / 1e6, 2)} MtCO2e/yr)"),
        x = "Emission Reduction (MtCO2e/yr)", y = NULL
    ) +
    theme_minimal(base_size = 11, base_family = "sans") +
    theme(
        plot.background = element_rect(fill = surface, color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        legend.position = "top", legend.justification = "left",
        plot.title    = element_text(face = "bold", size = 14, color = ink),
        plot.subtitle = element_text(size = 9, color = ink2, margin = margin(b = 6)),
        axis.text.y   = element_text(size = 7.5, color = ink)
    )
print(p_tornado)

ggsave(file.path(cfg$out_dir, "tornado_ad.png"),
       p_tornado, width = 10, height = 0.34 * cfg$top_n + 2, dpi = 150, bg = surface)

cat("\nDone. Outputs written to", cfg$out_dir, "\n")
