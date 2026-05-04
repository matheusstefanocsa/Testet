# motor-functions.R
# Price/cost decomposition motor for Designer Roller Shades (DRS)
# Four additive functions: shades_motor, control_system_motor,
#                          top_treatment_motor, total_motor

# ── Bilinear interpolation (no external package needed) ───────────────────────
# x_grid / y_grid: sorted numeric vectors (heights, widths)
# z_mat: matrix where row = x index, col = y index
# Inputs are clamped to grid bounds before interpolation.

bilinear_interp <- function(x_grid, y_grid, z_mat, x0, y0) {
  x0 <- pmin(pmax(x0, min(x_grid)), max(x_grid))
  y0 <- pmin(pmax(y0, min(y_grid)), max(y_grid))

  i1 <- findInterval(x0, x_grid, all.inside = TRUE)
  i2 <- i1 + 1L
  j1 <- findInterval(y0, y_grid, all.inside = TRUE)
  j2 <- j1 + 1L

  t <- (x0 - x_grid[i1]) / (x_grid[i2] - x_grid[i1])
  u <- (y0 - y_grid[j1]) / (y_grid[j2] - y_grid[j1])

  z_mat[i1, j1] * (1-t) * (1-u) +
  z_mat[i2, j1] *    t  * (1-u) +
  z_mat[i1, j2] * (1-t) *    u  +
  z_mat[i2, j2] *    t  *    u
}

# ── Constants ─────────────────────────────────────────────────────────────────
# HD production cost as fraction of MSRP — derived from CC + No treatment subset
# Source: 05-shade-price-assumptions/shade-cost-ratios.R

shade_cost_to_msrp <- c(
  DRSA = 0.217, DRSB = 0.187, DRSC = 0.197,
  DRSD = 0.184, DRSE = 0.162, DRSF = 0.144, DRSG = 0.144
)

.motorized_addon_cost_to_msrp <- 0.198

# Height breakpoints shared across all DRS grids
heights <- c(36, 48, 60, 72, 84, 96, 108, 120, 132)

# ── MSRP price matrices (row = height index, col = width index) ───────────────

# DRS-1 / DRSA (Addy, Hadleigh, Sawyer) — max width 107"
widths_drs1 <- c(24, 30, 36, 48, 60, 72,   84,   96,  107)

prices_drs1 <- matrix(c(
  227, 237, 256, 303, 348, 440,  504,  555,  613,
  230, 245, 267, 327, 381, 476,  538,  604,  749,
  234, 261, 282, 349, 413, 515,  613,  734,  824,
  243, 274, 301, 375, 441, 554,  729,  794,  885,
  251, 288, 318, 398, 472, 589,  816,  872,  964,
  262, 302, 333, 420, 505, 627,  878,  950, 1042,
  272, 318, 349, 447, 533, 665,  952, 1035, 1122,
  284, 331, 371, 470, 565, 703, 1024, 1101, 1193,
  292, 339, 381, 483, 583, 721, 1065, 1139, 1229
), nrow = 9, byrow = TRUE)

# DRS-2 / DRSB — max width 119"
widths_drs2 <- c(24, 30, 36, 48, 60, 72,   84,   96,  108,  119)

prices_drs2 <- matrix(c(
   237,  247,  278,  330,  376,  475,  605,  765,  932, 1102,
   258,  266,  293,  353,  413,  514,  651,  798,  964, 1132,
   271,  280,  303,  376,  447,  555,  696,  850, 1019, 1182,
   291,  295,  327,  406,  478,  596,  743,  899, 1067, 1229,
   305,  310,  342,  432,  510,  635,  837,  950, 1117, 1279,
   317,  328,  362,  457,  547,  677,  895,  996, 1163, 1323,
   335,  342,  376,  486,  578,  714,  983, 1083, 1250, 1410,
   360,  376,  403,  508,  611,  755, 1075, 1204, 1370, 1523,
   373,  393,  413,  522,  632,  777, 1121, 1249, 1416, 1564
), nrow = 9, byrow = TRUE)

# DRS-3 / DRSC — max width 119" (shares widths_drs2)
prices_drs3 <- matrix(c(
   256,  277,  317,  388,  446,  564,  719,  877, 1009, 1151,
   274,  297,  341,  418,  487,  610,  774,  934, 1057, 1176,
   293,  315,  362,  447,  528,  661,  830,  998, 1119, 1238,
   308,  358,  384,  480,  567,  707,  954, 1056, 1179, 1303,
   340,  402,  410,  509,  605,  755, 1012, 1122, 1251, 1379,
   374,  418,  431,  538,  647,  804, 1080, 1181, 1308, 1440,
   414,  433,  448,  572,  684,  850, 1147, 1249, 1379, 1517,
   451,  462,  474,  604,  723,  903, 1216, 1310, 1498, 1846,
   472,  473,  488,  618,  743,  928, 1302, 1344, 1557, 1989
), nrow = 9, byrow = TRUE)

# DRS-4 / DRSD — max width 119" (shares widths_drs2)
prices_drs4 <- matrix(c(
    266,  305,  349,  432,  511,  643,  818,  960, 1116, 1236,
    282,  330,  385,  478,  567,  710,  910, 1025, 1169, 1300,
    305,  353,  414,  522,  622,  781, 1046, 1113, 1224, 1353,
    322,  406,  444,  564,  679,  888, 1091, 1214, 1338, 1432,
    349,  456,  475,  609,  739,  954, 1184, 1320, 1453, 1554,
    385,  473,  508,  655,  899, 1042, 1277, 1424, 1572, 1683,
    433,  508,  536,  696,  963, 1097, 1370, 1531, 1688, 1810,
    481,  556,  567,  742, 1091, 1180, 1459, 1630, 1803, 1955,
    511,  581,  583,  766, 1153, 1223, 1509, 1687, 1866, 2081
), nrow = 9, byrow = TRUE)

# DRS-5 / DRSE — max width 119" (shares widths_drs2)
prices_drs5 <- matrix(c(
    277,  322,  375,  473,  562,  706,  956, 1040, 1199, 1263,
    303,  352,  416,  525,  633,  793, 1076, 1199, 1293, 1331,
    328,  385,  456,  586,  706,  882, 1195, 1308, 1419, 1461,
    351,  448,  495,  637,  777,  971, 1311, 1442, 1571, 1633,
    374,  485,  530,  696,  849, 1055, 1455, 1576, 1724, 1792,
    409,  512,  572,  749,  923, 1271, 1552, 1713, 1877, 1951,
    458,  545,  609,  808,  992, 1328, 1670, 1850, 2028, 2109,
    518,  593,  651,  862, 1144, 1470, 1786, 1982, 2180, 2266,
    547,  619,  673,  891, 1202, 1531, 1850, 2054, 2262, 2353
), nrow = 9, byrow = TRUE)

# DRS-6 / DRSF — max width 117"
widths_drs6 <- c(24, 30, 36, 48, 60, 72,   84,   96,  108,  117)

prices_drs6 <- matrix(c(
    317,  366,  427,  539,  638,  805, 1086, 1182, 1273, 1360,
    343,  400,  474,  597,  722,  905, 1224, 1338, 1445, 1516,
    374,  436,  516,  665,  805, 1004, 1361, 1487, 1617, 1701,
    399,  477,  564,  726,  885, 1118, 1497, 1644, 1789, 1885,
    426,  506,  605,  793,  965, 1262, 1630, 1798, 1964, 2077,
    454,  543,  652,  856, 1049, 1350, 1766, 1950, 2140, 2264,
    502,  586,  696,  917, 1128, 1447, 1900, 2104, 2306, 2445,
    563,  644,  739,  980, 1211, 1557, 2034, 2260, 2475, 2630,
    595,  676,  763, 1015, 1255, 1608, 2105, 2344, 2569, 2728
), nrow = 9, byrow = TRUE)

# DRS-7 / DRSG — max width 117" (shares widths_drs6)
prices_drs7 <- matrix(c(
    361,  417,  485,  616,  732,  916, 1240, 1347, 1449, 1458,
    394,  456,  541,  682,  827, 1031, 1395, 1525, 1650, 1663,
    428,  502,  589,  763,  921, 1171, 1550, 1696, 1842, 1861,
    463,  548,  653,  832, 1015, 1283, 1707, 1877, 2041, 2060,
    495,  595,  710,  907, 1106, 1373, 1859, 2048, 2238, 2265,
    534,  636,  766,  979, 1250, 1487, 2013, 2220, 2435, 2469,
    565,  677,  813, 1050, 1310, 1603, 2170, 2399, 2629, 2664,
    596,  717,  865, 1123, 1388, 1720, 2318, 2577, 2825, 2865,
    614,  740,  892, 1175, 1425, 1783, 2400, 2676, 2930, 2971
), nrow = 9, byrow = TRUE)

# Grid dispatch tables
grid_widths <- list(
  DRSA = widths_drs1,
  DRSB = widths_drs2, DRSC = widths_drs2,
  DRSD = widths_drs2, DRSE = widths_drs2,
  DRSF = widths_drs6, DRSG = widths_drs6
)

grid_prices <- list(
  DRSA = prices_drs1, DRSB = prices_drs2, DRSC = prices_drs3,
  DRSD = prices_drs4, DRSE = prices_drs5, DRSF = prices_drs6, DRSG = prices_drs7
)

# ── MSRP lookup via bilinear interpolation ────────────────────────────────────

lookup_msrp <- function(height, width, grid) {
  if (!(grid %in% names(grid_prices))) {
    stop("Unknown grid: '", grid, "'")
  }

  bilinear_interp(
    heights, grid_widths[[grid]], grid_prices[[grid]], height, width
  )
}

.safe_ratio <- function(num, den) {
  ifelse(den > 0, num / den, NA_real_)
}

.component_result <- function(component, msrp, cost_factor, unit_cost) {
  unit_price <- msrp * cost_factor

  list(
    component    = component,
    msrp         = msrp,
    unit_price   = unit_price,
    unit_cost    = unit_cost,
    gross_margin = .safe_ratio(unit_price - unit_cost, unit_price),
    cost_to_msrp = .safe_ratio(unit_cost, msrp)
  )
}

.to_df <- function(x) {
  data.frame(
    component    = x$component,
    msrp         = x$msrp,
    unit_price   = x$unit_price,
    unit_cost    = x$unit_cost,
    gross_margin = x$gross_margin,
    cost_to_msrp = x$cost_to_msrp,
    stringsAsFactors = FALSE
  )
}

.lookup_by_width <- function(width, widths, values) {
  i <- which(width <= widths)[1]
  if (is.na(i)) i <- length(widths)
  values[[i]]
}

# ── 1. shades_motor ───────────────────────────────────────────────────────────
# Base shade cost: fabric + roller mechanism, no surcharges.
# MSRP from per-grid price table (bilinear interpolation).
# unit_cost = MSRP × shade_cost_to_msrp[grid] (constant per grid).

shades_motor <- function(height, width, grid, cost_factor) {
  msrp       <- lookup_msrp(height, width, grid)
  ctm        <- shade_cost_to_msrp[[grid]]
  unit_cost  <- msrp * ctm

  .component_result("shade_base", msrp, cost_factor, unit_cost)
}

# ── 2. control_system_motor ───────────────────────────────────────────────────
# Incremental contribution of the control system surcharge.
# Returns zeros for Custom Clutch (baseline).
# PowerView systems use a dimension-based staircase (zone = first where
# both HEIGHT ≤ and WIDTH ≤ conditions are satisfied).
# PowerView Gen 3 AC Coupled uses panel count instead of dimensions.

.pv_zone <- function(height, width, h_breaks, w_breaks) {
  for (i in seq_along(h_breaks)) {
    if (height <= h_breaks[i] && width <= w_breaks[i]) return(i)
  }
  length(h_breaks)
}

control_system_motor <- function(height, width, ControlSystemName,
                                 cost_factor, panels = NULL) {

  fixed <- list(
    "Custom Clutch"                  = list(msrp =   0, cost =   0.00),
    "UltraGlide"                     = list(msrp = 130, cost =  20.68),
    "LiteRise"                       = list(msrp = 130, cost =  23.93),
    "Cordless Lift"                  = list(msrp = 130, cost =   5.01),
    "SoftTouch Motorization"         = list(msrp = 200, cost =  39.60),
    "SoftTouch w/ Rechargeable Wand" = list(msrp = 270, cost =  53.46),
    "SoftTouch Rechargeable Wand"    = list(msrp = 270, cost =  53.46),
    "Somfy Motorized (RF & H motor)" = list(
      msrp = 1165,
      cost = 1165 * .motorized_addon_cost_to_msrp
    )
  )

  pv <- list(
    "PowerView" = list(
      h = c(60,  84, 180), w = c( 48,  84, 156),
      msrp = c( 440,  515,  595), cost = c( 87.12, 101.97, 117.81)
    ),
    "PowerView Gen 3" = list(
      h = c(60,  84, 180), w = c( 48,  84, 156),
      msrp = c( 440,  515,  595), cost = c( 87.12, 101.97, 117.81)
    ),
    "PowerView Gen 3 - Rechargeable Battery Wand" = list(
      h = c(60,  84, 180), w = c( 48,  84, 156),
      msrp = c( 510,  600,  690), cost = c(100.98, 118.80, 136.62)
    ),
    "PowerView Gen 3 - Internal Rechargeable Battery" = list(
      h = c(60,  84, 180), w = c( 48,  84, 156),
      msrp = c( 510,  600,  690), cost = c(100.98, 118.80, 136.62)
    ),
    "PowerView+ (low-voltage wired)" = list(
      h = c(60,  84, 180), w = c( 48,  84, 156),
      msrp = c( 520,  610,  700), cost = c(102.96, 120.78, 138.60)
    ),
    "PowerView+ Gen 3" = list(
      h = c(60,  84, 180), w = c( 48,  84, 156),
      msrp = c( 520,  610,  700), cost = c(102.96, 120.78, 138.60)
    ),
    "PowerView AC" = list(
      h = c(48,  60, 180), w = c( 30, 240, 240),
      msrp = c( 880, 1035, 1190), cost = c(174.24, 204.93, 235.62)
    ),
    "PowerView Gen 3 AC" = list(
      h = c(48,  60, 180), w = c( 30, 240, 240),
      msrp = c( 880, 1035, 1190), cost = c(174.24, 204.93, 235.62)
    )
  )

  ac_coupled <- list(
    "2" = list(msrp =  230, cost =  45.54),
    "3" = list(msrp =  455, cost =  90.09),
    "4" = list(msrp =  685, cost = 135.63),
    "5" = list(msrp =  910, cost = 180.18)
  )

  if (ControlSystemName %in% names(fixed)) {
    entry <- fixed[[ControlSystemName]]
  } else if (ControlSystemName == "PowerView Gen 3 AC Coupled") {
    if (is.null(panels))
      stop("panels argument required for PowerView Gen 3 AC Coupled")
    entry <- ac_coupled[[as.character(panels)]]
    if (is.null(entry)) stop("panels must be 2, 3, 4, or 5 — got: ", panels)
  } else if (ControlSystemName %in% names(pv)) {
    tbl   <- pv[[ControlSystemName]]
    zone  <- .pv_zone(height, width, tbl$h, tbl$w)
    entry <- list(msrp = tbl$msrp[[zone]], cost = tbl$cost[[zone]])
  } else {
    stop("Unknown ControlSystemName: '", ControlSystemName, "'")
  }

  .component_result(ControlSystemName, entry$msrp, cost_factor, entry$cost)
}

# ── 3. top_treatment_motor ────────────────────────────────────────────────────
# Incremental contribution of the top treatment surcharge.
# Returns zeros for "No" (baseline). MSRP surcharge = cost = cost_per_inch × width.
# height and grid accepted for interface consistency; not used.

top_treatment_motor <- function(height, width, TopTreatmentName,
                                grid, cost_factor) {

  grid_width <- grid_widths[[grid]]

  cassette_prices <- list(
    DRSA = c(129, 152, 174, 226, 277, 327, 376, 425, 475),
    DRSB = c(132, 156, 178, 230, 281, 331, 380, 429, 503, 527),
    DRSC = c(136, 159, 182, 233, 285, 334, 384, 432, 506, 531),
    DRSD = c(141, 164, 187, 238, 289, 339, 389, 437, 511, 535),
    DRSE = c(144, 167, 189, 241, 292, 342, 391, 440, 514, 540),
    DRSF = c(146, 170, 192, 244, 295, 345, 394, 443, 517, 541),
    DRSG = c(152, 175, 198, 249, 301, 350, 400, 448, 522, 548)
  )[[grid]]

  dust_cover_prices <- list(
    DRSA = c(73, 85, 91, 105, 123, 136, 152, 184, 200),
    DRSB = c(77, 87, 96, 110, 126, 142, 157, 188, 203, 218),
    DRSC = c(83, 92, 101, 117, 133, 150, 165, 196, 213, 231),
    DRSD = c(84, 93, 101, 118, 134, 151, 166, 197, 214, 232),
    DRSE = c(85, 95, 102, 122, 135, 153, 168, 200, 216, 233),
    DRSF = c(87, 97, 103, 123, 137, 154, 170, 200, 218, 234),
    DRSG = c(88, 99, 106, 127, 142, 156, 173, 202, 219, 236)
  )[[grid]]

  fascia_prices <- if (identical(grid_width, widths_drs1)) {
    c(140, 149, 157, 174, 190, 213, 232, 250, 269)
  } else {
    c(140, 149, 157, 174, 190, 213, 232, 250, 269, 286)
  }

  pocket_prices <- if (identical(grid_width, widths_drs1)) {
    c(277, 320, 362, 454, 539, 631, 720, 811, 899)
  } else {
    c(277, 320, 362, 454, 539, 631, 720, 811, 899, 991)
  }

  list_price <- list(
    "No"                 = rep(0, length(grid_width)),
    "Cassette"           = cassette_prices,
    "Dust Cover Valance" = dust_cover_prices,
    "Fascia"             = fascia_prices,
    "Pocket"             = pocket_prices
  )

  cost_per_inch <- c(
    "No"                 = 0.00,
    "Cassette"           = 1.09,
    "Fascia"             = 0.71,
    "Dust Cover Valance" = 0.71,
    "Pocket"             = 0.71
  )

  if (!(TopTreatmentName %in% names(cost_per_inch))) {
    stop("Unknown TopTreatmentName: '", TopTreatmentName, "'")
  }

  cpi        <- cost_per_inch[[TopTreatmentName]]
  msrp       <- .lookup_by_width(width, grid_width, list_price[[TopTreatmentName]])
  unit_cost  <- cpi * width

  .component_result(TopTreatmentName, msrp, cost_factor, unit_cost)
}

# ── 4. total_motor ────────────────────────────────────────────────────────────
# Full shade: sums shade base + control system + top treatment components.

total_motor <- function(height, width, grid,
                        ControlSystemName, TopTreatmentName,
                        cost_factor, panels = NULL) {
  s  <- shades_motor(height, width, grid, cost_factor)
  cs <- control_system_motor(
    height, width, ControlSystemName, cost_factor, panels
  )
  tt <- top_treatment_motor(height, width, TopTreatmentName, grid, cost_factor)

  msrp       <- s$msrp + cs$msrp + tt$msrp
  unit_price <- msrp * cost_factor
  unit_cost  <- s$unit_cost + cs$unit_cost + tt$unit_cost

  list(
    component    = "TOTAL",
    msrp         = msrp,
    unit_price   = unit_price,
    unit_cost    = unit_cost,
    gross_margin = (unit_price - unit_cost) / unit_price,
    cost_to_msrp = unit_cost / msrp
  )
}

motor_breakdown <- function(height, width, grid,
                            ControlSystemName, TopTreatmentName,
                            cost_factor, panels = NULL) {
  s  <- shades_motor(height, width, grid, cost_factor)
  cs <- control_system_motor(
    height, width, ControlSystemName, cost_factor, panels
  )
  tt <- top_treatment_motor(height, width, TopTreatmentName, grid, cost_factor)
  tot <- total_motor(
    height, width, grid, ControlSystemName, TopTreatmentName,
    cost_factor, panels
  )

  out <- rbind(.to_df(s), .to_df(cs), .to_df(tt), .to_df(tot))
  row.names(out) <- NULL
  out
}


# ── Reporting helpers ────────────────────────────────────────────────────────

.fmt_d <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

.fmt_usd <- function(x) {
  ifelse(
    is.na(x),
    "-",
    paste0("$", formatC(x, format = "f", digits = 2, big.mark = ","))
  )
}

.fmt_pct <- function(x) {
  ifelse(
    is.na(x),
    "-",
    paste0(formatC(x * 100, format = "f", digits = 1), "%")
  )
}

.section <- function(title) {
  cat("\n", strrep("═", 70), "\n", sep = "")
  cat("  ", title, "\n", sep = "")
  cat(strrep("═", 70), "\n\n", sep = "")
}

.subsection <- function(title) {
  cat("\n── ", title, " ──\n", sep = "")
}

.format_motor_table <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "msrp", "unit_price", "unit_cost",
          "price", "cost",
          "treatment_msrp", "treatment_cost",
          "shade_msrp", "shade_price", "shade_cost",
          "control_msrp", "control_price", "control_cost",
          "treatment_price",
          "total_msrp", "total_price", "total_cost"
        )),
        .fmt_usd
      ),
      dplyr::across(
        dplyr::any_of(c(
          "gross_margin", "cost_to_msrp",
          "margin",
          "shade_margin", "shade_cost_to_msrp",
          "control_margin", "control_cost_to_msrp",
          "treatment_margin", "treatment_cost_to_msrp",
          "total_margin", "total_cost_to_msrp",
          "control_cost_to_total_msrp",
          "top_treatment_cost_to_msrp",
          "top_treatment_cost_to_total_msrp",
          "top_treatment_msrp_to_total_msrp"
        )),
        .fmt_pct
      )
    )
}

print_motor_breakdown <- function(height, width, grid,
                                  ControlSystemName, TopTreatmentName,
                                  cost_factor, panels = NULL) {
  .section(paste0(
    "Configured shade breakdown\n  ",
    "Grid: ", grid,
    "  |  H: ", height, "\"",
    "  |  W: ", width, "\"",
    "  |  System: ", ControlSystemName,
    "  |  Treatment: ", TopTreatmentName,
    "  |  Cost factor: ", .fmt_pct(cost_factor)
  ))

  motor_breakdown(
    height, width, grid,
    ControlSystemName, TopTreatmentName,
    cost_factor, panels
  ) |>
    .format_motor_table() |>
    knitr::kable(format = "simple", align = c("l", rep("r", 5))) |>
    cat(sep = "\n")

  cat("\n")
}

build_grid_sweep <- function(height, width,
                             ControlSystemName, TopTreatmentName,
                             cost_factor,
                             grids = names(shade_cost_to_msrp),
                             panels = NULL) {
  do.call(rbind, lapply(grids, function(grid) {
    r <- shades_motor(height, width, grid, cost_factor)

    data.frame(
      grid         = grid,
      msrp         = r$msrp,
      unit_price   = r$unit_price,
      unit_cost    = r$unit_cost,
      gross_margin = r$gross_margin,
      cost_to_msrp = r$cost_to_msrp,
      stringsAsFactors = FALSE
    )
  }))
}

print_grid_sweep <- function(height, width,
                             ControlSystemName, TopTreatmentName,
                             cost_factor,
                             grids = names(shade_cost_to_msrp),
                             panels = NULL) {
  .section(paste0(
    "Grid sweep — isolated shade base only\n  ",
    "H: ", height, "\"",
    "  |  W: ", width, "\"",
    "  |  Cost factor: ", .fmt_pct(cost_factor)
  ))

  build_grid_sweep(
    height, width, ControlSystemName, TopTreatmentName,
    cost_factor, grids, panels
  ) |>
    .format_motor_table() |>
    knitr::kable(format = "simple", align = c("l", rep("r", 5))) |>
    cat(sep = "\n")

  cat("\n")
}

build_control_system_sweep <- function(height, width, grid,
                                       TopTreatmentName, cost_factor,
                                       control_systems,
                                       panels = NULL) {
  do.call(rbind, lapply(control_systems, function(control_system) {
    r <- control_system_motor(
      height, width, control_system,
      cost_factor, panels
    )

    data.frame(
      control_system = control_system,
      msrp           = r$msrp,
      unit_price     = r$unit_price,
      unit_cost      = r$unit_cost,
      gross_margin   = r$gross_margin,
      cost_to_msrp   = r$cost_to_msrp,
      stringsAsFactors = FALSE
    )
  }))
}

print_control_system_sweep <- function(height, width, grid,
                                       TopTreatmentName, cost_factor,
                                       control_systems,
                                       panels = NULL) {
  .section(paste0(
    "Control system sweep — isolated add-on only\n  ",
    "H: ", height, "\"",
    "  |  W: ", width, "\"",
    "  |  Cost factor: ", .fmt_pct(cost_factor)
  ))

  build_control_system_sweep(
    height, width, grid, TopTreatmentName,
    cost_factor, control_systems, panels
  ) |>
    .format_motor_table() |>
    knitr::kable(format = "simple", align = c("l", rep("r", 5))) |>
    cat(sep = "\n")

  cat("\n")
}

build_top_treatment_sweep <- function(height, width, grid,
                                      ControlSystemName, cost_factor,
                                      top_treatments,
                                      panels = NULL) {
  do.call(rbind, lapply(top_treatments, function(top_treatment) {
    comp <- top_treatment_motor(
      height, width, top_treatment, grid, cost_factor
    )

    data.frame(
      top_treatment = top_treatment,
      msrp          = comp$msrp,
      unit_price    = comp$unit_price,
      unit_cost     = comp$unit_cost,
      gross_margin  = comp$gross_margin,
      cost_to_msrp  = comp$cost_to_msrp,
      stringsAsFactors = FALSE
    )
  }))
}

print_top_treatment_sweep <- function(height, width, grid,
                                      ControlSystemName, cost_factor,
                                      top_treatments,
                                      panels = NULL) {
  .section(paste0(
    "Top treatment sweep — isolated add-on only\n  ",
    "Grid: ", grid,
    "  |  H: ", height, "\"",
    "  |  W: ", width, "\"",
    "  |  Cost factor: ", .fmt_pct(cost_factor)
  ))

  build_top_treatment_sweep(
    height, width, grid, ControlSystemName,
    cost_factor, top_treatments, panels
  ) |>
    .format_motor_table() |>
    knitr::kable(format = "simple", align = c("l", rep("r", 5))) |>
    cat(sep = "\n")

  cat("\n")
}

build_dimension_sweep <- function(grid, ControlSystemName, TopTreatmentName,
                                  cost_factor, heights, widths,
                                  panels = NULL) {
  grid_df <- tidyr::expand_grid(width = widths, height = heights) |>
    dplyr::select(height, width)

  dplyr::bind_cols(
    grid_df,
    do.call(rbind, lapply(seq_len(nrow(grid_df)), function(i) {
      s <- shades_motor(
        grid_df$height[[i]], grid_df$width[[i]], grid, cost_factor
      )
      cs <- control_system_motor(
        grid_df$height[[i]], grid_df$width[[i]],
        ControlSystemName, cost_factor, panels
      )
      tt <- top_treatment_motor(
        grid_df$height[[i]], grid_df$width[[i]],
        TopTreatmentName, grid, cost_factor
      )
      total <- total_motor(
        grid_df$height[[i]], grid_df$width[[i]], grid,
        ControlSystemName, TopTreatmentName,
        cost_factor, panels
      )

      data.frame(
        shade_msrp            = s$msrp,
        shade_price           = s$unit_price,
        shade_cost            = s$unit_cost,
        shade_margin          = s$gross_margin,
        shade_cost_to_msrp    = s$cost_to_msrp,
        control_msrp          = cs$msrp,
        control_price         = cs$unit_price,
        control_cost          = cs$unit_cost,
        control_margin        = cs$gross_margin,
        control_cost_to_msrp  = cs$cost_to_msrp,
        treatment_msrp        = tt$msrp,
        treatment_price       = tt$unit_price,
        treatment_cost        = tt$unit_cost,
        treatment_margin      = tt$gross_margin,
        treatment_cost_to_msrp = tt$cost_to_msrp,
        total_msrp            = total$msrp,
        total_price           = total$unit_price,
        total_cost            = total$unit_cost,
        total_margin          = total$gross_margin,
        total_cost_to_msrp    = total$cost_to_msrp
      )
    }))
  )
}

print_dimension_sweep <- function(grid, ControlSystemName, TopTreatmentName,
                                  cost_factor, heights, widths,
                                  panels = NULL) {
  .section(paste0(
    "Dimension sweep — configured total, grouped by width\n  ",
    "Grid: ", grid,
    "  |  System: ", ControlSystemName,
    "  |  Treatment: ", TopTreatmentName,
    "  |  Cost factor: ", .fmt_pct(cost_factor)
  ))

  sweep <- build_dimension_sweep(
    grid, ControlSystemName, TopTreatmentName,
    cost_factor, heights, widths, panels
  )

  .subsection("1. Consolidated configured unit")
  sweep |>
    dplyr::select(
      height, width,
      msrp = total_msrp,
      price = total_price,
      cost = total_cost,
      margin = total_margin,
      cost_to_msrp = total_cost_to_msrp
    ) |>
    .format_motor_table() |>
    knitr::kable(format = "simple", align = c(rep("r", 7))) |>
    cat(sep = "\n")

  cat("\n")

  .subsection("2. Shade base")
  sweep |>
    dplyr::select(
      height, width,
      msrp = shade_msrp,
      price = shade_price,
      cost = shade_cost,
      margin = shade_margin,
      cost_to_msrp = shade_cost_to_msrp
    ) |>
    .format_motor_table() |>
    knitr::kable(format = "simple", align = c(rep("r", 7))) |>
    cat(sep = "\n")

  cat("\n")

  .subsection("3. Control system")
  sweep |>
    dplyr::select(
      height, width,
      msrp = control_msrp,
      price = control_price,
      cost = control_cost,
      margin = control_margin,
      cost_to_msrp = control_cost_to_msrp
    ) |>
    .format_motor_table() |>
    knitr::kable(format = "simple", align = c(rep("r", 7))) |>
    cat(sep = "\n")

  cat("\n")

  .subsection("4. Top treatment")
  sweep |>
    dplyr::select(
      height, width,
      msrp = treatment_msrp,
      price = treatment_price,
      cost = treatment_cost,
      margin = treatment_margin,
      cost_to_msrp = treatment_cost_to_msrp
    ) |>
    .format_motor_table() |>
    knitr::kable(format = "simple", align = c(rep("r", 7))) |>
    cat(sep = "\n")

  cat("\n")
}


# ── Verification ──────────────────────────────────────────────────────────────

.check <- function(label, got, expected, tol = 0.01) {
  ok     <- abs(got - expected) <= tol
  status <- if (ok) "PASS" else "FAIL"
  cat(sprintf("  [%s]  %-52s got %s  expected %s\n",
              status, label, .fmt_d(got), .fmt_d(expected)))
  invisible(ok)
}

verify_motors <- function() {

  # 1. bilinear_interp — exact grid nodes
  .section("bilinear_interp — exact grid nodes (DRSA)")
  .check("H=36  W=24  → 227",
         bilinear_interp(heights, widths_drs1, prices_drs1, 36,  24),  227)
  .check("H=36  W=107 → 613",
         bilinear_interp(heights, widths_drs1, prices_drs1, 36, 107),  613)
  .check("H=132 W=24  → 292",
         bilinear_interp(heights, widths_drs1, prices_drs1, 132,  24),  292)
  .check("H=132 W=107 → 1229",
         bilinear_interp(heights, widths_drs1, prices_drs1, 132, 107), 1229)

  .section("bilinear_interp — midpoint H=42 W=24 → ~228.5")
  .check("H=42 W=24 → ~228.5",
         bilinear_interp(heights, widths_drs1, prices_drs1, 42, 24),
         228.5, tol = 0.1)

  .section("bilinear_interp — clamping (H=20 → H=36)")
  .check("H=20 clamps to H=36 W=24",
         bilinear_interp(heights, widths_drs1, prices_drs1, 20, 24),
         bilinear_interp(heights, widths_drs1, prices_drs1, 36, 24))

  # 2. shades_motor
  .section("shades_motor — exact node")
  sm <- shades_motor(36, 24, "DRSA", cost_factor = 0.55)
  .check("DRSA H=36 W=24  msrp = 227",          sm$msrp,        227)
  .check("DRSA H=36 W=24  unit_price = 124.85",  sm$unit_price,  227 * 0.55)
  .check("DRSA H=36 W=24  unit_cost  =  49.26",  sm$unit_cost,   227 * 0.217)
  .check("DRSA H=36 W=24  gross_margin",         sm$gross_margin,
         (227 * 0.55 - 227 * 0.217) / (227 * 0.55), tol = 0.001)

  .section("shades_motor — all grids H=60 W=48")
  expected_msrp <- c(DRSA = 349, DRSB = 376, DRSC = 447, DRSD = 522,
                     DRSE = 586, DRSF = 665, DRSG = 763)
  for (g in names(expected_msrp)) {
    sm_g <- shades_motor(60, 48, g, cost_factor = 0.55)
    .check(paste(g, "H=60 W=48 msrp"), sm_g$msrp, expected_msrp[[g]])
  }

  # 3. control_system_motor — fixed systems
  .section("control_system_motor — fixed systems")
  cs_cc <- control_system_motor(60, 36, "Custom Clutch", 0.55)
  .check("Custom Clutch  msrp  = 0",     cs_cc$msrp,     0)
  .check("Custom Clutch  cost  = 0",     cs_cc$unit_cost, 0)

  cs_ug <- control_system_motor(60, 36, "UltraGlide", 0.55)
  .check("UltraGlide     msrp  = 130",   cs_ug$msrp,     130)
  .check("UltraGlide     cost  = 20.68", cs_ug$unit_cost, 20.68)

  cs_st <- control_system_motor(60, 36, "SoftTouch Motorization", 0.55)
  .check("SoftTouch Mot  msrp  = 200",   cs_st$msrp,     200)
  .check("SoftTouch Mot  cost  = 39.60", cs_st$unit_cost, 39.60)

  .section("control_system_motor — PowerView Gen 3 zones")
  cs_s <- control_system_motor(50,  40, "PowerView Gen 3", 0.55)
  cs_m <- control_system_motor(70,  70, "PowerView Gen 3", 0.55)
  cs_l <- control_system_motor(100, 100, "PowerView Gen 3", 0.55)
  .check("PV Gen3 Small  msrp  = 440",    cs_s$msrp,     440)
  .check("PV Gen3 Small  cost  = 87.12",  cs_s$unit_cost, 87.12)
  .check("PV Gen3 Medium msrp  = 515",    cs_m$msrp,     515)
  .check("PV Gen3 Medium cost  = 101.97", cs_m$unit_cost, 101.97)
  .check("PV Gen3 Large  msrp  = 595",    cs_l$msrp,     595)
  .check("PV Gen3 Large  cost  = 117.81", cs_l$unit_cost, 117.81)

  .section("control_system_motor — PowerView Gen 3 AC zone boundary")
  # H=48 W=30 → Small; W=31 pushes into Medium
  cs_ac_s <- control_system_motor(48, 30, "PowerView Gen 3 AC", 0.55)
  cs_ac_m <- control_system_motor(48, 31, "PowerView Gen 3 AC", 0.55)
  .check("PV Gen3 AC Small  msrp =  880", cs_ac_s$msrp,  880)
  .check("PV Gen3 AC Medium msrp = 1035", cs_ac_m$msrp, 1035)

  .section("control_system_motor — AC Coupled panel count")
  cs_ac3 <- control_system_motor(
    60, 36, "PowerView Gen 3 AC Coupled", 0.55, panels = 3
  )
  .check("AC Coupled 3 panels  msrp = 455",   cs_ac3$msrp,     455)
  .check("AC Coupled 3 panels  cost = 90.09", cs_ac3$unit_cost, 90.09)

  # 4. top_treatment_motor
  .section("top_treatment_motor")
  tt_no <- top_treatment_motor(60, 36, "No",       "DRSC", 0.55)
  tt_ca <- top_treatment_motor(60, 36, "Cassette", "DRSC", 0.55)
  tt_fa <- top_treatment_motor(60, 48, "Fascia",   "DRSC", 0.55)
  .check("No treatment   msrp = 0",    tt_no$msrp,     0)
  .check("No treatment   cost = 0",    tt_no$unit_cost, 0)
  .check("Cassette W=36  msrp = 182",   tt_ca$msrp,    182)
  .check("Cassette W=36  cost = 39.24", tt_ca$unit_cost, 1.09 * 36)
  .check("Fascia   W=48  msrp = 174",   tt_fa$msrp,    174)

  # 5. total_motor — additivity
  .section("total_motor — component additivity")
  ex_h <- 72; ex_w <- 54; ex_g <- "DRSC"; ex_cf <- 0.52
  sm_r  <- shades_motor(ex_h, ex_w, ex_g, ex_cf)
  cs_r  <- control_system_motor(ex_h, ex_w, "LiteRise", ex_cf)
  tt_r  <- top_treatment_motor(ex_h, ex_w, "Cassette", ex_g, ex_cf)
  tot   <- total_motor(ex_h, ex_w, ex_g, "LiteRise", "Cassette", ex_cf)
  .check("total msrp = shades + cs + tt",
         tot$msrp, sm_r$msrp + cs_r$msrp + tt_r$msrp)
  .check("total unit_cost = shades + cs + tt",
         tot$unit_cost, sm_r$unit_cost + cs_r$unit_cost + tt_r$unit_cost)
  .check("total unit_price = total_msrp x cost_factor",
         tot$unit_price, tot$msrp * ex_cf)

  .section("total_motor — worked example")
  cat(sprintf(
    "\n  Config: H=%d W=%d Grid=%s  LiteRise  Cassette  CF=%.2f\n",
    ex_h, ex_w, ex_g, ex_cf
  ))
  cat(sprintf("  MSRP:         $%s\n",  .fmt_d(tot$msrp)))
  cat(sprintf("  Unit price:   $%s\n",  .fmt_d(tot$unit_price)))
  cat(sprintf("  Unit cost:    $%s\n",  .fmt_d(tot$unit_cost)))
  cat(sprintf("  Gross margin: %s\n",   .fmt_pct(tot$gross_margin)))
  cat(sprintf("  Cost/MSRP:    %s\n",   .fmt_pct(tot$cost_to_msrp)))
  cat(sprintf("  Components:\n"))
  cat(sprintf("    Shade base — msrp $%s  cost $%s  ctm %s\n",
              .fmt_d(sm_r$msrp), .fmt_d(sm_r$unit_cost),
              .fmt_pct(sm_r$cost_to_msrp)))
  cat(sprintf("    LiteRise   — msrp $%s  cost $%s\n",
              .fmt_d(cs_r$msrp), .fmt_d(cs_r$unit_cost)))
  cat(sprintf("    Cassette   — msrp $%s  cost $%s\n",
              .fmt_d(tt_r$msrp), .fmt_d(tt_r$unit_cost)))

  cat("\n── Done ──\n")
}
