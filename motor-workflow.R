# ============================================================
# MOTOR WORKFLOW — ROLLER SHADES (08)
# Calculate MSRP, price, cost, and gross margin for any
# shade configuration. Edit the parameter blocks below
# and re-run the relevant section.
# ============================================================

workflow_path <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      rstudioapi::getActiveDocumentContext()$path
    } else {
      "motor-workflow.R"
    }
  }
)

workflow_dir <- dirname(normalizePath(workflow_path, winslash = "/", mustWork = TRUE))

required_packages <- c("dplyr", "tidyr", "knitr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Instale os pacotes faltantes antes de executar: ",
    paste(missing_packages, collapse = ", "),
    "\nNo Console do R, nao no PowerShell, use: install.packages(c(",
    paste(sprintf('\"%s\"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

source(file.path(workflow_dir, "motor-functions.R"))


# ── VERIFY (run once after any change to motor-functions.R) ───────────────────

# verify_motors()


# ============================================================
# SECTION 1 — Single configured shade
#
# Full decomposition: base shade + control system + top treatment
# plus the total configured unit.
# ============================================================

cfg_height         <- 72
cfg_width          <- 54
cfg_grid           <- "DRSC"
cfg_control_system <- "PowerView Gen 3"
cfg_top_treatment  <- "Cassette"
cfg_cost_factor    <- 0.40

print_motor_breakdown(
  cfg_height, cfg_width, cfg_grid,
  cfg_control_system, cfg_top_treatment, cfg_cost_factor
)


# ============================================================
# SECTION 2 — Grid sweep
#
# Isolated base shade only. Same dimensions and cost factor.
# Only the fabric grid changes.
# ============================================================

sw_height         <- 72
sw_width          <- 54
sw_control_system <- "Custom Clutch"
sw_top_treatment  <- "No"
sw_cost_factor    <- 0.40

print_grid_sweep(
  sw_height, sw_width,
  sw_control_system, sw_top_treatment, sw_cost_factor
)


# ============================================================
# SECTION 3 — Control system sweep
#
# Isolated control-system add-on only. Same dimensions and
# cost factor. Only the control system changes.
# ============================================================

cs_height        <- 72
cs_width         <- 54
cs_grid          <- "DRSC"
cs_top_treatment <- "No"
cs_cost_factor   <- 0.40

control_systems <- c(
  "Custom Clutch", "UltraGlide", "LiteRise", "Cordless Lift",
  "SoftTouch Motorization",
  "PowerView", "PowerView AC", "PowerView+ (low-voltage wired)",
  "Somfy Motorized (RF & H motor)"
)

print_control_system_sweep(
  cs_height, cs_width, cs_grid,
  cs_top_treatment, cs_cost_factor,
  control_systems
)


# ============================================================
# SECTION 4 — Top treatment sweep
#
# Isolated top-treatment add-on only. Same grid, dimensions,
# and cost factor. Only the top treatment changes.
# ============================================================

tt_height         <- 72
tt_width          <- 54
tt_grid           <- "DRSC"
tt_control_system <- "Custom Clutch"
tt_cost_factor    <- 0.40

top_treatments <- c("No", "Cassette", "Fascia", "Dust Cover Valance", "Pocket")

print_top_treatment_sweep(
  tt_height, tt_width, tt_grid,
  tt_control_system, tt_cost_factor,
  top_treatments
)


# ============================================================
# SECTION 5 — Dimension sweep
#
# Configured total by dimension. Same grid, control system,
# treatment, and cost factor. Width and height move across
# representative points.
# ============================================================

dim_grid           <- "DRSC"
dim_control_system <- "PowerView Gen 3"
dim_top_treatment  <- "Cassette"
dim_cost_factor    <- 0.40

print_dimension_sweep(
  dim_grid, dim_control_system, dim_top_treatment, dim_cost_factor,
  heights = c(36, 60, 84, 108, 132),
  widths  = c(24, 48, 72, 96)
)
