rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(rxode2)
})

set.seed(20260726)

N_VIRTUAL <- 100L
N_SIM <- 1000L
N_PER_GROUP <- 2:4
IIV_CV <- 0.30
FOLD_LIMITS <- c(0.5, 2)
MONKEY_BW_KG <- 3.5
AUC_END_H <- 28 * 24

MOLECULES <- c(
  "Atezolizumab",
  "Avelumab",
  "Nivolumab",
  "Pembrolizumab"
)
STUDY_DOSES <- c(0.3, 3, 10)
DESIGN_DOSES <- list(
  `2 dose groups` = c(0.3, 10),
  `3 dose groups` = c(0.3, 3, 10)
)
ENDPOINT_LEVELS <- c("AUC0-28d", "Cmax", "Terminal clearance")

SPARSE_TIME_H <- sort(unique(
  c(0.25, 1, 2, 4, 8, 1, 2, 6, 10, 14, 21, 28) *
    c(rep(1, 5), rep(24, 7))
))
RICH_TIME_H <- c(0.25, 0.5, seq(1, AUC_END_H, by = 1))

script_file <- sub(
  "^--file=",
  "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
)
study_dir <- if (!is.na(script_file) && nzchar(script_file)) {
  dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
} else if (
  basename(normalizePath(".", winslash = "/", mustWork = TRUE)) ==
    "06_StudySimulations"
) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else {
  normalizePath("06_StudySimulations", winslash = "/", mustWork = TRUE)
}
project_dir <- normalizePath(
  file.path(study_dir, ".."),
  winslash = "/",
  mustWork = TRUE
)
output_root <- file.path(study_dir, "SimsOutputs")
table_dir <- file.path(output_root, "Tables")
figure_dir <- file.path(output_root, "Figures")
walk(
  c(output_root, table_dir, figure_dir),
  dir.create,
  recursive = TRUE,
  showWarnings = FALSE
)

run4_file <- file.path(
  project_dir,
  "04_TopDown",
  "SimsOutputs",
  "TopDownTMDD_Run4_CL_Vc_Q_Vp_fixed_TMDD_Monkey.rds"
)
monkey_data_file <- file.path(
  project_dir,
  "01_Data",
  "Monkey",
  "mAb_data_clean.csv"
)

study_theme <- theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

mean_or_na <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else mean(x)
}

linup_logdown_auc <- function(time_h, concentration) {
  profile <- tibble(
    time_h = as.numeric(time_h),
    concentration = as.numeric(concentration)
  ) |>
    filter(
      is.finite(time_h),
      is.finite(concentration),
      time_h >= 0,
      time_h <= AUC_END_H,
      concentration >= 0
    ) |>
    group_by(time_h) |>
    summarise(concentration = mean(concentration), .groups = "drop") |>
    arrange(time_h)

  if (nrow(profile) < 2L) return(NA_real_)
  if (!any(profile$time_h == 0)) {
    profile <- bind_rows(
      tibble(time_h = 0, concentration = first(profile$concentration)),
      profile
    ) |>
      arrange(time_h)
  }

  dt <- diff(profile$time_h)
  c1 <- head(profile$concentration, -1)
  c2 <- tail(profile$concentration, -1)
  declining <- c2 < c1 & c1 > 0 & c2 > 0
  intervals <- dt * (c1 + c2) / 2
  intervals[declining] <-
    dt[declining] * (c1[declining] - c2[declining]) /
    log(c1[declining] / c2[declining])
  sum(intervals)
}

best_terminal_lambda_z <- function(time_h, concentration, min_points = 3L) {
  terminal_data <- tibble(
    time_h = as.numeric(time_h),
    concentration = as.numeric(concentration)
  ) |>
    filter(
      is.finite(time_h),
      is.finite(concentration),
      concentration > 0
    ) |>
    group_by(time_h) |>
    summarise(concentration = mean(concentration), .groups = "drop") |>
    arrange(time_h)

  if (nrow(terminal_data) < min_points) return(NA_real_)
  tmax_index <- which.max(terminal_data$concentration)[1]
  start_index <- min(tmax_index + 1L, nrow(terminal_data))
  post_cmax <- terminal_data |> slice(start_index:n())
  if (nrow(post_cmax) < min_points) {
    post_cmax <- terminal_data |> slice(tmax_index:n())
  }
  if (nrow(post_cmax) < min_points) return(NA_real_)

  x <- post_cmax$time_h
  y <- log(post_cmax$concentration)
  total_n <- length(x)
  suffix_sum <- function(z) rev(cumsum(rev(z)))
  sx <- suffix_sum(x)
  sy <- suffix_sum(y)
  sxx <- suffix_sum(x * x)
  syy <- suffix_sum(y * y)
  sxy <- suffix_sum(x * y)
  terminal_n <- seq.int(min_points, total_n)
  candidate_start <- total_n - terminal_n + 1L
  s_xx <- sxx[candidate_start] - sx[candidate_start]^2 / terminal_n
  s_yy <- syy[candidate_start] - sy[candidate_start]^2 / terminal_n
  s_xy <- sxy[candidate_start] -
    sx[candidate_start] * sy[candidate_start] / terminal_n
  slope <- s_xy / s_xx
  r2 <- pmin(pmax((s_xy^2) / (s_xx * s_yy), 0), 1)
  adj_r2 <- 1 - (1 - r2) * (terminal_n - 1) / (terminal_n - 2)

  candidates <- tibble(slope, adj_r2, terminal_n) |>
    filter(is.finite(slope), slope < 0, is.finite(adj_r2)) |>
    arrange(desc(adj_r2), desc(terminal_n))

  if (nrow(candidates) == 0L) NA_real_ else -candidates$slope[1]
}

auc_inf_ug_h_ml <- function(time_h, concentration, lambda_z) {
  if (!is.finite(lambda_z) || lambda_z <= 0) return(NA_real_)

  profile <- tibble(
    time_h = as.numeric(time_h),
    concentration = as.numeric(concentration)
  ) |>
    filter(
      is.finite(time_h),
      is.finite(concentration),
      time_h >= 0,
      time_h <= AUC_END_H,
      concentration >= 0
    ) |>
    group_by(time_h) |>
    summarise(concentration = mean(concentration), .groups = "drop") |>
    arrange(time_h)

  if (nrow(profile) < 2L || !any(profile$concentration > 0)) {
    return(NA_real_)
  }
  if (!any(profile$time_h == 0)) {
    profile <- bind_rows(
      tibble(time_h = 0, concentration = first(profile$concentration)),
      profile
    ) |>
      arrange(time_h)
  }

  positive <- profile |> filter(concentration > 0)
  c_last <- last(positive$concentration)
  auc_last <- linup_logdown_auc(profile$time_h, profile$concentration)
  auc_last + c_last / lambda_z
}

terminal_clearance_l_day <- function(
  dose_mg_kg,
  body_weight_kg,
  auc_inf
) {
  if (!is.finite(auc_inf) || auc_inf <= 0) return(NA_real_)
  24 * dose_mg_kg * body_weight_kg / auc_inf
}

make_lognormal <- function(n, typical, cv) {
  sdlog <- sqrt(log1p(cv^2))
  rlnorm(n, meanlog = log(typical), sdlog = sdlog)
}

draw_structural_parameter <- function(n, typical, rse_fraction, iiv_cv) {
  uncertainty_multiplier <- make_lognormal(n, 1, rse_fraction)
  individual_multiplier <- make_lognormal(n, 1, iiv_cv)
  typical * uncertainty_multiplier * individual_multiplier
}

conc_nm_to_ug_ml <- function(concentration_nm, mw_g_mol) {
  as.numeric(concentration_nm) * as.numeric(mw_g_mol) / 1e6
}

dose_mgkg_to_nmolkg <- function(dose_mgkg, mw_g_mol) {
  as.numeric(dose_mgkg) * 1e6 / as.numeric(mw_g_mol)
}

theta_value <- function(theta, candidates, label, molecule) {
  selected <- candidates[candidates %in% names(theta)][1]
  if (is.na(selected)) {
    stop(
      "Run 4 theta for ", molecule, " does not contain ", label,
      ". Tried: ", paste(candidates, collapse = ", ")
    )
  }
  value <- unname(theta[[selected]])
  if (!is.finite(value) || value <= 0) {
    stop("Invalid ", label, " for ", molecule, ": ", value)
  }
  value
}

rse_fraction <- function(rse_percent, candidates, label, molecule) {
  selected <- candidates[candidates %in% names(rse_percent)][1]
  if (is.na(selected)) {
    message(
      "No Run 4 RSE for fixed ", label, " in ", molecule,
      "; assuming 40% RSE."
    )
    return(0.40)
  }
  value <- unname(rse_percent[[selected]]) / 100
  if (!is.finite(value) || value < 0) {
    stop("Invalid ", label, " RSE for ", molecule, ": ", value)
  }
  value
}

run4_fits <- readRDS(run4_file)
missing_molecules <- setdiff(MOLECULES, names(run4_fits))
if (length(missing_molecules) > 0L) {
  stop("Run 4 fits are missing: ", paste(missing_molecules, collapse = ", "))
}

mw_lookup <- read.csv(
  monkey_data_file,
  stringsAsFactors = FALSE,
  check.names = FALSE
) |>
  transmute(Drug = Molecule, MW = as.numeric(`Molecular.Weight`)) |>
  filter(Drug %in% MOLECULES, is.finite(MW), MW > 0) |>
  group_by(Drug) |>
  summarise(MW = median(MW), .groups = "drop")

typical_parameters <- map_dfr(MOLECULES, function(drug) {
  theta <- run4_fits[[drug]]$fit$theta
  rse <- run4_fits[[drug]]$fit$rse_percent
  tibble(
    Drug = drug,
    CL_L_h_kg = theta_value(theta, c("cl"), "CL", drug),
    CL_RSE_fraction = rse_fraction(rse, c("lcl"), "CL", drug),
    Vc_L_kg = theta_value(theta, c("vc", "v1"), "V1", drug),
    Vc_RSE_fraction = rse_fraction(rse, c("lvc", "lv1"), "V1", drug),
    Q_L_h_kg = theta_value(theta, c("q"), "Q", drug),
    Q_RSE_fraction = rse_fraction(rse, c("lq"), "Q", drug),
    Vp_L_kg = theta_value(theta, c("vp", "v2"), "V2", drug),
    Vp_RSE_fraction = rse_fraction(rse, c("lvp", "lv2"), "V2", drug),
    Rtot_nM = theta_value(theta, c("r0", "rtot"), "Rtot", drug),
    kdeg_h = theta_value(theta, c("kdeg"), "kdeg", drug),
    kint_h = theta_value(theta, c("kint"), "kint", drug),
    Kss_nM = theta_value(theta, c("kss"), "Kss", drug)
  )
}) |>
  left_join(mw_lookup, by = "Drug") |>
  relocate(MW, .after = Drug)

if (any(!is.finite(typical_parameters$MW))) {
  stop("Missing molecular weight for: ", paste(
    typical_parameters$Drug[!is.finite(typical_parameters$MW)],
    collapse = ", "
  ))
}

parameter_names <- c(
  "CL_L_h_kg",
  "Vc_L_kg",
  "Q_L_h_kg",
  "Vp_L_kg",
  "Rtot_nM",
  "kdeg_h",
  "kint_h",
  "Kss_nM"
)
structural_parameter_names <- c(
  "CL_L_h_kg",
  "Vc_L_kg",
  "Q_L_h_kg",
  "Vp_L_kg"
)
structural_rse_names <- c(
  CL_L_h_kg = "CL_RSE_fraction",
  Vc_L_kg = "Vc_RSE_fraction",
  Q_L_h_kg = "Q_RSE_fraction",
  Vp_L_kg = "Vp_RSE_fraction"
)
fixed_tmdd_parameter_names <- setdiff(
  parameter_names,
  structural_parameter_names
)

parameter_draws <- map_dfr(MOLECULES, function(drug) {
  typical <- typical_parameters |> filter(Drug == drug)
  structural_draws <- map(
    structural_parameter_names,
    ~ draw_structural_parameter(
      N_VIRTUAL,
      typical[[.x]],
      typical[[structural_rse_names[[.x]]]],
      IIV_CV
    )
  ) |>
    set_names(structural_parameter_names) |>
    as_tibble()
  fixed_tmdd_draws <- map(
    fixed_tmdd_parameter_names,
    ~ rep(typical[[.x]], N_VIRTUAL)
  ) |>
    set_names(fixed_tmdd_parameter_names) |>
    as_tibble()
  draws <- bind_cols(structural_draws, fixed_tmdd_draws) |>
    select(all_of(parameter_names))

  tibble(
    Drug = drug,
    MW = typical$MW,
    IndividualId = seq_len(N_VIRTUAL)
  ) |>
    bind_cols(draws)
})

write.csv(
  parameter_draws,
  file.path(table_dir, "Gate2_topdown_reference_population_parameters.csv"),
  row.names = FALSE
)

qss_rxode_model <- rxode2::rxode({
  Ltot <- centr / vc
  Cp <- peri / vp
  free_drug <- 0.5 * ((Ltot - total_target - kss) +
    sqrt((Ltot - total_target - kss)^2 + 4 * kss * Ltot + 1e-12))
  complex <- Ltot - free_drug
  free_target <- total_target - complex

  d/dt(centr) = -cl * free_drug - q * free_drug + q * Cp -
    kint * complex * vc
  d/dt(peri) = q * free_drug - q * Cp
  d/dt(total_target) = ksyn - kdeg * free_target - kint * complex
})

simulate_topdown_profile <- function(parameter_row, dose_mgkg) {
  theta <- c(
    cl = parameter_row$CL_L_h_kg,
    vc = parameter_row$Vc_L_kg,
    q = parameter_row$Q_L_h_kg,
    vp = parameter_row$Vp_L_kg,
    r0 = parameter_row$Rtot_nM,
    kdeg = parameter_row$kdeg_h,
    kint = parameter_row$kint_h,
    kss = parameter_row$Kss_nM,
    ksyn = parameter_row$Rtot_nM * parameter_row$kdeg_h
  )

  event_table <- data.frame(
    id = parameter_row$IndividualId,
    time = c(0, RICH_TIME_H),
    amt = c(
      dose_mgkg_to_nmolkg(dose_mgkg, parameter_row$MW),
      rep(NA_real_, length(RICH_TIME_H))
    ),
    evid = c(1, rep(0, length(RICH_TIME_H))),
    cmt = "centr"
  )

  solution <- rxode2::rxSolve(
    qss_rxode_model,
    params = theta,
    events = event_table,
    inits = c(centr = 0, peri = 0, total_target = theta[["r0"]]),
    rtol = 1e-6,
    atol = 1e-9,
    hmin = 1e-12,
    maxsteps = 100000,
    returnType = "data.frame"
  ) |>
    as_tibble()

  if ("evid" %in% names(solution)) {
    solution <- solution |> filter(evid == 0 | is.na(evid))
  }

  solution |>
    transmute(
      Drug = parameter_row$Drug,
      IndividualId = as.character(parameter_row$IndividualId),
      Dose_mgkg = dose_mgkg,
      Time_h = as.numeric(time),
      Concentration_ug_mL = conc_nm_to_ug_ml(
        pmax(as.numeric(Ltot), 0),
        parameter_row$MW
      )
    )
}

message("Simulating rich top-down profiles...")
topdown_rich_profiles <- map_dfr(STUDY_DOSES, function(dose_mgkg) {
  map_dfr(
    seq_len(nrow(parameter_draws)),
    ~ simulate_topdown_profile(parameter_draws[.x, ], dose_mgkg)
  )
}) |>
  group_by(Drug, IndividualId, Dose_mgkg, Time_h) |>
  summarise(
    Concentration_ug_mL = mean(Concentration_ug_mL, na.rm = TRUE),
    .groups = "drop"
  )

topdown_sparse_profiles <- topdown_rich_profiles |>
  filter(Time_h %in% SPARSE_TIME_H)

calculate_endpoints <- function(profile, profile_type) {
  profile |>
    group_by(Drug, IndividualId, Dose_mgkg) |>
    group_modify(~ {
      lambda_z <- best_terminal_lambda_z(
        .x$Time_h,
        .x$Concentration_ug_mL
      )
      auc_inf <- auc_inf_ug_h_ml(
        .x$Time_h,
        .x$Concentration_ug_mL,
        lambda_z
      )
      tibble(
        Profile_type = profile_type,
        AUC0_28d_ug_h_mL = linup_logdown_auc(
          .x$Time_h,
          .x$Concentration_ug_mL
        ),
        Cmax_ug_mL = max(.x$Concentration_ug_mL, na.rm = TRUE),
        Lambda_z_1_h = lambda_z,
        AUCinf_ug_h_mL = auc_inf,
        Terminal_CL_L_day = terminal_clearance_l_day(
          .y$Dose_mgkg,
          MONKEY_BW_KG,
          auc_inf
        )
      )
    }) |>
    ungroup()
}

rich_endpoints <- calculate_endpoints(topdown_rich_profiles, "Rich")
sparse_endpoints <- calculate_endpoints(topdown_sparse_profiles, "Sparse")

validate_endpoints <- function(data, label) {
  required <- c(
    "AUC0_28d_ug_h_mL",
    "Cmax_ug_mL",
    "Terminal_CL_L_day"
  )
  invalid <- data |>
    filter(if_any(all_of(required), ~ !is.finite(.x) | .x <= 0))
  if (nrow(invalid) > 0L) {
    stop(label, " endpoint calculation failed for ", nrow(invalid), " rows.")
  }
  invisible(data)
}

validate_endpoints(rich_endpoints, "Rich")
validate_endpoints(sparse_endpoints, "Sparse")

write.csv(
  rich_endpoints,
  file.path(table_dir, "Gate2_topdown_rich_PK_endpoints.csv"),
  row.names = FALSE
)
write.csv(
  sparse_endpoints,
  file.path(table_dir, "Gate2_topdown_sparse_PK_endpoints.csv"),
  row.names = FALSE
)

endpoint_long <- function(data, value_name) {
  data |>
    select(
      Drug,
      IndividualId,
      Dose_mgkg,
      AUC0_28d_ug_h_mL,
      Cmax_ug_mL,
      Terminal_CL_L_day
    ) |>
    pivot_longer(
      cols = c(
        AUC0_28d_ug_h_mL,
        Cmax_ug_mL,
        Terminal_CL_L_day
      ),
      names_to = "Endpoint",
      values_to = value_name
    ) |>
    mutate(
      Endpoint = recode(
        Endpoint,
        AUC0_28d_ug_h_mL = "AUC0-28d",
        Cmax_ug_mL = "Cmax",
        Terminal_CL_L_day = "Terminal clearance"
      ),
      Endpoint = factor(Endpoint, levels = ENDPOINT_LEVELS)
    )
}

sparse_endpoint_long <- endpoint_long(
  sparse_endpoints,
  "Sparse_value"
)
rich_reference <- endpoint_long(rich_endpoints, "Rich_value") |>
  group_by(Drug, Dose_mgkg, Endpoint) |>
  summarise(
    Reference_value = mean(Rich_value),
    Reference_CV_percent = 100 * sd(Rich_value) / mean(Rich_value),
    .groups = "drop"
  )

write.csv(
  rich_reference,
  file.path(table_dir, "Gate2_reference_PK_endpoints.csv"),
  row.names = FALSE
)

sparse_lookup <- split(
  sparse_endpoint_long$Sparse_value,
  paste(
    sparse_endpoint_long$Drug,
    sparse_endpoint_long$Dose_mgkg,
    sparse_endpoint_long$Endpoint,
    sep = "|"
  )
)
reference_lookup <- setNames(
  rich_reference$Reference_value,
  paste(
    rich_reference$Drug,
    rich_reference$Dose_mgkg,
    rich_reference$Endpoint,
    sep = "|"
  )
)

simulate_design_replicates <- function(drug, design, n_per_group) {
  doses <- DESIGN_DOSES[[design]]

  map_dfr(seq_len(N_SIM), function(replicate_id) {
    crossing(
      Dose_mgkg = doses,
      Endpoint = ENDPOINT_LEVELS
    ) |>
      mutate(
        Study_value = map2_dbl(Dose_mgkg, Endpoint, ~ {
          key <- paste(drug, .x, .y, sep = "|")
          mean(sample(sparse_lookup[[key]], n_per_group, replace = TRUE))
        }),
        Reference_value = map2_dbl(Dose_mgkg, Endpoint, ~ {
          key <- paste(drug, .x, .y, sep = "|")
          unname(reference_lookup[[key]])
        }),
        Fold_error = Study_value / Reference_value,
        Within_2fold =
          Fold_error >= FOLD_LIMITS[1] &
          Fold_error <= FOLD_LIMITS[2],
        Drug = drug,
        Replicate = replicate_id,
        Design = design,
        N_per_group = n_per_group,
        Total_animals = length(doses) * n_per_group
      ) |>
      select(
        Drug,
        Replicate,
        Design,
        N_per_group,
        Total_animals,
        Dose_mgkg,
        Endpoint,
        Reference_value,
        Study_value,
        Fold_error,
        Within_2fold
      )
  })
}

message("Resampling reduced-study designs...")
design_fold_error_replicates <- crossing(
  Drug = MOLECULES,
  Design = names(DESIGN_DOSES),
  N_per_group = N_PER_GROUP
) |>
  mutate(
    Replicates = pmap(
      list(Drug, Design, N_per_group),
      simulate_design_replicates
    )
  ) |>
  select(Replicates) |>
  unnest(Replicates) |>
  mutate(
    Design = factor(Design, levels = names(DESIGN_DOSES)),
    Endpoint = factor(Endpoint, levels = ENDPOINT_LEVELS)
  )

design_fold_error_summary <- design_fold_error_replicates |>
  group_by(
    Drug,
    Design,
    N_per_group,
    Total_animals,
    Dose_mgkg,
    Endpoint
  ) |>
  summarise(
    Fraction_within_2fold = mean(Within_2fold),
    Median_fold_error = median(Fold_error),
    Fold_error_5th_percentile = quantile(Fold_error, 0.05),
    Fold_error_95th_percentile = quantile(Fold_error, 0.95),
    .groups = "drop"
  )

design_endpoint_success <- design_fold_error_replicates |>
  group_by(
    Drug,
    Replicate,
    Design,
    N_per_group,
    Total_animals,
    Endpoint
  ) |>
  summarise(
    All_doses_within_2fold = all(Within_2fold),
    .groups = "drop"
  ) |>
  group_by(Drug, Design, N_per_group, Total_animals, Endpoint) |>
  summarise(
    Fraction_within_2fold = mean(All_doses_within_2fold),
    .groups = "drop"
  )

overall_design_success <- design_fold_error_replicates |>
  group_by(
    Drug,
    Replicate,
    Design,
    N_per_group,
    Total_animals
  ) |>
  summarise(
    All_endpoints_and_doses_within_2fold = all(Within_2fold),
    .groups = "drop"
  ) |>
  group_by(Drug, Design, N_per_group, Total_animals) |>
  summarise(
    Fraction_within_2fold = mean(All_endpoints_and_doses_within_2fold),
    .groups = "drop"
  )

write.csv(
  design_fold_error_replicates,
  file.path(
    table_dir,
    "Gate2_design_standard_fold_error_replicates_by_endpoint.csv"
  ),
  row.names = FALSE
)
write.csv(
  design_fold_error_summary,
  file.path(
    table_dir,
    "Gate2_design_standard_fold_error_summary_by_endpoint.csv"
  ),
  row.names = FALSE
)
write.csv(
  design_endpoint_success,
  file.path(table_dir, "Gate2_design_endpoint_success.csv"),
  row.names = FALSE
)
write.csv(
  overall_design_success,
  file.path(table_dir, "Gate2_design_overall_success.csv"),
  row.names = FALSE
)

sample_size_colors <- setNames(
  viridisLite::viridis(
    length(N_PER_GROUP),
    option = "D",
    direction = -1,
    begin = 0.12,
    end = 0.82
  ),
  N_PER_GROUP
)

standard_fold_error_plot <- ggplot(
  design_fold_error_replicates,
  aes(
    x = Fold_error,
    y = forcats::fct_rev(factor(Drug)),
    fill = factor(N_per_group),
    color = factor(N_per_group)
  )
) +
  geom_vline(xintercept = 1, color = "grey35", linewidth = 0.5) +
  geom_vline(
    xintercept = FOLD_LIMITS,
    color = "#B22222",
    linetype = 2,
    linewidth = 0.7
  ) +
  geom_boxplot(
    width = 0.62,
    outlier.shape = NA,
    alpha = 0.55,
    linewidth = 0.4,
    position = position_dodge2(width = 0.72, preserve = "single")
  ) +
  scale_x_log10(
    breaks = c(0.5, 0.75, 1, 1.5, 2),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  coord_cartesian(xlim = FOLD_LIMITS) +
  scale_fill_manual(values = sample_size_colors) +
  scale_color_manual(values = sample_size_colors) +
  facet_grid(Endpoint ~ Design) +
  labs(
    x = "Standard fold error (study arithmetic mean / rich reference mean)",
    y = NULL,
    fill = "Animals per dose",
    color = "Animals per dose",
    title = "Reduced-study fold errors by PK endpoint and dose design",
    subtitle = "Dashed lines mark the prespecified 0.5- to 2-fold interval"
  ) +
  study_theme

ggsave(
  file.path(
    figure_dir,
    "Gate2_design_standard_fold_error_by_endpoint.png"
  ),
  standard_fold_error_plot,
  width = 13.5,
  height = 9,
  dpi = 350
)

power_plot <- ggplot(
  design_endpoint_success,
  aes(
    x = N_per_group,
    y = Fraction_within_2fold,
    color = Endpoint,
    linetype = Design,
    group = interaction(Endpoint, Design)
  )
) +
  geom_hline(
    yintercept = 0.8,
    color = "grey45",
    linetype = 3,
    linewidth = 0.6
  ) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.5) +
  facet_wrap(vars(Drug), ncol = 2) +
  scale_x_continuous(breaks = N_PER_GROUP) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    labels = scales::label_percent(accuracy = 1)
  ) +
  scale_color_viridis_d(option = "D", direction = -1, end = 0.85) +
  labs(
    x = "Sample size per dose group",
    y = "Fraction of simulated studies within 2-fold",
    color = "PK endpoint",
    linetype = "Dose design",
    title = "Power-style operating characteristics of reduced sparse studies",
    subtitle = paste0(
      "A study is within 2-fold only when every selected dose group meets ",
      "the endpoint criterion"
    )
  ) +
  study_theme

ggsave(
  file.path(figure_dir, "Gate2_design_power_by_endpoint.png"),
  power_plot,
  width = 11,
  height = 8,
  dpi = 350
)

message("Analysis complete. Outputs written to ", output_root)
