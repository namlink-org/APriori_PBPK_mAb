# Calculate the percentage of AUCinf extrapolated for the a priori PBPK
# observed and predicted concentration-time profiles.
#
# Output:
#   03_APriori/SimsOutputs/AUCinf_Percent_Extrapolated.csv

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

AUC_END_H <- 28 * 24
EXTRAPOLATION_THRESHOLD_PCT <- 20
SPECIES_BW_KG <- c(Monkey = 3.5, Human = 70)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) {
  stop("Run this file with Rscript so its project-relative paths can be resolved.")
}
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg)))
project_root <- normalizePath(file.path(script_dir, "..", ".."))
data_dir <- file.path(project_root, "01_Data")
results_root <- file.path(project_root, "03_APriori", "SimsOutputs", "SimulationResults")
output_file <- file.path(
  project_root, "03_APriori", "SimsOutputs", "AUCinf_Percent_Extrapolated.csv"
)

normalize_species <- function(x) {
  case_when(
    str_to_lower(x) == "human" ~ "Human",
    str_to_lower(x) %in% c("nhp", "monkey", "primate") ~ "Monkey",
    TRUE ~ str_to_title(x)
  )
}

unit_to_hour <- function(time, unit) {
  unit_l <- str_to_lower(str_trim(unit))
  case_when(
    unit_l %in% c("h", "hr", "hrs", "hour", "hours") ~ as.numeric(time),
    unit_l %in% c("day", "days", "d") ~ as.numeric(time) * 24,
    unit_l %in% c("min", "mins", "minute", "minutes") ~ as.numeric(time) / 60,
    TRUE ~ as.numeric(time)
  )
}

parse_scenario <- function(path) {
  base <- tools::file_path_sans_ext(basename(path))
  base <- sub("_mpk$", "", base)
  parts <- strsplit(base, "_", fixed = TRUE)[[1]]
  n <- length(parts)
  if (n < 3L) stop("Could not parse scenario file name: ", basename(path))

  list(
    Molecule = paste(parts[seq_len(n - 2L)], collapse = "_"),
    Species = normalize_species(parts[n - 1L]),
    Dose = suppressWarnings(as.numeric(parts[n]))
  )
}

best_terminal_fit <- function(time_h, conc, min_points = 3L) {
  terminal_data <- tibble(
    time_h = as.numeric(time_h),
    conc = as.numeric(conc)
  ) %>%
    filter(
      is.finite(time_h), is.finite(conc),
      time_h >= 0, time_h <= AUC_END_H, conc > 0
    ) %>%
    group_by(time_h) %>%
    summarise(conc = mean(conc), .groups = "drop") %>%
    arrange(time_h)

  if (nrow(terminal_data) < min_points) {
    return(tibble(lambda_z_1_h = NA_real_, terminal_points_n = NA_integer_))
  }

  tmax_index <- which.max(terminal_data$conc)[1]
  post_cmax <- if (tmax_index < nrow(terminal_data)) {
    terminal_data[(tmax_index + 1L):nrow(terminal_data), , drop = FALSE]
  } else {
    terminal_data[0, , drop = FALSE]
  }
  if (nrow(post_cmax) < min_points) {
    post_cmax <- terminal_data[tmax_index:nrow(terminal_data), , drop = FALSE]
  }
  if (nrow(post_cmax) < min_points) {
    return(tibble(lambda_z_1_h = NA_real_, terminal_points_n = NA_integer_))
  }

  x <- post_cmax$time_h
  y <- log(post_cmax$conc)
  total_n <- length(x)
  suffix_sum <- function(z) rev(cumsum(rev(z)))
  sx <- suffix_sum(x)
  sy <- suffix_sum(y)
  sxx <- suffix_sum(x * x)
  syy <- suffix_sum(y * y)
  sxy <- suffix_sum(x * y)
  terminal_n <- seq.int(min_points, total_n)
  start_index <- total_n - terminal_n + 1L
  s_xx <- sxx[start_index] - sx[start_index]^2 / terminal_n
  s_yy <- syy[start_index] - sy[start_index]^2 / terminal_n
  s_xy <- sxy[start_index] - sx[start_index] * sy[start_index] / terminal_n
  slope <- s_xy / s_xx
  r2 <- pmin(pmax((s_xy^2) / (s_xx * s_yy), 0), 1)
  adj_r2 <- 1 - (1 - r2) * (terminal_n - 1) / (terminal_n - 2)

  candidates <- tibble(
    lambda_z_1_h = -slope,
    adjusted_r_squared = adj_r2,
    terminal_points_n = terminal_n
  ) %>%
    filter(
      is.finite(lambda_z_1_h), lambda_z_1_h > 0,
      is.finite(adjusted_r_squared)
    ) %>%
    arrange(desc(adjusted_r_squared), desc(terminal_points_n))

  if (nrow(candidates) == 0L) {
    tibble(lambda_z_1_h = NA_real_, terminal_points_n = NA_integer_)
  } else {
    candidates %>% select(lambda_z_1_h, terminal_points_n) %>% slice(1)
  }
}

profile_auc_extrapolation <- function(time_h, conc) {
  profile <- tibble(
    time_h = as.numeric(time_h),
    conc = as.numeric(conc)
  ) %>%
    filter(
      is.finite(time_h), is.finite(conc),
      time_h >= 0, time_h <= AUC_END_H, conc >= 0
    ) %>%
    group_by(time_h) %>%
    summarise(conc = mean(conc), .groups = "drop") %>%
    arrange(time_h)

  terminal_fit <- best_terminal_fit(time_h, conc)
  lambda_z <- terminal_fit$lambda_z_1_h

  if (
    nrow(profile) == 0L || !any(profile$conc > 0) ||
    length(lambda_z) != 1L || !is.finite(lambda_z)
  ) {
    return(tibble(
      AUClast_ug_h_mL = NA_real_,
      AUCinf_ug_h_mL = NA_real_,
      AUC_extrapolated_pct = NA_real_,
      lambda_z_1_h = NA_real_,
      terminal_half_life_day = NA_real_,
      last_time_h = NA_real_,
      terminal_points_n = NA_integer_
    ))
  }

  if (!any(profile$time_h == 0)) {
    profile <- bind_rows(tibble(time_h = 0, conc = 0), profile) %>%
      arrange(time_h)
  }

  positive <- profile %>% filter(conc > 0)
  last_row <- positive %>% slice_max(time_h, n = 1, with_ties = FALSE)
  through_last <- profile %>% filter(time_h <= last_row$time_h)
  dt <- diff(through_last$time_h)
  c1 <- through_last$conc[-nrow(through_last)]
  c2 <- through_last$conc[-1]
  declining <- c2 < c1 & c1 > 0 & c2 > 0
  auc_last <- sum(ifelse(
    declining,
    dt * (c1 - c2) / log(c1 / c2),
    dt * (c1 + c2) / 2
  ))
  auc_extrapolated <- last_row$conc / lambda_z
  auc_inf <- auc_last + auc_extrapolated

  tibble(
    AUClast_ug_h_mL = auc_last,
    AUCinf_ug_h_mL = auc_inf,
    AUC_extrapolated_pct = 100 * auc_extrapolated / auc_inf,
    lambda_z_1_h = lambda_z,
    terminal_half_life_day = log(2) / lambda_z / 24,
    last_time_h = last_row$time_h,
    terminal_points_n = terminal_fit$terminal_points_n
  )
}

calculate_profiles <- function(df, group_columns) {
  df %>%
    group_by(across(all_of(group_columns))) %>%
    group_modify(~ profile_auc_extrapolation(.x$Time_h, .x$Conc_ugmL)) %>%
    ungroup() %>%
    mutate(
      BW_kg = unname(SPECIES_BW_KG[Species]),
      Clearance_dose_AUCinf_L_day = 24 * Dose * BW_kg / AUCinf_ug_h_mL
    )
}

summarise_profiles <- function(df, prefix) {
  prefix_title <- str_to_title(prefix)
  df %>%
    group_by(Species, Molecule, Dose) %>%
    summarise(
      profiles_n = sum(is.finite(AUC_extrapolated_pct)),
      AUClast_mean_ug_h_mL = mean(AUClast_ug_h_mL, na.rm = TRUE),
      AUCinf_mean_ug_h_mL = mean(AUCinf_ug_h_mL, na.rm = TRUE),
      AUC_extrapolated_mean_pct = mean(AUC_extrapolated_pct, na.rm = TRUE),
      AUC_extrapolated_min_pct = min(AUC_extrapolated_pct, na.rm = TRUE),
      AUC_extrapolated_max_pct = max(AUC_extrapolated_pct, na.rm = TRUE),
      terminal_half_life_mean_day = mean(terminal_half_life_day, na.rm = TRUE),
      Clearance_dose_AUCinf_mean_L_day =
        mean(Clearance_dose_AUCinf_L_day, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(
      where(is.numeric),
      ~ ifelse(is.nan(.) | is.infinite(.), NA_real_, .)
    )) %>%
    mutate(
      AUC_extrapolated_over_20pct_flag = case_when(
        !is.finite(AUC_extrapolated_mean_pct) ~ "Not estimable",
        AUC_extrapolated_mean_pct > EXTRAPOLATION_THRESHOLD_PCT ~ ">20%",
        TRUE ~ "<=20%"
      )
    ) %>%
    rename_with(~ paste(prefix_title, ., sep = " "), -c(Species, Molecule, Dose))
}

read_observed <- function(path, species) {
  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  column_ending <- function(ending) {
    hits <- names(raw)[endsWith(names(raw), ending)]
    if (length(hits) != 1L) {
      stop("Expected one column ending in '", ending, "' in ", path)
    }
    hits
  }
  raw$Study_ID_for_profile <- as.character(raw[[column_ending("Study.Id")]])
  raw$Subject_ID_for_profile <- as.character(raw[[column_ending("Subject.Id")]])
  raw$Group_ID_for_profile <- as.character(raw[[column_ending("Group.Id")]])
  raw %>%
    mutate(
      Species = normalize_species(.data[["Species"]]),
      Time_h = unit_to_hour(.data[["Time"]], .data[["Time.unit"]]),
      Conc_ugmL = as.numeric(.data[["Measurement"]]),
      Subject_key = coalesce(
        na_if(Subject_ID_for_profile, "."),
        na_if(Group_ID_for_profile, "."),
        Study_ID_for_profile
      ),
      Profile_ID = paste(Study_ID_for_profile, Subject_key, sep = " | ")
    ) %>%
    filter(
      Species == species,
      is.finite(Time_h), Time_h <= AUC_END_H,
      is.finite(Conc_ugmL), Conc_ugmL > 0
    ) %>%
    {
      if (species == "Human") {
        filter(
          .,
          !grepl("^SC$", trimws(.data[["Route"]]), ignore.case = TRUE)
        )
      } else {
        .
      }
    } %>%
    transmute(
      Species, Molecule, Dose = as.numeric(Dose),
      Profile_ID, Time_h, Conc_ugmL
    )
}

observed <- bind_rows(
  read_observed(file.path(data_dir, "Monkey", "mAb_data_clean.csv"), "Monkey"),
  read_observed(file.path(data_dir, "Human", "mAb_data_human.csv"), "Human")
)

read_mw_lookup <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE) %>%
    transmute(Molecule, MW = as.numeric(Molecular.Weight))
}

mw_lookup <- bind_rows(
  read_mw_lookup(file.path(data_dir, "Monkey", "mAb_data_clean.csv")),
  read_mw_lookup(file.path(data_dir, "Human", "mAb_data_human.csv"))
) %>%
  filter(is.finite(MW)) %>%
  distinct(Molecule, .keep_all = TRUE)

simulation_folders <- list.dirs(results_root, recursive = FALSE, full.names = TRUE)
if (length(simulation_folders) == 0L) {
  stop("No a priori simulation result folders found in ", results_root)
}
latest_results <- simulation_folders[which.max(file.mtime(simulation_folders))]
scenario_files <- list.files(latest_results, pattern = "\\.csv$", full.names = TRUE)
scenario_files <- scenario_files[!grepl("_population\\.csv$", scenario_files)]

read_simulation <- function(path) {
  meta <- parse_scenario(path)
  raw <- data.table::fread(path, data.table = FALSE, check.names = FALSE)
  conc_columns <- grep(
    "PeripheralVenousBlood.*Plasma.*\\[", names(raw), value = TRUE
  )
  conc_columns <- conc_columns[grepl("\\|mAb\\|", conc_columns)]
  if (length(conc_columns) == 0L) {
    stop("No plasma mAb concentration column found in ", basename(path))
  }
  mw <- mw_lookup$MW[mw_lookup$Molecule == meta$Molecule]
  if (length(mw) != 1L || !is.finite(mw)) {
    stop("No unique molecular weight found for ", meta$Molecule)
  }

  tibble(
    Species = meta$Species,
    Molecule = meta$Molecule,
    Dose = meta$Dose,
    Individual_ID = as.character(raw$IndividualId),
    Time_h = as.numeric(raw[["Time [min]"]]) / 60,
    Conc_ugmL = as.numeric(raw[[conc_columns[1]]]) * mw / 1000
  )
}

predicted <- map_dfr(scenario_files, read_simulation)
scenario_keys <- predicted %>% distinct(Species, Molecule, Dose)
observed <- observed %>% semi_join(scenario_keys, by = c("Species", "Molecule", "Dose"))

observed_profiles <- calculate_profiles(
  observed,
  c("Species", "Molecule", "Dose", "Profile_ID")
)
predicted_profiles <- calculate_profiles(
  predicted,
  c("Species", "Molecule", "Dose", "Individual_ID")
)

observed_summary <- summarise_profiles(observed_profiles, "Observed")
predicted_summary <- summarise_profiles(predicted_profiles, "Predicted")

output <- full_join(
  observed_summary,
  predicted_summary,
  by = c("Species", "Molecule", "Dose")
) %>%
  mutate(
    Approach = "A priori PBPK",
    `Generic BW (kg)` = unname(SPECIES_BW_KG[Species])
  ) %>%
  relocate(Approach, Species, Molecule, Dose, `Generic BW (kg)`) %>%
  arrange(factor(Species, levels = c("Monkey", "Human")), Molecule, Dose) %>%
  rename(`Dose (mg/kg)` = Dose) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

if (nrow(output) != 44L) {
  warning("Expected 44 molecule-dose scenarios, but produced ", nrow(output), ".")
}

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write.csv(output, output_file, row.names = FALSE, na = "")
message("Wrote ", nrow(output), " rows to ", output_file)
