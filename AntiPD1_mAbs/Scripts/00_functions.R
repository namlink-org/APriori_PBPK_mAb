# =============================================================================
# ModelParameters.xlsx Builder — mAb PBPK (esqlabsR)
# =============================================================================
#
# Functions for generating the ModelParameters.xlsx configuration file used by
# esqlabsR bulk simulations. Each workbook sheet corresponds to one
# Molecule_Species combination and contains PK-Sim parameter paths + values
# that override the defaults in the loaded .pkml simulation file.
#
# Required columns in every sheet:
#   Container Path | Parameter Name | Value | Units
#
# DEPENDENCIES: tidyverse, openxlsx, ospsuite, esqlabsR
# =============================================================================


# -----------------------------------------------------------------------------
# Normalize species labels to title-case while preserving known acronyms
# (e.g. "human" → "Human", "NHP" → "Monkey", "nhp" → "Monkey")
# -----------------------------------------------------------------------------
normalize_species <- function(x) {
  dplyr::case_when(
    tolower(x) == "human"          ~ "Human",
    tolower(x) %in% c("nhp", "monkey", "primate") ~ "Monkey",
    tolower(x) == "rat"            ~ "Rat",
    tolower(x) == "mouse"          ~ "Mouse",
    tolower(x) == "dog"            ~ "Dog",
    TRUE                           ~ stringr::str_to_title(x)
  )
}


# -----------------------------------------------------------------------------
# Build parameter rows for ONE mAb from a loaded simulation
#
# The function extracts the correct PK-Sim container paths from `sim` so that
# parameter assignments always match the actual model structure, regardless of
# the number of organ endosome compartments present.
#
# @param sim        Loaded PK-Sim simulation (ospsuite::loadSimulation)
# @param mw         Molecular weight (g/mol)
# @param kd_fcrn    FcRn binding affinity in endosomal space (µmol/l)
# @param kd_target  Target (e.g. PD-1) binding affinity Kd (nmol/l)
# @param koff       Target dissociation rate constant (1/s)
# @return tibble: Container Path | Parameter Name | Value | Units
# -----------------------------------------------------------------------------
build_mab_params <- function(sim, mw, kd_fcrn, kd_target, koff) {

  # Helper: strip the last "|ParameterName" segment to get the container path
  container_of <- function(param) sub("\\|[^|]*$", "", param$path)

  # Organ-level endosomal FcRn paths (one per organ compartment in the model)
  fcrn_organ <- ospsuite::getAllParametersMatching(
    "Organism|**|Endosome|mAb|Kd (FcRn) in endosomal space of container", sim
  )

  dplyr::bind_rows(

    # Molecular weight
    tibble::tibble(
      `Container Path` = container_of(ospsuite::getParameter("mAb|Molecular weight", sim)),
      `Parameter Name` = ospsuite::getParameter("mAb|Molecular weight", sim)$name,
      Value            = mw,
      Units            = "g/mol"
    ),

    # Top-level FcRn affinity
    tibble::tibble(
      `Container Path` = container_of(ospsuite::getParameter("mAb|Kd (FcRn) in endosomal space", sim)),
      `Parameter Name` = ospsuite::getParameter("mAb|Kd (FcRn) in endosomal space", sim)$name,
      Value            = kd_fcrn,
      Units            = "µmol/l"
    ),

    # Per-organ endosomal FcRn affinities
    purrr::map_dfr(fcrn_organ, ~ tibble::tibble(
      `Container Path` = container_of(.x),
      `Parameter Name` = .x$name,
      Value            = kd_fcrn,
      Units            = "µmol/l"
    )),

    # Target binding affinity (Kd)
    tibble::tibble(
      `Container Path` = container_of(ospsuite::getParameter("mAb-PD-1-mAb_PD-1_Complex|Kd", sim)),
      `Parameter Name` = ospsuite::getParameter("mAb-PD-1-mAb_PD-1_Complex|Kd", sim)$name,
      Value            = kd_target,
      Units            = "nmol/l"
    ),

    # Target dissociation rate constant (koff)
    tibble::tibble(
      `Container Path` = container_of(ospsuite::getParameter("mAb-PD-1-mAb_PD-1_Complex|koff", sim)),
      `Parameter Name` = ospsuite::getParameter("mAb-PD-1-mAb_PD-1_Complex|koff", sim)$name,
      Value            = koff,
      Units            = "1/s"
    )
  )
}


# -----------------------------------------------------------------------------
# Write ModelParameters.xlsx
#
# Creates one sheet per Molecule × Species combination plus a Global sheet.
# The Global sheet is left empty (no project-wide parameter overrides).
#
# @param mol_input  data.frame with columns:
#                     Molecule   – molecule name (matches Scenarios.xlsx)
#                     mw         – molecular weight (g/mol)
#                     kd_fcrn    – FcRn Kd (µmol/l)
#                     kd_target  – target Kd (nmol/l)
#                     koff       – target koff (1/s)
# @param sim        Loaded PK-Sim simulation (used to look up container paths)
# @param species    Character vector of species labels, already normalised
#                   (e.g. c("Human", "Monkey"))
# @param out_path   Output file path for the .xlsx workbook
# -----------------------------------------------------------------------------
write_model_params_xlsx <- function(mol_input, sim, species, out_path) {

  required_cols <- c("Molecule", "mw", "kd_fcrn", "kd_target", "koff")
  missing <- setdiff(required_cols, names(mol_input))
  if (length(missing) > 0) {
    stop("mol_input is missing required columns: ", paste(missing, collapse = ", "))
  }

  wb <- openxlsx::createWorkbook()

  # ── Global sheet (empty — no project-wide overrides needed) ──────────────
  openxlsx::addWorksheet(wb, "Global")
  openxlsx::writeData(
    wb, "Global",
    data.frame(`Container Path` = NA, `Parameter Name` = NA,
               Value = NA, Units = NA, check.names = FALSE),
    startRow = 1
  )

  # ── One sheet per molecule × species ─────────────────────────────────────
  for (i in seq_len(nrow(mol_input))) {
    row    <- mol_input[i, ]
    params <- build_mab_params(
      sim       = sim,
      mw        = row$mw,
      kd_fcrn   = row$kd_fcrn,
      kd_target = row$kd_target,
      koff      = row$koff
    )

    for (sp in species) {
      sheet_name <- paste0(row$Molecule, "_", sp)
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, params)

      # Widen columns for readability
      openxlsx::setColWidths(wb, sheet_name,
                             cols   = seq_len(ncol(params)),
                             widths = c(60, 55, 12, 10))
    }
  }

  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  message("Saved ModelParameters.xlsx → ", normalizePath(out_path, mustWork = FALSE))
  invisible(wb)
}


# =============================================================================
# Applications.xlsx Builder
# =============================================================================
#
# Applications.xlsx defines the dosing protocol for each simulation scenario.
# esqlabsR expects one sheet per scenario, named to match the ScenarioName in
# Scenarios.xlsx. Each sheet contains a single row that maps directly to the
# PK-Sim application event in the loaded .pkml model.
#
# Required columns per sheet:
#   Container Path | Parameter Name | Value | Units | Infusion time | Infusion time unit
# =============================================================================


# -----------------------------------------------------------------------------
# Parse infusion duration in minutes from a free-text route string.
#
# Handles patterns commonly found in clinical/NHP (non-human primate) study data:
#   "IV 1hr Infusion"  → 60 min
#   "IV 0.5hr Infusion"→ 30 min
#   "IV 30min Infusion"→ 30 min
#   "IV Bolus"         → bolus_min (default 1 min — short infusion approximation)
#
# @param route_str  Character; route label from the data (e.g. dat$Route).
# @param bolus_min  Numeric; infusion time to assign when route is a bolus.
# @return Numeric infusion duration in minutes.
# -----------------------------------------------------------------------------
parse_infusion_min <- function(route_str, bolus_min = 1) {
  r <- tolower(trimws(route_str))

  # Explicit bolus label → short infusion approximation
  if (grepl("bolus", r)) return(bolus_min)

  # "Xhr" or "X hr" pattern
  hr_m <- regmatches(r, regexpr("[0-9]+\\.?[0-9]*\\s*hr", r))
  if (length(hr_m) > 0 && nchar(hr_m) > 0) {
    hrs <- as.numeric(gsub("[^0-9.]", "", hr_m))
    return(round(hrs * 60))
  }

  # "Xmin" or "X min" pattern
  min_m <- regmatches(r, regexpr("[0-9]+\\.?[0-9]*\\s*min", r))
  if (length(min_m) > 0 && nchar(min_m) > 0) {
    return(round(as.numeric(gsub("[^0-9.]", "", min_m))))
  }

  # Could not parse — warn and fall back to bolus approximation
  warning("Could not parse infusion time from route '", route_str,
          "'. Defaulting to ", bolus_min, " min.")
  bolus_min
}


# -----------------------------------------------------------------------------
# Build the single-row parameter table for one dosing scenario sheet.
#
# @param dose           Numeric dose value (mg/kg).
# @param route_str      Route label used to derive infusion duration.
# @param container_path PK-Sim container path for the application event.
# @param bolus_min      Fallback infusion time (min) for bolus routes.
# @return data.frame with one row and six columns.
# -----------------------------------------------------------------------------
build_application_rows <- function(dose,
                                   route_str,
                                   container_path,
                                   bolus_min = 1) {
  data.frame(
    `Container Path`     = container_path,
    `Parameter Name`     = "DosePerBodyWeight",
    Value                = dose,
    Units                = "mg/kg",
    `Infusion time`      = parse_infusion_min(route_str, bolus_min),
    `Infusion time unit` = "min",
    check.names          = FALSE,
    stringsAsFactors     = FALSE
  )
}


# -----------------------------------------------------------------------------
# Write Applications.xlsx — one sheet per unique Molecule × Species × Dose.
#
# Sheet names follow the convention used in Scenarios.xlsx:
#   {Molecule}_{Species}_{Dose}_mpk   (truncated to 31 characters for Excel)
#
# @param dat            Observed-data data.frame; must contain columns
#                       Molecule, Species, Dose, Route.
# @param out_path       Output .xlsx file path.
# @param container_path PK-Sim container path string for the IV application
#                       event (must match the event name in the .pkml model).
# @param bolus_min      Infusion duration (min) assigned to bolus routes.
# -----------------------------------------------------------------------------
write_applications_xlsx <- function(dat,
                                    out_path,
                                    container_path = "Events|Single_IV_Infusion|Application_1|ProtocolSchemaItem",
                                    bolus_min      = 1) {

  # One row per unique dosing scenario
  scenarios <- dat %>%
    dplyr::distinct(Molecule, Species, Dose, Route) %>%
    dplyr::mutate(
      Species_label = normalize_species(Species),
      SheetName     = stringr::str_trunc(
        paste(Molecule, Species_label, Dose, "mpk", sep = "_"), 31
      )
    ) %>%
    dplyr::arrange(Molecule, Species_label, Dose)

  wb <- openxlsx::createWorkbook()

  for (i in seq_len(nrow(scenarios))) {
    s        <- scenarios[i, ]
    sheet_df <- build_application_rows(
      dose           = s$Dose,
      route_str      = s$Route,
      container_path = container_path,
      bolus_min      = bolus_min
    )
    openxlsx::addWorksheet(wb, s$SheetName)
    openxlsx::writeData(wb, s$SheetName, sheet_df)
    openxlsx::setColWidths(wb, s$SheetName,
                           cols   = seq_len(ncol(sheet_df)),
                           widths = c(55, 22, 8, 8, 14, 18))
  }

  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  message(
    "Saved Applications.xlsx → ", normalizePath(out_path, mustWork = FALSE), "\n",
    "  Total sheets : ", nrow(scenarios), " scenarios\n",
    "  Molecules    : ", paste(sort(unique(scenarios$Molecule)),       collapse = ", "), "\n",
    "  Species      : ", paste(sort(unique(scenarios$Species_label)), collapse = ", "), "\n",
    "  Doses (mg/kg): ", paste(sort(unique(scenarios$Dose)),           collapse = ", ")
  )
  invisible(wb)
}


# =============================================================================
# Scenarios.xlsx Builder
# =============================================================================
#
# Scenarios.xlsx is the master run-list read by esqlabsR. It has two sheets:
#
#   Scenarios   — one row per simulation (molecule × species × dose)
#   OutputPaths — maps short IDs to full PK-Sim output path strings
#
# Column reference for the Scenarios sheet (all 13 columns):
#   Scenario_name        : unique key; must match the sheet name in Applications.xlsx
#   IndividualId         : NA (population simulations used here)
#   PopulationId         : ID defined in Populations.xlsx (e.g. "Human", "Monkey")
#   ReadPopulationFromCSV: NA (populations defined via Populations.xlsx)
#   ModelParameterSheets : quoted, comma-separated sheet names from ModelParameters.xlsx
#   ApplicationProtocol  : sheet name in Applications.xlsx (= Scenario_name)
#   SimulationTime       : "start, end, resolution;" all in SimulationTimeUnit
#   SimulationTimeUnit   : time unit for SimulationTime (default "h")
#   SteadyState          : NA (no steady-state pre-simulation needed for IV mAb)
#   SteadyStateTime      : NA
#   SteadyStateTimeUnit  : NA
#   ModelFile            : .pkml filename (basename only; must sit in Models/ folder)
#   OutputPathsIds       : comma-separated IDs from the OutputPaths sheet
# =============================================================================


# -----------------------------------------------------------------------------
# Write Scenarios.xlsx
#
# Simulation end times are derived automatically from the maximum observed time
# point per species, rounded up to the next full day and extended by a buffer
# so the simulated curve covers and slightly exceeds all observed data.
#
# @param dat              Observed-data data.frame; must contain columns
#                         Molecule, Species, Dose, Time, `Time unit`.
# @param model_file       Basename of the .pkml simulation file (e.g.
#                         "PD-1_mAb_wTMDD.pkml").
# @param out_path         Output .xlsx file path.
# @param population_map   Named character vector mapping normalised species
#                         labels to PopulationIds defined in Populations.xlsx
#                         (e.g. Human = "WhiteAmerican_NHANES_1997",
#                         Monkey = "Monkey").
# @param model_mol_name   Molecule name as used inside the .pkml model (e.g.
#                         "mAb"). Must match the entity name visible in the
#                         PK-Sim / Mobi model tree. If NULL, the first data
#                         molecule name is used (only correct when they match).
# @param sim_end_h        Simulation end time in hours (default 672 = 28 days).
# @param time_res         Output time step in hours (default 4 h = one output
#                         point every 4 hours). esqlabsR SimulationTime format
#                         is "start, end, resolution;" all in SimulationTimeUnit,
#                         e.g. "0, 672, 4;" → run 0–672 h, output every 4 h.
# -----------------------------------------------------------------------------
write_scenarios_xlsx <- function(dat,
                                  model_file,
                                  out_path,
                                  population_map   = c(Human  = "WhiteAmerican_NHANES_1997",
                                                       Monkey = "Monkey"),
                                  model_mol_name   = NULL,
                                  sim_end_h        = 28 * 24,   # 672 h = 28 days
                                  time_res         = 4) {        # output step size (h)

  # ── Normalise species and build scenario skeleton ─────────────────────────
  scenarios <- dat %>%
    dplyr::distinct(Molecule, Species, Dose) %>%
    dplyr::mutate(
      Species_label = normalize_species(Species),
      Scenario_name = stringr::str_trunc(
        paste(Molecule, Species_label, Dose, "mpk", sep = "_"), 31
      )
    ) %>%
    dplyr::arrange(Molecule, Species_label, Dose)

  # ── Assemble the full Scenarios table ─────────────────────────────────────
  scenarios_sheet <- scenarios %>%
    dplyr::transmute(
      Scenario_name,
      IndividualId          = NA,
      PopulationId          = population_map[Species_label],
      ReadPopulationFromCSV = NA,
      ModelParameterSheets  = paste0('"Global", "', Molecule, "_", Species_label, '"'),
      ApplicationProtocol   = Scenario_name,
      SimulationTime        = paste0("0, ", sim_end_h, ", ", time_res, ";"),
      SimulationTimeUnit    = "h",
      SteadyState           = NA,
      SteadyStateTime       = NA,
      SteadyStateTimeUnit   = NA,
      ModelFile             = model_file,
      OutputPathsIds        = "PVB"   # single shared ID — all scenarios point to the same output path
    )

  # ── Build OutputPaths sheet ───────────────────────────────────────────────
  # A single "PVB" entry covers all molecules because they all map to the same
  # generic model entity (e.g. "mAb"). The OutputPath uses the model's internal
  # molecule name supplied via the model_mol_name argument.
  pvb_mol <- if (!is.null(model_mol_name)) model_mol_name else dat$Molecule[1]
  output_paths <- data.frame(
    OutputPathId = "PVB",
    OutputPath   = paste0(
      "Organism|PeripheralVenousBlood|", pvb_mol,
      "|Plasma (Peripheral Venous Blood)"
    ),
    stringsAsFactors = FALSE
  )

  # ── Write workbook ────────────────────────────────────────────────────────
  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb, "Scenarios")
  openxlsx::writeData(wb, "Scenarios", scenarios_sheet)
  openxlsx::setColWidths(wb, "Scenarios",
                         cols   = seq_len(ncol(scenarios_sheet)),
                         widths = c(32, 12, 14, 20, 28, 32, 18, 18,
                                    12, 14, 18, 24, 18))

  openxlsx::addWorksheet(wb, "OutputPaths")
  openxlsx::writeData(wb, "OutputPaths", output_paths)
  openxlsx::setColWidths(wb, "OutputPaths", cols = 1:2, widths = c(22, 65))

  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)

  message(
    "Saved Scenarios.xlsx → ", normalizePath(out_path, mustWork = FALSE), "\n",
    "  Total scenarios : ", nrow(scenarios_sheet), "\n",
    "  Molecules       : ", paste(sort(unique(scenarios$Molecule)),       collapse = ", "), "\n",
    "  Species         : ", paste(sort(unique(scenarios$Species_label)), collapse = ", "), "\n",
    "  OutputPath      : PVB → ", output_paths$OutputPath
  )
  invisible(wb)
}
