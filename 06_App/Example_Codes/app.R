# ============================================================
# Preclinical PK Shiny App  |  Enveda Biosciences
# Single & Multi-Dose PBPK Simulation Framework
# ============================================================

library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(rhandsontable)
library(shinycssloaders)
library(readxl)
library(openxlsx)
library(writexl)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(esqlabsR)
library(ospsuite)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ============================================================
# Paths  —  all local to App/
# ============================================================
APP_DIR    <- normalizePath(getwd())                              # .../App/
PROJ_ROOT  <- normalizePath(file.path(APP_DIR, ".."))            # .../Preclinical_Study_Simulator/
CONF_DIR   <- file.path(APP_DIR, "Configurations")               # App/Configurations/
POP_DIR    <- file.path(CONF_DIR, "PopulationsCSV")              # App/Configurations/PopulationsCSV/
RESULTS_DIR <- file.path(APP_DIR, "SimOutputs", "Results")   # App/SimOutputs/Results/
CONFIG_SD   <- file.path(APP_DIR, "ProjectConfiguration_SD.xlsx")
CONFIG_MD   <- file.path(APP_DIR, "ProjectConfiguration_MD.xlsx")

MODEL_DIR_SD <- file.path(PROJ_ROOT, "00_Models", "SingleDose")
MODEL_DIR_MD <- file.path(PROJ_ROOT, "00_Models", "MultipleDose")

# Max application slots in the MultipleDose PKML models (BID, 60 total)
MODEL_MAX_APPS_MD <- 60L

# ============================================================
# First-run bootstrap  —  copy shared data, create config files
# ============================================================

# 1. Ensure directories exist
dir.create(CONF_DIR,    recursive = TRUE, showWarnings = FALSE)
dir.create(POP_DIR,     recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

# 2. Copy Population CSVs from Workflow_tests (shared reference data)
if (length(list.files(POP_DIR, pattern = "\\.csv$")) == 0) {
  src_pop <- normalizePath(
    file.path(PROJ_ROOT, "Workflow_tests", "SingleDose", "Configurations", "PopulationsCSV"),
    mustWork = FALSE
  )
  if (dir.exists(src_pop)) {
    csvs <- list.files(src_pop, pattern = "\\.csv$", full.names = TRUE)
    file.copy(csvs, POP_DIR, overwrite = FALSE)
    message("[App] Population CSVs copied to App/Configurations/PopulationsCSV/")
  } else {
    warning("[App] Population CSV source not found: ", src_pop)
  }
}

# 3. Copy myfuns.R into App/ if absent
MYFUNS_APP <- file.path(APP_DIR, "myfuns.R")
if (!file.exists(MYFUNS_APP)) {
  src_myfuns <- normalizePath(
    file.path(PROJ_ROOT, "Workflow_tests", "SingleDose", "02_Scripts", "myfuns.R"),
    mustWork = FALSE
  )
  if (file.exists(src_myfuns)) {
    file.copy(src_myfuns, MYFUNS_APP)
    message("[App] myfuns.R copied to App/myfuns.R")
  }
}
source(MYFUNS_APP)

# 4. Create ProjectConfiguration xlsx files (SD and MD) if absent
make_project_config_xlsx <- function(path, model_rel_path) {
  if (file.exists(path)) return(invisible(NULL))
  rows <- data.frame(
    Property = c("modelFolder", "configurationsFolder", "modelParamsFile",
                 "individualsFile", "populationsFile", "populationsFolder",
                 "scenariosFile", "applicationsFile", "plotsFile",
                 "dataFolder", "dataFile", "dataImporterConfigurationFile",
                 "outputFolder"),
    Value = c(model_rel_path,
              "Configurations/",
              "ModelParameters.xlsx",
              "Individuals.xlsx",
              "Populations.xlsx",
              "PopulationsCSV",
              "Scenarios.xlsx",
              "Applications.xlsx",
              "Plots.xlsx",
              "Data/",
              "TimeValuesData.xlsx",
              "esqlabs_dataImporter_configuration.xml",
              "SimOutputs/Results/"),
    Description = c(
      "Path to pkml simulation files; relative to this file",
      "Path to configuration Excel files; relative to this file",
      "Model parameters file",
      "Individual biometrics file",
      "Population demographics file",
      "Population CSV subfolder",
      "Simulation scenarios file",
      "Application protocols file",
      "Plot definitions file",
      "Observed data folder",
      "Observed data file",
      "Data importer config",
      "Results output folder"
    ),
    stringsAsFactors = FALSE
  )
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "ProjectConfiguration")
  openxlsx::writeData(wb, "ProjectConfiguration", rows, startRow = 1)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  message("[App] Created ", basename(path))
}

make_project_config_xlsx(CONFIG_SD, "../00_Models/SingleDose/")
make_project_config_xlsx(CONFIG_MD, "../00_Models/MultipleDose/")

# ============================================================
# Constants
# ============================================================

ENV_BLACK  <- "#1A1A2E"
ENV_GRAY   <- "#6B7280"
ENV_LGRAY  <- "#F3F4F6"
ENV_GREEN  <- "#22C55E"
ENV_LGREEN <- "#86EFAC"
ENV_PURPLE <- "#8B5CF6"
ENV_LPUR   <- "#DDD6FE"
ENV_WHITE  <- "#FFFFFF"

SPECIES_META <- data.frame(
  Species      = c("Mouse",  "Rat",    "Dog",    "NHP",      "Human"),
  PopulationId = c("Mouse",  "Rat",    "Dog",    "Monkey",   "Human"),
  IndividualId = c("Indiv1", "Indiv2", "Indiv3", "Indiv4",   "Indiv5"),
  clh_suffix   = c("Mouse",  "Rat",    "Dog",    "Monkey",   "Human"),
  pop_csv      = c("Mouse-Population.csv", "Rat-Population.csv",  "Dog-Population.csv",
                   "Monkey-Population.csv", "Human-Population.csv"),
  pop_skip     = c(2L, 2L, 2L, 0L, 0L),
  stringsAsFactors = FALSE
)

CONTAINER_PATH    <- "GenericSmallMolecule"
PO_WATER_VOL      <- 0.0035
PO_WATER_VOL_UNIT <- "L/kg"

# Fixed event names hardcoded in the PKML models
FIXED_PROTOCOL_NAMES <- c("IV" = "1mpk_IV", "PO" = "10mpk_PO")

TISSUE_CATALOG <- list(
  "Plasma" = list(
    id           = "Plasma_Conc",
    path         = paste0("Organism|PeripheralVenousBlood|", CONTAINER_PATH,
                          "|Plasma (Peripheral Venous Blood)"),
    grep_pattern = "Plasma \\(Peripheral Venous Blood\\)"
  ),
  "Lung" = list(
    id           = "Lung_Tissue",
    path         = paste0("Organism|Lung|", CONTAINER_PATH, "|Tissue"),
    grep_pattern = paste0("Lung\\|", CONTAINER_PATH, "\\|Tissue")
  ),
  "Brain" = list(
    id           = "Brain_Tissue",
    path         = paste0("Organism|Brain|", CONTAINER_PATH, "|Tissue"),
    grep_pattern = paste0("Brain\\|", CONTAINER_PATH, "\\|Tissue")
  ),
  "Liver" = list(
    id           = "Liver_Tissue",
    path         = paste0("Organism|Liver|", CONTAINER_PATH, "|Tissue (Liver)"),
    grep_pattern = paste0("Liver\\|", CONTAINER_PATH, "\\|Tissue \\(Liver\\)")
  ),
  "Kidney" = list(
    id           = "Kidney_Tissue",
    path         = paste0("Organism|Kidney|", CONTAINER_PATH, "|Tissue"),
    grep_pattern = paste0("Kidney\\|", CONTAINER_PATH, "\\|Tissue")
  ),
  "Muscle" = list(
    id           = "Muscle_Tissue",
    path         = paste0("Organism|Muscle|", CONTAINER_PATH, "|Tissue"),
    grep_pattern = paste0("Muscle\\|", CONTAINER_PATH, "\\|Tissue")
  ),
  "Spleen" = list(
    id           = "Spleen_Tissue",
    path         = paste0("Organism|Spleen|", CONTAINER_PATH, "|Tissue"),
    grep_pattern = paste0("Spleen\\|", CONTAINER_PATH, "\\|Tissue")
  ),
  "Fat" = list(
    id           = "Fat_Tissue",
    path         = paste0("Organism|Fat|", CONTAINER_PATH, "|Tissue"),
    grep_pattern = paste0("Fat\\|", CONTAINER_PATH, "\\|Tissue")
  )
)

# ============================================================
# Helpers
# ============================================================

auc_trap <- function(t, c) {
  o <- order(t); t <- t[o]; c <- c[o]
  sum(diff(t) * (head(c, -1) + tail(c, -1)) / 2)
}

default_scenarios <- function() {
  data.frame(
    Species   = c("Mouse", "Mouse", "Rat", "Rat", "Dog", "Dog"),
    Route     = c("IV",    "PO",    "IV",  "PO",  "IV",  "PO"),
    Dose      = c(1,       10,      1,     10,    1,     10),
    DoseUnits = rep("mg/kg", 6),
    stringsAsFactors = FALSE
  )
}

# ============================================================
# Core simulation function  (SD and MD unified)
# ============================================================

run_pbpk_simulation <- function(
    inputs, scenarios, conf_dir, pop_dir, config_xlsx, results_dir,
    selected_tissues = "Plasma",
    dose_mode   = "SD",   # "SD" or "MD"
    ndose       = 30,
    schedule    = "BID",
    progress_cb = function(f, m) invisible()
) {

  MoleculeId     <- inputs$molecule_id
  active_species <- SPECIES_META |> dplyr::filter(Species %in% unique(scenarios$Species))

  CLH_LOOKUP <- c(
    Mouse = inputs$clh_mouse,
    Rat   = inputs$clh_rat,
    Dog   = inputs$clh_dog,
    Human = inputs$clh_human
  )

  # ── Dose-mode parameters ─────────────────────────────────────────────────────
  if (dose_mode == "MD") {
    schedule     <- toupper(trimws(schedule))
    interval_hr  <- if (schedule == "BID") 12 else 24
    n_bid_slots  <- if (schedule == "BID") ndose else 2L * ndose - 1L
    sim_end_hr   <- ndose * interval_hr
    sim_pts      <- as.integer(sim_end_hr * 4)
    sim_time      <- paste0("0, ", sim_end_hr, ", ", sim_pts, ";")
    sim_time_unit <- "h"
  } else {
    interval_hr  <- NULL
    n_bid_slots  <- NULL
    sim_time      <- inputs$sim_time
    sim_time_unit <- inputs$sim_time_unit
  }

  # ── Simulation plan ───────────────────────────────────────────────────────────
  progress_cb(0.05, "Building simulation plan…")
  sim_plan <- scenarios |>
    dplyr::mutate(
      Route         = toupper(Route),
      ModelProtocol = FIXED_PROTOCOL_NAMES[Route]
    )

  # ── ModelParameters.xlsx ─────────────────────────────────────────────────────
  progress_cb(0.10, "Writing ModelParameters.xlsx…")
  mp_rows <- data.frame(
    ContainerPath = rep(CONTAINER_PATH, 7),
    ParameterName = c(
      "Lipophilicity",
      "Fraction unbound (plasma, reference value)",
      "Molecular weight",
      "pKa value 0",
      "Solubility at reference pH",
      "Reference pH",
      "Specific intestinal permeability (transcellular)"
    ),
    Value = c(inputs$lipophilicity, inputs$fu, inputs$mw, inputs$pka,
              inputs$solubility, inputs$ref_ph, inputs$perm),
    Units = c("Log Units", "", "g/mol", "", "mg/L", "", "cm/min"),
    stringsAsFactors = FALSE
  )
  mp_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(mp_wb, "Global")
  openxlsx::writeData(mp_wb, "Global",
    as.data.frame(t(c("Container Path", "Parameter Name", "Value", "Units"))),
    startRow = 1, colNames = FALSE)
  openxlsx::addWorksheet(mp_wb, MoleculeId)
  openxlsx::writeData(mp_wb, MoleculeId,
    as.data.frame(t(c("Container Path", "Parameter Name", "Value", "Units"))),
    startRow = 1, colNames = FALSE)
  openxlsx::writeData(mp_wb, MoleculeId, mp_rows, startRow = 2, colNames = FALSE)
  openxlsx::saveWorkbook(mp_wb, file.path(conf_dir, "ModelParameters.xlsx"), overwrite = TRUE)

  # ── Individuals.xlsx ──────────────────────────────────────────────────────────
  progress_cb(0.18, "Writing Individuals.xlsx…")
  biometrics <- dplyr::bind_rows(lapply(seq_len(nrow(active_species)), function(i) {
    sm  <- active_species[i, ]
    pop <- read_pop(file.path(pop_dir, sm$pop_csv), skip_lines = sm$pop_skip) |> dplyr::slice(1)
    extract_biometrics(pop, sm$IndividualId, sm$PopulationId)
  }))
  indiv_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(indiv_wb, "IndividualBiometrics")
  openxlsx::writeData(indiv_wb, "IndividualBiometrics", biometrics, startRow = 1)
  for (i in seq_len(nrow(active_species))) {
    sp  <- active_species[i, ]
    clh <- CLH_LOOKUP[sp$Species]
    indiv_params <- if (!is.na(clh)) {
      data.frame(
        ContainerPath = paste0(CONTAINER_PATH, "-Total Hepatic Clearance-CLh_", sp$clh_suffix),
        ParameterName = "Plasma clearance",
        Value         = as.numeric(clh),
        Units         = "mL/min/kg",
        stringsAsFactors = FALSE)
    } else {
      data.frame(ContainerPath = character(), ParameterName = character(),
                 Value = numeric(), Units = character(), stringsAsFactors = FALSE)
    }
    openxlsx::addWorksheet(indiv_wb, sp$IndividualId)
    openxlsx::writeData(indiv_wb, sp$IndividualId,
      as.data.frame(t(c("Container Path", "Parameter Name", "Value", "Units"))),
      startRow = 1, colNames = FALSE)
    if (nrow(indiv_params) > 0)
      openxlsx::writeData(indiv_wb, sp$IndividualId, indiv_params, startRow = 2, colNames = FALSE)
  }
  openxlsx::saveWorkbook(indiv_wb, file.path(conf_dir, "Individuals.xlsx"), overwrite = TRUE)

  # ── Populations.xlsx ─────────────────────────────────────────────────────────
  progress_cb(0.27, "Writing Populations.xlsx…")
  pop_stats <- dplyr::bind_rows(lapply(seq_len(nrow(active_species)), function(i) {
    sm  <- active_species[i, ]
    pop <- read_pop(file.path(pop_dir, sm$pop_csv), skip_lines = sm$pop_skip)
    bw  <- pop[["Organism|Weight [kg]"]]
    data.frame(PopulationId = sm$PopulationId,
               pksim_pop_name = pop[["Population"]][1],
               n_individuals  = 1L,
               weightMin = min(bw, na.rm = TRUE),
               weightMax = max(bw, na.rm = TRUE),
               stringsAsFactors = FALSE)
  }))
  demo <- dplyr::left_join(
    data.frame(PopulationName = active_species$PopulationId,
               species = active_species$Species,
               proportionOfFemales = 50, weightUnit = "kg",
               heightMin = NA, heightMax = NA, heightUnit = NA,
               ageMin = NA, ageMax = NA,
               BMIMin = NA, BMIMax = NA, BMIUnit = NA,
               `Protein Ontogenies` = NA, check.names = FALSE, stringsAsFactors = FALSE),
    pop_stats, by = c("PopulationName" = "PopulationId")
  ) |>
    dplyr::rename(population = pksim_pop_name, numberOfIndividuals = n_individuals) |>
    dplyr::select(PopulationName, species, population, numberOfIndividuals,
                  proportionOfFemales, weightMin, weightMax, weightUnit,
                  heightMin, heightMax, heightUnit, ageMin, ageMax,
                  BMIMin, BMIMax, BMIUnit, `Protein Ontogenies`)
  uv <- data.frame(`Container Path` = character(), `Parameter Name` = character(),
                   Mean = character(), SD = character(), Distribution = character(),
                   check.names = FALSE, stringsAsFactors = FALSE)
  openxlsx::write.xlsx(list(Demographics = demo, UserDefinedVariability = uv),
                       file = file.path(conf_dir, "Populations.xlsx"), overwrite = TRUE)

  # ── Applications.xlsx ────────────────────────────────────────────────────────
  progress_cb(0.38, "Writing Applications.xlsx…")
  unique_protos <- sim_plan |>
    dplyr::group_by(ModelProtocol, Route, DoseUnits) |>
    dplyr::summarise(Dose = dplyr::first(Dose), .groups = "drop")

  if (dose_mode == "SD") {
    # Single dose: one Application_1 row per protocol (same as before)
    app_sheets <- lapply(seq_len(nrow(unique_protos)), function(i) {
      p <- unique_protos[i, ]
      if (p$Route == "IV") {
        data.frame(
          `Container Path` = paste0("Events|", p$ModelProtocol, "|Application_1|ProtocolSchemaItem"),
          `Parameter Name` = "DosePerBodyWeight",
          Value = p$Dose, Units = p$DoseUnits, check.names = FALSE, stringsAsFactors = FALSE)
      } else {
        data.frame(
          `Container Path` = paste0("Events|", p$ModelProtocol, "|Dissolved|Application_1|ProtocolSchemaItem"),
          `Parameter Name` = c("DosePerBodyWeight", "Volume of water/body weight"),
          Value = c(p$Dose, PO_WATER_VOL), Units = c(p$DoseUnits, PO_WATER_VOL_UNIT),
          check.names = FALSE, stringsAsFactors = FALSE)
      }
    })
  } else {
    # Multiple dose: explicitly set DosePerBodyWeight for ALL MODEL_MAX_APPS_MD slots.
    # Active slots (1..n_bid_slots): BID gets the dose; QD uses odd=dose, even=0
    #   to achieve 24-h intervals from the model's hardcoded 720-min BID structure.
    # Remaining slots (n_bid_slots+1..MODEL_MAX_APPS_MD): always 0 so leftover
    #   model-default doses from previous runs / different Ndose runs don't fire.
    make_all_slot_doses <- function(dose_val) {
      active <- seq_len(n_bid_slots)
      active_doses <- ifelse(schedule == "BID" | active %% 2L == 1L, dose_val, 0)
      tail_zeros   <- rep(0, MODEL_MAX_APPS_MD - n_bid_slots)
      c(active_doses, tail_zeros)
    }

    app_sheets <- lapply(seq_len(nrow(unique_protos)), function(i) {
      p          <- unique_protos[i, ]
      slot_doses <- make_all_slot_doses(p$Dose)

      dplyr::bind_rows(lapply(seq_along(slot_doses), function(j) {
        n    <- j
        dose <- slot_doses[j]
        if (p$Route == "IV") {
          data.frame(
            `Container Path` = paste0("Events|", p$ModelProtocol, "|Application_", n, "|ProtocolSchemaItem"),
            `Parameter Name` = "DosePerBodyWeight",
            Value = dose, Units = p$DoseUnits, check.names = FALSE, stringsAsFactors = FALSE)
        } else {
          data.frame(
            `Container Path` = paste0("Events|", p$ModelProtocol, "|Dissolved|Application_", n, "|ProtocolSchemaItem"),
            `Parameter Name` = c("DosePerBodyWeight", "Volume of water/body weight"),
            Value = c(dose, PO_WATER_VOL), Units = c(p$DoseUnits, PO_WATER_VOL_UNIT),
            check.names = FALSE, stringsAsFactors = FALSE)
        }
      }))
    })
  }
  names(app_sheets) <- unique_protos$ModelProtocol
  openxlsx::write.xlsx(app_sheets, file = file.path(conf_dir, "Applications.xlsx"), overwrite = TRUE)

  # ── Scenarios.xlsx ────────────────────────────────────────────────────────────
  progress_cb(0.48, "Writing Scenarios.xlsx…")

  valid_tissues    <- intersect(selected_tissues, names(TISSUE_CATALOG))
  if (length(valid_tissues) == 0) valid_tissues <- "Plasma"
  sel_catalog      <- TISSUE_CATALOG[valid_tissues]
  output_paths_df  <- data.frame(
    OutputPathId = sapply(sel_catalog, `[[`, "id"),
    OutputPath   = sapply(sel_catalog, `[[`, "path"),
    stringsAsFactors = FALSE
  )
  output_path_ids_str <- paste(output_paths_df$OutputPathId, collapse = ",")

  scenario_rows <- lapply(seq_len(nrow(sim_plan)), function(i) {
    sp <- active_species |> dplyr::filter(Species == sim_plan$Species[i])
    if (nrow(sp) == 0) { warning("Species '", sim_plan$Species[i], "' not in SPECIES_META"); return(NULL) }
    data.frame(
      Scenario_name         = paste0(MoleculeId, "_", sp$Species, "_",
                                     sim_plan$Dose[i], "mpk_", sim_plan$Route[i]),
      IndividualId          = sp$IndividualId,
      PopulationId          = sp$PopulationId,
      ReadPopulationFromCSV = FALSE,
      ModelParameterSheets  = paste0('"', MoleculeId, '"'),
      ApplicationProtocol   = sim_plan$ModelProtocol[i],
      SimulationTime        = sim_time,
      SimulationTimeUnit    = sim_time_unit,
      SteadyState           = FALSE,
      SteadyStateTime       = NA_real_,
      SteadyStateTimeUnit   = NA_character_,
      ModelFile             = paste0("GenericSmallMolecule_", sim_plan$Route[i], "_", sp$clh_suffix, ".pkml"),
      OutputPathsIds        = output_path_ids_str,
      stringsAsFactors      = FALSE
    )
  })
  all_scenarios <- dplyr::bind_rows(scenario_rows)
  openxlsx::write.xlsx(list(Scenarios = all_scenarios, OutputPaths = output_paths_df),
                       file = file.path(conf_dir, "Scenarios.xlsx"), overwrite = TRUE)

  # ── Run esqlabsR ─────────────────────────────────────────────────────────────
  progress_cb(0.58, "Loading project configuration…")
  myProjectConfiguration <- createProjectConfiguration(config_xlsx)

  progress_cb(0.63, "Reading scenario configurations…")
  scenarioConfigurations <- readScenarioConfigurationFromExcel(
    scenarioNames        = all_scenarios$Scenario_name,
    projectConfiguration = myProjectConfiguration
  )

  progress_cb(0.70, "Creating simulation scenarios…")
  myScenarios <- createScenarios(scenarioConfigurations = scenarioConfigurations)

  simulationRunOptions <- ospsuite::SimulationRunOptions$new()
  simulationRunOptions$checkForNegativeValues <- FALSE

  progress_cb(0.80, paste0("Running ", nrow(all_scenarios), " simulations…"))
  simulatedScenariosResults <- runScenarios(
    scenarios            = myScenarios,
    simulationRunOptions = simulationRunOptions
  )

  progress_cb(0.91, "Saving results…")
  outputFolder <- saveScenarioResults(simulatedScenariosResults,
                                      outputFolder = results_dir,
                                      myProjectConfiguration)

  # ── Post-process ─────────────────────────────────────────────────────────────
  progress_cb(0.96, "Processing results…")
  results_raw <- read_all_results(outputFolder)
  results     <- process_results(
    results_df        = results_raw,
    model_params_path = file.path(conf_dir, "ModelParameters.xlsx"),
    tissue_catalog    = sel_catalog
  )

  # Re-parse scenario name to extract Species / Dose / Route reliably
  sp_pat           <- paste(SPECIES_META$Species, collapse = "|")
  results$Route    <- sub(".*_(IV|PO)$", "\\1", results$Scenario_name)
  results$Dose     <- suppressWarnings(
    as.numeric(sub(".*_(\\d+(?:\\.\\d+)?)mpk_(?:IV|PO)$", "\\1", results$Scenario_name))
  )
  results$Species  <- regmatches(results$Scenario_name,
                                  regexpr(sp_pat, results$Scenario_name))
  results$Conc_ngml <- results$Conc_umolL * inputs$mw
  if (!"Tissue" %in% names(results)) results$Tissue <- "Plasma"

  # Attach interval_hr so NCA code can use it without recalculating
  attr(results, "interval_hr") <- interval_hr
  attr(results, "ndose")       <- if (dose_mode == "MD") ndose else NULL

  progress_cb(1.00, "Done.")
  results
}

# ============================================================
# Custom CSS
# ============================================================
css <- "
:root {
  --env-black:  #1A1A2E;
  --env-gray:   #6B7280;
  --env-lgray:  #F3F4F6;
  --env-green:  #22C55E;
  --env-lgreen: #86EFAC;
  --env-purple: #8B5CF6;
  --env-lpur:   #DDD6FE;
}

body { background: #EDEEF0 !important; }

.navbar {
  background: var(--env-black) !important;
  border-bottom: 3px solid var(--env-green) !important;
  padding: 0.6rem 1.5rem !important;
}
.navbar-brand {
  color: #fff !important; font-weight: 700; font-size: 1rem; letter-spacing: 0.4px;
}
.navbar-brand span.brand-accent { color: var(--env-lgreen); }
.nav-link { color: rgba(255,255,255,0.75) !important; font-size: 0.82rem !important; }
.nav-link.active, .nav-link:hover { color: var(--env-lgreen) !important; font-weight: 600 !important; }

.env-card {
  background: #fff; border-radius: 12px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.07);
  margin-bottom: 1rem; overflow: hidden;
}
.env-card-header {
  background: var(--env-black); color: #fff;
  padding: 0.65rem 1rem; font-weight: 600; font-size: 0.84rem;
  letter-spacing: 0.2px; border-left: 4px solid var(--env-green);
  display: flex; align-items: center; gap: 0.5rem;
}
.env-card-header .header-badge {
  background: var(--env-lgreen); color: var(--env-black);
  font-size: 0.68rem; padding: 0.1rem 0.5rem;
  border-radius: 10px; font-weight: 700; margin-left: auto;
}
.env-card-body { padding: 0.9rem 1rem; }

.form-label {
  font-size: 0.76rem; color: var(--env-gray); font-weight: 600;
  margin-bottom: 0.15rem; text-transform: uppercase; letter-spacing: 0.4px;
}
.form-control, .form-select, .shiny-input-container input, .shiny-input-container select {
  font-size: 0.84rem !important; border-radius: 7px !important; border-color: #D1D5DB !important;
}
.form-control:focus { border-color: var(--env-green) !important; box-shadow: 0 0 0 3px rgba(34,197,94,0.15) !important; }

.section-label {
  font-size: 0.68rem; font-weight: 700; color: var(--env-gray);
  text-transform: uppercase; letter-spacing: 1px;
  border-bottom: 1px solid #E5E7EB; padding-bottom: 0.25rem; margin: 0.8rem 0 0.5rem 0;
}

.dose-mode-bar {
  background: #fff; border-radius: 10px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.06);
  padding: 0.6rem 1rem; margin-bottom: 1rem;
  display: flex; align-items: center; gap: 1.5rem;
}
.dose-mode-bar .shiny-input-container { margin: 0; }
.dose-mode-bar .control-label { font-size: 0.72rem; font-weight: 700; text-transform: uppercase; letter-spacing: 0.5px; color: var(--env-gray); margin-right: 0.5rem; }

.btn-run {
  background: var(--env-black) !important; color: var(--env-lgreen) !important;
  border: 2px solid var(--env-green) !important; border-radius: 8px !important;
  font-weight: 700 !important; font-size: 0.85rem !important;
  padding: 0.55rem 1.5rem !important; width: 100%; letter-spacing: 0.3px; transition: all 0.2s !important;
}
.btn-run:hover { background: var(--env-green) !important; color: var(--env-black) !important; }
.btn-dl {
  background: transparent !important; color: var(--env-purple) !important;
  border: 1.5px solid var(--env-purple) !important; border-radius: 7px !important;
  font-weight: 600 !important; font-size: 0.78rem !important;
  padding: 0.3rem 0.9rem !important; transition: all 0.15s !important;
}
.btn-dl:hover { background: var(--env-lpur) !important; color: var(--env-black) !important; }
.btn-add-row {
  background: var(--env-lpur) !important; color: var(--env-black) !important;
  border: 1.5px solid var(--env-purple) !important; border-radius: 7px !important;
  font-size: 0.78rem !important; padding: 0.3rem 0.8rem !important;
}
.btn-rm-row {
  background: #FEE2E2 !important; color: #991B1B !important;
  border: 1.5px solid #FCA5A5 !important; border-radius: 7px !important;
  font-size: 0.78rem !important; padding: 0.3rem 0.8rem !important;
}

.sim-status {
  font-size: 0.78rem; padding: 0.4rem 0.8rem; border-radius: 6px;
  margin-top: 0.5rem; display: none;
}
.sim-status.running { background: var(--env-lpur); color: var(--env-black); display: block; }
.sim-status.done    { background: var(--env-lgreen); color: var(--env-black); display: block; }
.sim-status.error   { background: #FEE2E2; color: #991B1B; display: block; }

.page-placeholder {
  display: flex; flex-direction: column; align-items: center;
  justify-content: center; min-height: 60vh; color: var(--env-gray);
  text-align: center; gap: 1rem;
}
.page-placeholder .ph-icon { font-size: 3rem; opacity: 0.3; }
.page-placeholder h4 { font-weight: 600; color: var(--env-black); }
.page-placeholder p  { max-width: 400px; font-size: 0.88rem; }

.dataTables_wrapper { font-size: 0.82rem; }
.dataTable thead th {
  background: var(--env-black) !important; color: #fff !important;
  font-weight: 600; font-size: 0.78rem; text-transform: uppercase; letter-spacing: 0.5px;
}
.nca-section-label {
  font-size: 0.7rem; font-weight: 700; color: var(--env-purple);
  text-transform: uppercase; letter-spacing: 0.8px;
  border-bottom: 2px solid var(--env-lpur); padding-bottom: 0.2rem; margin-bottom: 0.5rem;
}
.shiny-plot-output { border-radius: 8px; }
.handsontable th { background: var(--env-lgray) !important; font-size: 0.78rem !important; }
.handsontable td { font-size: 0.82rem !important; }
"

# ============================================================
# UI helpers
# ============================================================

env_card <- function(..., header, badge = NULL) {
  div(class = "env-card",
    div(class = "env-card-header",
      header,
      if (!is.null(badge)) span(class = "header-badge", badge)
    ),
    div(class = "env-card-body", ...)
  )
}

section_label <- function(txt) div(class = "section-label", txt)

num_input <- function(id, label, value, min = NA, max = NA, step = NA, width = "100%") {
  numericInput(id, label = label, value = value, min = min, max = max, step = step, width = width)
}

# ============================================================
# UI
# ============================================================

ui <- page_navbar(
  title = tags$span("Preclinical PK ", tags$span(class = "brand-accent", "Simulator")),
  theme = bs_theme(
    version = 5, bg = "#EDEEF0", fg = ENV_BLACK,
    primary = ENV_GREEN, secondary = ENV_PURPLE,
    base_font = font_google("Inter"), heading_font = font_google("Inter"),
    font_scale = 0.9
  ),
  tags$head(tags$style(HTML(css))),
  window_title = "Enveda Preclinical Study Simulator",
  fillable = FALSE,

  # ==========================================================
  # PAGE 1 — A Priori Single Molecule
  # ==========================================================
  nav_panel(
    title = "A Priori · Single Molecule",
    icon  = icon("flask"),
    fluidPage(
      style = "padding: 1.2rem 1.5rem;",

      # ── Dose-mode selector (full-width bar above both columns) ──────────────
      div(class = "dose-mode-bar",
        radioButtons(
          "dose_mode",
          label    = "Simulation Mode:",
          choices  = c("Single Dose", "Multiple Dose"),
          selected = "Single Dose",
          inline   = TRUE
        ),
        # MD-specific controls shown inline
        conditionalPanel(
          "input.dose_mode == 'Multiple Dose'",
          div(style = "display:flex; align-items:flex-end; gap:1rem;",
            div(
              tags$label(class = "control-label", "Number of Doses"),
              numericInput("ndose", NULL, value = 30, min = 1, max = 60, step = 1, width = "110px")
            ),
            div(
              tags$label(class = "control-label", "Schedule"),
              selectInput("schedule", NULL,
                          choices = c("BID (twice daily)" = "BID", "QD (once daily)" = "QD"),
                          selected = "BID", width = "170px")
            ),
            div(style = "padding-bottom:0.35rem;",
              tags$small(style = "color:#6B7280; font-size:0.74rem;",
                "BID: max 60 doses  |  QD: max 30 doses"
              )
            )
          )
        )
      ),

      fluidRow(

        # ── LEFT: Molecule Inputs + Scenarios ───────────────────────────────
        column(4,

          env_card(
            header = tagList(icon("atom"), " Molecule Inputs"),

            section_label("Compound Identity"),
            textInput("mol_id", "Molecule ID", value = "ENV-XXXX", width = "100%"),

            section_label("Physicochemical Properties"),
            fluidRow(
              column(6, num_input("lipophilicity", "Lipophilicity (Log units)", 2.5)),
              column(6, num_input("fu",            "fu (plasma)",               0.8, 0, 1, 0.01))
            ),
            fluidRow(
              column(6, num_input("mw",  "MW (g/mol)",  358.4, 0)),
              column(6, num_input("pka", "pKa",         10.2))
            ),
            fluidRow(
              column(6, num_input("solubility", "Solubility (mg/L)", 1620, 0)),
              column(6, num_input("ref_ph",     "Reference pH",      7.4, 0, 14, 0.1))
            ),
            num_input("perm", "Specific Intestinal Permeability (cm/min)",
                      value = 1.6e-5, step = 1e-7, width = "100%"),
            num_input("gfr",  "GFR Fraction", 1, 0, 1, 0.01, "100%"),

            section_label("Hepatic Clearance by Species (mL/min/kg)"),
            fluidRow(
              column(6, num_input("clh_mouse", "Mouse",  32,   0)),
              column(6, num_input("clh_rat",   "Rat",    32,   0))
            ),
            fluidRow(
              column(6, num_input("clh_dog",   "Dog",    0.01, 0)),
              column(6, num_input("clh_human", "Human",  0.01, 0))
            ),

            # Sim time only relevant for single dose (MD auto-computes it)
            conditionalPanel(
              "input.dose_mode == 'Single Dose'",
              section_label("Simulation Time"),
              fluidRow(
                column(8, textInput("sim_time", "Time (start, end, res;)", "0, 24, 48;")),
                column(4, selectInput("sim_time_unit", "Unit",
                                     choices = c("h", "min"), selected = "h", width = "100%"))
              )
            )
          ),

          env_card(
            header = tagList(icon("table"), " Simulation Scenarios"),
            p(style = "font-size:0.78rem; color:#6B7280; margin-bottom:0.5rem;",
              "Edit directly. Routes: IV or PO. Dose units apply to all rows."),
            rHandsontableOutput("scenario_table", height = "220px"),
            fluidRow(
              style = "margin-top:0.6rem;",
              column(4, actionButton("add_row",    "＋ Add Row",    class = "btn-add-row w-100")),
              column(4, actionButton("remove_row", "－ Remove Last", class = "btn-rm-row w-100")),
              column(4, selectInput("dose_units", "Dose Units",
                                   choices = c("mg/kg", "mg", "µmol/kg"),
                                   selected = "mg/kg", width = "100%"))
            )
          )
        ),

        # ── RIGHT: Tissues + Run + Plot + NCA ───────────────────────────────
        column(8,

          # Tissue selection — available for both Single and Multiple Dose
          env_card(
            header = tagList(icon("lungs"), " Output Tissues"),
            p(style = "font-size:0.78rem; color:#6B7280; margin-bottom:0.5rem;",
              "Select tissue compartments to include in the simulation output."),
            checkboxGroupInput(
              "tissues",
              label    = NULL,
              choices  = names(TISSUE_CATALOG),
              selected = "Plasma",
              inline   = TRUE
            )
          ),

          # Run button + status
          fluidRow(
            column(6,
              actionButton("run_sim", tagList(icon("play"), " Run Simulation"), class = "btn-run")
            ),
            column(6,
              uiOutput("sim_status_ui")
            )
          ),
          br(),

          # Plot
          env_card(
            header = tagList(icon("chart-line"), " Concentration–Time Profile"),
            fluidRow(
              column(8,
                fluidRow(
                  column(4, checkboxInput("log_scale", "Log Y-axis", value = TRUE)),
                  column(4, selectInput("color_by", "Color by",
                                       choices = c("Route", "Species", "Dose", "Tissue"),
                                       selected = "Route", width = "100%")),
                  column(4, selectInput("facet_by", "Facet by (rows)",
                                       choices = c("None", "Species", "Route", "Tissue", "Dose"),
                                       selected = "Species", width = "100%")),
                  column(4, selectInput("facet_by2", "Facet by (cols)",
                                       choices = c("None", "Species", "Route", "Tissue", "Dose"),
                                       selected = "None", width = "100%"))
                ),
                fluidRow(
                  column(4, checkboxInput("show_ref_conc", "Reference line", value = FALSE)),
                  column(4, conditionalPanel("input.show_ref_conc",
                    numericInput("ref_conc_val", "Ref. conc. (ng/mL)", 100, min = 0, step = 1, width = "100%"))),
                  column(4, conditionalPanel("input.show_ref_conc",
                    textInput("ref_conc_label", "Line label", "Ref. conc.", width = "100%")))
                )
              ),
              column(4, style = "text-align:right;",
                downloadButton("dl_plot", tagList(icon("download"), " Download Plot"), class = "btn-dl")
              )
            ),
            plotOutput("pk_plot", height = "320px") |>
              withSpinner(color = ENV_GREEN, type = 6, size = 0.8)
          ),

          # NCA — Single Dose mode: one table with configurable window
          conditionalPanel(
            "input.dose_mode == 'Single Dose'",
            env_card(
              header = tagList(icon("table-cells"), " PK Summary (NCA)"),
              fluidRow(
                column(8,
                  fluidRow(
                    column(4, num_input("nca_start", "AUC Start (h)", 0, 0)),
                    column(4, num_input("nca_end",   "AUC End (h)",  24, 0)),
                    column(4, style = "padding-top:1.6rem;",
                      actionButton("recalc_nca", "Recalculate",
                                   class = "btn-add-row w-100", icon = icon("rotate")))
                  )
                ),
                column(4, style = "text-align:right;",
                  downloadButton("dl_table_sd", tagList(icon("download"), " Download"),
                                 class = "btn-dl")
                )
              ),
              DTOutput("pk_table_sd") |> withSpinner(color = ENV_PURPLE, type = 6, size = 0.8)
            )
          ),

          # NCA — Multiple Dose mode: two tables (1st dose + last dose / SS)
          conditionalPanel(
            "input.dose_mode == 'Multiple Dose'",
            env_card(
              header = tagList(icon("table-cells"), " PK Summary — Multiple Dose NCA"),
              badge  = "Multiple Dose",
              fluidRow(
                column(12, style = "text-align:right; margin-bottom:0.5rem;",
                  downloadButton("dl_table_md", tagList(icon("download"), " Download All"),
                                 class = "btn-dl")
                )
              ),
              div(class = "nca-section-label", "1st Dose (Dose 1 Interval)"),
              DTOutput("pk_table_md_first") |> withSpinner(color = ENV_PURPLE, type = 6, size = 0.6),
              br(),
              div(class = "nca-section-label", "Last Dose / Steady State"),
              DTOutput("pk_table_md_last") |> withSpinner(color = ENV_GREEN, type = 6, size = 0.6)
            )
          )

        )  # end col 8
      )  # end fluidRow
    )  # end fluidPage
  ),  # end nav_panel 1

  # ==========================================================
  # PAGE 2 — placeholder
  # ==========================================================
  nav_panel(
    title = "A Priori · Multiple Molecules",
    icon  = icon("layer-group"),
    fluidPage(style = "padding: 2rem;",
      div(class = "page-placeholder",
        div(class = "ph-icon", icon("layer-group")),
        tags$h4("A Priori · Multiple Molecules"),
        tags$p("Batch simulation of multiple compounds using CDD-derived ADMET parameters. Coming soon."),
        tags$span(style = paste0("display:inline-block; background:", ENV_LPUR, "; color:", ENV_BLACK,
                                 "; padding:0.4rem 1.2rem; border-radius:20px; font-size:0.82rem; font-weight:600;"),
                  "Under Development")
      )
    )
  ),

  # ==========================================================
  # PAGE 3 — placeholder
  # ==========================================================
  nav_panel(
    title = "Precision Mode",
    icon  = icon("sliders"),
    fluidPage(style = "padding: 2rem;",
      div(class = "page-placeholder",
        div(class = "ph-icon", icon("sliders")),
        tags$h4("Precision Mode"),
        tags$p("Overlay simulated vs. observed PK, sensitivity analysis, population variability. Coming soon."),
        tags$span(style = paste0("display:inline-block; background:", ENV_LPUR, "; color:", ENV_BLACK,
                                 "; padding:0.4rem 1.2rem; border-radius:20px; font-size:0.82rem; font-weight:600;"),
                  "Under Development")
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    scenarios  = default_scenarios(),
    results    = NULL,
    status     = "idle",
    status_msg = ""
  )

  # ── Scenario table ────────────────────────────────────────────────────────────
  output$scenario_table <- renderRHandsontable({
    rhandsontable(rv$scenarios, rowHeaders = NULL, stretchH = "all", height = 200) |>
      hot_col("Species",   type = "dropdown",
              source = c("Mouse", "Rat", "Dog", "NHP", "Human")) |>
      hot_col("Route",     type = "dropdown", source = c("IV", "PO")) |>
      hot_col("Dose",      type = "numeric",  format = "0.###") |>
      hot_col("DoseUnits", readOnly = TRUE)
  })

  observeEvent(input$scenario_table, {
    if (!is.null(input$scenario_table)) rv$scenarios <- hot_to_r(input$scenario_table)
  })

  observeEvent(input$dose_units, { rv$scenarios$DoseUnits <- input$dose_units })

  observeEvent(input$add_row, {
    rv$scenarios <- dplyr::bind_rows(rv$scenarios,
      data.frame(Species = "Mouse", Route = "IV",
                 Dose = 1, DoseUnits = input$dose_units, stringsAsFactors = FALSE))
  })

  observeEvent(input$remove_row, {
    if (nrow(rv$scenarios) > 1) rv$scenarios <- rv$scenarios[-nrow(rv$scenarios), ]
  })

  # ── Status ────────────────────────────────────────────────────────────────────
  output$sim_status_ui <- renderUI({
    s   <- rv$status
    cls <- switch(s, running = "running", done = "done", error = "error", NULL)
    if (is.null(cls)) return(NULL)
    div(class = paste("sim-status", cls), rv$status_msg)
  })

  # ── Run ───────────────────────────────────────────────────────────────────────
  observeEvent(input$run_sim, {
    req(nrow(rv$scenarios) > 0)
    rv$status     <- "running"
    rv$status_msg <- "Initialising…"
    rv$results    <- NULL

    dose_mode <- if (input$dose_mode == "Multiple Dose") "MD" else "SD"
    ndose     <- if (dose_mode == "MD") as.integer(input$ndose) else 1L
    schedule  <- if (dose_mode == "MD") input$schedule else "BID"

    # Validate MD limits
    if (dose_mode == "MD") {
      max_n <- if (schedule == "QD") MODEL_MAX_APPS_MD %/% 2L else MODEL_MAX_APPS_MD
      if (ndose > max_n) {
        rv$status     <- "error"
        rv$status_msg <- paste0("Ndose (", ndose, ") exceeds model capacity for ",
                                schedule, " (max ", max_n, ").")
        return()
      }
    }

    inputs <- list(
      molecule_id   = trimws(input$mol_id),
      lipophilicity = input$lipophilicity,
      fu            = input$fu,
      mw            = input$mw,
      pka           = input$pka,
      solubility    = input$solubility,
      ref_ph        = input$ref_ph,
      perm          = input$perm,
      gfr           = input$gfr,
      clh_mouse     = input$clh_mouse,
      clh_rat       = input$clh_rat,
      clh_dog       = input$clh_dog,
      clh_human     = input$clh_human,
      sim_time      = input$sim_time,
      sim_time_unit = input$sim_time_unit
    )

    scenarios         <- rv$scenarios
    scenarios$DoseUnits <- input$dose_units

    # Use whatever tissues the user has checked (both SD and MD)
    selected_tissues <- if (length(input$tissues) > 0) input$tissues else "Plasma"
    config_xlsx      <- if (dose_mode == "MD") CONFIG_MD else CONFIG_SD

    # Wipe Configurations (stale xlsx) and Results from previous run
    xlsx_stale <- list.files(CONF_DIR, pattern = "\\.xlsx$", full.names = TRUE)
    if (length(xlsx_stale) > 0) unlink(xlsx_stale)
    if (dir.exists(RESULTS_DIR)) {
      unlink(list.files(RESULTS_DIR, full.names = TRUE), recursive = TRUE)
    }
    dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

    prog <- shiny::Progress$new(session, min = 0, max = 1)
    prog$set(message = "Starting…", value = 0)
    on.exit(prog$close(), add = TRUE)

    progress_cb <- function(frac, msg) {
      prog$set(value = frac, detail = msg)
      rv$status_msg <- sprintf("[%d%%] %s", round(frac * 100), msg)
    }

    tryCatch({
      results <- run_pbpk_simulation(
        inputs           = inputs,
        scenarios        = scenarios,
        conf_dir         = CONF_DIR,
        pop_dir          = POP_DIR,
        config_xlsx      = config_xlsx,
        results_dir      = RESULTS_DIR,
        selected_tissues = selected_tissues,
        dose_mode        = dose_mode,
        ndose            = ndose,
        schedule         = schedule,
        progress_cb      = progress_cb
      )

      rv$results    <- results
      rv$status     <- "done"
      rv$status_msg <- paste0("Complete — ", nrow(results), " time points.")

    }, error = function(e) {
      rv$status     <- "error"
      rv$status_msg <- paste("Error:", conditionMessage(e))
    })
  })

  # ── NCA helpers ───────────────────────────────────────────────────────────────
  compute_nca <- function(df, t_start, t_end) {
    df |>
      dplyr::filter(Time_hr >= t_start, Time_hr <= t_end) |>
      dplyr::group_by(Species, Route, Tissue) |>
      dplyr::summarise(
        `Dose (mg/kg)`  = unique(Dose)[1],
        `AUC (ng·h/mL)` = round(auc_trap(Time_hr, Conc_ngml), 2),
        `Cmax (ng/mL)`  = round(max(Conc_ngml, na.rm = TRUE), 2),
        `Tmax (h)`      = round(Time_hr[which.max(Conc_ngml)], 2),
        .groups = "drop"
      ) |>
      dplyr::arrange(Species, Route, Tissue)
  }

  compute_nca_last <- function(df, t_start, t_end) {
    df |>
      dplyr::filter(Time_hr >= t_start, Time_hr <= t_end) |>
      dplyr::group_by(Species, Route, Tissue) |>
      dplyr::summarise(
        `Dose (mg/kg)`       = unique(Dose)[1],
        `AUCtau (ng·h/mL)`  = round(auc_trap(Time_hr, Conc_ngml), 2),
        `Cmax_ss (ng/mL)`   = round(max(Conc_ngml, na.rm = TRUE), 2),
        `Tmax_ss (h in tau)` = round(Time_hr[which.max(Conc_ngml)] - t_start, 2),
        `Ctrough (ng/mL)`   = round(Conc_ngml[which.max(Time_hr)], 2),
        .groups = "drop"
      ) |>
      dplyr::arrange(Species, Route, Tissue)
  }

  render_nca_dt <- function(df) {
    datatable(df, rownames = FALSE,
              options = list(dom = "t", paging = FALSE, scrollX = TRUE,
                             columnDefs = list(list(className = "dt-center", targets = "_all"))),
              class = "cell-border stripe hover")
  }

  # ── SD NCA ───────────────────────────────────────────────────────────────────
  nca_sd <- reactive({
    req(rv$results, input$dose_mode == "Single Dose")
    input$recalc_nca
    t_start <- isolate(input$nca_start) %||% 0
    t_end   <- isolate(input$nca_end)   %||% 24
    compute_nca(rv$results, t_start, t_end)
  })

  output$pk_table_sd <- renderDT({ render_nca_dt(nca_sd()) })

  # ── MD NCA ───────────────────────────────────────────────────────────────────
  md_nca_first <- reactive({
    req(rv$results, input$dose_mode == "Multiple Dose")
    interval_hr <- attr(rv$results, "interval_hr") %||% 12
    compute_nca(rv$results, 0, interval_hr)
  })

  md_nca_last <- reactive({
    req(rv$results, input$dose_mode == "Multiple Dose")
    interval_hr <- attr(rv$results, "interval_hr") %||% 12
    ndose_val   <- attr(rv$results, "ndose") %||% as.integer(input$ndose)
    t_start     <- (ndose_val - 1) * interval_hr
    t_end       <- ndose_val * interval_hr
    compute_nca_last(rv$results, t_start, t_end)
  })

  output$pk_table_md_first <- renderDT({ render_nca_dt(md_nca_first()) })
  output$pk_table_md_last  <- renderDT({ render_nca_dt(md_nca_last()) })

  # ── Plot ─────────────────────────────────────────────────────────────────────
  pk_plot_obj <- reactive({
    req(rv$results)
    results    <- rv$results
    color_var  <- input$color_by
    facet_var  <- input$facet_by
    facet_var2 <- input$facet_by2

    sp_order     <- intersect(c("Mouse","Rat","Dog","NHP","Human"), unique(results$Species))
    results$Species <- factor(results$Species, levels = sp_order)

    n_tissues <- length(unique(results$Tissue))
    y_label   <- if (n_tissues == 1 && unique(results$Tissue) == "Plasma")
      "Plasma Concentration (ng/mL)" else "Concentration (ng/mL)"

    # For MD, add a vertical dashed line at the 1st dose end (first interval)
    interval_hr <- attr(results, "interval_hr")

    p <- ggplot(results, aes(x = Time_hr, y = Conc_ngml,
                              color = .data[[color_var]],
                              group = interaction(Species, Route, Dose, Tissue))) +
      geom_line(linewidth = 0.9, alpha = 0.9) +
      labs(
        title   = paste0("Simulated PK — ", isolate(input$mol_id),
                         if (!is.null(interval_hr)) paste0("  [", input$schedule, ", ", input$ndose, " doses]") else ""),
        x       = "Time (h)", y = y_label, color = color_var,
        caption = paste0("Preclinical PK Simulator | Enveda | ", format(Sys.Date(), "%Y-%m-%d"))
      ) +
      scale_color_manual(values = c("#EAB308","#8B5CF6","#F97316","#0EA5E9","#EC4899",
                                    "#22C55E","#14B8A6","#F43F5E","#A78BFA","#FB923C")) +
      theme_bw(base_size = 13) +
      theme(
        plot.title       = element_text(face = "bold", size = 12, color = ENV_BLACK),
        strip.background = element_rect(fill = ENV_BLACK, color = NA),
        strip.text       = element_text(color = "#fff", face = "bold", size = 10),
        legend.position  = "bottom",
        panel.grid.minor = element_blank(),
        axis.title       = element_text(size = 11, color = ENV_GRAY),
        plot.caption     = element_text(size = 7, color = ENV_GRAY, hjust = 1)
      )

    # Add 1st-dose / SS marker line for MD
    if (!is.null(interval_hr)) {
      p <- p + geom_vline(xintercept = interval_hr, linetype = "dotted",
                          color = "#94A3B8", linewidth = 0.6) +
               annotate("text", x = interval_hr, y = -Inf,
                        label = "1st dose end", angle = 90, vjust = -0.4, hjust = -0.1,
                        size = 2.8, color = "#94A3B8")
    }

    if (input$log_scale) p <- p + scale_y_log10()

    has1 <- facet_var  != "None"
    has2 <- facet_var2 != "None"
    if (has1 && has2) {
      p <- p + facet_grid(as.formula(paste(facet_var, "~", facet_var2)), scales = "free_y")
    } else if (has1) {
      p <- p + facet_wrap(as.formula(paste0("~", facet_var)), scales = "free_y")
    } else if (has2) {
      p <- p + facet_wrap(as.formula(paste0("~", facet_var2)), scales = "free_y")
    }

    if (isTRUE(input$show_ref_conc) && !is.na(input$ref_conc_val)) {
      ref_lbl <- trimws(input$ref_conc_label)
      p <- p + geom_hline(yintercept = input$ref_conc_val,
                          linetype = "dashed", color = "#DC2626", linewidth = 0.7)
      if (nchar(ref_lbl) > 0)
        p <- p + annotate("text", x = -Inf, y = input$ref_conc_val,
                          label = ref_lbl, hjust = -0.1, vjust = -0.4,
                          color = "#DC2626", size = 3.2, fontface = "italic")
    }
    p
  })

  output$pk_plot <- renderPlot({ pk_plot_obj() }, res = 110)

  # ── Downloads ─────────────────────────────────────────────────────────────────
  output$dl_plot <- downloadHandler(
    filename = function() paste0(input$mol_id, "_PK_plot_", Sys.Date(), ".pdf"),
    content  = function(file) {
      ggplot2::ggsave(file, plot = pk_plot_obj(), width = 12, height = 5, device = "pdf")
    }
  )

  output$dl_table_sd <- downloadHandler(
    filename = function() paste0(input$mol_id, "_NCA_SD_", Sys.Date(), ".csv"),
    content  = function(file) utils::write.csv(nca_sd(), file, row.names = FALSE)
  )

  output$dl_table_md <- downloadHandler(
    filename = function() paste0(input$mol_id, "_NCA_MD_", Sys.Date(), ".csv"),
    content  = function(file) {
      first <- md_nca_first()
      last  <- md_nca_last()
      first$dose_period <- "1st Dose"
      last$dose_period  <- "Last Dose (SS)"
      utils::write.csv(dplyr::bind_rows(first, last), file, row.names = FALSE)
    }
  )
}

# ============================================================
shinyApp(ui, server)
