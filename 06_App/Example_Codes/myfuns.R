# =============================================================================
# PK Data Converter — Wide Format → PK-Sim / OSPS Long Format
# =============================================================================
#
# PURPOSE
#   Converts raw preclinical PK spreadsheets (wide matrix: animals × time-points)
#   into the long-format tidy table required by PK-Sim and the OSPS toolbox,
#   as shown in TestProject_TimeValuesData.xlsx.
#
# TARGET OUTPUT COLUMNS
#   Study Id | Subject Id | Organ | Compartment | Species | Gender | Dose |
#   Molecule | Molecular Weight | Time | Time unit | Measurement |
#   Measurement unit | Error | LLOQ | Route | Group Id | Population |
#   Weight [kg] | Height [cm] | Age [year(s)]
#
# SUPPORTED INPUT LAYOUT
#   The raw PK sheet may contain one or more side-by-side blocks
#   (typically one per route: IV, PO, SC …). Each block follows the pattern:
#
#     Row A  │  [Title: "Intravenous PK study … at X mg/Kg"]
#     Row B  │  "Animal No."  │  "Plasma Conc. … / Time (h)"
#     Row C  │  <blank>       │  t1    t2    t3  … tn        ← TIME POINT HEADER
#     Row D+ │  Animal-1      │  c1    c2    c3  … cn        ← DATA ROWS
#            │  Animal-2      │  …
#     Row X  │  Mean / SD / CV%  (skipped automatically)
#     Row Y  │  "LLOQ: XX ng/mL, BLQ: …"                    ← NOTE ROW
#
# USAGE (interactive)
#   source("pk_converter.R")
#
#   # Process a single file
#   df <- convert_pk_file(
#     pk_file     = "MyStudy_Dog_PK.xlsx",
#     study_id    = "MyStudy-001",
#     molecule    = "CompoundX",
#     mw          = 450.5          # Molecular weight (Da); NA if unknown
#   )
#
#   # Process multiple files and combine
#   combined <- convert_pk_batch(
#     file_list = list(
#       list(pk_file = "Dog_PK.xlsx", study_id = "ESN-2987", molecule = "ESN-2987"),
#       list(pk_file = "Rat_PK.xlsx", study_id = "ESN-2987", molecule = "ESN-2987")
#     ),
#     output_xlsx = "ESN2987_converted.xlsx"
#   )
#
# HANDLING SPECIAL VALUES
#   BLQ / <LLOQ / ND / NA  → Measurement = NA  (below limit of quantitation)
#   "18.403*"              → Measurement = 18.403  (flag noted in console)
#   LLOQ                   → Extracted from note row in the sheet
#
# DEPENDENCIES  tidyverse, readxl, openxlsx
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(openxlsx)
})


# =============================================================================
# SECTION 1 — Study Protocol Parser
# =============================================================================

#' Parse the "Study Protocol" sheet and return a named list of metadata.
#'
#' @param file  Path to Excel workbook.
#' @param sheet Sheet name or index containing protocol info.
#' @return Named list with keys: species, gender, weight_kg, age_years,
#'         dose_str, route_str, n_per_group.
parse_protocol <- function(file, sheet = "Study Protocol") {
  
  # ── Fuzzy sheet name matching (handles trailing spaces, etc.) ──
  available_sheets <- tryCatch(excel_sheets(file), error = function(e) character(0))
  matched_sheet <- NULL
  if (length(available_sheets) > 0) {
    # Exact match first
    if (sheet %in% available_sheets) {
      matched_sheet <- sheet
    } else {
      # Fuzzy: trim whitespace and compare case-insensitively
      trimmed <- str_trim(available_sheets)
      idx <- which(tolower(trimmed) == tolower(str_trim(sheet)))
      if (length(idx) > 0) matched_sheet <- available_sheets[idx[1]]
    }
  }
  
  if (is.null(matched_sheet)) {
    message("  [Protocol] Sheet not found: ", sheet)
    return(list(species = NA, gender = NA, weight_kg = NA,
                age_years = NA, dose_str = NA, group_doses = list()))
  }
  
  raw <- tryCatch(
    read_excel(file, sheet = matched_sheet, col_names = FALSE, .name_repair = "unique_quiet"),
    error = function(e) { message("  [Protocol] Error reading sheet: ", conditionMessage(e)); NULL }
  )
  if (is.null(raw)) return(list(species = NA, gender = NA, weight_kg = NA,
                                age_years = NA, dose_str = NA, group_doses = list()))
  
  raw <- raw %>% mutate(across(everything(), as.character))
  # Standardise column names for safe access
  names(raw) <- paste0("...", seq_len(ncol(raw)))
  
  # Generic key-value extractor (scans column 1 for key, returns column 2 value)
  get_val <- function(...) {
    patterns <- c(...)
    for (pat in patterns) {
      rows <- raw %>% filter(str_detect(coalesce(`...1`, ""), regex(pat, ignore_case = TRUE)))
      if (nrow(rows) > 0 && ncol(rows) >= 2) {
        v <- rows[["...2"]][1]
        if (!is.na(v) && nchar(v) > 0) return(v)
      }
    }
    NA_character_
  }
  
  species_raw  <- get_val("strain.*species|species.*sex", "species", "test system$")
  bw_raw       <- get_val("body weight|age.*body weight|age/.*body weight", "weight")
  age_raw      <- get_val("animal age|age.*body weight|age/.*body weight")
  dose_raw     <- get_val("dose level")
  
  # ── Species ──────────────────────────────────────────────────────────────
  species <- case_when(
    str_detect(tolower(coalesce(species_raw, "")), "dog|beagle|canine") ~ "Dog",
    str_detect(tolower(coalesce(species_raw, "")), "rat")               ~ "Rat",
    str_detect(tolower(coalesce(species_raw, "")), "mouse|mice")        ~ "Mouse",
    str_detect(tolower(coalesce(species_raw, "")), "human")             ~ "Human",
    str_detect(tolower(coalesce(species_raw, "")), "monkey|primate|nhp")~ "Monkey",
    str_detect(tolower(coalesce(species_raw, "")), "rabbit")            ~ "Rabbit",
    TRUE ~ NA_character_
  )
  
  # If species not found in the key-value field, scan for it in the title / study name
  if (is.na(species)) {
    all_text <- paste(na.omit(as.character(unlist(raw[1:min(5, nrow(raw)), ]))), collapse = " ")
    species <- case_when(
      str_detect(tolower(all_text), "dog|beagle|canine") ~ "Dog",
      str_detect(tolower(all_text), "\\brat\\b")         ~ "Rat",
      str_detect(tolower(all_text), "mouse|mice")        ~ "Mouse",
      str_detect(tolower(all_text), "monkey|primate|nhp") ~ "Monkey",
      TRUE ~ "Unknown"
    )
  }
  
  # ── Gender ───────────────────────────────────────────────────────────────
  # Check dedicated Sex field first, then fall back to species_raw
  sex_raw <- get_val("^sex$")
  gender_source <- coalesce(sex_raw, species_raw, "")
  gender <- case_when(
    str_detect(tolower(gender_source), "female") ~ "FEMALE",
    str_detect(tolower(gender_source), "male")   ~ "MALE",
    TRUE ~ NA_character_
  )
  
  # ── Body Weight → kg ─────────────────────────────────────────────────────
  # Handle combined "age / body weight" fields like "7-10 weeks/ 17-28 g"
  # by splitting on "/" and using the part that mentions a weight unit (g, kg)
  bw_text <- coalesce(bw_raw, "")
  if (str_detect(bw_text, "/") && str_detect(tolower(bw_text), "week|month")) {
    # Split on "/" and use the part with weight units (g, kg, lb)
    parts <- str_split(bw_text, "/")[[1]]
    wt_part <- parts[str_detect(tolower(parts), "\\bg\\b|kg|lb|gram")]
    if (length(wt_part) > 0) bw_text <- wt_part[1]
  }
  bw_nums <- suppressWarnings(as.numeric(str_extract_all(bw_text, "[0-9]+\\.?[0-9]*")[[1]]))
  bw_nums <- bw_nums[bw_nums > 0]
  weight_kg <- if (length(bw_nums) > 0) {
    bw_mean <- mean(bw_nums)
    # Check for explicit unit in the text
    bw_text_lower <- tolower(bw_text)
    if (str_detect(bw_text_lower, "\\bkg\\b")) {
      round(bw_mean, 4)  # already in kg
    } else if (str_detect(bw_text_lower, "\\bg\\b|gram")) {
      round(bw_mean / 1000, 4)  # grams → kg
    } else if (str_detect(bw_text_lower, "\\blb\\b|pound")) {
      round(bw_mean * 0.4536, 4)  # pounds → kg
    } else {
      # Heuristic fallback: values > 100 are likely grams (rodents) → convert to kg
      if (bw_mean > 100) round(bw_mean / 1000, 4) else round(bw_mean, 4)
    }
  } else NA_real_
  
  # ── Age → years ──────────────────────────────────────────────────────────
  # Handle combined "age / body weight" fields
  age_text <- coalesce(age_raw, bw_raw, "")
  if (str_detect(age_text, "/") && str_detect(tolower(age_text), "\\bg\\b|kg")) {
    # Split on "/" and use the part with time units (weeks, months)
    parts <- str_split(age_text, "/")[[1]]
    age_part <- parts[str_detect(tolower(parts), "week|month|year|day")]
    if (length(age_part) > 0) age_text <- age_part[1]
  }
  age_nums <- suppressWarnings(as.numeric(str_extract_all(age_text, "[0-9]+\\.?[0-9]*")[[1]]))
  age_nums <- age_nums[age_nums > 0]
  age_years <- if (length(age_nums) > 0) {
    age_mean <- mean(age_nums)
    if (str_detect(tolower(age_text), "week")) {
      round(age_mean / 52.18, 4)
    } else if (str_detect(tolower(age_text), "month")) {
      round(age_mean / 12, 4)
    } else {
      round(age_mean, 4)
    }
  } else NA_real_
  
  # ── Group allocation table (dose per route) ──────────────────────────────
  # Some protocols have a "Group Allocation" table with columns:
  #   Group | Treatment | Animal ID | Dose (mg/kg) | ROA | ...
  # We extract dose-per-route from this table as a fallback for detect_blocks.
  group_doses <- list()  # named by lowercase route → "dose_val dose_unit"
  tryCatch({
    # Find the header row containing "Dose" and "ROA" or "Route"
    dose_hdr_rows <- which(apply(raw, 1, function(r) {
      txt <- tolower(paste(na.omit(r), collapse = " "))
      str_detect(txt, "dose.*mg") && str_detect(txt, "roa|route")
    }))
    if (length(dose_hdr_rows) > 0) {
      hdr_row <- dose_hdr_rows[1]
      hdr_vals <- tolower(as.character(raw[hdr_row, ]))
      dose_col <- which(str_detect(hdr_vals, "dose.*mg"))[1]
      roa_col  <- which(str_detect(hdr_vals, "^roa$|route"))[1]
      if (!is.na(dose_col) && !is.na(roa_col)) {
        for (gi in seq(hdr_row + 1, min(hdr_row + 10, nrow(raw)))) {
          roa_val  <- str_trim(as.character(raw[gi, roa_col]))
          dose_val <- str_trim(as.character(raw[gi, dose_col]))
          if (is.na(roa_val) || roa_val == "" || is.na(dose_val) || dose_val == "NA") next
          # Classify route
          route_key <- case_when(
            str_detect(tolower(roa_val), "iv|intravenous") ~ "iv",
            str_detect(tolower(roa_val), "po|oral")        ~ "po",
            str_detect(tolower(roa_val), "sc|subcutaneous") ~ "sc",
            str_detect(tolower(roa_val), "im|intramuscular")~ "im",
            str_detect(tolower(roa_val), "ip|intraperitoneal") ~ "ip",
            TRUE ~ tolower(roa_val)
          )
          group_doses[[route_key]] <- paste(dose_val, "mg/kg")
          message("  [Protocol] Group dose: ", toupper(route_key), " = ", dose_val, " mg/kg")
        }
      }
    }
  }, error = function(e) message("  [Protocol] Could not parse group allocation: ", conditionMessage(e)))
  
  list(
    species     = species,
    gender      = gender,
    weight_kg   = weight_kg,
    age_years   = age_years,
    dose_str    = dose_raw,
    group_doses = group_doses
  )
}


# =============================================================================
# SECTION 2 — LLOQ Extractor
# =============================================================================

#' Scan every cell in a matrix for the first LLOQ value.
#' Handles formats like "LLOQ: 10.3 ng/mL", "LLOQ = 10.39", "LLOQ:10.3".
#'
#' @param mat  Character matrix (raw sheet data).
#' @return Numeric LLOQ or NA.
extract_lloq <- function(mat) {
  pat <- regex("LLOQ\\s*[=:]\\s*([0-9]+\\.?[0-9]*)", ignore_case = TRUE)
  for (v in as.vector(mat)) {
    if (!is.na(v)) {
      m <- str_match(v, pat)
      if (!is.na(m[1, 1])) return(as.numeric(m[1, 2]))
    }
  }
  NA_real_
}


# =============================================================================
# SECTION 3 — Block Detector
# =============================================================================

#' Identify PK data blocks within the raw matrix.
#'
#' Blocks are anchored by title cells that contain BOTH a route keyword AND an
#' explicit dose value (e.g. "Intravenous PK study … at 1 mg/Kg").
#' Shorter labels such as "IV - PK at 1 mg/Kg" from PK-parameter summary tables
#' are also supported.
#' Returns a list of block descriptors sorted left-to-right, top-to-bottom.
#'
#' @param mat  Character matrix (raw sheet, all values as strings).
#' @return List of lists, each with: title, route, dose_val, dose_unit,
#'         title_row (1-based), title_col (1-based), end_col (1-based, exclusive).
detect_blocks <- function(mat) {
  route_pat <- regex(
    "intravenous|\\biv\\b|\\boral\\b|\\bpo\\b|subcutaneous|\\bsc\\b|intramuscular|\\bim\\b|intraperitoneal|\\bip\\b",
    ignore_case = TRUE
  )
  # Require a dose value to be present in the same cell → rejects pure labels
  dose_pat  <- regex("[0-9]+\\.?[0-9]*\\s*(mg/kg|µg/kg|ug/kg|mg/mL|nmol/kg)", ignore_case = TRUE)
  
  # ── Helper to classify route from cell text ──
  classify_route <- function(cell) {
    case_when(
      str_detect(cell, regex("intravenous|\\biv\\b",   ignore_case = TRUE)) ~ "iv",
      str_detect(cell, regex("\\boral\\b|\\bpo\\b",    ignore_case = TRUE)) ~ "po",
      str_detect(cell, regex("subcutaneous|\\bsc\\b",  ignore_case = TRUE)) ~ "sc",
      str_detect(cell, regex("intramuscular|\\bim\\b", ignore_case = TRUE)) ~ "im",
      str_detect(cell, regex("intraperitoneal|\\bip\\b",ignore_case=TRUE))  ~ "ip",
      TRUE ~ "unknown"
    )
  }
  
  blocks <- list()
  seen   <- list()
  
  # ── Pass 1: Original logic — title cells with BOTH route keyword AND dose ──
  for (ri in seq_len(nrow(mat))) {
    for (ci in seq_len(ncol(mat))) {
      cell <- mat[ri, ci]
      if (is.na(cell) || nchar(str_trim(cell)) < 8) next
      if (!str_detect(cell, route_pat))  next
      if (!str_detect(cell, dose_pat))   next   # must contain a dose value
      key <- paste(ri, ci)
      if (key %in% seen) next
      seen <- c(seen, key)
      
      route <- classify_route(cell)
      
      # ── Dose ──
      dose_m    <- str_match(cell, regex(
        "(at\\s+)?([0-9]+\\.?[0-9]*)\\s*(mg/kg|µg/kg|ug/kg|mg/mL|nmol/kg)",
        ignore_case = TRUE
      ))
      dose_val  <- if (!is.na(dose_m[1,1])) dose_m[1,3] else NA_character_
      dose_unit <- if (!is.na(dose_m[1,1])) dose_m[1,4] else "mg/kg"
      
      blocks <- c(blocks, list(list(
        title      = cell,
        route      = route,
        dose_val   = dose_val,
        dose_unit  = dose_unit,
        title_row  = ri,
        title_col  = ci,
        end_col    = ncol(mat)   # will be refined below
      )))
    }
  }
  
  # ── Pass 2 (fallback): If no blocks found, look for short route-only titles ──
  # Handles formats like "route IV dosing", "route PO dosing", "IV - PK", etc.
  # where dose is not embedded in the title cell.
  if (length(blocks) == 0) {
    # Broader pattern: any cell mentioning a route keyword (min 5 chars)
    route_title_pat <- regex(
      "route\\s+(iv|po|sc|im|ip)\\b|\\b(iv|po|sc|im|ip)\\s+(dosing|dose|data|pk|study)",
      ignore_case = TRUE
    )
    for (ri in seq_len(nrow(mat))) {
      for (ci in seq_len(ncol(mat))) {
        cell <- mat[ri, ci]
        if (is.na(cell) || nchar(str_trim(cell)) < 5) next
        if (!str_detect(cell, route_title_pat)) next
        key <- paste(ri, ci)
        if (key %in% seen) next
        seen <- c(seen, key)
        
        route <- classify_route(cell)
        
        blocks <- c(blocks, list(list(
          title      = cell,
          route      = route,
          dose_val   = NA_character_,
          dose_unit  = "mg/kg",
          title_row  = ri,
          title_col  = ci,
          end_col    = ncol(mat)
        )))
      }
    }
    if (length(blocks) > 0)
      message("  [detect_blocks] Used fallback route-title detection (no dose in title cells)")
  }
  
  # ── Pass 3 (last resort): Find "Animal No." headers followed by numeric rows ──
  # For sheets that have no route title at all but have the standard layout
  if (length(blocks) == 0) {
    animal_hdr_pat <- regex("^animal\\s*(no\\.?|number|id|#)", ignore_case = TRUE)
    for (ri in seq_len(nrow(mat))) {
      for (ci in seq_len(ncol(mat))) {
        cell <- mat[ri, ci]
        if (is.na(cell)) next
        if (!str_detect(str_trim(cell), animal_hdr_pat)) next
        key <- paste(ri, ci)
        if (key %in% seen) next
        seen <- c(seen, key)
        
        blocks <- c(blocks, list(list(
          title      = cell,
          route      = "unknown",
          dose_val   = NA_character_,
          dose_unit  = "mg/kg",
          title_row  = ri,
          title_col  = ci,
          end_col    = ncol(mat)
        )))
      }
    }
    if (length(blocks) > 0)
      message("  [detect_blocks] Used Animal No. header fallback detection")
  }
  
  # Sort left-to-right, then top-to-bottom
  if (length(blocks) == 0) return(blocks)
  ord    <- order(sapply(blocks, `[[`, "title_col"), sapply(blocks, `[[`, "title_row"))
  blocks <- blocks[ord]
  
  # ── Deduplicate: per route, keep the block whose title_row is earliest ──
  # (Handles files where both the concentration table and parameter table have
  #  a route+dose title cell — we want the concentration table one.)
  keep   <- logical(length(blocks))
  seen_r <- character(0)
  for (i in seq_along(blocks)) {
    b <- blocks[[i]]
    rkey <- b$route
    if (!(rkey %in% seen_r)) {
      keep[i]   <- TRUE
      seen_r    <- c(seen_r, rkey)
    }
  }
  blocks <- blocks[keep]
  
  # ── Assign end_col ──
  # For stacked blocks (same column), end_col stays as ncol(mat).
  # For side-by-side blocks (different columns), each ends before the next.
  for (i in seq_along(blocks)) {
    if (i < length(blocks) && blocks[[i + 1]]$title_col > blocks[[i]]$title_col) {
      blocks[[i]]$end_col <- blocks[[i + 1]]$title_col - 1
    } else {
      blocks[[i]]$end_col <- ncol(mat)
    }
  }
  
  blocks
}


# =============================================================================
# SECTION 4 — Time-Point Row Finder
# =============================================================================

#' Scan downward from a block's title row to find the row containing
#' numeric time-point column headers.
#'
#' @param mat        Character matrix.
#' @param from_row   1-based starting row (title row).
#' @param start_col  1-based column of the animal-ID column for this block.
#' @param max_scan   Maximum rows to scan below title.
#' @return 1-based row index of the time-point header row, or NA.
find_timepoint_row <- function(mat, from_row, start_col, max_scan = 8) {
  for (ri in seq(from_row, min(from_row + max_scan, nrow(mat)))) {
    # Time points live in columns to the RIGHT of the animal column
    candidates <- mat[ri, seq(start_col + 1, ncol(mat))]
    n_numeric  <- sum(!is.na(suppressWarnings(as.numeric(candidates))))
    if (n_numeric >= 2) return(ri)
  }
  NA_integer_
}


# =============================================================================
# SECTION 5 — Animal Row Finder
# =============================================================================

#' Find rows that contain individual animal observation data.
#' Skips summary rows (Mean, SD, CV%, etc.) and note rows.
#'
#' @param mat         Character matrix.
#' @param from_row    1-based row to start searching (time-point row).
#' @param animal_col  1-based column index for animal IDs.
#' @param max_scan    Maximum rows to scan downward.
#' @return Integer vector of 1-based row indices.
find_animal_rows <- function(mat, from_row, animal_col, max_scan = 20) {
  # Pattern for named animal IDs (e.g. "Rat-1", "Mouse2", "Dog 3")
  # Must have ≥2 letters before the number, or a separator, to avoid matching
  # PK parameter labels like "C0", "Vd", "t½", "CL", "Ke" etc.
  named_animal_pat <- regex(
    "^[A-Za-z]{3,}[-_ ]?\\d+$|^[A-Za-z]{2,}[-_]\\d+$|^[A-Za-z]{2,}\\s+\\d+$",
    ignore_case = TRUE
  )
  # Pattern for plain numeric animal IDs (e.g. "1", "12", "001")
  numeric_id_pat <- regex("^\\d+$")
  
  # Rows to skip (summaries / notes)
  skip_pat <- regex(
    "^(mean|sd|cv|%cv|median|sem|n\\s*=|lloq|blq|note|animal\\s+no|plasma|time|auc|cmax|c0|cl|vd|vss|t1\\/2|tmax|tlast|parameter|\\(h\\)|\\(ng)",
    ignore_case = TRUE
  )
  
  rows <- integer(0)
  for (ri in seq(from_row + 1, min(from_row + max_scan, nrow(mat)))) {
    cell <- mat[ri, animal_col]
    if (is.na(cell) || nchar(str_trim(cell)) == 0) next
    cell_trimmed <- str_trim(cell)
    if (str_detect(cell_trimmed, skip_pat)) next
    if (str_detect(cell_trimmed, named_animal_pat) || str_detect(cell_trimmed, numeric_id_pat)) {
      rows <- c(rows, ri)
    }
  }
  rows
}


# =============================================================================
# SECTION 6 — Concentration Value Cleaner
# =============================================================================

#' Clean a vector of raw concentration strings into numerics.
#'
#' Handles:
#'   - "BLQ", "<LLOQ", "ND", "NA", "N/A", "NR"  → NA  (below quantitation)
#'   - "18.403*"                                  → 18.403 (flag stripped)
#'   - Any remaining non-numeric                  → NA with a warning
#'
#' @param x  Character vector of raw concentration values.
#' @return Numeric vector (same length, NAs where non-quantifiable).
clean_conc <- function(x) {
  x <- str_trim(as.character(x))
  # Strip annotation markers (* # @ ! +)
  x_clean <- str_remove_all(x, "[\\*#@!\\+]+")
  x_clean <- str_trim(x_clean)
  # Mark below-quantitation tokens as NA
  blq_pat <- regex("^(BLQ|BLOQ|<LLOQ|<LLQ|ND|N\\.D\\.|NA|N/A|NR|not\\s+det)$", ignore_case = TRUE)
  x_clean[str_detect(x_clean, blq_pat)] <- NA_character_
  suppressWarnings(as.numeric(x_clean))
}


# =============================================================================
# SECTION 7 — Single Block Extractor
# =============================================================================

#' Extract and reshape one IV/PO block into a long-format tibble.
#'
#' @param mat         Character matrix (full sheet).
#' @param block       Block descriptor from detect_blocks() (includes end_col).
#' @param time_row    1-based row index of time-point header.
#' @param animal_rows 1-based row indices of individual animal rows.
#' @param lloq        Numeric LLOQ value (or NA).
#' @return Tibble with columns: SubjectId, Route, DoseVal, DoseUnit,
#'         Time, Measurement, LLOQ, FlaggedValue.
extract_block <- function(mat, block, time_row, animal_rows, lloq) {
  
  animal_col <- block$title_col
  end_col    <- block$end_col   # exclusive upper bound for this block's columns
  
  # ── Identify time-point columns within this block's column range ─────────
  col_range  <- seq(animal_col + 1, min(end_col, ncol(mat)))
  tp_raw     <- mat[time_row, col_range]
  tp_numeric <- suppressWarnings(as.numeric(tp_raw))
  
  # Keep only contiguous numeric time points (stop at first NA gap)
  first_gap <- which(is.na(tp_numeric))[1]
  valid_idx  <- if (!is.na(first_gap) && first_gap > 1) {
    seq_len(first_gap - 1)
  } else {
    which(!is.na(tp_numeric))
  }
  
  if (length(valid_idx) == 0) {
    warning("Block [", block$route, "] at row ", block$title_row, ": no numeric time points found.")
    return(NULL)
  }
  
  time_cols <- col_range[valid_idx]
  time_vals <- tp_numeric[valid_idx]
  
  # ── Build long-format rows ────────────────────────────────────────────────
  map_dfr(animal_rows, function(ri) {
    animal_id  <- str_trim(mat[ri, animal_col])
    concs_raw  <- mat[ri, time_cols]
    concs_num  <- clean_conc(concs_raw)
    flagged    <- str_detect(as.character(concs_raw), "\\*") & !is.na(clean_conc(concs_raw))
    
    tibble(
      SubjectId    = animal_id,
      Route        = block$route,
      DoseVal      = block$dose_val,
      DoseUnit     = block$dose_unit,
      Time         = time_vals,
      Measurement  = concs_num,
      LLOQ         = lloq,
      FlaggedValue = flagged
    )
  })
}


# =============================================================================
# SECTION 8 — Master Converter: Single File
# =============================================================================

#' Convert one PK Excel file to the PK-Sim/OSPS long format.
#'
#' @param pk_file       Path to the PK data Excel file.
#' @param pk_sheet      Name/index of the sheet with raw PK table data.
#' @param proto_sheet   Name/index of the study protocol sheet (NULL to skip).
#' @param study_id      Study identifier string (defaults to file name stem).
#' @param molecule      Molecule/compound name.
#' @param mw            Molecular weight in Da (NA if unknown).
#' @param organ         Tissue/organ string (default "Blood").
#' @param compartment   Compartment string (default "Plasma").
#' @param meas_unit     Measurement unit string (default "ng/mL").
#' @param time_unit     Time unit string (default "h").
#' @param gender        Override gender ("MALE"/"FEMALE"); NULL = auto-detect.
#' @param weight_kg     Override body weight in kg; NULL = auto-detect.
#' @param age_years     Override age in years; NULL = auto-detect.
#' @param dose_override Named list to override doses per route, e.g.
#'                      list(iv = "1 mg/kg", po = "5 mg/kg").
#' @return Tibble in the target long format.
convert_pk_file <- function(
    pk_file,
    pk_sheet      = "PK data",
    proto_sheet   = "Study Protocol",
    study_id      = NULL,
    molecule      = NULL,
    mw            = NA_real_,
    organ         = "Blood",
    compartment   = "Plasma",
    meas_unit     = "ng/mL",
    time_unit     = "h",
    gender        = NULL,
    weight_kg     = NULL,
    age_years     = NULL,
    dose_override = list()
) {
  
  stopifnot(file.exists(pk_file))
  message("\n── Processing: ", basename(pk_file), " ──")
  
  # ── Default study_id from filename ──────────────────────────────────────
  if (is.null(study_id)) {
    study_id <- tools::file_path_sans_ext(basename(pk_file))
    study_id <- str_remove(study_id, "_\\d{2}_[A-Za-z]{3}_\\d{4}.*$")  # trim date suffix
  }
  
  # ── Protocol metadata ────────────────────────────────────────────────────
  proto <- if (!is.null(proto_sheet)) {
    parse_protocol(pk_file, proto_sheet)
  } else {
    list(species = NA, gender = NA, weight_kg = NA, age_years = NA,
         group_doses = list())
  }
  
  species   <- coalesce(proto$species,   "Unknown")
  gender    <- coalesce(gender,   proto$gender)
  weight_kg <- coalesce(weight_kg, proto$weight_kg)
  age_years <- coalesce(age_years, proto$age_years)
  group_doses <- if (!is.null(proto$group_doses)) proto$group_doses else list()
  
  species   <- coalesce(proto$species,   "Unknown")
  gender    <- coalesce(gender,   proto$gender)
  weight_kg <- coalesce(weight_kg, proto$weight_kg)
  age_years <- coalesce(age_years, proto$age_years)
  
  message("  Species: ", species, "  |  Gender: ", coalesce(gender, "?"),
          "  |  BW: ", coalesce(as.character(weight_kg), "?"), " kg",
          "  |  Age: ", coalesce(as.character(age_years), "?"), " yr")
  
  # ── Read PK sheet as raw character matrix ────────────────────────────────
  raw_df  <- read_excel(pk_file, sheet = pk_sheet, col_names = FALSE, .name_repair = "unique_quiet")
  raw_mat <- apply(raw_df, 2, as.character)   # full character matrix
  
  # ── Global LLOQ ─────────────────────────────────────────────────────────
  lloq <- extract_lloq(raw_mat)
  message("  LLOQ: ", ifelse(is.na(lloq), "not found", paste(lloq, meas_unit)))
  
  # ── Detect blocks ────────────────────────────────────────────────────────
  blocks <- detect_blocks(raw_mat)
  if (length(blocks) == 0) stop("No PK blocks detected in '", pk_sheet, "'. Check route keywords in title rows.")
  
  route_labels <- sapply(blocks, `[[`, "route")
  message("  Blocks detected (", length(blocks), "): ", paste(toupper(route_labels), collapse = ", "))
  
  # ── Fill missing dose values from protocol group allocation table ────────
  if (length(group_doses) > 0) {
    for (i in seq_along(blocks)) {
      rt <- blocks[[i]]$route
      if (is.na(blocks[[i]]$dose_val) && rt %in% names(group_doses)) {
        dose_str <- group_doses[[rt]]
        d_m <- str_match(dose_str, "([0-9]+\\.?[0-9]*)\\s*(mg/kg|µg/kg|ug/kg|mg/mL|nmol/kg)?")
        if (!is.na(d_m[1,1])) {
          blocks[[i]]$dose_val  <- d_m[1,2]
          blocks[[i]]$dose_unit <- coalesce(d_m[1,3], "mg/kg")
          message("  [", toupper(rt), "] Dose filled from protocol: ", d_m[1,2], " ", coalesce(d_m[1,3], "mg/kg"))
        }
      }
    }
  }
  
  # ── Extract each block ───────────────────────────────────────────────────
  all_data <- map_dfr(blocks, function(b) {
    
    # ── Find the actual animal column near this block ──
    # In some formats the title cell is offset from the data columns
    # (e.g. "route IV dosing" in col E but "Animal No." in col A).
    # Scan nearby rows for "Animal No." header to find the real anchor column.
    animal_col <- b$title_col
    animal_hdr_pat <- regex("^animal\\s*(no\\.?|number|id|#)", ignore_case = TRUE)
    for (scan_ri in seq(max(1, b$title_row - 1), min(b$title_row + 4, nrow(raw_mat)))) {
      for (scan_ci in seq(max(1, b$title_col - 10), min(b$title_col + 2, ncol(raw_mat)))) {
        cell_val <- raw_mat[scan_ri, scan_ci]
        if (!is.na(cell_val) && str_detect(str_trim(cell_val), animal_hdr_pat)) {
          animal_col <- scan_ci
          break
        }
      }
      if (animal_col != b$title_col) break
    }
    
    tp_row <- find_timepoint_row(raw_mat, b$title_row, animal_col)
    if (is.na(tp_row)) {
      warning("  [", toupper(b$route), "] Could not find time-point row — block skipped.")
      return(NULL)
    }
    
    an_rows <- find_animal_rows(raw_mat, tp_row, animal_col)
    if (length(an_rows) == 0) {
      warning("  [", toupper(b$route), "] No individual animal rows found — block skipped.")
      return(NULL)
    }
    
    # Count time points within this block's column range (respects end_col)
    tp_col_range <- seq(animal_col + 1, min(b$end_col, ncol(raw_mat)))
    tp_vals_raw  <- raw_mat[tp_row, tp_col_range]
    tp_nums_raw  <- suppressWarnings(as.numeric(tp_vals_raw))
    first_gap    <- which(is.na(tp_nums_raw))[1]
    n_tp <- if (!is.na(first_gap) && first_gap > 1) first_gap - 1 else sum(!is.na(tp_nums_raw))
    
    message("  [", toupper(b$route), "] ", length(an_rows), " subjects × ", n_tp, " time points")
    
    # Override block's title_col with the actual animal column for extraction
    b_adj <- b
    b_adj$title_col <- animal_col
    
    extract_block(raw_mat, b_adj, tp_row, an_rows, lloq)
  })
  
  if (is.null(all_data) || nrow(all_data) == 0) stop("No data extracted.")
  
  # Warn about asterisk-flagged values
  n_flagged <- sum(all_data$FlaggedValue, na.rm = TRUE)
  if (n_flagged > 0) {
    message("  Note: ", n_flagged, " value(s) had annotation markers (*) — numeric values retained, ",
            "flags recorded in 'FlaggedValue' column.")
  }
  
  # ── Apply dose overrides ─────────────────────────────────────────────────
  if (length(dose_override) > 0) {
    for (rt in names(dose_override)) {
      all_data <- all_data %>%
        mutate(
          DoseVal  = if_else(tolower(Route) == tolower(rt),
                             str_extract(dose_override[[rt]], "[0-9]+\\.?[0-9]*"),
                             DoseVal),
          DoseUnit = if_else(tolower(Route) == tolower(rt),
                             str_extract(dose_override[[rt]], "mg/kg|µg/kg|ug/kg|mg/mL|nmol/kg"),
                             DoseUnit)
        )
    }
  }
  
  # ── Assemble target long format ──────────────────────────────────────────
  out <- all_data %>%
    mutate(
      DoseStr = case_when(
        !is.na(DoseVal) & !is.na(DoseUnit) ~ paste(DoseVal, DoseUnit),
        !is.na(DoseVal)                    ~ DoseVal,
        TRUE                               ~ NA_character_
      )
    ) %>%
    transmute(
      `Study Id`          = study_id,
      `Subject Id`        = SubjectId,
      `Organ`             = organ,
      `Compartment`       = compartment,
      `Species`           = species,
      `Gender`            = gender,
      `Dose`              = DoseStr,
      `Molecule`          = coalesce(molecule, study_id),
      `Molecular Weight`  = mw,
      `Time`              = Time,
      `Time unit`         = time_unit,
      `Measurement`       = Measurement,
      `Measurement unit`  = meas_unit,
      `Error`             = NA_real_,
      `LLOQ`              = LLOQ,
      `Route`             = Route,
      `Group Id`          = NA_character_,
      `Population`        = NA_character_,
      `Weight [kg]`       = weight_kg,
      `Height [cm]`       = NA_real_,
      `Age [year(s)]`     = age_years,
      # Internal diagnostic column (remove before final export if desired)
      `.FlaggedValue`     = FlaggedValue
    ) %>%
    arrange(Route, `Subject Id`, Time)
  
  message("  Output rows: ", nrow(out),
          " (", sum(!is.na(out$Measurement)), " quantifiable, ",
          sum(is.na(out$Measurement)), " BLQ/NA)")
  out
}


# =============================================================================
# SECTION 9 — Batch Converter: Multiple Files
# =============================================================================

#' Convert multiple PK files and write a single formatted Excel workbook.
#'
#' Each file produces one or more sheets in the output workbook, named as
#' "<StudyId>.<Species>.<Route>" (e.g. "ESN-2987.Dog.IV").
#' A "Combined" sheet with all data merged is also added.
#'
#' @param file_list   List of named lists; each must have `pk_file` and may
#'                    include any argument accepted by convert_pk_file().
#' @param output_xlsx Path for the output Excel file (NULL = no file written).
#' @param ...         Default arguments passed to convert_pk_file() for all files.
#' @return Named list of tibbles (one per sheet).
convert_pk_batch <- function(file_list, output_xlsx = NULL, ...) {
  
  all_sheets <- list()
  
  for (entry in file_list) {
    args  <- modifyList(list(...), entry)
    df    <- tryCatch(
      do.call(convert_pk_file, args),
      error = function(e) {
        message("  ERROR processing ", entry$pk_file, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(df) || nrow(df) == 0) next
    
    # One sheet per route within this file
    for (rt in unique(df$Route)) {
      species_lbl <- unique(df$Species)[1]
      sheet_name  <- paste(unique(df$`Study Id`)[1], species_lbl, toupper(rt), sep = ".")
      sheet_name  <- str_trunc(sheet_name, 31)  # Excel sheet name limit
      all_sheets[[sheet_name]] <- df %>% filter(Route == rt)
    }
  }
  
  if (length(all_sheets) == 0) {
    message("No data to write.")
    return(invisible(list()))
  }
  
  # Combined sheet (drop internal flag column)
  combined <- bind_rows(all_sheets) %>%
    select(-any_of(".FlaggedValue")) %>%
    arrange(`Study Id`, Species, Route, `Subject Id`, Time)
  all_sheets[["Combined"]] <- combined
  
  message("\n── Summary ──────────────────────────────────────────────────")
  message("  Total rows  : ", nrow(combined))
  message("  Quantifiable: ", sum(!is.na(combined$Measurement)))
  message("  BLQ / NA    : ", sum(is.na(combined$Measurement)))
  message("  Sheets      : ", paste(names(all_sheets), collapse = ", "))
  
  if (!is.null(output_xlsx)) write_pk_excel(all_sheets, output_xlsx)
  
  invisible(all_sheets)
}


# =============================================================================
# SECTION 10 — Excel Output Writer
# =============================================================================

#' Write the converted long-format data to a styled Excel workbook.
#'
#' @param sheets_list Named list of tibbles (output of convert_pk_batch).
#' @param output_file Path for the .xlsx file to create.
write_pk_excel <- function(sheets_list, output_file) {
  
  wb <- createWorkbook()
  
  # Style definitions
  hdr_style <- createStyle(
    fontName      = "Arial", fontSize = 10, fontColour = "white",
    fgFill        = "#2E4057", halign = "CENTER", valign = "CENTER",
    textDecoration = "bold", wrapText = TRUE,
    border = "TopBottomLeftRight", borderColour = "#FFFFFF"
  )
  even_style <- createStyle(fontName = "Arial", fontSize = 9, fgFill = "#F2F6FB")
  odd_style  <- createStyle(fontName = "Arial", fontSize = 9, fgFill = "#FFFFFF")
  blq_style  <- createStyle(fontName = "Arial", fontSize = 9,
                            fgFill = "#FFF3CD", fontColour = "#856404")
  flag_style <- createStyle(fontName = "Arial", fontSize = 9,
                            fgFill = "#FFF0E0", fontColour = "#7A4100")
  num_style  <- createStyle(numFmt = "0.0000")
  num_style2 <- createStyle(numFmt = "0.000")
  
  for (sname in names(sheets_list)) {
    df <- sheets_list[[sname]]
    
    # Remove internal diagnostic column for output
    has_flag_col <- ".FlaggedValue" %in% names(df)
    flag_vec     <- if (has_flag_col) df$.FlaggedValue else rep(FALSE, nrow(df))
    df_out       <- df %>% select(-any_of(".FlaggedValue"))
    
    addWorksheet(wb, sheetName = sname, gridLines = TRUE)
    
    writeData(wb, sname, df_out, startRow = 1, startCol = 1,
              headerStyle = hdr_style, borders = "all", borderColour = "#CCCCCC")
    
    # Striped row formatting
    for (ri in seq_len(nrow(df_out))) {
      sty <- if (ri %% 2 == 0) even_style else odd_style
      addStyle(wb, sname, sty, rows = ri + 1, cols = seq_len(ncol(df_out)),
               gridExpand = TRUE, stack = FALSE)
    }
    
    # Highlight BLQ rows (missing Measurement) in amber
    blq_rows <- which(is.na(df_out$Measurement)) + 1
    if (length(blq_rows) > 0)
      addStyle(wb, sname, blq_style, rows = blq_rows, cols = seq_len(ncol(df_out)),
               gridExpand = TRUE, stack = FALSE)
    
    # Highlight flagged-value rows in light orange
    flag_rows <- which(flag_vec) + 1
    if (length(flag_rows) > 0)
      addStyle(wb, sname, flag_style, rows = flag_rows, cols = seq_len(ncol(df_out)),
               gridExpand = TRUE, stack = FALSE)
    
    # Number formats for numeric columns
    time_col <- which(names(df_out) == "Time")
    meas_col <- which(names(df_out) == "Measurement")
    mw_col   <- which(names(df_out) == "Molecular Weight")
    bw_col   <- which(names(df_out) == "Weight [kg]")
    
    if (length(time_col) > 0 && nrow(df_out) > 0)
      addStyle(wb, sname, num_style2, rows = seq(2, nrow(df_out) + 1),
               cols = time_col, gridExpand = TRUE, stack = TRUE)
    if (length(meas_col) > 0 && nrow(df_out) > 0)
      addStyle(wb, sname, num_style,  rows = seq(2, nrow(df_out) + 1),
               cols = meas_col, gridExpand = TRUE, stack = TRUE)
    
    # Column widths (based on header + a little padding)
    widths <- pmax(nchar(names(df_out)) + 3, 10)
    setColWidths(wb, sname, cols = seq_along(df_out), widths = widths)
    
    freezePane(wb, sname, firstRow = TRUE)
    showGridLines(wb, sname, showGridLines = FALSE)
  }
  
  saveWorkbook(wb, output_file, overwrite = TRUE)
  message("\n  Saved → ", normalizePath(output_file, mustWork = FALSE))
}


# =============================================================================
# SECTION 11 — Validation Helper
# =============================================================================

#' Print a concise QC summary comparing the converted output to expectations.
#'
#' @param df  Long-format tibble (output of convert_pk_file or from a sheet list).
validate_output <- function(df) {
  df <- df %>% select(-any_of(".FlaggedValue"))
  
  target_cols <- c("Study Id", "Subject Id", "Organ", "Compartment", "Species",
                   "Gender", "Dose", "Molecule", "Molecular Weight", "Time",
                   "Time unit", "Measurement", "Measurement unit", "Error",
                   "LLOQ", "Route", "Group Id", "Population",
                   "Weight [kg]", "Height [cm]", "Age [year(s)]")
  
  missing_cols  <- setdiff(target_cols, names(df))
  extra_cols    <- setdiff(names(df), target_cols)
  
  cat("\n══════════════════════════════════════════════════════\n")
  cat(" PK Data Validation Report\n")
  cat("══════════════════════════════════════════════════════\n")
  cat(sprintf("  %-22s %s\n", "Total rows:", nrow(df)))
  cat(sprintf("  %-22s %s\n", "Quantifiable rows:", sum(!is.na(df$Measurement))))
  cat(sprintf("  %-22s %s\n", "BLQ / NA rows:", sum(is.na(df$Measurement))))
  cat(sprintf("  %-22s %s\n", "Missing target cols:", if (length(missing_cols)) paste(missing_cols, collapse = ", ") else "none ✓"))
  cat(sprintf("  %-22s %s\n", "Extra cols:", if (length(extra_cols)) paste(extra_cols, collapse = ", ") else "none"))
  
  cat("\n  Per-route summary:\n")
  summary_tbl <- df %>%
    group_by(Route, Species) %>%
    summarise(
      Subjects   = n_distinct(`Subject Id`),
      TimePoints = n_distinct(Time),
      Rows       = n(),
      BLQ_rows   = sum(is.na(Measurement)),
      LLOQ       = unique(na.omit(LLOQ))[1],
      .groups    = "drop"
    )
  print(as.data.frame(summary_tbl))
  
  cat("\n  Column presence:\n")
  col_status <- tibble(
    Column  = target_cols,
    Present = target_cols %in% names(df),
    AllNA   = sapply(target_cols, function(c) if (c %in% names(df)) all(is.na(df[[c]])) else NA)
  )
  print(as.data.frame(col_status), row.names = FALSE)
  cat("══════════════════════════════════════════════════════\n\n")
  
  invisible(df)
}




############# Individuals sheet developer

# ── Helper: read population CSVs ─────────────────────────────────────────────
# Dog / Mouse / NHP / Rat have two PK-Sim comment lines before the real header;
# Human does not.
read_pop <- function(path, skip_lines = 0) {
  read_csv(path, skip = skip_lines, show_col_types = FALSE)
}


# ── Helper: extract common biometric fields ───────────────────────────────────
extract_biometrics <- function(pop_row, indiv_id, species) {
  tibble(
    IndividualId          = indiv_id,
    Species               = species,
    Population            = pop_row[["Population Name"]],
    Gender                = pop_row[["Gender"]],
    `Weight [kg]`         = pop_row[["Organism|Weight [kg]"]],
    # Height only available for Human (stored in dm → convert to cm)
    `Height [cm]`         = if ("Organism|Height [dm]" %in% names(pop_row))
      pop_row[["Organism|Height [dm]"]] * 10
    else NA_real_,
    # Age only available for Human
    `Age [year(s)]`       = if ("Organism|Age [year(s)]" %in% names(pop_row))
      pop_row[["Organism|Age [year(s)]"]]
    else NA_real_,
    `Protein Ontogenies`  = NA_character_
  )
}


# ── Build per-individual parameter sheets ────────────────────────────────────
# Rat and Mouse receive the CLh parameter; others get an empty parameter sheet.
make_param_sheet <- function(has_param) {
  if (has_param) {
    tibble(
      `Container Path` = param_path,
      `Parameter Name` = param_name,
      Value            = param_value,
      Units            = param_unit
    )
  } else {
    tibble(
      `Container Path` = character(0),
      `Parameter Name` = character(0),
      Value            = numeric(0),
      Units            = character(0)
    )
  }
}

############################ Generate model parameters ############

#' Convert ESN molecule ID to ENV sheet name
#' e.g. "ESN-0044172" -> "ENV-44172"
convert_mol_id <- function(esn_id) {
  numeric_part <- sub("^ESN-00", "", esn_id)
  paste0("ENV-", numeric_part)
}

#' Calculate solubility (mg/L) from logS and molecular weight
#' Formula: solubility = 10^logS * MW
calc_solubility <- function(log_s, mw) {
  if (is.na(log_s) || is.na(mw)) return(NA_real_)
  round((10^log_s) * mw, 4)
}

#' Build parameter rows for one molecule
#' @param mol_row A single-row data.frame from the input table
#' @return data.frame with columns: ContainerPath, ParameterName, Value, Units
build_molecule_params <- function(mol_row) {
  mw        <- mol_row[["Molecular weight (g/mol)"]]
  log_d     <- mol_row[["log D"]]
  log_s     <- mol_row[["log S"]]
  pka_acid  <- mol_row[["pKa (Acidic)"]]
  pka_basic <- mol_row[["pKa (Basic)"]]
  
  solubility <- calc_solubility(log_s, mw)
  
  rows <- list()
  
  add_row <- function(param, value, units = "") {
    rows[[length(rows) + 1]] <<- data.frame(
      ContainerPath = CONTAINER_PATH,
      ParameterName = param,
      Value         = ifelse(is.na(value), "", as.character(value)),
      Units         = units,
      stringsAsFactors = FALSE
    )
  }
  
  add_row("Lipophilicity",  log_d, "Log Units")
  add_row("pKa value 0",    pka_acid)
  if (!is.na(pka_basic))
    add_row("pKa value 1",  pka_basic)
  add_row("Fraction unbound (plasma, reference value)", NA)
  add_row("Molecular weight",                           mw,         "g/mol")
  add_row("Solubility at reference pH",                 solubility, "mg/L")
  add_row("Reference pH",                               DEFAULT_REF_PH)
  add_row("Specific intestinal permeability (transcellular)", NA, "cm/min")
  add_row("Permeability",                               NA, "cm/min")
  
  do.call(rbind, rows)
}

#' Write the empty Global sheet
write_global_sheet <- function(wb) {
  addWorksheet(wb, "Global")
  headers <- c("Container Path", "Parameter Name", "Value", "Units")
  writeData(wb, "Global", as.data.frame(t(headers)), startRow = 1, colNames = FALSE)
  setColWidths(wb, "Global", cols = 1, widths = 22)
  setColWidths(wb, "Global", cols = 2, widths = 50)
  setColWidths(wb, "Global", cols = 3, widths = 14)
  setColWidths(wb, "Global", cols = 4, widths = 12)
}

#' Write a single molecule parameter sheet into the workbook
write_molecule_sheet <- function(wb, sheet_name, param_df) {
  addWorksheet(wb, sheet_name)
  
  headers <- c("Container Path", "Parameter Name", "Value", "Units")
  writeData(wb, sheet_name, as.data.frame(t(headers)), startRow = 1, colNames = FALSE)
  
  if (nrow(param_df) > 0) {
    writeData(wb, sheet_name, param_df, startRow = 2, colNames = FALSE)
  }
  
  setColWidths(wb, sheet_name, cols = 1, widths = 22)
  setColWidths(wb, sheet_name, cols = 2, widths = 50)
  setColWidths(wb, sheet_name, cols = 3, widths = 14)
  setColWidths(wb, sheet_name, cols = 4, widths = 12)
}


# ==============================================================================
# FUNCTIONS for editing ModelParameters
# ==============================================================================

#' Check if a value is blank (NA, empty string, or literal "NA")
is_blank <- function(x) {
  is.na(x) | trimws(as.character(x)) == "" | x == "NA"
}

#' Read a single compound sheet (skip header row)
#' @return data.frame with ContainerPath, ParameterName, Value, Units
read_param_sheet <- function(path, sheet) {
  df <- read_excel(path, sheet = sheet, col_names = FALSE,
                   .name_repair = "unique_quiet")
  if (nrow(df) < 2) {
    return(data.frame(
      ContainerPath = character(), ParameterName = character(),
      Value = character(), Units = character(), stringsAsFactors = FALSE
    ))
  }
  df <- df[-1, ]
  colnames(df) <- c("ContainerPath", "ParameterName", "Value", "Units")
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df$Value[is.na(df$Value)] <- ""
  df$Units[is.na(df$Units)] <- ""
  df
}

#' Scan all compound sheets and extract rows with blank Values
#' @return data.frame: Compound, ContainerPath, ParameterName, Value, Units
scan_blanks <- function(path) {
  sheets <- excel_sheets(path)
  sheets <- sheets[sheets != "Global"]
  
  blanks <- list()
  for (s in sheets) {
    df <- read_param_sheet(path, s)
    blank_mask <- is_blank(df$Value)
    if (any(blank_mask)) {
      bdf <- df[blank_mask, , drop = FALSE]
      bdf$Compound <- s
      blanks[[length(blanks) + 1]] <- bdf[, c("Compound", "ContainerPath",
                                              "ParameterName", "Value", "Units")]
    }
  }
  
  if (length(blanks) == 0) return(data.frame())
  do.call(rbind, blanks)
}

#' Write the blank-parameter template CSV for the user to fill in
#' @param blanks_df  data.frame from scan_blanks()
#' @param out_csv    Output CSV file path
write_fill_template <- function(blanks_df, out_csv) {
  blanks_df$Value <- ""
  write.csv(blanks_df, out_csv, row.names = FALSE, quote = TRUE)
}

#' Read the user-completed CSV and return only rows with non-blank Value
#' @return data.frame: Compound, ContainerPath, ParameterName, Value, Units
read_filled_csv <- function(csv_path) {
  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  required_cols <- c("Compound", "ContainerPath", "ParameterName", "Value")
  missing <- setdiff(required_cols, colnames(df))
  if (length(missing) > 0) {
    stop("CSV missing required columns: ", paste(missing, collapse = ", "))
  }
  if (!"Units" %in% colnames(df)) df$Units <- ""
  df$Value <- trimws(as.character(df$Value))
  df$Units <- trimws(as.character(df$Units))
  df <- df[!is_blank(df$Value), , drop = FALSE]
  df
}

#' Apply filled values from CSV to a compound's parameter sheet
#' @param param_df  data.frame of existing parameters for one compound
#' @param fills     data.frame of filled values (filtered to this compound)
#' @return list(df = updated data.frame, log = character vector of actions)
apply_fills <- function(param_df, fills) {
  log_msgs <- character()
  
  for (i in seq_len(nrow(fills))) {
    f <- fills[i, ]
    match_idx <- which(param_df$ParameterName == f$ParameterName &
                         param_df$ContainerPath == f$ContainerPath)
    
    if (length(match_idx) > 0) {
      idx <- match_idx[1]
      param_df$Value[idx] <- f$Value
      if (f$Units != "" && is_blank(param_df$Units[idx])) {
        param_df$Units[idx] <- f$Units
      }
      log_msgs <- c(log_msgs, sprintf("  filled: %s = %s", f$ParameterName, f$Value))
    } else {
      new_row <- data.frame(
        ContainerPath = f$ContainerPath, ParameterName = f$ParameterName,
        Value = f$Value, Units = f$Units, stringsAsFactors = FALSE
      )
      param_df <- rbind(param_df, new_row)
      log_msgs <- c(log_msgs, sprintf("  added:  %s = %s", f$ParameterName, f$Value))
    }
  }
  
  list(df = param_df, log = log_msgs)
}

#' Write an updated parameter sheet into the workbook
write_updated_sheet <- function(wb, sheet_name, param_df) {
  addWorksheet(wb, sheet_name)
  headers <- c("Container Path", "Parameter Name", "Value", "Units")
  writeData(wb, sheet_name, as.data.frame(t(headers)), startRow = 1, colNames = FALSE)
  if (nrow(param_df) > 0) {
    writeData(wb, sheet_name, param_df, startRow = 2, colNames = FALSE)
  }
  setColWidths(wb, sheet_name, cols = 1, widths = 50)
  setColWidths(wb, sheet_name, cols = 2, widths = 50)
  setColWidths(wb, sheet_name, cols = 3, widths = 18)
  setColWidths(wb, sheet_name, cols = 4, widths = 14)
}

# ---- Bulk read simulation result CSVs ----------------------------------------

#' Read all scenario CSV files from a results folder, excluding population CSVs.
#' Adds a Scenario_name column derived from the filename.
#'
#' @param results_dir Path to folder containing simulation output CSVs
#' @return A single data.frame with all scenarios bound together
read_all_results <- function(results_dir) {
  
  # List all CSVs, exclude population files
  all_csv <- list.files(results_dir, pattern = "\\.csv$", full.names = TRUE)
  result_csv <- all_csv[!grepl("_population\\.csv$", all_csv)]
  
  message("Found ", length(result_csv), " result CSV files")
  
  # Read each file, tag with scenario name from filename
  all_results <- lapply(result_csv, function(f) {
    scenario_name <- tools::file_path_sans_ext(basename(f))
    df <- read.csv(f, check.names = FALSE)
    df$Scenario_name <- scenario_name
    df
  })
  
  do.call(rbind, all_results)
}

# ---- Extract MW from ModelParameters -----------------------------------------

#' Read molecular weight for each compound from ModelParameters.xlsx
#'
#' @param model_params_path Path to ModelParameters.xlsx
#' @return A data.frame with columns: CompoundId, MW
get_mw_table <- function(model_params_path) {
  sheets <- excel_sheets(model_params_path)
  compound_sheets <- sheets[sheets != "Global"]
  
  do.call(rbind, lapply(compound_sheets, function(cid) {
    df <- read_excel(model_params_path, sheet = cid)
    mw_row <- df[df$`Parameter Name` == "Molecular weight", ]
    data.frame(
      CompoundId = cid,
      MW = as.numeric(mw_row$Value[1]),
      stringsAsFactors = FALSE
    )
  }))
}

# ---- Read and process results ------------------------------------------------

#' Process simulation results: parse scenario name, convert units, and merge MW.
#'
#' @param results_df        A data.frame of combined simulation results
#' @param model_params_path Path to ModelParameters.xlsx (for MW lookup)
#' @param tissue_catalog    Optional named list (from TISSUE_CATALOG) of selected
#'                          tissues; each entry must have `grep_pattern` and the
#'                          list name is used as the Tissue label.  When NULL,
#'                          falls back to legacy single-plasma behaviour.
#' @return A long-format data.frame with Time_hr, Conc_umolL, Conc_ngml,
#'         Tissue, Scenario_name, CompoundId, Species, Route
process_results <- function(results_df, model_params_path, tissue_catalog = NULL) {

  mw_table <- get_mw_table(model_params_path)

  names(results_df)[names(results_df) == "Time [min]"] <- "Time_min"
  results_df$Time_min <- as.numeric(results_df$Time_min)

  if (!is.null(tissue_catalog) && length(tissue_catalog) > 0) {
    # Pivot each requested tissue column into a long-format block
    tissue_dfs <- lapply(names(tissue_catalog), function(tname) {
      tc  <- tissue_catalog[[tname]]
      col <- grep(tc$grep_pattern, names(results_df), value = TRUE)
      if (length(col) == 0) {
        message("  [process_results] No column matched pattern '", tc$grep_pattern,
                "' for tissue '", tname, "' — skipped.")
        return(NULL)
      }
      df_t <- results_df[, c("Time_min", "Scenario_name"), drop = FALSE]
      df_t$Conc_umolL <- as.numeric(results_df[[col[1]]])
      df_t$Tissue     <- tname
      df_t
    })
    results_long <- do.call(rbind, Filter(Negate(is.null), tissue_dfs))
  } else {
    # Legacy: single plasma column
    conc_col <- grep("Plasma \\(Peripheral Venous Blood\\)", names(results_df), value = TRUE)
    results_long <- results_df[, c("Time_min", "Scenario_name"), drop = FALSE]
    results_long$Conc_umolL <- as.numeric(results_df[[conc_col]])
    results_long$Tissue     <- "Plasma"
  }

  # Parse Scenario_name → CompoundId, Species, Route
  # (caller may override these with regex for compound IDs containing underscores)
  results_long <- results_long %>%
    tidyr::separate(
      Scenario_name,
      into   = c("CompoundId", "Species", "Route"),
      sep    = "_",
      remove = FALSE,
      extra  = "drop",
      fill   = "right"
    )

  # Merge MW and convert units
  results_long %>%
    left_join(mw_table, by = "CompoundId") %>%
    mutate(
      Time_hr   = Time_min / 60,
      Conc_ngml = Conc_umolL * MW   # 1 µmol/L = MW ng/mL
    )
}

# ---- Read observed data ------------------------------------------------------

#' Dynamically read all observed PK data files from a directory.
#'
#' Expects files named "{CompoundId}-PK-{Species}.xlsx" (e.g. ENV-0527-PK-Dog.xlsx)
#' produced by convert_pk_batch(), each containing a "Combined" sheet in OSPS
#' long format with columns: Time, "Time unit", Measurement, "Measurement unit", Route.
#'
#' @param data_dir Path to folder containing observed data xlsx files (non-recursive).
#' @return A data.frame with columns: CompoundId, Species, Route, Time_hr, Conc_ngml,
#'         or NULL if no files are found.
read_observed_data <- function(data_dir) {
  xlsx_files <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE, recursive = FALSE)
  
  if (length(xlsx_files) == 0) {
    message("No observed data files found in: ", data_dir)
    return(NULL)
  }
  
  all_obs <- lapply(xlsx_files, function(f) {
    base  <- tools::file_path_sans_ext(basename(f))
    parts <- strsplit(base, "-PK-")[[1]]
    if (length(parts) != 2) {
      message("Skipping file with unexpected name format: ", basename(f))
      return(NULL)
    }
    compound_id  <- parts[1]
    species_file <- parts[2]
    
    df <- tryCatch(
      read_excel(f, sheet = "Combined"),
      error = function(e) {
        message("Could not read 'Combined' sheet from: ", basename(f), " — ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    req_cols <- c("Time", "Time unit", "Measurement", "Route")
    missing  <- setdiff(req_cols, names(df))
    if (length(missing) > 0) {
      message("Skipping ", basename(f), ": missing columns: ", paste(missing, collapse = ", "))
      return(NULL)
    }
    
    df %>%
      filter(!is.na(Measurement)) %>%
      mutate(
        Time_hr    = if_else(`Time unit` == "h", as.numeric(Time), as.numeric(Time) / 60),
        Conc_ngml  = as.numeric(Measurement),
        Route      = toupper(Route),
        CompoundId = compound_id,
        Species    = species_file
      ) %>%
      select(CompoundId, Species, Route, Time_hr, Conc_ngml)
  })
  
  obs_combined <- do.call(rbind, Filter(Negate(is.null), all_obs))
  
  if (is.null(obs_combined) || nrow(obs_combined) == 0) {
    message("No observed data loaded.")
    return(NULL)
  }
  
  message("Loaded observed data: ", nrow(obs_combined), " rows from ", length(xlsx_files), " file(s)")
  obs_combined
}

