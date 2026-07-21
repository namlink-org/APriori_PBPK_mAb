# Species- and metric-specific fold-error tornado plots for A priori and
# TopDown PBPK predictions
#
# Fold error is directional: predicted / observed. Bars begin at 1 on a
# base-2 logarithmic axis, so underprediction extends left and overprediction
# extends right. The script writes one PNG for each species-metric combination.

required_packages <- c("dplyr", "tidyr", "ggplot2", "readxl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Install the following R packages before running this script: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

get_script_directory <- function() {
  file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_argument) == 1L) {
    return(dirname(normalizePath(sub("^--file=", "", file_argument), mustWork = TRUE)))
  }
  normalizePath(getwd(), mustWork = TRUE)
}

script_directory <- get_script_directory()
project_root <- normalizePath(file.path(script_directory, ".."), mustWork = TRUE)

apriori_file <- file.path(
  project_root,
  "03_APriori", "SimsOutputs", "Figures", "PK_Metrics_ObsPred.xlsx"
)
topdown_monkey_file <- file.path(
  project_root,
  "04_TopDown", "SimsOutputs", "TopDownTMDD_metrics_Monkey.csv"
)
topdown_human_file <- file.path(
  project_root,
  "04_TopDown", "SimsOutputs", "TopDownTMDD_to_Human_metrics.csv"
)
input_files <- c(apriori_file, topdown_monkey_file, topdown_human_file)
missing_files <- input_files[!file.exists(input_files)]
if (length(missing_files) > 0L) {
  stop(
    "Required input file(s) not found:\n",
    paste0("  - ", missing_files, collapse = "\n"),
    call. = FALSE
  )
}

metric_columns <- c(
  "Terminal CL" = "FE_terminal_CL",
  "AUC0-28d" = "FE_AUC0_28d",
  "Cmax" = "FE_Cmax"
)

read_apriori_sheet <- function(sheet_name) {
  readxl::read_excel(apriori_file, sheet = sheet_name) %>%
    transmute(
      Approach = "A priori",
      Species = as.character(Species),
      Molecule = as.character(Molecule),
      Dose_mg_kg = as.numeric(`Dose (mg/kg)`),
      FE_terminal_CL = as.numeric(`Fold Error terminal CL`),
      FE_AUC0_28d = as.numeric(`Fold Error AUC0-28d`),
      FE_Cmax = as.numeric(`Fold Error Cmax`)
    )
}

read_topdown_file <- function(path) {
  read.csv(path, check.names = FALSE, stringsAsFactors = FALSE) %>%
    transmute(
      Approach = "TopDown",
      Species = as.character(Species),
      Molecule = as.character(Molecule),
      Dose_mg_kg = as.numeric(`Dose (mg/kg)`),
      FE_terminal_CL = as.numeric(`FE terminal CL`),
      FE_AUC0_28d = as.numeric(`FE AUC0-28d`),
      FE_Cmax = as.numeric(`FE Cmax`)
    )
}

apriori_data <- bind_rows(
  read_apriori_sheet("Monkey_Metrics"),
  read_apriori_sheet("Human_Metrics")
)

topdown_data <- bind_rows(
  read_topdown_file(topdown_monkey_file),
  read_topdown_file(topdown_human_file)
)

scenario_keys <- c("Species", "Molecule", "Dose_mg_kg")

validate_unique_scenarios <- function(data, source_name) {
  duplicates <- data %>%
    count(across(all_of(scenario_keys)), name = "Records") %>%
    filter(Records > 1L)

  if (nrow(duplicates) > 0L) {
    stop(
      source_name,
      " contains duplicate species-molecule-dose records.",
      call. = FALSE
    )
  }
}

validate_unique_scenarios(apriori_data, "A priori output")
validate_unique_scenarios(topdown_data, "TopDown output")

only_apriori <- anti_join(apriori_data, topdown_data, by = scenario_keys)
only_topdown <- anti_join(topdown_data, apriori_data, by = scenario_keys)

if (nrow(only_apriori) > 0L || nrow(only_topdown) > 0L) {
  stop(
    paste0(
      "A priori and TopDown scenario coverage does not match (",
      nrow(only_apriori), " only in A priori; ",
      nrow(only_topdown), " only in TopDown)."
    ),
    call. = FALSE
  )
}

fold_error_data <- bind_rows(apriori_data, topdown_data) %>%
  pivot_longer(
    cols = all_of(unname(metric_columns)),
    names_to = "Metric_column",
    values_to = "Fold_error"
  ) %>%
  mutate(
    Metric = names(metric_columns)[match(Metric_column, metric_columns)],
    Metric = factor(Metric, levels = names(metric_columns)),
    Approach = factor(Approach, levels = c("A priori", "TopDown")),
    Species = factor(Species, levels = c("Monkey", "Human")),
    Dose_label = format(
      Dose_mg_kg,
      trim = TRUE,
      scientific = FALSE,
      drop0trailing = TRUE
    ),
    Scenario = paste0(Molecule, " | ", Dose_label, " mg/kg")
  )

invalid_fold_errors <- fold_error_data %>%
  filter(!is.finite(Fold_error) | Fold_error <= 0)

if (nrow(invalid_fold_errors) > 0L) {
  stop(
    "All requested fold errors must be finite and greater than zero; found ",
    nrow(invalid_fold_errors), " invalid value(s).",
    call. = FALSE
  )
}

fold_error_data <- fold_error_data %>%
  mutate(Log2_fold_error = log2(Fold_error))

# Use the same symmetric x-axis limits for all six plots so reciprocal errors
# (for example, 0.5 and 2) have equal visual distances and plots are comparable.
maximum_log2_error <- max(abs(fold_error_data$Log2_fold_error))
log2_limit <- max(1, ceiling(maximum_log2_error))
log2_breaks <- seq(-log2_limit, log2_limit, by = 1)
fold_break_labels <- format(2^log2_breaks, trim = TRUE, scientific = FALSE)

species_colors <- c(
  "Monkey" = "#0072B2",
  "Human" = "#D55E00"
)

png_device <- if (requireNamespace("ragg", quietly = TRUE)) {
  ragg::agg_png
} else {
  "png"
}

metric_file_stubs <- c(
  "Terminal CL" = "Terminal_CL",
  "AUC0-28d" = "AUC0_28d",
  "Cmax" = "Cmax"
)

make_tornado_plot <- function(species_name, metric_name) {
  plot_data <- fold_error_data %>%
    filter(
      as.character(Species) == species_name,
      as.character(Metric) == metric_name
    )

  scenario_levels <- plot_data %>%
    distinct(Molecule, Dose_mg_kg, Scenario) %>%
    arrange(Molecule, Dose_mg_kg) %>%
    pull(Scenario)

  plot_data <- plot_data %>%
    mutate(
      Scenario = factor(Scenario, levels = rev(scenario_levels)),
      Fold_error_label = sprintf("%.2f", Fold_error),
      Label_hjust = if_else(Log2_fold_error >= 0, -0.12, 1.12)
    )

  plot_title <- paste(species_name, metric_name, "directional fold error")

  plot_object <- ggplot(
    plot_data,
    aes(x = Log2_fold_error, y = Scenario)
  ) +
    annotate(
      "rect",
      xmin = log2(0.5), xmax = log2(2),
      ymin = -Inf, ymax = Inf,
      fill = "grey70", alpha = 0.12
    ) +
    geom_vline(
      xintercept = log2(c(0.5, 2)),
      color = "grey55", linewidth = 0.35, linetype = "22"
    ) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.55) +
    geom_col(
      width = 0.68,
      fill = unname(species_colors[species_name]),
      color = "white",
      linewidth = 0.15
    ) +
    geom_text(
      aes(label = Fold_error_label, hjust = Label_hjust),
      size = 3.0,
      color = "grey15"
    ) +
    facet_grid(cols = vars(Approach)) +
    scale_x_continuous(
      breaks = log2_breaks,
      labels = fold_break_labels,
      expand = expansion(mult = 0)
    ) +
    scale_y_discrete(
      limits = rev(scenario_levels),
      breaks = rev(scenario_levels),
      drop = FALSE
    ) +
    coord_cartesian(
      xlim = c(-log2_limit - 0.45, log2_limit + 0.45),
      clip = "off"
    ) +
    labs(
      title = plot_title,
      subtitle = paste(
        "Fold error = predicted / observed; bars extend from 1.",
        "Shading and dashed lines mark the 0.5- to 2-fold interval."
      ),
      x = "Fold error (base-2 logarithmic scale)",
      y = NULL
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle = element_text(
        size = 10.5, color = "grey25", margin = margin(b = 7)
      ),
      strip.background = element_rect(
        fill = "grey92", color = "grey50", linewidth = 0.4
      ),
      strip.text.x = element_text(face = "bold", size = 12.5),
      axis.text.x = element_text(size = 9.5),
      axis.text.y = element_text(size = 9.5),
      axis.title.x = element_text(size = 11.5, margin = margin(t = 7)),
      panel.grid.major.y = element_line(color = "grey91", linewidth = 0.25),
      panel.grid.major.x = element_line(color = "grey88", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      panel.spacing.x = grid::unit(0.8, "lines"),
      plot.margin = margin(10, 22, 10, 10)
    )

  output_file <- file.path(
    script_directory,
    paste0("FoldError_", species_name, "_", metric_file_stubs[[metric_name]], ".png")
  )
  plot_height <- max(6.2, 3.2 + 0.26 * length(scenario_levels))

  ggsave(
    filename = output_file,
    plot = plot_object,
    device = png_device,
    width = 14,
    height = plot_height,
    units = "in",
    dpi = 300,
    bg = "white",
    limitsize = FALSE
  )

  message(
    "Saved ", normalizePath(output_file, winslash = "/", mustWork = TRUE),
    " (", length(scenario_levels), " molecule-dose rows)."
  )

  invisible(output_file)
}

output_files <- unlist(lapply(c("Human", "Monkey"), function(species_name) {
  lapply(names(metric_columns), function(metric_name) {
    make_tornado_plot(species_name, metric_name)
  })
}), use.names = FALSE)
