rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
})

set.seed(20260628)

FOLD_ACCEPTANCE <- 2
N_SIM <- 1000L
N_PER_GROUP <- 2:5
DESIGN_DOSES <- c(0.3, 10)

script_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
script_dir <- if (!is.na(script_file) && nzchar(script_file)) {
  dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
} else {
  normalizePath(".", winslash = "/", mustWork = TRUE)
}
if (!dir.exists(file.path(script_dir, "SimsOutputs"))) {
  script_dir <- normalizePath("05_StudySimulations", winslash = "/", mustWork = TRUE)
}

table_dir <- file.path(script_dir, "SimsOutputs", "Tables")
figure_dir <- file.path(script_dir, "SimsOutputs", "Figures")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

geometric_mean <- function(x) {
  x <- x[is.finite(x) & x > 0]
  exp(mean(log(x)))
}

topdown_sparse_auc <- read.csv(
  file.path(table_dir, "Gate2_topdown_sparse_AUC.csv"),
  stringsAsFactors = FALSE
)

mabs_for_design <- sort(unique(topdown_sparse_auc$Drug))

simulate_design_fold_error_replicates <- function(drug, n_per_group) {
  map_dfr(seq_len(N_SIM), function(replicate_id) {
    dose_results <- map_dfr(DESIGN_DOSES, function(dose) {
      auc_values <- topdown_sparse_auc %>%
        filter(Drug == drug, Dose_mgkg == dose) %>%
        pull(Sparse_AUC_ug_h_mL)
      reference <- geometric_mean(auc_values)
      study_gmean <- geometric_mean(
        sample(auc_values, n_per_group, replace = TRUE)
      )
      ratio <- study_gmean / reference
      tibble(
        Dose_mgkg = dose,
        Reference_AUC_geomean_ug_h_mL = reference,
        Study_AUC_geomean_ug_h_mL = study_gmean,
        Ratio_to_reference = ratio,
        Symmetric_fold_error = pmax(ratio, 1 / ratio)
      )
    })

    tibble(
      Drug = drug,
      Replicate = replicate_id,
      Design = "2 dose groups",
      N_per_group = n_per_group,
      Total_animals = length(DESIGN_DOSES) * n_per_group,
      Max_symmetric_fold_error = max(dose_results$Symmetric_fold_error),
      Success_within_2fold = Max_symmetric_fold_error <= FOLD_ACCEPTANCE
    )
  })
}

design_fold_error_replicates <- crossing(
  Drug = mabs_for_design,
  N_per_group = N_PER_GROUP
) %>%
  mutate(
    Replicates = map2(Drug, N_per_group, simulate_design_fold_error_replicates)
  ) %>%
  select(-Drug, -N_per_group) %>%
  unnest(Replicates) %>%
  mutate(
    N_per_group = factor(N_per_group, levels = N_PER_GROUP)
  )

sample_size_colors <- viridisLite::viridis(
  length(N_PER_GROUP),
  option = "D",
  direction = -1,
  begin = 0.12,
  end = 0.82
)

design_fold_error_summary <- design_fold_error_replicates %>%
  group_by(Drug, Design, N_per_group, Total_animals) %>%
  summarise(
    Probability_within_2fold = mean(Success_within_2fold),
    Median_max_symmetric_fold_error = median(Max_symmetric_fold_error),
    Fold_error_5th_percentile = quantile(Max_symmetric_fold_error, 0.05),
    Fold_error_95th_percentile = quantile(Max_symmetric_fold_error, 0.95),
    .groups = "drop"
  )

write.csv(
  design_fold_error_replicates,
  file.path(table_dir, "Gate2_design_fold_error_replicates_2dose_by_n.csv"),
  row.names = FALSE
)
write.csv(
  design_fold_error_summary,
  file.path(table_dir, "Gate2_design_fold_error_summary_2dose_by_n.csv"),
  row.names = FALSE
)

study_theme <- theme_bw(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

design_fold_error_labels <- design_fold_error_summary %>%
  mutate(
    Label = paste0(
      scales::percent(Probability_within_2fold, accuracy = 1),
      " within 2-fold"
    ),
    X = pmin(1.92, Fold_error_95th_percentile + 0.05)
  )

design_fold_error_plot <- ggplot(
  design_fold_error_replicates,
  aes(
    x = Max_symmetric_fold_error,
    y = forcats::fct_rev(factor(Drug)),
    fill = N_per_group,
    color = N_per_group
  )
) +
  geom_vline(xintercept = 1, color = "grey35", linewidth = 0.5) +
  geom_vline(
    xintercept = FOLD_ACCEPTANCE,
    color = "#B22222",
    linetype = 2,
    linewidth = 0.8
  ) +
  geom_boxplot(
    width = 0.62,
    outlier.shape = NA,
    alpha = 0.55,
    linewidth = 0.45,
    position = position_dodge2(width = 0.72, preserve = "single")
  ) +
  scale_x_continuous(
    limits = c(1, FOLD_ACCEPTANCE),
    breaks = c(1, 1.25, 1.5, 1.75, 2),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  scale_fill_manual(values = sample_size_colors) +
  scale_color_manual(values = sample_size_colors) +
  labs(
    x = "Geometric fold error across dose groups",
    y = NULL,
    fill = "Animals per dose",
    color = "Animals per dose"
  ) +
  study_theme +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0),
    plot.margin = margin(10, 18, 10, 10)
  )

ggsave(
  file.path(figure_dir, "Gate2_design_fold_error_2dose_by_n.png"),
  design_fold_error_plot,
  width = 10.5,
  height = 5.8,
  dpi = 450
)

# print(design_fold_error_summary)
