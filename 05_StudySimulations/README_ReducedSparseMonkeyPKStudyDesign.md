# Reduced Sparse Monkey PK Study Design

This folder contains a fresh two-gate analysis for evaluating whether an
a priori PBPK workflow plus a reduced sparse monkey PK study can support PK
characterization before human trial initiation.

## Workflow

Gate 1 screens a priori monkey AUC fold-error from the manuscript Table 1.
Molecules with at least one a priori AUC fold-error outside 0.5-2.0 trigger a
reduced monkey PK study.

Gate 2 evaluates the triggered molecules only. The observed-informed top-down
monkey model is treated as the retrospective reference for true monkey PK.
Reduced sparse studies are simulated by resampling small groups of animals from
the top-down virtual population and comparing the study geometric mean AUC with
the full top-down population geometric mean AUC.

This separates model bias from sampling/design adequacy:

- Gate 1 answers whether a priori PBPK is adequate.
- Gate 2 answers whether a reduced sparse monkey PK study is adequate once
  in vivo PK characterization is needed.

## Run

From the repository root:

```powershell
cd "C:\Users\krina\OneDrive\NAMs\APriori_PBPK_NAM"
$env:PATH = "C:\Program Files\R\R-4.5.1\bin\x64;" + $env:PATH
quarto render "05_StudySimulations\ReducedSparseMonkeyPKStudyDesign.qmd"
```

Or from inside `05_StudySimulations`:

```powershell
cd "C:\Users\krina\OneDrive\NAMs\APriori_PBPK_NAM\05_StudySimulations"
$env:PATH = "C:\Program Files\R\R-4.5.1\bin\x64;" + $env:PATH
quarto render "ReducedSparseMonkeyPKStudyDesign.qmd"
```

## Main Outputs

Tables:

- `SimsOutputs/Tables/Gate1_monkey_apriori_fold_error_screen.csv`
- `SimsOutputs/Tables/Gate1_triggered_molecules.csv`
- `SimsOutputs/Tables/Gate2_topdown_reference_population_parameters.csv`
- `SimsOutputs/Tables/Gate2_topdown_sparse_profiles.csv`
- `SimsOutputs/Tables/Gate2_topdown_sparse_AUC.csv`
- `SimsOutputs/Tables/Gate2_reference_AUC_CV_triggered_molecules.csv`
- `SimsOutputs/Tables/Gate2_single_dose_reduced_study_success.csv`
- `SimsOutputs/Tables/Gate2_two_vs_three_dose_design_success.csv`
- `SimsOutputs/Tables/Two_gate_decision_summary.csv`

Figures:

- `SimsOutputs/Figures/Gate2_topdown_sparse_profiles_triggered_molecules.png`
- `SimsOutputs/Figures/Gate2_single_dose_reduced_study_success.png`
- `SimsOutputs/Figures/Gate2_two_vs_three_dose_design_success.png`

## Manuscript Positioning

Suggested short framing:

> We used a two-gate decision framework. First, a priori PBPK predictions were
> screened against observed monkey exposure. Molecules with at least one a
> priori AUC fold-error outside the 2-fold interval were considered cases where
> a reduced monkey PK study would be triggered. Second, for these triggered
> molecules, the observed-informed top-down monkey model was used as the
> retrospective reference for true monkey PK. Reduced sparse PK studies were
> simulated by sampling small groups of animals from the top-down virtual
> population and comparing the study geometric mean AUC with the full
> top-down population geometric mean AUC.
