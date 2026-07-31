# A Priori PBPK Modeling of Monoclonal Antibodies

This repository contains the data, models, analysis scripts, and results supporting a retrospective evaluation of **in vitro-informed, a priori physiologically based pharmacokinetic (PBPK) modeling for monoclonal antibodies**.

The project evaluates whether mechanistic PBPK modeling can predict first-in-human pharmacokinetics without fitting to monkey PK data and thereby support the development of **new approach methodologies (NAMs)** that reduce reliance on conventional nonhuman primate PK studies. A priori PBPK predictions are compared with observed monkey and human PK data and with predictions from top-down target-mediated drug disposition models. Simulation-based evaluations of reduced monkey PK study designs are also included.

## Repository structure

* `01_Data`: Observed PK data and model input datasets
* `02_Docs`: Manuscript and supporting documentation
* `03_APriori`: A priori PBPK models, scripts, and outputs
* `04_TopDown`: Top-down TMDD models, scripts, and outputs
* `05_Outputs`: Comparative analyses, tables, and figures
* `06_StudySimulations`: Simulation-based evaluation of reduced monkey PK study designs

The PBPK workflow was implemented using **PK-Sim/MoBi**, `ospsuite`, `esqlabsR`, and R. This repository is intended to support transparency and reproducibility of the associated research.

