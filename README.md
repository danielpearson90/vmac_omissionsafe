
# vmac\_omissionsafe

<!-- badges: start -->

<!-- badges: end -->

This repository contains the data and analysis scripts for a project investigating the role that learned response relationships with reward play in attentional selection. A preprint for this project can be found at: [https://psyarxiv.com/prqmg](https://psyarxiv.com/prqmg)

The structure of the repository is as follows:

- [analysis](analysis) contains the analysis scripts for Experiment 1 and 2
- [preprocessing](preprocessing) contains the preprocessing scripts for turning the raw data into the preprocessed .csv files in [processed_data](processed_data)
- [processed_data](processed_data) contains single .csv files for behavioural and saccade data for each experiment
- [raw_data](raw_data) contains the raw matlab data files that were saved while running the experiment
- [write_up](write_up) contains .pdf files of the preprint and supplementary materials for this project. These files are also hosted at: [https://psyarxiv.com/prqmg](https://psyarxiv.com/prqmg)
- [scripts](scripts) contains some R scripts that are used in preprocessing and/or data analysis 

## Reproducibility

This project uses [renv](https://rstudio.github.io/renv/index.html) to ensure that the exact package versions that were used to run this analysis, are the same ones that get used for any reproduction of this analysis. When downloading and opening this project for the first time, renv and the appropriate versions of all necessary packages should be automatically installed. 

For information on how to use renv in collaborative research environments, see: [rstudio.github.io/renv/articles/collaborating.html](https://rstudio.github.io/renv/articles/collaborating.html).
