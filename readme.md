# A validation of machine learning-based risk scores in the prehospital setting
#### Douglas Spangler, Thomas Hermansson, David Smekal, Hans Blomberg

[doi.org/10.1371/journal.pone.0226518](https://doi.org/10.1371/journal.pone.0226518)

This repository contains the source code used to generate the results reported in this study based on a cleaned data file. If no data file is provided, a synthetic dataset will be generated possessing univariate characteristics mimicking the dataset used to generate the results reported upon in the manuscript. The repository contains a number of resources:

* **analysis.Rmd** - This R markdown file contains the main analysis executed to produce the reported results. This file may be run chunk by chunk in an IDE (we recommend [Rstudio](https://rstudio.com)), or knitted to a .pdf or .docx file. Executing this script will generate a number of data files including the results of multiple imputation (mi.rda), fitted XGBoost models (xgb.rda and xgb_cv.rda), and a file containing the univariate properties of the dataset (data/data_properties.rda). These files will only be generated if no file already exists. If no data file exists (by default data/studydata.rda), the data_properties file will be used to generate a synthetic dataset for analysis (Note that this garbage data will produce garbage results, though we hope that it will be helpful in troubleshooting and getting the format of your own dataset right if you're interested in replicating our results).

* **analysis.pdf** - This file contains the results of knitting the above R markdown file to a pdf based on our data. This file differs from the analysis presented in the manuscript in that it includes the public-release model used in the demo in the analysis.

* **functions.R** - This file contains a number of functions used by analysis.Rmd to perform the analysis, as well as a list of longer and more human-readable variable names.

* **data/data_properties.rda** - This R data file contains a dataframe of the univariate distributions of the variables included in our dataset. It is used by analysis.Rmd to generate synthetic data if no real data is available. Since its contents are somewhat interesting given that it describes the distribution of each of the variables in our dataset, we also provide a .csv file containing the same data for non-R users.

* **demo/app.R** - This file contains the code used in the demo web application. Open this in Rstudio and press the "run app" button to check it out. (or visit https://ucpr.se/openTriage_demo/)

* **demo/pub_mods.r** - This file contains a list object including the public-release models used in the demo (mods), 10 similar models based on boostrap resampled datasets (bootmods), Median values for each predictor (medvals), scaling parameters and cross-validated predictions in our training data (scale), equivalent parameters for bootstrapped models (bootscale), and 2x2 table data at various threshold values used to compute decision rule diagnostics (roc)

While we are unable to publish individual level data for legal and ethical reasons, please contact ambulanssjukvard@akademiska.se to arrange access to data for researchers meeting criteria for access to confidential data. If re-using this code in research, please cite the above PLoS ONE article. Thank you!