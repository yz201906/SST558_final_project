ST558 Final Project
================
Yinzhou Zhu

  - [Packages required in the shiny
    app](#packages-required-in-the-shiny-app)
  - [SEER 18 data set](#seer-18-data-set)
      - [Variables used](#variables-used)
      - [Data pre-processing](#data-pre-processing)
  - [Shiny app](#shiny-app)
      - [Variables for visualization](#variables-for-visualization)
      - [Variable encoding in shiny
        app](#variable-encoding-in-shiny-app)
      - [Data processing in the
        back-end](#data-processing-in-the-back-end)

## Packages required in the shiny app

`shiny`  
`tidyverse`  
`DT`  
`shinycssloaders`  
`caret`  
`mltools`  
`data.table`  
`parallel`  
`doParallel`  
`ggfortify`  
`shinydashboard`

## SEER 18 data set

Data set chosen for this project is the
[**SEER 18**](https://seer.cancer.gov/registries/terms.html),

  - “Available for cases diagnosed from 2000 through the current data
    year and includes expanded races.”  
  - The data is downloaded in matrix format through SEER\*Stat software,
    and then imported into R using `SEER2R` package.

### Variables used

The Following variables are used from the data set:

| Variables                      |                                  Descriptions                                   |
| ------------------------------ | :-----------------------------------------------------------------------------: |
| Sex                            |                            Includes Male and Female                             |
| Year of diagnosis              |                              Values are 2000-2019                               |
| Race Recode (W, B, AI, API)    |   White,Black,American Indian/Alaska Native,Asian or Pacific Islander,Unknown   |
| Age Recode with \<1 year olds  |               Age of diagnosis:\< 1 year, 1-4 years,…, 85+ years                |
| Primary Site                   |                    Cecum, Lips etc. but with a numeric code                     |
| Histological Type              | Microscopic composition of cells and/or tissue for primary tumor (numeric code) |
| Grade                          |                              Cell type identified                               |
| Laterality                     |    The side of body/a paired organ on which the reportable tumor originated     |
| RX SUMM SURG PRIM SITE (1998+) |                     Surgery of Primary Site (numeric code)                      |
| Survival months                |                            Number of months survived                            |

### Data pre-processing

  - The downloaded data is pre-filtered to have data year 2000-2010.  
  - The 19 age groups are recoded into:

| original    | recoded (numeric) |
| ----------- | :---------------: |
| 00 years    |         1         |
| 01-04 years |         2         |
| 05-09 years |         3         |
| 10-14 years |         4         |
| 15-19 years |         5         |
| 20-24 years |         6         |
| 25-29 years |         7         |
| 30-34 years |         8         |
| ……          |        ……         |
| 85 years    |        19         |

  - Data is saved in `.rds` format and uploaded to github inside
    shinyapp directory.

## Shiny app

### Variables for visualization

  - Variables including `Primary Site`, `Histological Type` and `RX SUMM
    SURG PRIM SITE (1998+)` are not visualized in the but are are
    included for modeling (supervised).  
  - Data is segregated by Year\_of\_diagnosis for all visualization and
    modeling.

### Variable encoding in shiny app

  - All categorical variables are encoded with one-hot encoding with
    `one_hot()` function from `mltools` in the back end so that those
    variables can be used as predictors.

### Data processing in the back-end

#### Summary, visualization and supervised modeling

  - When a year is selected, that years data is subsetted. The subset is
    further sampled (randomly using `sample_n()`) to select 50000
    rows.  
  - The data table in summary section contains that 50000 records.  
  - The 50000 records are further split into training and testing data
    sets with a 70% to 30% ratio.  
  - Model training is based on the training set, and prediction is based
    on test set, which allows user to choose.

#### Unsupervised modeling

  - Data is sampled 50000 times from the original data (input rds),
    which contains all 10 years data.
  - The sampled data is modeled against `Survival_months`, `age_group`,
    `Sex`, `Primary Site`, `Histological Type` and `RX SUMM SURG PRIM
    SITE (1998+)`.
