ST558 Final Project
================
Yinzhou Zhu

  - [Purpose of this project](#purpose-of-this-project)
  - [Data used in this project](#data-used-in-this-project)
      - [SEER 18 data set](#seer-18-data-set)
      - [Data subsetting](#data-subsetting)
      - [Variables used](#variables-used)

# Purpose of this project

# Data used in this project

Data is obtained by registering at The Surveillance, Epidemiology, and
End Results ([NIH SEER](https://seer.cancer.gov/data/access.html)) and
requesting access through SEER\*Stat software. Tables are queried
through the software and imported to R using `SEER2R` package.

## SEER 18 data set

Data set chosen for this project is the
[**SEER 18**](https://seer.cancer.gov/registries/terms.html), which
contains data for cases from year 2000 through 2017. Although this set
does not include data from earlier times such as from year 1975 or 1992
onward like the **SEER 9/13** data set, improvement in modern diagnosis,
treatments and post-treatment care could impact the trend of survival.
Thus, the **SEER 18** data set can be more appropriate for traditional
ML predictions.

## Data subsetting

I have removed all rows with “Unknown” in `Grade` variable, since cell
type is an important aspect (regardless of whether the models eventually
use this variable or not).  
User will be able to determine the ration between training and testing
data.

## Variables used

The Following variables are used from the data set:

| Variables                      |                                Descriptions                                 |
| ------------------------------ | :-------------------------------------------------------------------------: |
| Sex                            |                        Includes 1= Male and 2=Female                        |
| Year of diagnosis              |                            Values are 2000-2019                             |
| Race Recode (W, B, AI, API)    | White,Black,American Indian/Alaska Native,Asian or Pacific Islander,Unknown |
| Age Recode with \<1 year olds  |             Age of diagnosis:\< 1 year, 1-4 years,…, 85+ years              |
| Primary Site                   |                      Cecum, Lips etc. but with a code                       |
| Histological Type              |   Microscopic composition of cells and/or tissue for primary tumor (code)   |
| Grade                          |                            Cell type identified                             |
| Laterality                     |  The side of body/a paired organ on which the reportable tumor originated   |
| RX SUMM SURG PRIM SITE (1998+) |                           Surgery of Primary Site                           |
| Survival months                |                          Number of months survived                          |
