pubmedRecords
================

[![Licence](https://img.shields.io/badge/licence-MIT+-lightgrey.svg)](http://choosealicense.com/) [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.3.0-6666ff.svg)](https://cran.r-project.org/) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/pubmedRecords)](https://cran.r-project.org/package=pubmedRecords) [![packageversion](https://img.shields.io/badge/Package%20version-0.1.7-orange.svg?style=flat-square)](commits/master) [![Last-changedate](https://img.shields.io/badge/last%20change-2023--01--11-yellowgreen.svg)](/commits/master)

## About

This package provides tools to download _PubmedArticle_ records from the NCBI [PubMed](https://www.ncbi.nlm.nih.gov/pubmed/) database based on user-specified search criteria, and to add CrossRef citation data for the returned records. The output is formatted as a tidy dataframe, facilitating downstream analysis using tools from the 'tidyverse'.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("kamermanpr/pubmedRecords")
```

## Documentation

Documentation can be found at: [kamermanpr.github.io/pubmedRecords/](https://kamermanpr.github.io/pubmedRecords/).
