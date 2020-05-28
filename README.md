[//]: This is a comment test [![License: GPL v3](pic https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[//]: This is a comment test [![DOI:](https://zenodo.org/)]()


# Cyclic Immunofluorescence Workflow

## Introduction

This is the cyclic immunofluorescence (CycIF) workflow developed my Megan Grout for Dr. Gordon Mills' lab at OHSU, originally written fall 2019. It is based upon data analysis code originally written by Dr. Marilyne Labrie and Nicholas Kendsersky. The purpose is to facilitate visualization and interaction with CycIF data in a highly customizable and modularized way. Balance is struck between convenient automation and easy customization, with extensive commenting and code snippet examples. The majority of the workflow is coded in Python. The library `pandas` was chosen for computationally efficient data processing. The two R Shiny apps, `filtering_app.R` and `PCA_apps.R`, were developed to allow for easy interaction with data at key locations in the workflow. Custom functions in `cyclic_modules.py` allow for encapsulation of repeated code blocks, which makes the notebooks visually cleaner and easier for the user to interface with.

## Usage

The workflow is composed of four Python Jupyter Notebooks and two R Shiny apps. The Python code uses custom modules, found in `cyclic_modules.py`. The Python Jupyter Notebooks should be used with a Python 3 kernel. The R Shiny apps may be used at key points in the workflow, to help the user evaluate data integrity and determine the best parameters with which to proceed. They should be executed in order:

1) `qc_eda.ipynb`
2) `BS.ipynb` with `filtering_app.R` and `PCA_app.R`
3) `log2_z-score.ipynb`
4) `KMeans.ipynb`



## Licensing

TBD