PCLakeR
====


R package for basic [PCLake](https://en.wikipedia.org/wiki/PCLake) model running. This package does not contain the source code for the model, only the executable. This package was inspired by [GLMr](https://github.com/GLEON/GLMr).

## Installation

You can install PCLakeR from Github with:

```{r gh-installation, eval = FALSE}
# install.packages("devtools")
devtools::install_github("aemon-j/PCLakeR")
```
## Usage
Compatible with Linux (tested on Ubuntu 18.04.3 LTS).

### Run

```{r example, eval=FALSE}
library(PCLakeR)
sim_folder <- system.file('extdata', package = 'PCLakeR')
run_pclake(sim_folder, par_file = 'langtjern.par')
```
