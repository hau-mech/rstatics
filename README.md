
# rstatics

## Overview

*rstatics* package provides functions for the course MAS142 Statics and
Mechanics of Materials at HVL.

## Installation

``` r

if (!require("remotes")) install.packages("remotes")
remotes::install_github("hau-mech/rstatics")
```

## Usage

``` r

library(rstatics)
library(ggplot2)
library(patchwork)

loads <- tibble::tribble( ~x, ~force,
                         0.025,  50,
                         0.325, -50,
                         0.500, -50,
                         0.800,  50)
plot_internal_forces(.beam_length = 0.825, 
                     .resolution = 0.0001, 
                     .loads = loads)
```

![](man/figures/README-unnamed-chunk-3-1.png)
