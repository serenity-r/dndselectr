
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dndselectr

[![Travis-CI Build
Status](https://travis-ci.org/serenity-r/dndselectr.svg?branch=master)](https://travis-ci.org/serenity-r/dndselectr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/serenity-r/dndselectr?branch=master&svg=true)](https://ci.appveyor.com/project/mdlama/dndselectr)
[![Coverage
Status](https://img.shields.io/codecov/c/github/serenity-r/dndselectr/master.svg)](https://codecov.io/github/serenity-r/dndselectr?branch=master)
[![Lifecycle
Experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

## Overview

Implements a drag-and-drop Shiny select input

This implementation creates a Shiny input that replicates much of the
functionality of selectInput. Multiple zones for dragging and dropping
are allowed. Currently utilizes Dragula JS library,
<https://github.com/bevacqua/dragula>.

## Installation

``` r
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("serenity-r/dndselectr")
```

## Examples

``` r
dndselectr::runExample()
#> Valid examples are "01-basic.R", "02-hidden.R", "03-multivalued.R", "04-renderUI.R", "05-multivalued-renderUI.R", "06-selectable.R", "07-renderUI-selectable.R", "08-hidden-entangled.R", "09-selectable-togglevis.R", "10-togglelock.R", "11-partytime.R", "12-removeOnSpill.R", "13-change-direction.R", "14-frozen-items.R", "15-complex-choices.R", "16-max-input.R", "17-check-settings.R", "18-check-api.R", "19-flex.R", "20-shiny-inputs.R", "21-shiny-inputs-modules.R", "22-shiny-inputs-modules-render.R"
```

Please note that the ‘dndselectr’ project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to
this project, you agree to abide by its terms.
