
<!-- README.md is generated from README.Rmd. Please edit that file -->

# elco

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Overview

‘elco’ extends the [‘units’](https://r-quantities.github.io/units/) R
package with measurement units specific for chemical elements. This
allows to consider the identity of chemical elements in calculations
with measurement units.

‘elco’ defines units such as g$_\text{C}$ (grams of carbon) and helps to
avoid erroneous calculations, such as
$\frac{1~\text{g}_\text{C} \cdot \text{mol}_\text{N}}{14.0067~\text{g}_\text{N}} = 0.07~\text{mol}_\text{C}$.

In addition, ‘elco’ provides the following functions:

1.  `elco_convert()`: batch-conversion of elemental contents in data
    frames (e.g. from g to mol).
2.  `elco_nosc()`, `elco_or()`, `elco_du()`: Compute the nominal
    oxidation state of carbon, oxidative ratio, and degree of
    unsaturation, respectively (see e.g., Worrall et al. (2016),
    Masiello et al. (2008)).

### How to install

‘elco’ can be installed from GitHub:

``` r
remotes::install_github("henningte/elco")
```

### How to use

Here is a quick overview on how ‘elco’ helps with computing elemental
ratios.

``` r
library(elco)
#> Loading required package: quantities
#> Loading required package: units
#> udunits database from C:/Users/henni/AppData/Local/R/win-library/4.3/units/share/udunits/udunits2.xml
#> Loading required package: errors

# load other required packages
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(magrittr)
```

First, we have a short look at the sample data and its structure:

``` r
d <- elco::chno
d
#>                             C                           H
#> 1 0.4928286(0) [g_C/g_sample]  0.177478(0) [g_H/g_sample]
#> 2 0.4970892(0) [g_C/g_sample] 0.1692088(0) [g_H/g_sample]
#> 3 0.5218047(0) [g_C/g_sample]  0.162892(0) [g_H/g_sample]
#> 4 0.5008897(0) [g_C/g_sample] 0.1642393(0) [g_H/g_sample]
#> 5 0.5016323(0) [g_C/g_sample] 0.1738859(0) [g_H/g_sample]
#>                              N                           O          sample_mass
#> 1 0.09203495(0) [g_N/g_sample] 0.1705965(0) [g_O/g_sample]  3.78(1) [mg_sample]
#> 2 0.09216568(0) [g_N/g_sample] 0.1640896(0) [g_O/g_sample]  3.81(1) [mg_sample]
#> 3 0.09125226(0) [g_N/g_sample] 0.1606575(0) [g_O/g_sample] 3.494(4) [mg_sample]
#> 4 0.08918042(0) [g_N/g_sample]  0.165484(0) [g_O/g_sample] 4.251(3) [mg_sample]
#> 5 0.09247861(0) [g_N/g_sample] 0.1609109(0) [g_O/g_sample] 4.046(9) [mg_sample]
```

Contents for one element are stored in a numeric vector. Measurement
units and errors are tracked using the
[quantities](https://github.com/r-quantities/quantities) package.

elco supports unit conversion (using the functions of the units
package):

``` r
# g_C/g_sample to mol_C/g_sample (umol_C/g_sample)
units::set_units(d$C, value = "mol_C/g_sample", mode = "standard")
#> Units: [mol_C/g_sample]
#> Errors: 0 0 0 0 0
#> [1] 5.919217 5.970389 6.267240 6.016036 6.024955
units::set_units(d$C, value = "umol_C/g_sample", mode = "standard")
#> Units: [umol_C/g_sample]
#> Errors: 0 0 0 0 0
#> [1] 5919217 5970389 6267240 6016036 6024955
```

`elco_convert()` can be used to batch-convert units. This will convert
the numerator of the unit while it keeps the denominator. For example,
here we convert g_C/g_sample to mmol_C/g_sample (and similarly for all
other elements in `d`):

``` r
# g/g to mol
d %>% 
  dplyr::mutate(
    dplyr::across(
      ! dplyr::all_of("sample_mass"), 
      \(.x) elco_convert(.x, to = "mmol"))
  )
#>                               C                             H
#> 1 5919.217(0) [mmol_C/g_sample] 178.8871(0) [mmol_H/g_sample]
#> 2 5970.389(0) [mmol_C/g_sample] 170.5523(0) [mmol_H/g_sample]
#> 3  6267.24(0) [mmol_C/g_sample] 164.1854(0) [mmol_H/g_sample]
#> 4 6016.036(0) [mmol_C/g_sample] 165.5433(0) [mmol_H/g_sample]
#> 5 6024.955(0) [mmol_C/g_sample] 175.2665(0) [mmol_H/g_sample]
#>                               N                             O
#> 1 1289.106(0) [mmol_N/g_sample] 2729.441(0) [mmol_O/g_sample]
#> 2 1290.937(0) [mmol_N/g_sample] 2625.335(0) [mmol_O/g_sample]
#> 3 1278.143(0) [mmol_N/g_sample] 2570.423(0) [mmol_O/g_sample]
#> 4 1249.123(0) [mmol_N/g_sample] 2647.644(0) [mmol_O/g_sample]
#> 5  1295.32(0) [mmol_N/g_sample] 2574.478(0) [mmol_O/g_sample]
#>            sample_mass
#> 1  3.78(1) [mg_sample]
#> 2  3.81(1) [mg_sample]
#> 3 3.494(4) [mg_sample]
#> 4 4.251(3) [mg_sample]
#> 5 4.046(9) [mg_sample]
```

elco helps computing element ratios:

``` r
d %>%
  dplyr::mutate(
    cn_molar = elco_convert(C, to = "mol") / elco_convert(N, to = "mol")
  )
#>                             C                           H
#> 1 0.4928286(0) [g_C/g_sample]  0.177478(0) [g_H/g_sample]
#> 2 0.4970892(0) [g_C/g_sample] 0.1692088(0) [g_H/g_sample]
#> 3 0.5218047(0) [g_C/g_sample]  0.162892(0) [g_H/g_sample]
#> 4 0.5008897(0) [g_C/g_sample] 0.1642393(0) [g_H/g_sample]
#> 5 0.5016323(0) [g_C/g_sample] 0.1738859(0) [g_H/g_sample]
#>                              N                           O          sample_mass
#> 1 0.09203495(0) [g_N/g_sample] 0.1705965(0) [g_O/g_sample]  3.78(1) [mg_sample]
#> 2 0.09216568(0) [g_N/g_sample] 0.1640896(0) [g_O/g_sample]  3.81(1) [mg_sample]
#> 3 0.09125226(0) [g_N/g_sample] 0.1606575(0) [g_O/g_sample] 3.494(4) [mg_sample]
#> 4 0.08918042(0) [g_N/g_sample]  0.165484(0) [g_O/g_sample] 4.251(3) [mg_sample]
#> 5 0.09247861(0) [g_N/g_sample] 0.1609109(0) [g_O/g_sample] 4.046(9) [mg_sample]
#>                    cn_molar
#> 1 4.591723(0) [mol_C/mol_N]
#> 2 4.624849(0) [mol_C/mol_N]
#> 3 4.903395(0) [mol_C/mol_N]
#> 4 4.816207(0) [mol_C/mol_N]
#> 5 4.651325(0) [mol_C/mol_N]
```

Finally, elco provides functions to compute the nominal oxidation state
of carbon (NOSC), oxidative ratio (OR), and degree of unsaturation (DU)
(Worrall et al. 2016; Masiello et al. 2008)

``` r
d <- 
  d %>% 
  dplyr::mutate(
    dplyr::across(
      ! dplyr::all_of("sample_mass"),
      function(.x) elco_convert(.x * sample_mass, to = "mol")
    )
  ) %>%
  dplyr::mutate(
    nosc = elco_nosc(C = C, H = H, N = N, O = O),
    or = elco_or(C = C, H = H, N = N, O = O),
    du = elco_du(C = C, H = H, N = N)
  )
```

### How to cite

Please cite this package as:

> Teickner, H. and Knorr, K.-H., (2023). *elco: Handling data on
> chemical element contents and isotope signatures*. Accessed 25 Sep
> 2023. Online at <https://github.com/henningte/elco>

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :**
[CC-BY-SA-4.0](https://creativecommons.org/licenses/by-sa/4.0/deed.en).

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.

### Sources

Conversion constants between masses and molar amounts of chemical
elements are from
[PeriodicTable](https://github.com/cran/PeriodicTable).

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Masiello.2008" class="csl-entry">

Masiello, C. A., M. E. Gallagher, J. T. Randerson, R. M. Deco, and O. A.
Chadwick. 2008. “Evaluating Two Experimental Approaches for Measuring
Ecosystem Carbon Oxidation State and Oxidative Ratio.” *Journal of
Geophysical Research* 113 (G3): G03010.
<https://doi.org/10.1029/2007JG000534>.

</div>

<div id="ref-Worrall.2016b" class="csl-entry">

Worrall, Fred, Gareth D. Clay, Catherine S. Moody, Tim P. Burt, and Rob
Rose. 2016. “<span class="nocase">The effective oxidation state of a
peatland</span>.” *Journal of Geophysical Research: Biogeosciences* 121
(1): 145–58. <https://doi.org/10.1002/2015JG003182>.

</div>

</div>
