
<!-- README.md is generated from README.Rmd. Please edit that file -->

# elco

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

elco provides functions to handle and (rudimentary) analyse element
content data and isotope signatur data. Fetures are:

1.  **Import of XRF and IRMS data** as they can be exported from … and
    X-ray fluorescence spectrometers (Rigaku ZSX Primus II).

2.  Graphical display for easy **data checking**.

3.  **Correction of IRMS-measured element content data and isotope
    signatures** in cases of a mismatch in the signal areas between
    samples and standards.

4.  **Unit conversion** of element content data (e.g. g/g to mol).

5.  Computation of the **nominal oxidation state of carbon (NOSC),
    oxidative ratio (OR), and degree of unsaturation (DU)**
    \[@Worrall.2016b\].

### How to use

Examples will be provided here as soon as sample data is available.

Further information can be found in the vignettes:

  - [Reformatting and checking raw data (csv files) from the X-ray
    fluorescence device (ZSX Primus II, Rigaku)](vignettes/v001-xrf.Rmd)

  - [Importing and Manipulating IRMS
    data](vignettes/v002-irms-functions.Rmd)

  - [Computation with element contents - unit conversion, element
    ratios, nominal oxidation state of carbon, oxidative ratio, and
    degree of unsaturation](vignettes/v004-element-ratios.Rmd)

### How to cite

Please cite this compendium as:

> Teickner, H. and Knorr, K.-H., (2020). *elco: Handling data on
> chemical element contents and isotope signatures.*. Accessed 29 Sep
> 2020. Online at <https://github.com/henningte/elco>

### Licenses

**Text and figures :**
[CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :**
[CC-BY-SA-4.0](https://creativecommons.org/licenses/by-sa/4.0/deed.en)

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
