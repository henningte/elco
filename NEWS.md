# elco 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

## Bug fixes

*  Fix an error in `elco_irms_correct_elements()` caused by `errors::errors()` not dropping units any more.

## Improvements

* `elco_irms_correct_isotopes()`, `elco_irms_correct_elements()`: Cleaning up code. Improving plotting (Separating points of corrected and uncorrected data). Improving printing of plot objects (does not print the list of plots any more).
*`elco_xrf_import_csv()`: now can also import intensities instead of only elemental contents.
