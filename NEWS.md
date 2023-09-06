# elco 0.1.0.9000

This version introduces major changes which break past code:
* The `elco` class is deprecated. Instead, the 'elco' package defines custom units via the 'units' package to differentiate between masses and amounts of chemical elements.
* The `xrf` class is deprecated. `elco_xrf_import_csv()` now returns a data frame. This was done because there are not yet any methods for this type of data which would warrant introducing a new class.
* `elco_nosc()`, `elco_du()`, `elco_or()` now use capital letters as arguments for chemical elements.
* `elco_convert()` replaces `elco_elco_convert()` and there is no `elco_elco_convert_df()` function any more. Instead, you can use `elco_convert()`.


# elco 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

## Bug fixes

*  Fix an error in `elco_irms_correct_elements()` caused by `errors::errors()` not dropping units any more.

## Improvements

* `elco_irms_correct_isotopes()`, `elco_irms_correct_elements()`: Allowing more flexible use of standards by adding parameter `irms_standards_to_use`. Cleaning up code. Improving plotting (Separating points of corrected and uncorrected data). Improving printing of plot objects (does not print the list of plots any more).
*`elco_xrf_import_csv()`: now can also import intensities instead of only elemental contents.
