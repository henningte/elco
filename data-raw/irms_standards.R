## code to prepare `irms_standards` dataset goes here

# C and N mass of coffeine (for IAEA 600 standard)
m_caffeine <- units::set_units(194.19, "g_sample/mol")
m_caffeine_C <- units::set_units(8 * PeriodicTable::mass("C"), "g_C/mol")
m_caffeine_N <- units::set_units(4 * PeriodicTable::mass("N"), "g_N/mol")
m_caffeine_O <- units::set_units(2 * PeriodicTable::mass("O"), "g_O/mol")
m_caffeine_H <- units::set_units(10 * PeriodicTable::mass("H"), "g_H/mol")
m_caffeine_S <- units::set_units(0 * PeriodicTable::mass("S"), "g_S/mol")

irms_standards <-
  tibble::tibble(
    standard_name = c("IAEA 600", "BBOT", "BLS", "WFS", "SFS", "OAS_high", "OAS_low"),
    C = c(m_caffeine_C/m_caffeine, units::set_units(c(0.72, 0.4809, 0.398, 0.4126, 0.0915, 0.0161), "g_C/g_sample")),
    N = c( m_caffeine_N/m_caffeine, units::set_units(c(0.064, 0.0212, 0.0136, 0.0147, 0.0062, 0.00133), "g_N/g_sample")),
    S = c(m_caffeine_S/m_caffeine, units::set_units(c(0.0744, 0.00172, 0.00092, 0.00095, 0.0069, 0.00014), "g_S/g_sample")),
    H = c(m_caffeine_H/m_caffeine, units::set_units(c(NA, NA, NA, NA, NA, NA), "g_H/g_sample")),
    O = c(m_caffeine_O/m_caffeine, units::set_units(c(NA, NA, NA, NA, NA, NA), "g_O/g_sample")),
    "13C" = c(-27.771, -25.8, -27.2, -27.21, -13.68, -26.27, -26.66),
    "15N" = c(1, -0.6, -1.7, 2.85, 1.58, 4.42, 7.3),
    threshold_13C = c(0.2, NA, NA, NA, NA, NA, NA),
    threshold_15N = c(0.2 * 2, NA, NA, NA, NA, NA, NA),
    source = c("Coplen.2006", "IVAAnalysentechnikGmbH&Co.KGt.2016", NA, "IVAAnalysentechnikGmbH&Co.KGt.2016", "IVAAnalysentechnikGmbH&Co.KGt.2016", "IVAAnalysentechnikGmbH&Co.KGt.2016", "IVAAnalysentechnikGmbH&Co.KGt.2016")
  ) %>%
  dplyr::mutate(
    dplyr::across(where(~ inherits(.x, "units")), function(.x) errors::set_errors(.x, value = 0))
  )

usethis::use_data(irms_standards, overwrite = TRUE)



