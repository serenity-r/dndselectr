.onLoad <- function(libname, pkgname) {
  registerInputHandler("ds-fix-settings", fixSettings, force = TRUE)

  invisible()
}
