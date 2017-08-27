.onLoad <- function(libname, pkgname) {
  reticulate::py_available(TRUE)
  suppressMessages(suppressWarnings({
    path <- system.file('ws_model.hdf5', package = 'safadao')
    modelo <<- keras::load_model_hdf5(path)
  }))
}
