#' Load visible and invisible package functions
#'
#' This tends to be useful when debugging
#'
#' @param pkg_name The string of the package whose functions you would like
#'   to load.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{load_pkg_fns(BayesfMRI)}
load_pkg_fns <- function(pkg_name) {
  r <- unclass(lsf.str(envir = asNamespace(pkg_name), all = T))
  for(name in r) eval(parse(text=paste0(name, '<-',pkg_name,':::', name)))
  cat("All functions (visible and invisible) loaded for package:",pkg_name,"\n")
}
