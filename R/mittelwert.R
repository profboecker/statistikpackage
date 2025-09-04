#' Mittelwertfunktion
#'
#' @returns arithmetisches Mittel
#' @param daten Daten zur Berechnung
#' @export
mittelwert <- function(daten) {
  return(mean(daten))
}

#' Standardabweichung (unkorrigiert)
#'
#' @param daten Daten zur Berechnung
#'
#' @import stats
#'
#' @returns unkorrigierte Standardabweichung
#' @export
standardabweichung <- function(daten) {
  return(sd(daten) / sqrt(length(daten) / (length(daten)-1)))
}
