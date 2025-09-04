#' Mittelwertfunktion
#'
#' @returns arithmetisches Mittel
#' @param ... same arguments as the mean function
#' @export
mittelwert <- function(... ) {
  return(mean(... ))
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

#' Datensatz in Excel öffnen
#'
#' @import readr fs stringr
#'
#' @param daten Datensatz mit den Daten, die in Excel gezeigt werden sollen
#' @export
oeffne_in_excel <- function(daten) {
  csv_file <- stringr::str_glue("{tempfile()}.csv")
  readr::write_csv(
    x = daten,
    file = csv_file,
    na = ""
  )
  fs::file_show(path = csv_file)
}
