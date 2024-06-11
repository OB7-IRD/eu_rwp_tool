#' @name global_load_fides_data
#' @title Load FIDES data
#' @description Process for Load FIDES data from EU "official" export quota csv file.
#' @param reference_period {\link[base]{integer}} expected. Period of reference, in years.
#' @param file_path {\link[base]{character}} expected. Input FIDES file path.
#' @param eu_countries {\link[base]{character}} expected. European Union country(ies) id(s) for data extraction associated. Use 3-alpha country.
#' @return The function return a {\link[tibble]{tibble}}.
#' @export
global_load_fides_data <- function(reference_period,
                                   file_path,
                                   eu_countries) {
  message(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Start process for load data from FIDES data.")
  # global variables assignement ----
  level_code <- NULL
  # process ----
  countries <- c(eu_countries,
                 "XEU", "TAC")
  # countries <- c(eu_countries,
  #                "XEU")
  fides_file <- utils::read.table(file = file_path,
                                  dec = ".",
                                  sep = ";",
                                  header = TRUE)
  tac_final <- dplyr::filter(.data = fides_file,
                             level_code %in% !!countries)
  if (nrow(x = tac_final) == 0) {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - No FIDES data available regarding the input arguments.")
  } else {
    message(format(x = Sys.time(),
                   format = "%Y-%m-%d %H:%M:%S"),
            " - Successful process for load data from FIDES data.")
  }
  return(tac_final)
}
