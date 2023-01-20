#' @name global_load_fides_data
#' @title Load FIDES data
#' @description Process for Load FIDES data from EU "official" export quota csv file.
#' @param reference_period {\link[base]{integer}} expected. Period of reference, in years.
#' @param file_path {\link[base]{character}} expected. Input FIDES file path.
#' @param country {\link[base]{character}} expected. Country(ies) id(s) for data extraction associated. Use 3-alpha country.
#' @return The function return a {\link[tibble]{tibble}}.
#' @export
#' @importFrom dplyr filter
global_load_fides_data <- function(reference_period,
                                   file_path,
                                   country = NULL) {
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Start process for load data from FIDES data.\n",
      sep = "")
  # global variables assignement ----
  level_code <- NULL
  # # global arguments verifications ----
  # if (codama::r_type_checking(r_object = reference_period,
  #                             type = "integer",
  #                             output = "logical") != TRUE) {
  #   return(codama::r_type_checking(r_object = reference_period,
  #                                  type = "integer",
  #                                  output = "message"))
  # }
  # if (codama::r_type_checking(r_object = file_path,
  #                             type = "character",
  #                             length = 1L,
  #                             output = "logical") != TRUE) {
  #   return(codama::r_type_checking(r_object = file_path,
  #                                  type = "character",
  #                                  length = 1L,
  #                                  output = "message"))
  # }
  # if (codama::r_type_checking(r_object = country,
  #                             type = "character",
  #                             output = "logical") != TRUE) {
  #   return(codama::r_type_checking(r_object = country,
  #                                  type = "character",
  #                                  output = "message"))
  # }
  # process ----
  countries <- c(country,
                 "XEU")
  fides_file <- utils::read.table(file = file_path,
                                  dec = ".",
                                  sep = ";",
                                  header = TRUE)
  tac_final <- dplyr::filter(.data = fides_file,
                             level_code %in% !!countries)
  if (nrow(x = tac_final) == 0) {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Error, no FIDES data available regarding the input arguments.\n",
         sep = "")
  } else {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Successful process for load data from FIDES data.\n",
        sep = "")
  }
  return(tac_final)
}
