#' @name global_load_cl_landing_rdbes_data
#' @title Load CL landing RDBES data
#' @description Process for for load the data from CL landing RDBES data file.
#' @param input_path_file_rdbes_data {\link[base]{character}} expected. Input RDBES CL data file path (.csv format expected).
#' @return A tibble.
#' @export
global_load_cl_landing_rdbes_data <- function(input_path_file_rdbes_data) {
  message(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Start process for load data from CL landings RDB data.")
  # global variables assignement ----
  Year <- NULL
  Species <- NULL
  Area <- NULL
  geo <- NULL
  OfficialLandingCatchWeight <- NULL
  FlagCountry <- NULL
  TLW <- NULL
  # process ----
  # geo data (country codes)
  geo_data <- utils::read.table(file = system.file("geo.def",
                                                   package = "rwptool"),
                                header = TRUE,
                                sep = ";")
  EU27_2020 <- dplyr::filter(.data = geo_data,
                             ! level_description %in% c("EEC",
                                                        "GBR")) %>%
    dplyr::pull(level_description)
  cl_landing_rdbes_data_ori <- data.table::fread(input_path_file_rdbes_data,
                                                 sep = ",",
                                                 quote = "")
  # If read.csv, then set quote="" otherwise it skips a lot of data without warning
  # Only include BMS and LAN
  cl_landing_rdbes_data <- subset(cl_landing_rdbes_data_ori,
                                  CLcatchCategory %in% c("Lan",
                                                         "BMS"))
  # transform RDBES data into the same format as EUROSTAT
  # Add country codes from geo data file
  cl_landing_rdbes_data_1 <- dplyr::left_join(cl_landing_rdbes_data,
                                              geo_data,
                                              by = c("CLvesselFlagCountry" = "geo"),
                                              keep = T)
  # Add scientific name
  rdbes_spp <- icesVocab::getCodeList("SpecWoRMS")
  cl_landing_rdbes_data_2 <- dplyr::left_join(cl_landing_rdbes_data_1,
                                              rdbes_spp,
                                              by = c("CLspeciesCode" = "Key"))
  cl_landing_rdbes_data_2$CLspeciesName <- cl_landing_rdbes_data_2$Description
  # Summarise and create a EU27_2020 line
  cl_landing_rdbes_data_ctry <- dplyr::summarise(.data = dplyr::group_by(.data = cl_landing_rdbes_data_2,
                                                                         CLyear,
                                                                         CLspeciesName,
                                                                         CLarea,
                                                                         geo),
                                                 TLW = sum(CLofficialWeight / 1000,
                                                           na.rm = T),
                                                 .groups = "drop")
  cl_landing_rdbes_data_eu27 <- dplyr::summarise(.data = dplyr::group_by(.data = dplyr::filter(cl_landing_rdbes_data_2,
                                                                                               level_description %in% !!EU27_2020),
                                                                         CLyear,
                                                                         CLspeciesName,
                                                                         CLarea),
                                                 geo = "EU27_2020",
                                                 TLW = sum(CLofficialWeight / 1000,
                                                           na.rm = T),
                                                 .groups = "drop")
  # Include code for EU28 + warning if UK countries are missing
  cl_landing_rdbes_data_3 <- rbind(cl_landing_rdbes_data_ctry,
                                   cl_landing_rdbes_data_eu27)
  cl_landing_rdbes_data_t <- tidyr::spread(cl_landing_rdbes_data_3,
                                           key = CLyear,
                                           value = TLW,
                                           fill = 0) # To make sure the mean get right!
  cl_landing_rdbes_data_t <- dplyr::rename(.data = cl_landing_rdbes_data_t,
                                           Scientific_Name = CLspeciesName)
  message(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Successful process for load data from CL landings RDBES data.")
  return(cl_landing_rdbes_data_t)
}
