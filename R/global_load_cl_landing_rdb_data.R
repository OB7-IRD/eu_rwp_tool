#' @name global_load_cl_landing_rdb_data
#' @title Load CL landing RDB data
#' @description Process for for load the data from CL landing RDB data file.
#' @param input_path_cl_landing_rdbes_data {\link[base]{character}} expected. Input CL landing RDB data file path (.csv format expected).
#' @return A tibble.
#' @importFrom readr read_csv
#' @export
global_load_cl_landing_rdb_data <- function(input_path_cl_landing_rdb_data) {
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Start process for load data from CL landing RDBES data.\n",
      sep = "")
  # global arguments verifications ----
  # to do in the future
  # process ----
  cl_landing_rdbes_data <- withCallingHandlers(expr = readr::read_csv(file = input_path_cl_landing_rdbes_data,
                                                                      col_names = c("cl_commercial_landing_id",
                                                                                    "landing_country",
                                                                                    "flag_country",
                                                                                    "year",
                                                                                    "quarter",
                                                                                    "month",
                                                                                    "area",
                                                                                    "fishing_ground",
                                                                                    "region",
                                                                                    "statistical_rectangle",
                                                                                    "subpolygon",
                                                                                    "species_aphia_id",
                                                                                    "species",
                                                                                    "species_desc",
                                                                                    "stock",
                                                                                    "landing_category",
                                                                                    "size_category_scale",
                                                                                    "size_category",
                                                                                    "fishing_activity_category_national",
                                                                                    "fishing_activity_category_european_level5",
                                                                                    "fishing_activity_category_european_level6",
                                                                                    "harbour",
                                                                                    "harbour_desc",
                                                                                    "vessel_length_category",
                                                                                    "unallocated_catch_weight",
                                                                                    "area_misreported_catch_weight",
                                                                                    "official_landing_catch_weight",
                                                                                    "landing_multiplier",
                                                                                    "official_landing_value"),
                                                                      col_types = "dccddccccccdccccccccccccnnnnn",
                                                                      skip = 1,
                                                                      na = c("",
                                                                             "NA",
                                                                             "NULL")),
                                               warning = function(input_path_cl_landing_rdbes_data) {
                                                 stop("Troubles with the input file, check the format associated.\n")
                                               })
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Successful process for load data from CL landing RDBES data.\n",
      sep = "")
  return(cl_landing_rdbes_data)
}
