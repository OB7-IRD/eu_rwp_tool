#' @name rwp_table_2_1_template
#' @title Regional Work Plan table 2.1 template generation
#' @description Process for generation the table 2.1 template, list of required species/stocks, for a Regional Work Plan (RWP).
#' @param reference_period_start {\link[base]{integer}} expected. Start of reference period. Be careful, the process needs 3 years at least to run.
#' @param reference_period_end {\link[base]{integer}} expected. End of reference period. Be careful, the process needs 3 years at least to run.
#' @param eu_countries {\link[base]{character}} default to present 27 MS, but can be changed if needed. European Union country(ies) id(s) for data extraction associated. Use 3-alpha country.
#' @param landing_statistics {\link[base]{character}} expected. Landing data statistics source. You can choose between EUROSTAT source (use argument "eurostat", https://ec.europa.eu/eurostat) or regional database source (use argument "rcg_stats").
#' @param rfmo {\link[base]{character}} expected. RFMO's list to include in output.
#' @param input_path {\link[base]{character}} expected. Input path for input files.
#' @param output_path {\link[base]{character}} expected. Output path. By default NULL.
#' @return A list with two elements: "table_2_1_template" and "table_2_1_template_control".
#' @importFrom utils read.table
#' @importFrom dplyr filter rename select left_join tibble relocate group_by summarise mutate case_when
#' @export
rwp_table_2_1_template <- function(reference_period_start,
                                   reference_period_end,
                                   eu_countries = c("AUT", "BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
                                                      "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
                                                      "POL","PRT","ROU","SVK","SVN","ESP","SWE"),
                                   landing_statistics,
                                   rfmo,
                                   input_path,
                                   output_path = NULL) {
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Start process on RWP table 2.1 generation.\n",
      sep = "")
  # global variables assignement ----
  geo <- NULL
  geopolitical_entity <- NULL
  fishreg <- NULL
  level_description <- NULL
  Scientific_name <- NULL
  area <- NULL
  species_code <- NULL
  area_code <- NULL
  definition_year <- NULL
  level_code <- NULL
  initial_quantity <- NULL
  total_quota <- NULL
  landings <- NULL
  tac <- NULL
  share_landing <- NULL
  country <- NULL
  # arguments verifications ----
  # if (codama::r_type_checking(r_object = reference_period_start,
  #                             type = "integer",
  #                             length = as.integer(x = 1),
  #                             output = "logical") != TRUE) {
  #   return(codama::r_type_checking(r_object = reference_period_start,
  #                                  type = "integer",
  #                                  length = as.integer(x = 1),
  #                                  output = "message"))
  # }
  # if (codama::r_type_checking(r_object = reference_period_end,
  #                             type = "integer",
  #                             length = as.integer(x = 1),
  #                             output = "logical") != TRUE) {
  #   return(codama::r_type_checking(r_object = reference_period_end,
  #                                  type = "integer",
  #                                  length = as.integer(x = 1),
  #                                  output = "message"))
  # }
  # if (reference_period_end <= reference_period_start
  #     || (reference_period_end - reference_period_start + 1 < 3)) {
  #   stop(format(x = Sys.time(),
  #               "%Y-%m-%d %H:%M:%S"),
  #        " - Error, invalid \"reference_period\" arguments.\n",
  #        "\"reference_period_end\" must be less than or equal to \"reference_period_start\" and we must have at least 3 years between the two.\n")
  # }
  # if (codama::r_type_checking(r_object = eu_countries,
  #                             type = "character",
  #                             length = as.integer(x = 1),
  #                             output = "logical") != TRUE) {
  #   return(codama::r_type_checking(r_object = eu_countries,
  #                                  type = "character",
  #                                  length = as.integer(x = 1),
  #                                  output = "message"))
  # }
  # if (codama::r_type_checking(r_object = landing_statistics,
  #                             type = "character",
  #                             length = as.integer(x = 1),
  #                             allowed_values = c("eurostat",
  #                                                "rcg_stats"),
  #                             output = "logical") != TRUE) {
  #   return(codama::r_type_checking(r_object = landing_statistics,
  #                                  type = "character",
  #                                  length = as.integer(x = 1),
  #                                  allowed_values = c("eurostat",
  #                                                     "rcg_stats"),
  #                                  output = "message"))
  # }
  # if (codama::r_type_checking(r_object = rfmo,
  #                             type = "character",
  #                             output = "logical") != TRUE) {
  #   return(codama::r_type_checking(r_object = rfmo,
  #                                  type = "character",
  #                                  output = "message"))
  # }
  # if (codama::r_type_checking(r_object = input_path,
  #                             type = "character",
  #                             output = "logical") != TRUE) {
  #   return(codama::r_type_checking(r_object = input_path,
  #                                  type = "character",
  #                                  output = "message"))
  # }
  # if (! is.null(x = output_path)
  #     && codama::r_type_checking(r_object = output_path,
  #                                type = "character",
  #                                output = "logical") != TRUE) {
  #   return(codama::r_type_checking(r_object = output_path,
  #                                  type = "character",
  #                                  output = "message"))
  # }
  # setup ----
  reference_period <- c(reference_period_start:reference_period_end)
  if (length(x = reference_period) < 3) {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Error, the reference period contains less than 3 years.\n",
         sep = "")
  }
  # data imports ----
  # asfis
  asfis <- utils::read.table(system.file("asfis_sp_feb_2018.txt",
                                         package = "rwptool"),
                             header = TRUE,
                             sep = "\t",
                             as.is = TRUE)
  # eurostat
  if (landing_statistics == "eurostat") {
    eurostat_data <- global_load_eurostat_data(path = file.path(input_path,
                                                                "eurostat"))
    geo_data <- utils::read.table(file = system.file("geo.def",
                                                     package = "rwptool"),
                                  header = TRUE,
                                  sep = ";") %>%
      dplyr::filter(geo != "EU28") %>%
      dplyr::rename(country = geopolitical_entity)
    reference_period_eurostat <- reference_period[which(x = reference_period %in% names(x = eurostat_data))]
    if (length(x = reference_period_eurostat) != length(x = reference_period)) {
      cat(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Warning: years of the \"reference_period\" argument are not all available in the EUROSTAT data imported.\n",
          "Year(s) available in EUROSTAT data are: \n",
          paste0(reference_period_eurostat,
                 collapse = ", "),
          ".\n",
          sep = "")
    }
    eurostat_data_final <- dplyr::select(.data = eurostat_data,
                                         species,
                                         fishreg,
                                         geo,
                                         as.character(x = !!reference_period_eurostat)) %>%
      dplyr::left_join(geo_data,
                       by = "geo") %>%
      dplyr::filter(! is.na(x = country)) %>%
      dplyr::left_join(asfis[,c(3:6)],
                       by = c("species" = "X3A_CODE"))
  } else if (landing_statistics == "rcg_stats") {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Error, process associated to \"rcg_stats\" argument not developed yet.\n")
  }
  # fides
  fides_data <- global_load_fides_data(reference_period = reference_period,
                                       file_path = file.path(input_path,
                                                             "fides",
                                                             "export_quota_20220204tl.csv"),
                                       eu_countries = eu_countries)
  # table 2.1 linkage
  table_2_1_linkage <- utils::read.csv(file = system.file("eumap_table_2_1_linkage_version_2022_v1.0.csv",
                                                          package = "rwptool"),
                                       sep = ';',
                                       header = TRUE,
                                       as.is = TRUE,
                                       encoding = 'UTF-8')
  # table 2.1 design ----
  table_2_1_information_final <- data.frame()
  table_control_final <- data.frame()
  for (table_2_1_linkage_id in seq_len(length.out = nrow(x = table_2_1_linkage))) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Be patient, work in progress on row id ",
        table_2_1_linkage_id,
        ".\n",
        sep = "")
    if (landing_statistics == "eurostat") {
      # from eurostat data ----
      country_name <- dplyr::filter(.data = geo_data,
                                    level_description %in% !!country)$country
      species <- unlist(x = strsplit(x = as.character(x = table_2_1_linkage$latin_name[table_2_1_linkage_id]),
                                     split = ','))
      region <- unlist(x = strsplit(x = as.character(x = table_2_1_linkage$area_bis[table_2_1_linkage_id]),
                                    split=','))
      current_eurostat_data <- dplyr::filter(.data = eurostat_data_final,
                                             Scientific_name %in% !!species
                                             & fishreg %in% !!region) %>%
        dplyr::filter(level_description != "GBR")
      if (nrow(x = current_eurostat_data) == 0) {
        current_eurostat_data <- dplyr::tibble(x3a_code = rep(x = table_2_1_linkage[table_2_1_linkage_id, "x3a_code"],
                                                              2),
                                               geo = c(!!country,
                                                       "EU27_2020"),
                                               area = rep(x = table_2_1_linkage[table_2_1_linkage_id, "area"],
                                                          2),
                                               country = c(country,
                                                           "European union (27 MS)"),
                                               source = TRUE,
                                               level_description = c(dplyr::filter(.data = geo_data,
                                                                                   level_description %in% !!country)$level_description,
                                                                     "EU27_2020"),
                                               scientific_name = rep(x = table_2_1_linkage$latin_name[table_2_1_linkage_id],
                                                                     2),
                                               english_name = rep(x = NA,
                                                                  2),
                                               french_name = rep(x = NA,
                                                                 2))
        for (current_year in reference_period_eurostat) {
          current_expression <- parse(text = paste0("current_eurostat_data[1:2, \"",
                                                    current_year,
                                                    "\"] <- as.numeric(x = rep(x = NA, 2))"))
          eval(expr = current_expression)
        }
        years_columns <- names(x = current_eurostat_data)[(length(x = current_eurostat_data) - length(x = reference_period_eurostat) + 1):length(x = current_eurostat_data)]
        current_eurostat_data <- dplyr::relocate(.data = current_eurostat_data,
                                                 !!years_columns,
                                                 .after = area)
      } else {
        current_eurostat_data$source <- TRUE
      }
      current_eurostat_data$mean_years <- round(x = apply(X = current_eurostat_data[,4:6],
                                                          MARGIN = 1,
                                                          FUN = mean,
                                                          na.rm = TRUE),
                                                digits = 4)
      current_eurostat_data_country <- dplyr::filter(.data = current_eurostat_data,
                                                     level_description %in% !!country)
      n_current_eurostat_data_country <- nrow(x = current_eurostat_data_country)
      if (n_current_eurostat_data_country == 0) {
        current_eurostat_data_country <- dplyr::tibble(x3a_code = table_2_1_linkage[table_2_1_linkage_id, "x3a_code"],
                                                       geo = !!country,
                                                       area = table_2_1_linkage[table_2_1_linkage_id, "area"],
                                                       mean_years = NA,
                                                       country = !!country_name,
                                                       source = TRUE,
                                                       level_description = !!country,
                                                       scientific_name = table_2_1_linkage$latin_name[table_2_1_linkage_id],
                                                       english_name = NA,
                                                       french_name = NA)
        for (current_year in reference_period_eurostat) {
          current_expression <- parse(text = paste0("current_eurostat_data_country[1, \"",
                                                    current_year,
                                                    "\"] <- as.numeric(x = NA)"))
          eval(expr = current_expression)
        }
        years_columns <- names(x = current_eurostat_data_country)[(length(x = current_eurostat_data_country) - length(x = reference_period_eurostat) + 1):length(x = current_eurostat_data_country)]
        current_eurostat_data_country <- dplyr::relocate(.data = current_eurostat_data_country,
                                                         !!years_columns,
                                                         .after = area)
      }
      current_eurostat_data_eu <- dplyr::filter(.data = current_eurostat_data,
                                                geo == "EU27_2020")
      if (current_eurostat_data_eu[1, "source"] == TRUE) {
        source_eu <- "eurostat"
      } else {
        source_eu <- "-"
      }
      if (current_eurostat_data_country[1, "source"] == TRUE) {
        source_national <- "eurostat"
      } else {
        source_national <- ""
      }
      # construction table 2.1 line of information
      table_2_1_information <- dplyr::tibble(ms = !!country,
                                             reference_years = paste0(reference_period_eurostat,
                                                                      collapse = ", "),
                                             region = table_2_1_linkage[table_2_1_linkage_id, "region"],
                                             rfmo = table_2_1_linkage[table_2_1_linkage_id, "rfmo"],
                                             spp = table_2_1_linkage[table_2_1_linkage_id, "latin_name"],
                                             area = table_2_1_linkage[table_2_1_linkage_id, "area"],
                                             landings = NA,
                                             source_national = !!source_national,
                                             tac = NA,
                                             share_landing = NA,
                                             source_eu = !!source_eu,
                                             thresh = "Not coded yet",
                                             reg_coord = NA,
                                             covered_length = NA,
                                             selected_bio = NA,
                                             comments_eurostat = NA,
                                             national_stats = NA)
      if (sum(is.na(x = current_eurostat_data_country$mean_years)) < nrow(x = current_eurostat_data_country)) {
        table_2_1_information$landings <- sum(current_eurostat_data_country$mean_years,
                                              na.rm = TRUE)
        table_2_1_information$share_landing <- table_2_1_information$landings / sum(current_eurostat_data_eu$mean_years,
                                                                                    na.rm = TRUE)
      } else {
        table_2_1_information$landings <- 0
        table_2_1_information$share_landing <- 0
      }
      # construction of the control table
      table_control <- dplyr::tibble(region = table_2_1_linkage[table_2_1_linkage_id,
                                                                "region"],
                                     rfmo = table_2_1_linkage[table_2_1_linkage_id,
                                                              "rfmo"],
                                     spp = table_2_1_linkage[table_2_1_linkage_id,
                                                             "latin_name"],
                                     geo = rep(!!country,
                                               each = length(x = reference_period_eurostat)),
                                     area = table_2_1_linkage[table_2_1_linkage_id,
                                                              "area"],
                                     year = reference_period_eurostat,
                                     eurostat_country = apply(current_eurostat_data_country[, as.character(x = reference_period_eurostat)],
                                                              2,
                                                              sum,
                                                              na.rm = TRUE),
                                     eurostat_eu = apply(current_eurostat_data_eu[, as.character(x = reference_period_eurostat)],
                                                         2,
                                                         sum,
                                                         na.rm = TRUE))
    } else if (landing_statistics == "rcg_stats") {
      # from rcg data ----
      stop(format(x = Sys.time(),
                  format = "%Y-%m-%d %H:%M:%S"),
           " - Error, process associated to \"rcg_stats\" argument not developed yet.\n")
    }
    # from fides data ----
    if (! table_2_1_linkage[table_2_1_linkage_id,
                            "fides_area"] %in% "No TAC") {
      fides_area <- unlist(x = strsplit(x = as.character(x = table_2_1_linkage[table_2_1_linkage_id,
                                                                               "fides_area"]),
                                        split = ","))
      fides_specie <- unlist(x = strsplit(x = as.character(x = table_2_1_linkage[table_2_1_linkage_id,
                                                                                 "x3a_code"]),
                                          split = ","))
      reference_period_fides <- reference_period[which(x = reference_period %in% unique(x = fides_data$definition_year))]
      if (length(x = reference_period_fides) != length(x = reference_period)) {
        cat(format(x = Sys.time(),
                   format = "%Y-%m-%d %H:%M:%S"),
            " - Warning: years of the \"reference_period\" argument are not all available in the FIDES data imported.\n",
            "Year(s) available in FIDES data are: \n",
            paste0(reference_period_fides,
                   collapse = ", "),
            ".\n",
            sep = "")
      }
      current_fides_data <- dplyr::filter(.data = fides_data,
                                          species_code %in% !!fides_specie
                                          & area_code %in% !!fides_area
                                          & definition_year %in% !!reference_period_fides)

      current_fides_data_not_tac <- dplyr::filter(.data = fides_data,
                                                   species_code %in% !!fides_specie
                                                   & area_code %in% !!fides_area
                                                   & definition_year %in% !!reference_period_fides
                                                   & level_code != "TAC")
      if (all(! fides_area %in% c("No TAC",
                                  ""))
          && nrow(x = current_fides_data_not_tac) > 0) {
        if (length(x = fides_area) != 1) {
          current_fides_data <- current_fides_data %>%
            dplyr::group_by(level_code,
                            definition_year) %>%
            dplyr::summarise(initial_quantity = sum(initial_quantity),
                             .groups = "drop")
        }
        current_fides_data_country <- current_fides_data %>%
          dplyr::group_by(level_code) %>%
          dplyr::summarise(initial_quantity = sum(initial_quantity),
                           .groups = "drop")
        fides_country <- dplyr::filter(.data = current_fides_data_country,
                                       level_code == !!country)$initial_quantity
        fides_eu <- dplyr::filter(.data = current_fides_data_country,
                                  level_code == "XEU")$initial_quantity
        fides_tac <- dplyr::filter(.data = current_fides_data_country,
                                   level_code == "TAC")$initial_quantity
        if (length(x = fides_country) == 1) {
          table_2_1_information$tac <- fides_country / fides_eu
        } else {
          table_2_1_information$tac <- NA
        }
        current_fides_data_country <- dplyr::mutate(.data = current_fides_data_country,
                                                    total_quota = initial_quantity / !!fides_eu)
        if (! is.na(x = table_2_1_information$tac)
            && (table_2_1_information$tac < 0.1
                & table_2_1_information$tac > 0)) {
          table_2_1_information$comment_fides <- sum(dplyr::filter(.data = current_fides_data_country,
                                                                   total_quota < 0.1)$total_quota)

        } else {
          table_2_1_information$comment_fides <- NA
        }
        if (! is.na(x = fides_country)
            && length(x = fides_country) != 0) {
          table_2_1_information$comment_fides_bis <- paste("FIDES initial quantity mean = ",
                                                           round(x = fides_country / length(x = reference_period_fides),
                                                                 digits = 0))
        } else {
          table_2_1_information$comment_fides_bis <- NA
        }
        if (! is.na(x = table_2_1_information$comment_fides)) {
          table_2_1_information$comment_fides_25_rule <- paste0("Sum of MS TAC's below 10% = ",
                                                                round(x = as.numeric(x = table_2_1_information$comment_fides) * 100,
                                                                      digits = 0),
                                                                "% of EU TAC")
        } else {
          table_2_1_information$comment_fides_25_rule <- NA
        }
      } else {
        table_2_1_information$comment_fides <- NA
        table_2_1_information$comment_fides_bis <- NA
        table_2_1_information$comment_fides_25_rule <- NA
      }
    } else {
      table_2_1_information$tac <- -999
      table_2_1_information$comment_fides <- NA
      table_2_1_information$comment_fides_bis <- NA
      table_2_1_information$comment_fides_25_rule <- NA
    }
    table_2_1_information_final <- rbind(table_2_1_information_final,
                                         table_2_1_information)
    table_control_final <- rbind(table_control_final,
                                 table_control)
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Work done on row id ",
        table_2_1_linkage_id,
        ".\n",
        sep = "")
  }
  # formatting ----


  table_2_1_information_final_2 <- table_2_1_information_final %>%
    dplyr::mutate(
      thresh = dplyr::case_when(
        round(tac, 2) < 0.1 & tac > 0 ~ "TAC < 10%",
        tac %in% c("NA%", "NaN%", "Inf%",-999) &
          round(share_landing, 2) < 0.1 &
          share_landing > 0 ~ "Landings < 10%",
        round(landings, 0) < 200 ~ "Landings < 200t.",
        TRUE ~ thresh
      ),
      landings = as.character(x = round(x = landings, 0)),
      landings = dplyr::case_when(landings == 0 ~ "None", TRUE ~ landings),
      tac = paste0(round(100 * tac, 0), "%"),
      tac = dplyr::case_when(
        tac %in% c("NA%", "NaN%", "Inf%")
        |
          (spp == "Nephrops norvegicus" # kibi - I don't get this one
           & !(grepl(
             pattern = "TAC", x = area
           ))) ~ "None",
        tac == "-99900%" ~ NA_character_,
        TRUE ~ tac
      ),
      share_landing = paste0(round(x = 100 * share_landing, 0), "%"),
      share_landing = dplyr::case_when(
        share_landing %in% c("NA%", "NaN%", "Inf%", "0%") ~ "None",
        TRUE ~ share_landing
      ),
      thresh = dplyr::case_when(
        landings == "None"
        | tac == "None"
        | (share_landing == "None"
           & tac == "None")
        | (rfmo %in% c("ICCAT", "IOTC", "WCPFC"))
        | (spp %in% c("Anguilla anguilla", "salmo salar", "salmo trutta ")) ~ "None",
        TRUE ~ thresh
      ),
      covered_length = " ",
      selected_bio = " ",
      reg_coord = " "
    )
  rwp_table_2_1_export = list("table_2_1_template" = table_2_1_information_final_2,
                              "table_2_1_template_control" = table_control_final)
  names_export <- names(x = rwp_table_2_1_export)
  for (export_id in seq_len(length.out = length(x = names_export))) {
    names(x = rwp_table_2_1_export)[export_id] <- paste(names(rwp_table_2_1_export)[export_id],
                                                        tolower(x = country),
                                                        sep = "_")
  }
  # export ----
  if (! is.null(x = output_path)) {
    for (output_id in seq_len(length.out = length(x = rwp_table_2_1_export))) {
      utils::write.table(x = rwp_table_2_1_export[[output_id]],
                         file = file.path(output_path,
                                          paste0(format(x = Sys.time(),
                                                        format = "%Y%m%d_%H%M%S_"),
                                                 names(x = rwp_table_2_1_export)[output_id],
                                                 ".csv")),
                         row.names = FALSE,
                         sep = ";",
                         dec = ",")
    }
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Successful output generation in the folder:\n",
        output_path,
        "\n",
        sep = "")
  }
  cat(format(x = Sys.time(),
             format = "%Y-%m-%d %H:%M:%S"),
      " - Process RWP table 2.1 generation successfully ended.\n",
      sep = "")
  return(rwp_table_2_1_export)
}
