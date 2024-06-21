#' @name rwp_table_2_1_template
#' @title Regional Work Plan table 2.1 template generation
#' @description Process for generation the table 2.1 template, list of required species/stocks, for a Regional Work Plan (RWP).
#' @param reference_period_start {\link[base]{integer}} expected. Start of reference period. Be careful, the process needs 3 years at least to run.
#' @param reference_period_end {\link[base]{integer}} expected. End of reference period. Be careful, the process needs 3 years at least to run.
#' @param eu_countries {\link[base]{character}} expected. By default the 27 EU member states. European Union country(ies) id(s) for data extraction associated. Use 3-alpha country.
#' @param rcg_countries {\link[base]{character}} expected. By default NULL. You can choose between "lp", "nansea" or "baltic" to select automatically EU countries associated with the RCG. If the value if not one of this, value(s) of "eu_countries" (for lp, nansea and baltic) and "rfmo" (for lp) arguments will be used instead.
#' @param landing_data_source {\link[base]{character}} expected. Landing data statistics source. You can choose between EUROSTAT source (use value "eurostat", https://ec.europa.eu/eurostat/fr/web/fisheries/database) or RCG agreed statistics (use value "rcg_agreed_statistics"). So far value "rcg_agreed_statistics" is associated to RDBES data.
#' @param rfmo {\link[base]{character}} expected. RFMO's list to include in output. By default CCAMLR, CECAF, GFCM, IATTC, ICCAT, ICES, IOTC, NAFO, SEAFO, SPRFMO, WCPFC, WECAFC.
#' @param input_path_directory_eurostat {\link[base]{character}} expected. Input path directory for input eurostat files.
#' @param input_path_file_rcg_agreed_statistics {\link[base]{character}} expected. Input path and file name for input RDBES CL landing file.
#' @param input_path_file_fides {\link[base]{character}} expected. Input path and file name for input FIDES file.
#' @param linkage_file {\link[base]{character}} expected. Name of linkage file.
#' @param output_path {\link[base]{character}} expected. Output path. By default NULL.
#' @return A list with two elements: "table_2_1_template" and "table_2_1_template_control".
#' @export
rwp_table_2_1_template <- function(reference_period_start,
                                   reference_period_end,
                                   eu_countries = c("AUT",
                                                    "BEL",
                                                    "BGR",
                                                    "HRV",
                                                    "CYP",
                                                    "CZE",
                                                    "DNK",
                                                    "EST",
                                                    "FIN",
                                                    "FRA",
                                                    "DEU",
                                                    "GRC",
                                                    "HUN",
                                                    "IRL",
                                                    "ITA",
                                                    "LVA",
                                                    "LTU",
                                                    "LUX",
                                                    "MLT",
                                                    "NLD",
                                                    "POL",
                                                    "PRT",
                                                    "ROU",
                                                    "SVK",
                                                    "SVN",
                                                    "ESP",
                                                    "SWE"),
                                   rcg_countries = NULL,
                                   landing_data_source,
                                   rfmo = c("CCAMLR",
                                            "CECAF",
                                            "GFCM",
                                            "IATTC",
                                            "ICCAT",
                                            "ICES",
                                            "IOTC",
                                            "NAFO",
                                            "SEAFO",
                                            "SPRFMO",
                                            "WCPFC",
                                            "WECAFC"),
                                   input_path_directory_eurostat = NULL,
                                   input_path_file_rcg_agreed_statistics = NULL,
                                   input_path_file_fides,
                                   linkage_file,
                                   output_path = NULL,
                                   full_name_output = FALSE) {
  # global variables assignement ----
  geo <- NULL
  geopolitical_entity <- NULL
  fishreg <- NULL
  country <- NULL
  level_description <- NULL
  scientific_name <- NULL
  mean_years <- NULL
  landings <- NULL
  area <- NULL
  share_landing <- NULL
  tac <- NULL
  year <- NULL
  eurostat_country <- NULL
  eurostat_eu <-NULL
  species_code <- NULL
  area_code <- NULL
  definition_year <- NULL
  level_code <- NULL
  initial_quantity <- NULL
  total_quota <- NULL
  comment_fides <- NULL
  comment_fides_bis <- NULL
  Scientific_Name <- NULL
  Area <- NULL
  ms <- NULL
  reference_years <- NULL
  spp <- NULL
  thresh <- NULL
  reg_coord <- NULL
  covered_length <- NULL
  selected_bio <- NULL
  comments <- NULL
  # arguments verifications ----
  # to do in the future
  # setup ----
  #browser()
  reference_period <- c(reference_period_start:reference_period_end)
  if (length(x = reference_period) < 3) {
    warning(format(x = Sys.time(),
                   format = "%Y-%m-%d %H:%M:%S"),
            " - The reference period contains less than 3 years.")
  }
  if (! is.null(x = rcg_countries)) {
    if (rcg_countries == "lp") {
      eu_countries <- c("IRL", "PRT", "ESP", "FRA", "ITA", "HRV", "MLT", "GRC", "CYP")
      rfmo <- c("WCPFC", "IATTC", "ICCAT", "IOTC")
    } else if (rcg_countries == "nansea") {
      eu_countries <- c("SWE", "EST", "BEL", "ESP", "DEU", "DNK", "FRA", "IRL", "LTU", "LVA", "NLD", "POL", "PRT")
      rfmo <- c("CCAMLR", "CECAF", "GFCM", "ICES", "NAFO", "SEAFO", "SPRFMO", "WECAFC")
    } else if (rcg_countries == "baltic") {
      eu_countries <- c("DNK", "EST", "FIN", "DEU", "LVA", "LTU", "POL", "SWE")
      rfmo <- c("CCAMLR", "CECAF", "GFCM", "ICES", "NAFO", "SEAFO", "SPRFMO", "WECAFC")
    } else {
      warning(format(x = Sys.time(),
                     format = "%Y-%m-%d %H:%M:%S"),
              " - Value \"",
              rcg_countries,
              "\" of the argument \"rcg_countries\" is not filled in the process yet. Value(s) of the arguments \"eu_countries\" and \"rfmo\" use for the process instead.")
      rcg_countries <- ""
    }
  }
  message(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Start process on RWP table 2.1 generation on the reference period ",
          reference_period_start,
          " to ",
          reference_period_end,
          " and on the following:\ncountry(ies): ",
          paste(eu_countries,
                collapse = ", "),
          "\nRFMO(s): ",
          paste(rfmo,
                collapse = ", "))
  # data imports ----
  # asfis
  asfis <- utils::read.table(system.file("asfis_sp_2022_rev1.txt",
                                         package = "rwptool"),
                             header = TRUE,
                             sep = ",",
                             as.is = TRUE)
  # geo data (country codes)
  geo_data <- utils::read.table(file = system.file("geo.def",
                                                   package = "rwptool"),
                                header = TRUE,
                                sep = ";") %>%
    dplyr::filter(geo != "EU28") %>%
    dplyr::rename(country = geopolitical_entity)
  # eurostat
  if (landing_data_source == "eurostat") {
    eurostat_data <- global_load_eurostat_data(path = input_path_directory_eurostat)
    reference_period_data <- reference_period[which(x = reference_period %in% names(x = eurostat_data))]
    if (! all(reference_period %in% reference_period_data)) {
      warning(format(x = Sys.time(),
                     format = "%Y-%m-%d %H:%M:%S"),
              " - Year(s) of the \"reference_period\" argument are not all available in the EUROSTAT data imported.\n",
              "Year(s) not available in EUROSTAT data are: \n",
              paste0(dplyr::setdiff(x = reference_period,
                                    y = reference_period_data),
                     collapse = ", "),
              ".")
    }
    landing_data_source_final <- dplyr::select(.data = eurostat_data,
                                               species,
                                               fishreg,
                                               geo,
                                               as.character(x = !!reference_period_data)) %>%
      dplyr::left_join(geo_data,
                       by = "geo") %>%
      dplyr::filter(! is.na(x = country)) %>%
      dplyr::left_join(asfis[, c(3:6)],
                       by = c("species" = "X3A_CODE"))
  } else if (landing_data_source == "rcg_agreed_statistics") {
    rcg_stats_data <- global_load_cl_landing_rdbes_data(input_path_file_rdbes_data = input_path_file_rcg_agreed_statistics)
    reference_period_data <- reference_period[which(x = reference_period %in% names(x = rcg_stats_data))]
    if (! all(reference_period %in% reference_period_data)) {
      warning(format(x = Sys.time(),
                     format = "%Y-%m-%d %H:%M:%S"),
              " - Year(s) of the \"reference_period\" argument are not all available in the RCG agreed statistics data imported.\n",
              "Year(s) not available in RCG agreed statistics data are: \n",
              paste0(dplyr::setdiff(x = reference_period,
                                    y = reference_period_data),
                     collapse = ", "),
              ".")
    }
    landing_data_source_final <- dplyr::select(.data = rcg_stats_data,
                                               Scientific_Name,
                                               CLarea,
                                               geo,
                                               as.character(x = !!reference_period_data)) %>%
      dplyr::left_join(geo_data,
                       by = "geo") %>%
      dplyr::filter(! is.na(x = country)) %>%
      dplyr::rename(fishreg = CLarea)
  } else {
    stop(format(x = Sys.time(),
                format = "%Y-%m-%d %H:%M:%S"),
         " - Value \"",
         landing_data_source,
         "\" of argument \"landing_data_source\" not associated to a process yet.")
  }
  # fides
  fides_data <- global_load_fides_data(reference_period = reference_period,
                                       file_path = input_path_file_fides,
                                       eu_countries = eu_countries)
  # table 2.1 linkage
  table_2_1_linkage <- utils::read.csv(file = system.file(paste0(linkage_file,
                                                                 ".csv"),
                                                          package = "rwptool"),
                                       sep = ';',
                                       header = TRUE,
                                       encoding = 'UTF-8') %>%
    dplyr::filter(rfmo %in% !!rfmo)
  table_2_1_linkage <- subset(table_2_1_linkage,
                              include_in_table_2.1 == "yes")
  table_2_1_linkage$id <- row.names(x = table_2_1_linkage)
  # table 2.1 design ----
  table_2_1_information_final <- data.frame()
  table_control_final <- data.frame()
  for (table_2_1_linkage_id in seq_len(length.out = nrow(x = table_2_1_linkage))) {
    if(table_2_1_linkage_id == 39) {
      browser()
    }
    message(format(x = Sys.time(),
                   format = "%Y-%m-%d %H:%M:%S"),
            " - Be patient, work in progress on row id ",
            table_2_1_linkage_id)
    country_name <- dplyr::filter(.data = geo_data,
                                  level_description %in% !!eu_countries)$country
    species <- unlist(x = strsplit(x = as.character(x = table_2_1_linkage$latin_name[table_2_1_linkage_id]),
                                   split = ','))
    region <- unlist(x = strsplit(x = as.character(x = table_2_1_linkage$area_bis[table_2_1_linkage_id]),
                                  split=','))
    current_data <- dplyr::filter(.data = landing_data_source_final,
                                  Scientific_Name %in% !!species
                                  & fishreg %in% !!region) %>%
      dplyr::filter(level_description != "GBR")
    if (nrow(x = current_data) == 0) {
      current_data <- dplyr::tibble(species = table_2_1_linkage[table_2_1_linkage_id,
                                                                "x3a_code"],
                                    fishreg = table_2_1_linkage[table_2_1_linkage_id,
                                                                "area"],
                                    geo = c(dplyr::filter(.data = geo_data,
                                                          level_description %in% !!eu_countries)$geo,
                                            "EU27_2020"),
                                    country = c(!!country_name,
                                                "European union (27 MS)"),
                                    level_description = c(!!eu_countries,
                                                          "EU27_2020"),
                                    scientific_name = table_2_1_linkage$latin_name[table_2_1_linkage_id],
                                    english_name = NA_character_,
                                    french_name = NA_character_,
                                    source = TRUE)
      for (current_year in reference_period_data) {
        current_expression <- parse(text = paste0("current_data[, \"",
                                                  current_year,
                                                  "\"] <- NA_real_"))
        eval(expr = current_expression)
      }
      current_data <- dplyr::relocate(.data = current_data,
                                      !!as.character(x = reference_period_data),
                                      .after = geo)
    } else {
      current_data$source <- TRUE
    }
    if (all(is.na(apply(X = current_data[,c(as.character(x = reference_period_data))],
                        MARGIN = 1,
                        FUN = mean,
                        na.rm = TRUE)))) {
      current_data$mean_years = NA_real_
    } else {
      current_data <- dplyr::mutate(.data = current_data,
                                    mean_years = apply(X = current_data[,c(as.character(x = reference_period_data))],
                                                                 MARGIN = 1,
                                                                 FUN = mean,
                                                                 na.rm = TRUE)) %>%
        dplyr::filter((! is.na(x = mean_years))
                      | geo == "EU27_2020")
      # current_data <- dplyr::mutate(.data = current_data,
      #                               mean_years = round(x = apply(X = current_data[,c(as.character(x = reference_period_data))],
      #                                                            MARGIN = 1,
      #                                                            FUN = mean,
      #                                                            na.rm = TRUE),
      #                                                  digits = 4)) %>%
      #   dplyr::filter((! is.na(x = mean_years))
      #                 | geo == "EU27_2020")
    }
    #current_data_country <- dplyr::filter(.data = current_data,
                                         #level_description %in% !!eu_countries)
    current_data_country <- dplyr::filter(.data = current_data,
                                         level_description != "EEC")
    n_current_data_country <- nrow(x = current_data_country)
    if (n_current_data_country == 0) {
      current_data_country <- dplyr::tibble(species = table_2_1_linkage[table_2_1_linkage_id,
                                                                        "x3a_code"],
                                            fishreg = table_2_1_linkage[table_2_1_linkage_id,
                                                                        "area"],
                                            geo = c(dplyr::filter(.data = geo_data,
                                                                  level_description %in% !!eu_countries)$geo,
                                                    "EU27_2020"),
                                            country = c(!!country_name,
                                                        "European union (27 MS)"),
                                            level_description = c(!!eu_countries,
                                                                  "EU27_2020"),
                                            scientific_name = table_2_1_linkage$latin_name[table_2_1_linkage_id],
                                            english_name = NA_character_,
                                            french_name = NA_character_,
                                            source = TRUE,
                                            mean_years = NA_real_)
      for (current_year in reference_period_data) {
        current_expression <- parse(text = paste0("current_data_country[, \"",
                                                  current_year,
                                                  "\"] <- NA_real_"))
        eval(expr = current_expression)
      }
      current_data_country <- dplyr::relocate(.data = current_data_country,
                                              !!as.character(x = reference_period_data),
                                              .after = geo)
    }
    current_data_eu <- dplyr::filter(.data = current_data,
                                     geo == "EU27_2020")
    if (current_data_eu[1, "source"] == TRUE) {
      source_eu <- landing_data_source
    } else {
      source_eu <- "-"
    }
    if (current_data_country[1, "source"] == TRUE) {
      source_national <- landing_data_source
    } else {
      source_national <- ""
    }
    # construction table 2.1 line of information
    # share_landing = round(x = landings / sum(current_data_eu$mean_years),
    #                       digits = 4))
    table_2_1_information <- dplyr::tibble(ms = !!eu_countries) %>%
      dplyr::full_join(dplyr::group_by(.data = current_data_country,
                                       level_description) %>%
                         dplyr::summarise(landings = sum(mean_years),
                                          share_landing = landings / sum(current_data_eu$mean_years)),
                       by = c("ms" = "level_description")) %>%
      dplyr::mutate(reference_years = paste0(reference_period_data,
                                             collapse = ", "),
                    region = table_2_1_linkage[table_2_1_linkage_id,
                                               "region"],
                    rfmo = table_2_1_linkage[table_2_1_linkage_id,
                                             "rfmo"],
                    spp = table_2_1_linkage[table_2_1_linkage_id,
                                            "latin_name"],
                    area = table_2_1_linkage[table_2_1_linkage_id,
                                             "area"],
                    source_national = !!source_national,
                    source_eu = !!source_eu,
                    thresh = "None",
                    reg_coord = NA_character_,
                    covered_length = NA_character_,
                    selected_bio = NA_character_,
                    comments_eurostat = NA_character_,
                    national_stats = NA_character_) %>%
      dplyr::relocate(landings, .after = area) %>%
      dplyr::relocate(share_landing, .after = tac) %>%
      dplyr::mutate(landings = dplyr::case_when(
        is.na(x = landings) ~ 0,
        TRUE ~ landings
      ),
      share_landing = dplyr::case_when(
        is.na(x = share_landing) ~ 0,
        TRUE ~ share_landing
      ))
    # construction of the control table
    table_control <- dplyr::tibble(region = table_2_1_linkage[table_2_1_linkage_id,
                                                              "region"],
                                   rfmo = table_2_1_linkage[table_2_1_linkage_id,
                                                            "rfmo"],
                                   spp = table_2_1_linkage[table_2_1_linkage_id,
                                                           "latin_name"],
                                   geo = rep(x = table_2_1_information$ms,
                                             each = length(x = reference_period_data)),
                                   area = table_2_1_linkage[table_2_1_linkage_id,
                                                            "area"],
                                   year = rep(x = reference_period_data,
                                              times = length(x = table_2_1_information$ms)),
                                   eurostat_eu = rep(x = apply(current_data_eu[, as.character(x = reference_period_data)],
                                                               2,
                                                               sum,
                                                               na.rm = TRUE),
                                                     times = length(x = table_2_1_information$ms))) %>%
      dplyr::left_join(dplyr::group_by(.data = current_data_country[, c("level_description",
                                                                        reference_period_data)],
                                       level_description) %>%
                         dplyr::summarise(dplyr::across(.cols = dplyr::everything(),
                                                        .fns = ~ sum(x = .,
                                                                     na.rm = TRUE))) %>%
                         tidyr::pivot_longer(cols = !level_description,
                                             names_to = "year",
                                             values_to = "eurostat_country") %>%
                         dplyr::mutate(year = as.integer(x = year)),
                       by = c("geo" = "level_description",
                              "year")) %>%
      dplyr::relocate(eurostat_country, .before = eurostat_eu) %>%
      dplyr::mutate(eurostat_country = dplyr::case_when(
        is.na(x = eurostat_country) ~ 0,
        TRUE ~ eurostat_country
      ))
    table_control <- dplyr::rename(table_control,
                                   tons_country = eurostat_country,
                                   tons_eu = eurostat_eu)
    table_control$data_source <- ifelse(test = landing_data_source == "eurostat",
                                        yes = landing_data_source,
                                        no = "rdbes")
    table_control$comment <- table_2_1_linkage[table_2_1_linkage_id,
                                               "comment"]
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
      if (! all(reference_period %in% reference_period_fides)) {
        warning(format(x = Sys.time(),
                       format = "%Y-%m-%d %H:%M:%S"),
                " - Year(s) of the \"reference_period\" argument are not all available in the FIDES data imported.\n",
                "Year(s) not available in FIDES data are: \n",
                paste0(dplyr::setdiff(x = reference_period,
                                      y = reference_period_fides),
                       collapse = ", "),
                ".")
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
          && nrow(x = current_fides_data_not_tac) > 0
          && sum(current_fides_data_not_tac$initial_quantity) > 0) {
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
                                       level_code %in% !!eu_countries)
        fides_eu <- sum(dplyr::filter(.data = current_fides_data_country,
                                      level_code %in% !!eu_countries)$initial_quantity,
                        na.rm = T)
        fides_tac <- dplyr::filter(.data = current_fides_data_country,
                                   level_code == "TAC")$initial_quantity
        if (nrow(x = fides_country) >= 1) {
          table_2_1_information <- dplyr::left_join(x = table_2_1_information,
                                                    y = (dplyr::mutate(.data = fides_country,
                                                                       tac = round(x = initial_quantity / !!fides_eu,
                                                                                   digits = 4),
                                                                       comment_fides_bis = paste0("FIDES initial quantity mean = ",
                                                                                                  round(x = initial_quantity / length(x = !!reference_period_fides),
                                                                                                        digits = 1))) %>%
                                                           dplyr::select(-initial_quantity)),
                                                    by = c("ms" = "level_code"))
        } else {
          table_2_1_information <- dplyr::mutate(.data = table_2_1_information,
                                                 tac = NA_real_,
                                                 comment_fides_bis = NA_character_)
        }
        current_fides_data_country <- dplyr::mutate(.data = current_fides_data_country,
                                                    total_quota = initial_quantity / !!fides_eu)
        if (nrow(x = dplyr::filter(.data = current_fides_data_country,
                                   total_quota > 0 & total_quota < 0.1)) >= 1) {
          table_2_1_information <- dplyr::left_join(x = table_2_1_information,
                                                    y = (dplyr::filter(.data = dplyr::ungroup(current_fides_data_country),
                                                                       total_quota > 0 & total_quota < 0.1) %>%
                                                           dplyr::rename("comment_fides" = "total_quota") %>%
                                                           dplyr::select(-initial_quantity) %>%
                                                           dplyr::mutate(comment_fides_25_rule = paste0("Sum of MS TAC's below 10% = ",
                                                                                                        round(x = as.numeric(x = sum(comment_fides)) * 100,
                                                                                                              digits = 4),
                                                                                                        "% of EU TAC"))),
                                                    by = c("ms" = "level_code")) %>%
            dplyr::relocate(comment_fides,
                            .before = comment_fides_bis)
        } else {
          table_2_1_information <- dplyr::mutate(.data = table_2_1_information,
                                                 comment_fides = NA_real_,
                                                 comment_fides_25_rule = NA_character_)
        }
      } else {
        table_2_1_information <- dplyr::mutate(.data = table_2_1_information,
                                               tac = -999,
                                               comment_fides = NA_real_,
                                               comment_fides_bis = NA_character_,
                                               comment_fides_25_rule = NA_character_)
      }
    } else {
      table_2_1_information <- dplyr::mutate(.data = table_2_1_information,
                                             tac = -999,
                                             comment_fides = NA_real_,
                                             comment_fides_bis = NA_character_,
                                             comment_fides_25_rule = NA_character_)
    }
    table_2_1_information_final <- rbind(table_2_1_information_final,
                                         table_2_1_information)
    table_control_final <- rbind(table_control_final,
                                 table_control)
    message(format(x = Sys.time(),
                   format = "%Y-%m-%d %H:%M:%S"),
            " - Work done on row id ",
            table_2_1_linkage_id)
  }
  # formatting ----
  table_2_1_information_final_2 <- table_2_1_information_final %>%
    dplyr::mutate(
      thresh = dplyr::case_when(
        round(x = tac,
              digits = 2) < 0.1
        & tac > 0 ~ "TAC < 10%",
        tac %in% c("NA%",
                   "NaN%",
                   "Inf%",
                   -999)
        & round(x = share_landing,
                digits = 2) < 0.1
        & share_landing > 0 ~ "Landings < 10%",
        round(landings, 0) < 200 ~ "Landings < 200t.",
        TRUE ~ thresh
      ),
      landings = as.character(x = round(x = landings, 0)),
      landings = dplyr::case_when(
        landings == 0 ~ "None",
        TRUE ~ landings),
      tac = paste0(round(x = 100 * tac,
                         digits =  0),
                   "%"),
      tac = dplyr::case_when(
        tac %in% c("NA%",
                   "NaN%",
                   "Inf%")
        | (spp == "Nephrops norvegicus" # kibi - I don't get this one
           & !(grepl(
             pattern = "TAC",
             x = area))) ~ "None",
        tac == "-99900%" ~ NA_character_,
        TRUE ~ tac
      ),
      share_landing = paste0(round(x = 100 * share_landing,
                                   digits = 0),
                             "%"),
      share_landing = dplyr::case_when(
        share_landing %in% c("NA%",
                             "NaN%",
                             "Inf%",
                             "0%") ~ "None",
        TRUE ~ share_landing
      ),
      thresh = dplyr::case_when(
        (rfmo %in% c("ICCAT",
                     "IOTC",
                     "WCPFC"))
        | (spp %in% c("Anguilla anguilla",
                      "salmo salar",
                      "salmo trutta")) ~ "None",
        TRUE ~ thresh
      ),
      covered_length = " ",
      selected_bio = " ",
      reg_coord = " "
    )
  # Order cols correctly
  # As agreed, only keep comment_fides_25_rule in the WP table - the other comments will go in the control file
  table_2_1_information_final_2$comments <- table_2_1_information_final_2$comment_fides_25_rule
  table_2_1_information_final_2$comments[is.na(table_2_1_information_final_2$comments)] <- ""
  table_2_1_information_final_3 <- dplyr::select(table_2_1_information_final_2,
                                                 ms,
                                                 reference_years,
                                                 region,
                                                 rfmo,
                                                 spp,
                                                 area,
                                                 landings,
                                                 source_national,
                                                 tac,
                                                 share_landing,
                                                 source_eu,
                                                 thresh,
                                                 reg_coord, covered_length, selected_bio, comments)


  rwp_table_2_1_export = list("table_2_1_template" = table_2_1_information_final_3,
                              "table_2_1_template_control" = table_control_final)
  names_export <- names(x = rwp_table_2_1_export)
  for (export_id in seq_len(length.out = length(x = names_export))) {
    if (full_name_output == TRUE
        || length(x = eu_countries) == 1) {
      names(x = rwp_table_2_1_export)[export_id] <- paste0(names(x = rwp_table_2_1_export)[export_id],
                                                           "_",
                                                           ifelse(test = rcg_countries == "",
                                                                  yes = rcg_countries,
                                                                  no = paste0("rcg",
                                                                              rcg_countries,
                                                                              "_")),
                                                           paste(tolower(x = eu_countries),
                                                                 collapse = "_"))
    } else {
      names(x = rwp_table_2_1_export)[export_id] <- paste0(names(x = rwp_table_2_1_export)[export_id],
                                                           "_",
                                                           ifelse(test = rcg_countries == "",
                                                                  yes = rcg_countries,
                                                                  no = paste0("rcg",
                                                                              rcg_countries,
                                                                              "_")),
                                                           length(x = eu_countries),
                                                           "eu_countries")
    }
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
                         dec = ".")
    }
    message(format(x = Sys.time(),
                   format = "%Y-%m-%d %H:%M:%S"),
            " - Successful output generation in the folder:\n",
            output_path)
  }
  message(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Process RWP table 2.1 generation successfully ended.")
  return(rwp_table_2_1_export)
}

