
#' Title
#'
#' @param variables
#'
#' @return
#' @export
#'
#' @examples

library(dplyr)
library(stringr)
library(data.table)

path <- "Q:/mynd/kibi/projects_wks_wgs_rcgs/ISSG_RWP/2022/table_2_1_development/rwptool/inst/"

# Get area file from the RDB and keep only relevant info

rdb_areas <- read.csv(paste0(path, "Area.csv"), sep = ";")
names(rdb_areas)

unique(rdb_areas$X_AreaTypeCode)

rdb_areas <- subset(rdb_areas, X_AreaTypeCode != "Area" | Code == "27.5")

rdb_areas <- unique(str_subset(rdb_areas$Code, "[:digit:]"))

# Load linkage file

linkage <- read.csv(file.path(path, "eumap_table_2_1_linkage_version_2022_v1.0.csv"), sep = ";", header = T)

names(linkage)

# Removing blanks from areaBis

unique(linkage$area_bis)

linkage$area_bis <- gsub(" ", "", linkage$area_bis)


# Coding RDB areas ----

linkage$area_rdb <- paste0(linkage$area_bis, ",")
unique(linkage$area_rdb)

# linkage$area_rdb <- paste0(",", gsub('_', '.', linkage$area_rdb), ",") # , add to make the gsub a bit easier


# Replace areas on upper level with all underlying areas

# rdb_areas <- c("27.3.d.28.2", "27.3.d.28", "27.3.d.28.1")

for (i in rdb_areas) {

  j <- paste0(toupper(gsub("\\.", "\\_", i)), ",")

  linkage$area_rdb <- gsub(j, paste0(paste(rdb_areas[rdb_areas %like% i], collapse = ","), ","), linkage$area_rdb)

}


