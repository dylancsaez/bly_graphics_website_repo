#----------------------------
#Load Libraries
#----------------------------
ybl_packages <- c(
  "raster",
  "tidyverse",
  "tables",
  "xtable",
  "scales",
  "reshape",
  "zoo",
  "gtable",
  "grid",
  "gridExtra",
  "png",
  "ggtext",
  "jpeg",
  "cowplot",
  "lubridate",
  "data.table",
  "haven",
  "DBI",
  "gridGraphics",
  "ggpattern",
  "colorblindcheck",
  "httr",
  "fredr",
  "pkgconfig"
)

if(Sys.info()[1] == "Windows"){
  exclude_packages <- c("cowplot")
} else {
  exclude_packages <- c()
}


for(ybl_packages in setdiff(ybl_packages, exclude_packages)){
  try(suppressPackageStartupMessages(library(ybl_packages, character.only = TRUE)))
}

case_when <- dplyr::case_when
select <- dplyr::select
mutate <- dplyr::mutate
filter <- dplyr::filter
lag <- dplyr::lag
combine <- dplyr::combine
rename <- dplyr::rename
summarize <- dplyr::summarize
melt <- reshape::melt
ggsave <- ggplot2::ggsave



message("Welcome to the Budget Lab at Yale Source Scripts! Please let us know if you have questions. Package Contact: Dylan Saez\n")
message(paste(ybl_packages, "==", unlist(lapply(
  lapply(ybl_packages, packageVersion), as.character
)), "\n"))

policy_font = "Helvetica"
message("BLYFunctions tools loaded.")
message("BLY Functions 1.0.0")


ybl_mother <- paste("/gpfs/gibbs/project/sarin/ds3228/ylb_vis/programs/ybl_functions_mother.R", sep = "")


