

library(tidyverse)
library(here)
library(readxl)
library(lubridate)

# location for the raw data files
dat_files <- list.files("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/waterbirds_enter_historic_raw_data/entered_raw_data", full.names = TRUE)

# helps to have a file of column names 
wide_names <- read.csv("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/waterbirds_enter_historic_raw_data/waterbird_data_entry_template_wide_colnames.csv")


#' read and wrangle waterbird wide format raw data xlsx files
#'
#' @param dat_file file path to a wide format raw data file
#'
#' @return data frame
#' @export
#'
#' @details this function operates on a single file. can be iterated over with purr::map_df to merge all files into a single df
#'
#' @examples
#' wrangled_wide <- map_df(dat_files, wrangle_wide_raw)
wrangle_wide_raw <- function(dat_file) {
dat_date <- gsub("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/water_birds/waterbirds_enter_historic_raw_data/entered_raw_data/", "", dat_file)
dat_date <- gsub("\\.xlsx", "", dat_date)
dat_date <- gsub("_p2", "", dat_date)
dat_date <- ymd(dat_date)

dat <- read_excel(dat_file, sheet = "data")

dat2 <- dat[-(1:2),]
colnames(dat2) <- colnames(wide_names)
dat2 <- select(dat2, colnames(wide_names)) %>% 
  mutate(date = dat_date)
}


wrangled_wide <- map_df(dat_files, wrangle_wide_raw)

zz <- wrangled_wide %>%
     keep(~is.null(.x) )



wrangled_long <- pivot_longer(wrangled_wide, cols = c(contains("species"), contains("tally")))








dat <- wrangled_long %>% 
  filter(!is.na(value)) %>% 
  mutate(name = gsub("cypressgrove", "cypressgrove.a", name),
         name = gsub("inverness", "inverness.a", name),
         #name = gsub("millertonbivalve", "millertonbivalve.a", name),
         name = gsub("walkercreek", "walkercreek.a", name),
         name = gsub("bivalve", "bivalve.a", name)) %>% 
  separate(name, into = c("section", "transect", "name")) %>% 
  pivot_wider(id_cols = c("index", "date", "section", "transect"), names_from = name, values_from = value) %>% 
  select(index, date, section, transect, species, tally) 
