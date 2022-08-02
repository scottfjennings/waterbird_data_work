
useless_groupies <- c("AMCOGRSCLESCBUFF", "COMERBME", "GOOSE", "MERG", "MURRELET", "SWAN", "UNTE", "DUCK")

useful_groupies <- c("LOON", "RTPALO", "CORM", "HEGR", "WCGR", "PCLO")



######################
# functions below here are to be used on wbirds4analysis, from code/data_clean_manage/waterbird_cleaning4_create_wbirds4analysis.R

# starting point for any waterbird data work should be these 3 functions:
#wbirds <- wbird_qsel_all_data() %>% 
#  clean_waterbirds() %>% 
#  wbird_fix_spp_factors() %>% 
#  wbird_fix_precount_block_names()

# wbirds4analysis <- readRDS("data_files/working_rds/wbirds4analysis")

# add study day as number of days since Nov 1 ----
wbird_add_study_day <- function(df) {
  
  nov_1 <- data.frame(nov1 = as.Date(paste(seq(1989, 2020), "-11-01", sep = "")),
                      dec31 = as.Date(paste(seq(1989, 2020), "-12-31", sep = ""))) %>% 
  mutate(nov1.yday = yday(nov1),
         dec31.yday = yday(dec31),
         year = year(nov1))
  
  df <- df %>% 
    mutate(year = year(date),
           year.day = yday(date),
           study.year = ifelse(month(date) < 6, year(date) - 1, year(date))) %>% 
    left_join(., dplyr::select(nov_1, -nov1, -dec31), by = c("year")) %>% 
    mutate(study.day = ifelse(year.day > nov1.yday, year.day - nov1.yday, year.day + (dec31.yday - nov1.yday))) %>% 
    dplyr::select(-nov1.yday, -dec31.yday, -year)
  
}

# add study season - deprecated  ----
wbird_add_season <- function(wbird_df) {
  wbird_df <- wbird_df %>% 
    mutate(season = ifelse(month(date) < 6, year(date) - 1, year(date)))
}

# add survey period  ----
wbird_add_survey_period <- function(wbird_df) {
  wbird_df <- wbird_df %>% 
    mutate(survey.per = case_when(month(date) == 12 ~ 1,
                                  month(date) == 1 & day(date) <= 15 ~ 2,
                                  month(date) == 1 & day(date) > 15 ~ 3,
                                  month(date) == 2 ~ 4))
}

# combine scaup species  ----
wbird_combine_scaup <- function(zwbirds4analysis) {
  zwbirds4analysis <- zwbirds4analysis %>% 
    filter(alpha.code %in% c("GRSC", "LESC", "SCAUP")) %>% 
    group_by(season, date, survey.num, section) %>% 
    summarise(section.final = sum(section.final)) %>% 
    mutate(alpha.code = "SCAUP",
           data.is = "lumped.scaup") %>% 
    ungroup() %>% 
    rbind(., filter(zwbirds4analysis, !alpha.code %in% c("GRSC", "LESC", "SCAUP"))) %>% 
    arrange(date, section, alpha.code)
}

# combine western and clarks grebes ----
wbird_combine_wegr_clgr <- function(zwbirds4analysis) {
  zwbirds4analysis <- zwbirds4analysis %>% 
    filter(alpha.code %in% c("WEGR", "CLGR", "WCGR")) %>% 
    group_by(season, date, survey.num, section) %>% 
    summarise(section.final = sum(section.final)) %>% 
    mutate(alpha.code = "WCGR",
           data.is = "lumped.wcgr") %>% 
    ungroup() %>% 
    rbind(., filter(zwbirds4analysis, !alpha.code %in% c("WEGR", "CLGR", "WCGR"))) %>% 
    arrange(date, section, alpha.code)
}


# calculate total per survey across the entire bay ----
wbird_spp_survey_total <- function(zwbirds4analysis) {
  zwbirds4analysis <- zwbirds4analysis %>%
    group_by(alpha.code, study.year, date) %>%
    summarise(spp.survey.tot = sum(section.final)) %>%
    ungroup() 
}

# calculate average annual abundance ----
wbird_spp_annual_mean <- function(zwbirds4analysis) {
  zwbirds4analysis <- zwbirds4analysis %>%
    group_by(alpha.code, study.year, date) %>%
    summarise(spp.survey.tot = sum(section.final)) %>%
    ungroup() %>% 
    group_by(alpha.code, study.year) %>% 
    summarise(spp.annual.mean = mean(spp.survey.tot)) %>% 
    ungroup()
}


# calculate average annual abundance by section ----
wbird_spp_section_annual_mean <- function(zwbirds4analysis) {
  zwbirds4analysis <- zwbirds4analysis %>%
    group_by(alpha.code, study.year, section) %>%
    summarise(spp.section.annual.mean = mean(section.final)) %>% 
    ungroup()
}



# calculate number of years each species detected ----
wbird_num_years_detected <- function(zwbirds4analysis) {
  spp_years_detected <- zwbirds4analysis %>% 
  filter(section.final > 0) %>% 
  distinct(study.year, alpha.code) %>% 
  group_by(alpha.code) %>% 
  summarise(num.years.detected = n())
}




# check for duplicate date X section X alpha.code records ----
# there should be no more than one of each of these

check_wbird_duplicates <- function(zwbirds4analysis) {
  dup_recs <- zwbirds4analysis %>% 
    group_by(date, alpha.code, section) %>% 
    mutate(num.rec = n()) %>% 
    filter(num.rec > 1)
  
}
  
  
  

