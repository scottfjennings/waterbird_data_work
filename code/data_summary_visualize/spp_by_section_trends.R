library(tidyverse)
library(here)
library(birdnames)

options(scipen = 999)


source(here("code/utility/waterbird_utility_functions.r"))

# read cleaned data
wbirds4analysis <- readRDS(here("data_files/working_rds/wbirds4analysis")) %>% 
  wbird_add_study_day() %>% 
  wbird_add_season() %>% 
  wbird_add_survey_period()



  spp_section_mean <- wbirds4analysis %>%
    group_by(alpha.code, section) %>%
    summarise(spp.section.mean = mean(section.final)) %>% 
    ungroup()  %>% 
    mutate(spp.section.mean = round(spp.section.mean, 1)) %>% 
    full_join(., count(wbirds4analysis, alpha.code, section)) %>% 
    rename(num.surveys.detected = n)
  
  write.csv(spp_section_mean, here("figures_output/data_summaries/spp_section_mean.csv"), row.names = FALSE)



spp_section_trend_plot <- function(zspp, zsect) {

  spp_section_mean <- wbirds4analysis %>%
    filter(alpha.code == zspp, section == zsect) %>%
    summarise(spp.section.mean = mean(section.final)) %>% 
    mutate(spp.section.mean = round(spp.section.mean, 1))
  
  spp_section_trend <- wbirds4analysis %>%
    filter(alpha.code == zspp, section == zsect) %>% 
    group_by(study.year) %>%
    summarise(spp.section.year.mean = mean(section.final)) %>% 
    ungroup() 
  
  ggplot(spp_section_trend) +
    stat_smooth(aes(x = study.year, y = spp.section.year.mean), color = "black") +
    geom_point(aes(x = study.year, y = spp.section.year.mean), color = "black") +
    labs(x = "",
          y = "Mean count per survey") +
    theme_bw() +
    #annotate("text", x = 2000, y = -0.1 * max(spp_section_trend$spp.section.year.mean), label = paste("Long-term mean =", spp_section_mean$spp.section.mean), hjust = 0, vjust = 0) +
    annotate("text", x = 1990, y = -0.1 * max(spp_section_trend$spp.section.year.mean), label = paste(zspp, "Section", zsect, "\nLong-term mean =", spp_section_mean$spp.section.mean), hjust = 0, vjust = 0, size = 5)
}



spp_section_trend_plot("COLO", 3)
ggsave(here("figures_output/spp_section_trend_plots/colo_sect3.png"), width = 4, height = 4)
