# -----------------------
# Author(s): Mike Ackerman
# Purpose: Read in NOAA and NPT juvenile survival estimates and plot.
# 
# Created Date: March 18, 2024
#   Last Modified: 
#
# Notes: 

# clear environment
rm(list = ls())

# load necessary libraries
library(here)
library(tidyverse)
library(readxl)
library(janitor)

# read in NOAA Parr Tagging estimates
noaa_df = read_xlsx(path = here("data/IDSurvival93to23_forAckermanNezPerce.xlsx"),
                   sheet = "Sheet1",
                   skip = 2,
                   col_names = F) %>%
  rename(mig_year       = ...1,
         rel_site     = ...2,
         rel_num      = ...3,
         est_lgr_pass = ...4,
         lgr_surv     = ...5,
         lgr_surv_se  = ...6,
         lower95ci    = ...7,
         upper95ci    = ...8) %>%
  mutate(method = "Roving",
         brood_year = mig_year - 1,
         life_stage = "Parr") %>%
  select(rel_site,
         brood_year,
         mig_year,
         method,
         life_stage,
         rel_num,
         est_lgr_pass,
         lgr_surv,
         lgr_surv_se,
         lower95ci,
         upper95ci)

# read in NPT RST estimates
npt_df = read_xlsx(path = here("data/npt_rst_survivals_ts_20240304.xlsx"),
                   sheet = "Sheet1") %>%
  clean_names() %>%
  rename(rel_site = location,
         rel_num = tagged_and_released_n,
         lgr_surv = survival,
         lgr_surv_se = se,
         lower95ci = lower95,
         upper95ci = upper95,
         life_stage = lifestage) %>%
  mutate(est_lgr_pass = rel_num * lgr_surv) %>%
  mutate(mig_year = case_when(
    life_stage %in% c("Parr", "Presmolt") ~ brood_year + 1,
    life_stage == "Smolt" ~ brood_year + 2
  )) %>%
  select(rel_site,
         brood_year,
         mig_year,
         method,
         life_stage,
         rel_num,
         est_lgr_pass,
         lgr_surv,
         lgr_surv_se,
         lower95ci,
         upper95ci) %>%
  mutate(rel_site = case_when(
    grepl("Lolo Creek", rel_site) ~ "Lolo Creek",
    grepl("Secesh River", rel_site) ~ "Secesh River",
    grepl("Johnson Creek", rel_site) ~ "Johnson Creek",
    grepl("Newsome Creek", rel_site) ~ "Newsome Creek",
    grepl("Imnaha River", rel_site) ~ "Imnaha River",
    grepl("South Fork Clearwater", rel_site) ~ "SF Clearwater River"))

# combine data frames
surv_df = rbind(noaa_df, npt_df) %>%
  filter(rel_site != "Total")

# plot survival estimates
surv_p = surv_df %>%
  ggplot(aes(x = brood_year,
             y = lgr_surv,
             color = life_stage)) +
  #geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower95ci,
                    ymax = upper95ci),
                width = 0) +
  facet_wrap(~ rel_site, scales = "free") +
  labs(x = "Brood Year",
       y = "Survival",
       color = "Life Stage") +
  scale_color_manual(values = c("Parr" = "#00BFFF",
                                "Presmolt" = "#FF7F50",
                                "Smolt" = "#9932CC")) +
  theme_bw() +
  xlim(1992, 2022)
surv_p

# save plot
ggsave(filename = here("figures/survival_estimates.png"),
       width = 14,
       height = 11,
       units = "in")

### END SCRIPT