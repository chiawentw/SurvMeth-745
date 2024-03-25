# Setup
library(tidycensus)
library(tidyverse)
library(readxl)
library(dplyr)
library(sampling)
# Load data
data <- read_excel("G:/My Drive/0. study abroad/academic/11. 2024 Winter/1. SurvMeth 745 Practical Tools for Study Design and Inference/2. Assignments/Project/PrinceGeorgeMD.xlsx", sheet = 2) ## 540 obs with 52 var.

# Delete irrelevant data rows and columns
data <- data[-c(539, 540), ] ## 538 obs with 52 var.
data <- data[, -c(6:9, 30:33)] ## 538 obs with 44 var.

# Get Tract numbers by Block Groups and calculate domain population counts for each row (BG)
data <- data %>% 
  group_by(BlockGroup) %>% 
  mutate(Tract = substr(BlockGroup, 1, nchar(BlockGroup) - 1)) %>% 
  mutate(y18 = sum(Male18to19Yrs, Male20Yrs, Male21Yrs, Male22to24Yrs, Male25to29Yrs, Male30to34Yrs, Male35to39Yrs, Male40to44Yrs, Female18to19Yrs, Female20Yrs, Female21Yrs, Female22to24Yrs, Female25to29Yrs, Female30to34Yrs, Female35to39Yrs, Female40to44Yrs),
         y45 = sum(Male45to49Yrs, Male50to54Yrs, Male55to59Yrs, Male60to61Yrs, Male62to64Yrs, Female45to49Yrs, Female50to54Yrs, Female55to59Yrs, Female60to61Yrs, Female62to64Yrs),
         y65 = sum(Male65to66Yrs, Male67to69Yrs, Male70to74Yrs, Male75to79Yrs, Male80to84Yrs, MaleGE85Yrs, Female65to66Yrs, Female67to69Yrs, Female70to74Yrs, Female75to79Yrs, Female80to84Yrs, FemaleGE85Yrs)) %>% 
  relocate(Tract, .before = BlockGroup) %>% 
  relocate(c(y18, y45, y65), .before = TotHH) ## 538 obs. with 48 var
# Drop variables we are not using anymore.
data <- data[, -c(7:48)] ## 538 obs. with 6 var.

# Calculate the values of some terminologies
n_y18_weighted <- 100/0.6
f_d18 = n_y18_weighted/sum(data$y18)
n_y45_weighted <- 100/0.7
f_d45 = n_y45_weighted/sum(data$y45)
n_y65_weighted <- 100/0.85
f_d65 = n_y65_weighted/sum(data$y65)
n <- 300
n_weighted <- n_y18_weighted + n_y45_weighted + n_y65_weighted
m <- 15
n_bar <- n_weighted/m
N <- sum(c(data$y18, data$y45, data$y65))

U_tract <- data %>% 
  select(Tract) %>%   # Select the column containing tracts
  distinct() %>%      # Get distinct values
  nrow() ## 204 because we collapsed 10 Tracts with only one BG
##?

U_bg <- data %>% 
  select(BlockGroup) %>%   
  distinct() %>%
  nrow() ## 538

data <- data %>% 
  group_by(Tract) %>% 
  mutate(N_tract_18 = sum(y18), N_tract_45 = sum(y45), N_tract_65 = sum(y65)) %>% 
  mutate(S_tract = sum(f_d18*N_tract_18, f_d45*N_tract_45, f_d65*N_tract_65)) %>% 
  mutate("Selection Prob_tract" = m*(S_tract/sum(S_tract))) %>% 
  mutate("Sampling Rate 18_tract" = n_bar*f_d18/S_tract, "Sampling Rate 45_tract" = n_bar*f_d45/S_tract, "Sampling Rate 65_tract" = n_bar*f_d65/S_tract) %>% 
  mutate("Expected n 18_tract" = N_tract_18*`Sampling Rate 18_tract`, "Expected n 45_tract" = N_tract_45*`Sampling Rate 45_tract`, "Expected n 65_tract" = N_tract_65*`Sampling Rate 65_tract`)
  relocate(Tract, .before = BlockGroup) %>% 
  relocate(c(N_tract_18, N_tract_45, N_tract_65, S_tract, "Selection Prob_tract", "Sampling Rate 18_tract", "Expected n 18_tract", "Sampling Rate 45_tract", "Expected n 45_tract", "Sampling Rate 65_tract", "Expected n 65_tract"), .before = TotHH)

# Sample samples based on calculations above
set.seed(-1234)




"YOUR KEY" <- "452bfe5de763525e0910db4aeefe0fdb87e515eb"
census_api_key("YOUR KEY", install = T, overwrite = T)




# 214 tracts
# 214/15 using pps
# sample 100/0.6=167 18-44 yo from 378484; 100/0.7=143 45-64 yo from 245516; 100/0.85=118 65+ yo from 132049
# N = 756049, n = 428

# composite measure of size for PSU i = S_i = sum(f_d*N_id) --> the expected sample size in PSU i if desired overall sampling rates for all domains were used
# f_d18 = 167/378484=0.000441234; f_d45 = 143/245516=0.0005824468; f_d65 = 118/132049=0.0008936077
# m = 15, bar_n = 300/15=20; N_id=
# 10 block groups with only 1 PSU

