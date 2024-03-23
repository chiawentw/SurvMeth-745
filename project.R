library(tidycensus)
library(tidyverse)
library(haven)
library(readxl)
data <- read_excel("G:/My Drive/0. study abroad/academic/11. 2024 Winter/1. SurvMeth 745 Practical Tools for Study Design and Inference/2. Assignments/Project/PrinceGeorgeMD.xlsx", sheet = 2)
data_sheet1 <- read_excel("G:/My Drive/0. study abroad/academic/11. 2024 Winter/1. SurvMeth 745 Practical Tools for Study Design and Inference/2. Assignments/Project/PrinceGeorgeMD.xlsx", sheet = 1)


data <- data %>% 
  group_by(BlockGroup) %>% 
  mutate(Tract = substr(BlockGroup, 1, nchar(BlockGroup) - 1)) %>% 
  mutate(y18 = sum(Male18to19Yrs, Male20Yrs, Male21Yrs, Male22to24Yrs, Male25to29Yrs, Male30to34Yrs, Male35to39Yrs, Male40to44Yrs, Female18to19Yrs, Female20Yrs, Female21Yrs, Female22to24Yrs, Female25to29Yrs, Female30to34Yrs, Female35to39Yrs, Female40to44Yrs),
         y45 = sum(Male45to49Yrs, Male50to54Yrs, Male55to59Yrs, Male60to61Yrs, Male62to64Yrs, Female45to49Yrs, Female50to54Yrs, Female55to59Yrs, Female60to61Yrs, Female62to64Yrs),
         y65 = sum(Male65to66Yrs, Male67to69Yrs, Male70to74Yrs, Male75to79Yrs, Male80to84Yrs, MaleGE85Yrs, Female65to66Yrs, Female67to69Yrs, Female70to74Yrs, Female75to79Yrs, Female80to84Yrs, FemaleGE85Yrs))
  
data <- data %>% 
  group_by(Tract) %>% 
  mutate(N_i_18 = sum(y18), N_i_45 = sum(y45), N_i_65 = sum(y65)) %>% 
  mutate(S_i = sum((100/0.6)*N_i_18, (100/0.7)*N_i_45, (100/0.85)*N_i_65))





"YOUR KEY" <- "452bfe5de763525e0910db4aeefe0fdb87e515eb"
census_api_key("YOUR KEY", install = TRUE, overwrite = TRUE)




# 214 tracts
# 214/15 using pps
# sample 100/0.6=167 18-44 yo from 378484; 100/0.7=143 45-64 yo from 245516; 100/0.85=118 65+ yo from 132049
# N = 756049, n = 428

# composite measure of size for PSU i = S_i = sum(f_d*N_id) --> the expected sample size in PSU i if desired overall sampling rates for all domains were used
# f_d18 = 167/378484=0.000441234; f_d45 = 143/245516=0.0005824468; f_d65 = 118/132049=0.0008936077
# m = 15, bar_n = 300/15=20; N_id=
# 10 block groups with only 1 PSU

