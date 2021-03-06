#'---
#' title: 'Austin Police Data'
#' author: 'David Nichols'
#' date: '12 Oct 2018'
#' output:
#'  html_document:
#'    number_sections: false
#'    toc: true
#'    toc_depth: 4
#'    fig_width: 7
#'    fig_height: 4.5
#'    theme: cosmo
#'    highlight: tango
#'    code_folding: hide
#'---
#'   
#'```{r}
#'knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE, error=FALSE)
#'```
#'
#'###Introduction
#'This study will measure disparities in policing among
#'several large American cities. These measures will be used as 
#'benchmarks of racial justice within and between
#'cities. Recognition of these differences can be 
#'used by law enforcement to improve public safety
#'and trust between police and the communities they
#'serve.
#'
#'###Approach - Austin, TX - Police Data
#'This section will gather the data and transform it into a tidy form 
#'that can be used for later analysis. I will use random imputation to 
#'deal with NA's. Please see my blog (https://epi2020datascience.blogspot.com/) 
#'for a discussion of random imputation in several posts. 
#'I have used the Austin Police data to start with. This can be combined later
#'with the GIS data to add more features. I will do this in a following section. 
library(tidyverse)
library(stringr)
library(anytime)
library(lubridate)
library(sf)
library(purrr)
library(broom)
#'###Load Data
#'I saved the Austin Police data to disc for public use
data37 <- read_csv("D:/Kaggle_Policing/37-00027_UOF-P_2014-2016_prepped.csv", skip = 1)
glimpse(data37)
#'
#'Remove spaces from column names
nams <- names(data37) %>% 
  str_replace_all(" ", "")  
colnames(data37) <- nams
names(data37)

#'###Extract features from police data
#'
#'Injury to police officer
data37 <- data37 %>% 
  mutate(officer_injury = if_else(EffectonOfficer == "NO COMPLAINT OF INJURY/PAIN", 0, 1)) # 1 means officer had injury
#'convert NA to 0 - assuming that 0 is no injury
data37 <- data37 %>% 
  mutate(officer_injury = if_else(is.na(officer_injury), 0, officer_injury))
sum(is.na(data37$officer_injury))


#'Injury to subject
data37 <- data37 %>% 
  mutate(subject_injury = if_else(SubjectEffects == "NO COMPLAINT OF INJURY/PAIN", 0, 1)) # 1 means subject had injury
#'convert NA to 0- assuming that 0 is no injury
data37 <- data37 %>% 
  mutate(subject_injury = if_else(is.na(subject_injury), 0, subject_injury))
sum(is.na(data37$subject_injury))


#'Fatal outcome to subject
data37 <- data37 %>% 
  mutate(subject_death = if_else(SubjectEffects == "DEATH", 1, 0)) # 1 means subject had injury
#'convert NA to 0 - assuming that 0 is no fatal outcome
data37 <- data37 %>% 
  mutate(subject_death = if_else(is.na(subject_death), 0, subject_death))
sum(is.na(data37$subject_death)) # 1 means subject died


#'Weapon used by officer
data37 <- data37 %>% 
  mutate(officer_used_weapon = if_else(str_detect(data37$WeaponUsed1, "WEAPONLESS"), 0, 1))
#'1 means officer used some type of weapon


#'Firearm used by officer
data37 <- data37 %>% 
  mutate(officer_used_firearm = if_else(is.na(NumberShots), 0, as.numeric(NumberShots)))
sum(is.na(data37$officer_used_firearm)) 
#'1 means officer used some type of firearm

#'Change coordinate names
#'to be used in later section for additional geographic features
data37 <- data37 %>% 
  rename(latitude = "X-Coordinate",
         longitude = "Y-Coordinate")

#'Form the date related features
data37 <- data37 %>% 
  mutate(date = anytime(DateOccurred)) %>%  #det date in proper form
  mutate(day = lubridate::day(date)) %>% 
  mutate(dayofweek = wday(date, label = TRUE)) %>% 
  mutate(mon = month(date, label = TRUE)) %>% 
  mutate(yr = year(date))

#'Remove unnecessary features
data37_1 <- data37 %>% 
  dplyr::select(-c(PrimaryKey, DateOccurred, MasterSubjectID,
                   SubjectRole, SubjectConductDesc, SubjectResistance,
                   WeaponUsed1, WeaponUsed2, WeaponUsed3, WeaponUsed4,
                   NumberShots, SubjectEffects, EffectonOfficer,
                   CityCouncilDistrict, City, State, Latitude,
                   Longitude))

#'Continue modifications to dataset
#'
#'SubjectSex
data37_1 <- data37_1 %>% 
  mutate(male = if_else(SubjectSex == "M", 1, 0)) 
data37_1 <- data37_1 %>% dplyr::select(-SubjectSex)

#'Race
data37_1$race <- as.numeric(0)
data37_1$race <- 
  case_when(
    data37_1$Race == "Asian" ~ 1,
    data37_1$Race == "Black" ~ 2,
    data37_1$Race == "Hispanic" ~ 3,
    data37_1$Race == "Unknown" ~ 4,
    data37_1$Race == "White" ~ 5,
    TRUE ~ as.numeric(0)
  )
data37_1 <- data37_1 %>% dplyr::select(-Race)

#'OfficerYrsofService
data37_1 <- data37_1 %>% 
  rename(offserv = "OfficerYrsofService")

#'NatureofContact
data37_1$contact <- as.numeric(0)
data37_1$contact <- 
  case_when(
    data37_1$NatureofContact == "DISPATCHED CALL" ~ 1,
    data37_1$NatureofContact == "OTHER" ~ 2,
    data37_1$NatureofContact == "TACTICAL OPERATION" ~ 3,
    data37_1$NatureofContact == "TRAFFIC STOP" ~ 4,
    data37_1$NatureofContact == "VIEWED OFFENSE" ~ 5,
    data37_1$NatureofContact == "WARRANT SERVICE" ~ 6,
    TRUE ~ as.numeric(0)
  )
data37_1 <- data37_1 %>% dplyr::select(-NatureofContact)

#'AreaCommand 
data37_1$command <- as.numeric(0)
data37_1$command <- 
  case_when(
    data37_1$AreaCommand == "-" ~ 1,
    data37_1$AreaCommand == "88" ~ 2,
    data37_1$AreaCommand == "AD" ~ 3,
    data37_1$AreaCommand == "AP" ~ 4,
    data37_1$AreaCommand == "BA" ~ 5,
    data37_1$AreaCommand == "CH" ~ 6,
    data37_1$AreaCommand == "DA" ~ 7,
    data37_1$AreaCommand == "ED" ~ 8,
    data37_1$AreaCommand == "FR" ~ 9,
    data37_1$AreaCommand == "GE" ~ 10,
    data37_1$AreaCommand == "HE" ~ 11,
    data37_1$AreaCommand == "ID" ~ 12,
    TRUE ~ as.numeric(0)
  )
data37_1 <- data37_1 %>% dplyr::select(-AreaCommand)

#'OfficerOrganizationDesc
data37_1$org <- as.numeric(0) #initialize
data37_1$org <- 
  case_when(
    str_detect(data37_1$OfficerOrganizationDesc, "PATROL") == TRUE ~ 1,
    str_detect(data37_1$OfficerOrganizationDesc, "TEAM") == TRUE ~ 2,
    str_detect(data37_1$OfficerOrganizationDesc, "ENFORCEMENT") == TRUE ~ 3,
    str_detect(data37_1$OfficerOrganizationDesc, "REGION") == TRUE ~ 4,
    str_detect(data37_1$OfficerOrganizationDesc, "OCD") == TRUE ~ 5,
    str_detect(data37_1$OfficerOrganizationDesc, "SUPRT") == TRUE ~ 6
  )
data37_1 <-  data37_1 %>% 
  mutate(org = if_else(is.na(data37_1$org), 0, data37_1$org))
data37_1 <- data37_1 %>% dplyr::select(-OfficerOrganizationDesc)
glimpse(data37_1)

#'ReasonDesc
table(data37_1$ReasonDesc)
#'The reason descriptions to me seem vague. I don't think that these 
#'can be useful for answering any research questions that will come up.
#'Therefore I will delete this feature.
data37_1 <- data37_1 %>% dplyr::select(-ReasonDesc)
glimpse(data37_1)

#'###Dealing with NA's and Random Imputation
#'
#'####Check for NA's and random impute if needed
nas <- sum(is.na(data37_1))
paste("Total number of NA's in data is:", nas)

nacheck <- function(x) {
  nas <- sum(is.na(x))
  return(nas)
}

#'if no comment is present assume no NA's
nacheck(data37_1$RIN)
nacheck(data37_1$offserv)
nacheck(data37_1$latitude) # will deal with geographic data in a later section
nacheck(data37_1$longitude)# will deal with geographic data in a later section
nacheck(data37_1$Geolocation)
nacheck(data37_1$officer_injury)
nacheck(data37_1$subject_injury)
nacheck(data37_1$subject_death)

#'####Random Imputation
nacheck(data37_1$ officer_used_weapon)# 35 NA's
(tab1 <- xtabs(~data37_1$ officer_used_weapon))
prob <- tab1[[2]]/(tab1[[1]] + tab1[[2]])
paste0("The proportion of 1's to 0's is: ", round(prob, 2))
#'Replace the missing values using random imputation with binomial distribution.
data37_1$officer_used_weapon <- if_else(is.na(data37_1$officer_used_weapon), rbinom(1,1,prob), as.integer(data37_1$officer_used_weapon))
sum(is.na(data37_1$officer_used_weapon))

nacheck(data37_1$officer_used_firearm)
nacheck(data37_1$date) #will deal with time related data in a later section
nacheck(data37_1$day) #will deal with time related data in a later section
nacheck(data37_1$dayofweek) #will deal with time related data in a later section
nacheck(data37_1$mon) #will deal with time related data in a later section
nacheck(data37_1$yr) #will deal with time related data in a later section

#'Random Imputation
nacheck(data37_1$male) #324 NA's
(tab1 <- xtabs(~data37_1$male))
prob <- tab1[[2]]/(tab1[[1]] + tab1[[2]])
paste0("The proportion of 1's to 0's is: ", round(prob, 2))
#'Replace the missing values using random imputation with binomial distribution.
data37_1$male <- if_else(is.na(data37_1$male), rbinom(1,1,prob), as.integer(data37_1$male))
sum(is.na(data37_1$male))

nacheck(data37_1$race)
nacheck(data37_1$contact)
nacheck(data37_1$command)
nacheck(data37_1$org)

glimpse(data37_1)

#'###Combine police data with geocoding data
#'
#'You cannot tell from the data given what census tract each subject belongs to.
#'You can get the individual level tract numbers from the US Census.
#'You can block geocode up to 10000 units at a time. This is a free service.
#'Get the census tracts from [US Census](https://www.census.gov/geo/maps-data/data/geocoder.html) (Saved as UTF8 CSV)
tracts <- read_csv("AustinCensusTracts1.csv")
glimpse(tracts)
#'
tracts <- tracts %>%
  drop_na("Tract")
tracts$RIN <- as.character(tracts$RIN)
glimpse(tracts)
tracts1 <- tracts %>% dplyr::select(RIN, Tract)
glimpse(tracts1)
str(tracts1)
#'
data37_2 <- data37_1
glimpse(data37_2)
data37_2$RIN <- as.character(data37_2$RIN)
data37_2$dayofweek <- as.character(data37_2$dayofweek)
data37_2$mon <- as.character(data37_2$mon)
str(data37_2)
#'
#'Since we can only use subjects whose census tract we know,
#'we will eliminate those subjects who could not be geocoded.
austin1 <- data37_2 %>% 
  left_join(tracts, by = "RIN")
glimpse(austin1)
austin2 <- austin1 %>% 
  drop_na("Tract")
glimpse(austin2)
sum(is.na(austin2$Tract))
#'
#'###Extract educational attainment data
#'####Read and reduce the data
educ1 <- read_csv("Austin16_educ.csv")
#'
cols <- seq(0, length(educ1), 4) 
cols <- cols[-1]
cols <- c(2, 3, cols)
#'
#'reduce columns to the totals
educ2 <- educ1[ , cols]
#'
#'make row 1 the column names
colnames(educ2) <- educ2[1, ]
educ2 <- educ2[-1, ]
#'
#'select the most relevant columns
educ3 <- educ2[ , c(1:4,6,7,9,10,12,13,15,16,93,102,111,120,129,138)]
glimpse(educ3)
#'
#'####Extract the educational data by census tract
nams2 <- c("tract", "geog", "tot_1824", "m_1824", "tot_nohs",
          "m_nohs", "tot_hs", "m_hs", "tot_somecol", "m_somecol",
          "tot_bs", "m_bs", "white", "blk", "ind", "asian", "pacif",
          "other")
colnames(educ3) <- nams2
#'
#'Extract the 4-digit Census Tract code from tract
educ3 <- educ3 %>% 
  mutate(tract = str_extract(tract, pattern = "[0-9]{4}$"))
#'Don't need geography - remove it
educ3 <- educ3[ , -2]
#'Change education groups to numerics
#'First remove to education groups to work on them
ed_grps <- educ3[ , -1]
#'Change them to numerics
ed_grps <- as.tibble(sapply(ed_grps, as.numeric))
#Join the education groups back up to the tract codes
tracts <- data_frame(tract = educ3$tract)
educ4 <- bind_cols(tracts, ed_grps) 
glimpse(educ4)
#'Now we have tract as character and education groups as numerics
#'**********************************************************
#'
#'###Summarize the crime data by census tract. We will explore these later.
#'
#'First select the numeric columns and the tract data
austin3 <- austin2 %>% dplyr::select(offserv, officer_injury, subject_injury,
                              subject_death, officer_used_weapon,
                              officer_used_firearm, Tract)
austin3$Tract <- as.character(austin3$Tract)
#'
austin4 <- austin3 %>% 
  group_by(Tract, officer_injury, subject_injury ) %>% 
  summarize(tot_count = n())
austin5 <- austin4 %>% filter(officer_injury == 1 & subject_injury == 1)
             
officerInjury <- austin3 %>% 
  count(Tract, officer_injury) %>% 
  filter(officer_injury == 1) 

subjectInjury <- austin3 %>% 
  count(Tract, subject_injury) %>% 
  filter(subject_injury == 1)

subjectDeath <- austin3 %>% 
  count(Tract, subject_death) %>% 
  filter(subject_death == 1)

officerUsedWeapon <- austin3 %>% 
  count(Tract, officer_used_weapon) %>% 
  filter(officer_used_weapon == 1)

officerUsedFirearm <- austin3 %>% 
  count(Tract, officer_used_firearm) %>% 
  filter(officer_used_firearm == 1)

injuries <- officerInjury %>% 
  left_join(subjectInjury, by = "Tract") %>% 
  mutate(subj_off = round(n.y/n.x, 2))
#'
#'###Obtain more demographic data from US Census
#'
#'###Combine spatial data with attribute data
#'####Construct a Spatial Object
apd <- st_read("D:/Kaggle_Policing/Dept_37-00027/37-00027_Shapefiles/APD_DIST.shp")
summary(apd)
glimpse(apd)
apd$geometry[[1]]
plot(apd["CODE"])#picked code at random to show close-up

#T'ravis County is UTM Zone 14
#'+proj=utm +zone=14 +a=6378137 +b=6378135.99663591 +datum=WGS84 +units=m +no_defs 










