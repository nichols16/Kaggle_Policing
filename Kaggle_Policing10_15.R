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
#'
#'Form the date related features
data37 <- data37 %>% 
  mutate(date = anytime(DateOccurred)) %>%  #det date in proper form
  mutate(day = lubridate::day(date)) %>% 
  mutate(dayofweek = wday(date, label = TRUE)) %>% 
  mutate(mon = month(date, label = TRUE)) %>% 
  mutate(yr = year(date))
#'
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
#'
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
#'
#'OfficerYrsofService
data37_1 <- data37_1 %>% 
  rename(offserv = "OfficerYrsofService")
#'
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
#'
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
#'
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
#'
#'ReasonDesc
table(data37_1$ReasonDesc)
#'The reason descriptions to me seem vague. I don't think that these 
#'can be useful for answering any research questions that will come up.
#'Therefore I will delete this feature.
data37_1 <- data37_1 %>% dplyr::select(-ReasonDesc)
glimpse(data37_1)
#'
#'###Dealing with NA's and Random Imputation
#'
#'####Check for NA's and random impute if needed
nas <- sum(is.na(data37_1))
paste("Total number of NA's in data is:", nas)
#'
nacheck <- function(x) {
  nas <- sum(is.na(x))
  return(nas)
}
#'
#'if no comment is present assume no NA's
nacheck(data37_1$RIN)
nacheck(data37_1$offserv)
nacheck(data37_1$latitude) # will deal with geographic data in a later section
nacheck(data37_1$longitude)# will deal with geographic data in a later section
nacheck(data37_1$Geolocation)
nacheck(data37_1$officer_injury)
nacheck(data37_1$subject_injury)
nacheck(data37_1$subject_death)
#'
#'####Random Imputation
nacheck(data37_1$ officer_used_weapon)# 35 NA's
(tab1 <- xtabs(~data37_1$ officer_used_weapon))
prob <- tab1[[2]]/(tab1[[1]] + tab1[[2]])
paste0("The proportion of 1's to 0's is: ", round(prob, 2))
#'Replace the missing values using random imputation with binomial distribution.
data37_1$officer_used_weapon <- if_else(is.na(data37_1$officer_used_weapon), rbinom(1,1,prob), as.integer(data37_1$officer_used_weapon))
sum(is.na(data37_1$officer_used_weapon))
#'
nacheck(data37_1$officer_used_firearm)
nacheck(data37_1$date) #will deal with time related data in a later section
nacheck(data37_1$day) #will deal with time related data in a later section
nacheck(data37_1$dayofweek) #will deal with time related data in a later section
nacheck(data37_1$mon) #will deal with time related data in a later section
nacheck(data37_1$yr) #will deal with time related data in a later section
#'
#'Random Imputation
nacheck(data37_1$male) #324 NA's
(tab1 <- xtabs(~data37_1$male))
prob <- tab1[[2]]/(tab1[[1]] + tab1[[2]])
paste0("The proportion of 1's to 0's is: ", round(prob, 2))
#'Replace the missing values using random imputation with binomial distribution.
data37_1$male <- if_else(is.na(data37_1$male), rbinom(1,1,prob), as.integer(data37_1$male))
sum(is.na(data37_1$male))
#'
nacheck(data37_1$race)
nacheck(data37_1$contact)
nacheck(data37_1$command)
nacheck(data37_1$org)
#'
glimpse(data37_1)
#'
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
austin1 <- austin1 %>% 
  drop_na("Tract")
sum(is.na(austin1$Tract))
#change Tract to character
austin1$Tract <- as.character(austin1$Tract)
austin1 <- austin1 %>% rename(tract = "Tract")
glimpse(austin1)
#'
#'###Summarize police data by tract
#'
officer_injury <- austin1 %>% 
group_by(tract) %>% 
  summarize(officer_injury = sum(officer_injury))
subject_injury <- austin1 %>% 
  group_by(tract) %>% 
  summarize(subject_injury = sum(subject_injury))
yrs_on_force <- austin1 %>% 
  group_by(tract) %>% 
  summarize(yrs_on_force = sum(offserv))
officer_used_weapon <- austin1 %>% 
  group_by(tract) %>% 
  summarize(officer_used_weapon = sum(officer_used_weapon))
officer_used_firearm <- austin1 %>% 
  group_by(tract) %>% 
  summarize(officer_used_firearm = sum(officer_used_firearm))
subject_death <- austin1 %>% 
  group_by(tract) %>% 
  summarize(subject_death = sum(subject_death))
police_stats <- bind_cols(
  officer_injury, subject_injury, yrs_on_force,
  officer_used_weapon, officer_used_firearm,
  subject_death)
police_stats <- police_stats %>% 
  dplyr::select(tract, officer_injury, subject_injury,
                yrs_on_force, officer_used_weapon,
                officer_used_firearm, subject_death)
glimpse(police_stats)
#'*********************************************************
#'###Extract educational attainment data
#'####Read and reduce the data
educ1 <- read_csv("Austin16_educ.csv")
colnames(educ1) <- educ1[1, ]
educ2 <- educ1[-1, ]
#'select the most relevant columns
educ3 <- educ2[ , c(2,8,342,414)]
glimpse(educ3)
#'####Extract the educational data by census tract
nams2 <- c("tract", "pop_male_18_24", "pct_white_hsgrad", "pct_black_hsgrad")
colnames(educ3) <- nams2
#'Extract the 4-digit Census Tract code from tract
educ3 <- educ3 %>% 
  mutate(tract = str_extract(tract, pattern = "[0-9]{4}$"))
sum(is.na(educ3))
educ4 <- as.tibble(sapply(educ3, as.numeric))
glimpse(educ4)
sum(is.na(educ4$pct_black_hsgrad))
#'Deal with NA's
#'Median Imputation for 1 NA
med1 <- median(educ4$pct_white_hsgrad, na.rm = TRUE)
educ4 <- educ4 %>% 
  mutate(pct_white_hsgrad = if_else(is.na(educ4$pct_white_hsgrad), med1, educ4$pct_white_hsgrad))
#'Median Imputation for 8 NA's
med2 <- median(educ4$pct_black_hsgrad, na.rm = TRUE)
educ4 <- educ4 %>% 
  mutate(pct_black_hsgrad = if_else(is.na(educ4$pct_black_hsgrad), med2, educ4$pct_black_hsgrad))
sum(is.na(educ4$pct_black_hsgrad))
#change tract to character
educ4$tract <- as.character(educ4$tract)
glimpse(educ4)
#'Now we have tract as character and education groups as numerics
#'**********************************************************
#'###Extract housing data
#'####Read and reduce the data
#'similar procedure as for educational data
hous1 <- read_csv("Austin16_housing.csv")
colnames(hous1) <- hous1[1, ]
hous2 <- hous1[-1, ]
#'select the most relevant columns
hous3 <- hous2[ , c(2,10, 16)]
nams3 <- c("tract", "white_h", "black_h")
colnames(hous3) <- nams3
hous4 <- hous3 %>% 
  mutate(tract = str_extract(tract, pattern = "[0-9]{4}$"))
hous5 <- as.tibble(sapply(hous4, as.numeric))
nacheck(hous5)
nacheck(hous5$tract)
nacheck(hous5$white_h)
nacheck(hous5$black_h)
#Median imputation for 3 NA's
med_w <- median(hous5$white_h, na.rm = TRUE)
med_b <- median(hous5$black_h, na.rm = TRUE)
hous5 <- hous5 %>% mutate(white_h = if_else(is.na(hous5$white_h), med_w, hous5$white_h))
hous5 <- hous5 %>% mutate(black_h = if_else(is.na(hous5$black_h), med_w, hous5$black_h))
nacheck(hous5)
#'change tract back to character
hous5$tract <- as.character(hous5$tract)
glimpse(hous5)
#'####Combine education and housing data
ed_hous <- bind_cols(educ4, hous5)
glimpse(ed_hous)
#'
#'******************************************************
#'
#'###Extract poverty data
#'
#'####Read and reduce the data
#'similar procedure as for housing data
pov1 <- read_csv("Austin16_poverty.csv")
colnames(pov1) <- pov1[1, ]
pov2 <- pov1[-1, ]
#'select the most relevant columns
pov3 <- pov2[ , c(2,8,81,86)]
nams3 <- c("tract", "tot_pov", "white_pov", "black_pov")
colnames(pov3) <- nams3
pov3 <- pov3 %>% 
  mutate(tract = str_extract(tract, pattern = "[0-9]{4}$"))
sum(is.na(pov3))
#'convert to numerics
pov4 <- as.tibble(sapply(pov3, as.numeric))
#convert tract back to character
pov4$tract <- as.character(pov4$tract)
glimpse(pov4)
ed_hous_pov <- cbind(educ4, hous5, pov4)
#remove duplicate "tracts"
ed_hous_pov <- ed_hous_pov[ , -c(5,8)]
glimpse(ed_hous_pov)
#*********************************************************
#'###Combine police data and census data
combo <- police_stats %>% 
  left_join(ed_hous_pov, by = "tract")
glimpse(combo)
nacheck(combo)
combo %>% map(nacheck)
#Deal with NA's
#'#####This is a relatively simple method to impute missing data. 
#'First, dtermine the approximate probability distribution that the 
#'vector of data comes from. You can do this by simply looking at its 
#'probability distribution. ggplot it using geom_density. Then, 
#'you do some experimenting wih different probability distributions 
#'using random generators: rnorm, rgamma, rbeta, rcauchy etc. I have found that 
#'these three will give you an approximate distribution for many situations.
#'Get the proper scale by multiplying the distributon.
#'Then, impute the NA's from the distribution you have determined with an
#'if_else function. Use the absolute value if you don't want negative imputed values.
#'We are trying to accomplish two things with this method.
#'1. Approximate the counterfactual value.
#'2. Approximate the uncertainty of the counterfactual value.
#'This method stands up well against other
#'methods of imputaion: median or mean imputation, MICE, and probably others.
#'Plese see my blog for a demonstration and discussion of random imputation:
#'https://epi2020datascience.blogspot.com/
#' 
#'First, plot the density  ofthe vector:
ggplot(combo) +
  geom_density(aes(x = pop_male_18_24)) +
  xlim(0, 1000)
#'Next, experiment with some distributions and multiply for scale and plot it
df <- data.frame(x = 200*rgamma(1000, 1.5)) #<- this was my pick. I won't show this step again. 
ggplot(df) +
  geom_density(aes(x = x)) + 
  xlim(0, 1000)
#'Then, imput your NA's with this probability distribution.
combo <- combo %>% 
  mutate(pop_male_18_24 = if_else(is.na(combo$pop_male_18_24), abs(200*rgamma(1, 1.5)), combo$pop_male_18_24))
#'
ggplot(combo) +
  geom_density(aes(x = pct_white_hsgrad)) #--> 100*rbeta (5,1)
combo <- combo %>% 
  mutate(pct_white_hsgrad = if_else(is.na(combo$pct_white_hsgrad), abs(100*rbeta (1, 5,1)), combo$pct_white_hsgrad))
#'
ggplot(combo) +
  geom_density(aes(x = pct_black_hsgrad)) + 
  xlim(0, 150) #--> cauchy (1, 99, 8)
combo <- combo %>% 
  mutate(pct_black_hsgrad = if_else(is.na(combo$pct_black_hsgrad), abs(100*rbeta (1,10,2)), combo$pct_black_hsgrad))
#'
ggplot(combo) +
  geom_density(aes(x = white_h)) #-->100*rbeta (12,2.5)
combo <- combo %>% 
  mutate(white_h = if_else(is.na(combo$white_h), abs(100*rbeta (1,12,2.5)), combo$white_h))
#'
ggplot(combo) +
  geom_density(aes(x = black_h)) #--> 100*rbeta (.8,5))
combo <- combo %>% 
  mutate(black_h = if_else(is.na(combo$black_h), abs(100*rbeta (1,.8,5)), combo$black_h))
#'
ggplot(combo) +
geom_density(aes(x = tot_pov)) #--> 100*rbeta (.9,5)
combo <- combo %>% 
  mutate(tot_pov = if_else(is.na(combo$tot_pov), abs(100*rbeta (1,.9,5)), combo$tot_pov))
#'
ggplot(combo) +
geom_density(aes(x = white_pov)) #-->  8*rbeta (.9,1.5)
combo <- combo %>% 
  mutate(white_pov = if_else(is.na(combo$white_pov),  abs(8*rbeta (1,.9,1.5)), combo$white_pov))
#'
ggplot(combo) +
  geom_density(aes(x = black_pov)) #--> 100*rbeta (.5,1.2)
combo <- combo %>% 
  mutate(black_pov = if_else(is.na(combo$black_pov), abs(100*rbeta (1,.5,1.2)), combo$black_pov))
#'Check for any NA's left
nacheck(combo) #--> no more NA's
glimpse(combo)
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










