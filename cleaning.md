# Draconian cleaning of covidstress database
## 1. Complete answers for all scale items
## 2. Socio-demographic items are not cleaned
## 3. Scales with < 3 items are not cleaned (e.g. Country measures, trust in people)
## 4. AD gain and AD loss items not cleaned

# Libraries
    library(tidyverse)
    library(psych)
    library(lubridate)

# Reading the database
## About a dozen test answers cleaned, including one from the Mexican team
    stressW<- read.csv("~/covidstress/cs27apr.csv", encoding = "UTF-8")
    stressW <- stressW [3:158774,] %>% filter(!Dem_state =="VERACRUZ-PRUEBA") %>%
      filter(!Expl_Distress_txt=="Please exclude - test")#get rid of descriptions and test answer

# Recode marital status for languages different than english
## Copied from Thao Tran's cleaning file
## Load function 'recode_if'(Aden-Buie & Gerke, 2018)
    recode_if <- function(x, condition, ...) {
      if_else(condition, recode(x, ...), x)
    }

## Fix differences in scoring between english and other languages 
    stressW <- stressW %>%
      mutate(Dem_maritalstatus = 
           recode_if(Dem_maritalstatus, UserLanguage != "EN", 
                     "Single" = "Other or would rather not say",
                     "Married/cohabiting" = "Single",
                     "Divorced/widowed"= "Married/cohabiting",
                     "Other or would rather not say" = "Divorced/widowed"))

## Coerce as dates
    stressW$StartDate <- as_datetime(stressW$StartDate)
    stressW$EndDate <- as_datetime(stressW$EndDate)

# Recoding countries, adapted from Tran's
    stressW$Country <- as.character(stressW$Country)
    stressW <- stressW %>% 
      mutate(Country = ifelse(StartDate < "2020-04-08 01:53:18" & StartDate > "2020-03-28 13:30:02",
                          ifelse(UserLanguage == "BG",
                                 case_when(Country == "- other" ~ NA_character_, 
                                           Country == "Afghanistan" ~ "Afghanistan", 
                                           Country == "Algeria" ~ "Andorra",
                                           Country == "Angola" ~ "Antigua and Barbuda",
                                           Country == "Antigua and Barbuda" ~ "Argentina",
                                           Country == "Armenia" ~ "Australia",
                                           Country == "Australia" ~ "Austria",
                                           Country == "Belarus" ~ "Belgium",
                                           Country == "Brunei" ~ "Bulgaria",
                                           Country == "Bulgaria" ~ "Burkina Faso",
                                           
                                           Country == "Cameroon" ~ "Canada",
                                           Country == "Cuba" ~ "Cyprus",
                                           Country == "Cyprus" ~ "Czech Republic",
                                           Country == "Czech Republic" ~ "Denmark",
                                           Country == "Fiji" ~ "Finland",
                                           Country == "Finland" ~ "France",
                                           Country == "Georgia" ~ "Germany",
                                           Country == "Ghana" ~ "Greece",
                                           Country == "Iraq" ~ "Ireland",
                                           Country == "Ireland" ~ "Israel",
                                           
                                           Country == "Israel" ~ "Italy",
                                           Country == "Japan" ~ "Jordan",
                                           Country == "Korea, North" ~ "Korea, South",
                                           Country == "Korea, South" ~ "Kosovo",
                                           Country == "Liechtenstein" ~ "Lithuania",
                                           Country == "Lithuania" ~ "Luxembourg",
                                           Country == "Mali" ~ "Malta",
                                           Country == "Nepal" ~ "Netherlands",
                                           Country == "Nigeria" ~ "North Macedonia",
                                           Country == "Poland" ~ "Portugal",
                                           
                                           Country == "Portugal" ~ "Qatar",
                                           Country == "Qatar" ~ "Romania",
                                           Country == "South Africa" ~ "Spain",
                                           Country == "Spain" ~ "Sri Lanka",
                                           Country == "Suriname" ~ "Sweden",
                                           Country == "Sweden" ~ "Switzerland",
                                           Country == "Tanzania" ~ "Thailand",
                                           Country == "The Bahamas" ~ "Bahrain",
                                           Country == "Tunisia" ~ "Turkey",
                                           Country == "Uganda" ~ "Ukraine",
                                           
                                           Country == "Ukraine" ~ "United Arab Emirates",
                                           Country == "United Arab Emirates" ~ "United Kingdom",
                                           Country == "United Kingdom" ~ "United States",
                                           Country == "Zimbabwe" ~ NA_character_)
                                 , Country), Country)) 
  

# Another language issues
    stressW <- stressW %>% 
      mutate(Country = ifelse(UserLanguage == "AFR" & StartDate <= "2020-04-07 06:48:00",
                          ifelse(Country == "Somalia", "South Africa", "United States"), Country))
    stressW <- stressW %>% 
      mutate(Country = ifelse(UserLanguage == "HE",
                          case_when(Country == "Australia" ~ "Italy",
                                    Country == "Liberia" ~ "Israel"), Country))
    stressW$Country <- as.factor(stressW$Country)

# And now, the matter with female and male inverted for Mexico & Spain
    stressW$Dem_gender <- as.character(stressW$Dem_gender)
    stressW <- stressW %>% 
      mutate(Dem_gender = ifelse(Country == "Mexico"|Country=="Spain",
                          case_when(Dem_gender == "Female" ~ "Male",
                                    Dem_gender == "Male" ~ "Female",
                                    Dem_gender == "Other/would rather not say" ~ "Other/would rather not say"), 
                                    Dem_gender))
    stressW$Dem_gender <- as.factor(stressW$Dem_gender)

# Selecting complete scales with more than 2 items
    stressW <- filter_at(stressW, vars(contains("Expl_media_")), all_vars(.!=""))#Media
    stressW <- filter_at(stressW, vars(contains("Expl_Coping_"), -Expl_coping_txt), all_vars(.!=""))#Coping
    stressW <- filter_at(stressW, vars(contains("SPS_")), all_vars(.!=""))#Social provisions
    stressW <- filter_at(stressW, vars(contains("Expl_Distress_"), -Expl_Distress_txt), all_vars(.!=""))#Expl_Distress
    stressW <- filter_at(stressW, vars(contains("BFF_15_")), all_vars(.!=""))#Personality scales
    stressW <- filter_at(stressW, vars(contains("Compliance_")), all_vars(.!=""))#Compliance
    stressW <- filter_at(stressW, vars(contains("Corona_concerns_")), all_vars(.!=""))#Concerns
    stressW <- filter_at(stressW, vars(contains("OECD_insititutions_")), all_vars(.!=""))#Institutions, check mispelling
    stressW <- filter_at(stressW, vars(contains("Scale_PSS10_")), all_vars(.!=""))#Stress & loneliness scales

# Functions to recode scales
## Of time scales
    recodet <- function(x) { 
      levels(x) <- sub("Very often", "5", levels(x))
      levels(x) <- sub("Fairly often", "4", levels(x))
      levels(x) <- sub("Sometimes", "3", levels(x))
      levels(x) <- sub("Almost never", "2", levels(x))
      levels(x) <- sub("Never", "1", levels(x))
      return(x)
    }
    recodetR <- function(x) { 
      levels(x) <- sub("Very often", "1", levels(x))
      levels(x) <- sub("Fairly often", "2", levels(x))
      levels(x) <- sub("Sometimes", "3", levels(x))
      levels(x) <- sub("Almost never", "4", levels(x))
      levels(x) <- sub("Never", "5", levels(x))
      return(x)
    }  

# Of attitude scales
    recodea <- function(x) { 
      levels(x) <- sub("Strongly agree", "6", levels(x))
      levels(x) <- sub("Agree", "5", levels(x))
      levels(x) <- sub("Slightly agree", "4", levels(x))
      levels(x) <- sub("Slightly disagree", "3", levels(x))
      levels(x) <- sub("Disagree", "2", levels(x))
      levels(x) <- sub("Strongly disagree", "1", levels(x))
      return(x)
    }
    recodeaR <- function(x) { 
      levels(x) <- sub("Strongly agree", "1", levels(x))
      levels(x) <- sub("Agree", "2", levels(x))
      levels(x) <- sub("Slightly agree", "3", levels(x))
      levels(x) <- sub("Slightly disagree", "4", levels(x))
      levels(x) <- sub("Disagree", "5", levels(x))
      levels(x) <- sub("Strongly disagree", "6", levels(x))
      return(x)
    }

# Of the stress sources scale, Expl_Distress
    recodistress <- function(x) { 
      levels(x) <- sub("Strongly agree", "6", levels(x))
      levels(x) <- sub("Agree", "5", levels(x))
      levels(x) <- sub("Slightly agree", "4", levels(x))
      levels(x) <- sub("Slightly disagree", "3", levels(x))
      levels(x) <- sub("Disagree", "2", levels(x))
      levels(x) <- sub("Strongly disagree", "1", levels(x))
      levels(x) <- sub("Does not apply to my current situation", "0", levels(x))
      return(x)
    }

# Recoding and computing scales
## Stress scale
    stressW$Scale_PSS10_UCLA_1 <- as.numeric(as.character(recodet(stressW$Scale_PSS10_UCLA_1)))
    stressW$Scale_PSS10_UCLA_2 <- as.numeric(as.character(recodet(stressW$Scale_PSS10_UCLA_2)))
    stressW$Scale_PSS10_UCLA_3 <- as.numeric(as.character(recodet(stressW$Scale_PSS10_UCLA_3)))
    stressW$Scale_PSS10_UCLA_4 <- as.numeric(as.character(recodetR(stressW$Scale_PSS10_UCLA_4)))
    stressW$Scale_PSS10_UCLA_5 <- as.numeric(as.character(recodetR(stressW$Scale_PSS10_UCLA_5)))
    stressW$Scale_PSS10_UCLA_6 <- as.numeric(as.character(recodet(stressW$Scale_PSS10_UCLA_6)))
    stressW$Scale_PSS10_UCLA_7 <- as.numeric(as.character(recodetR(stressW$Scale_PSS10_UCLA_7)))
    stressW$Scale_PSS10_UCLA_8 <- as.numeric(as.character(recodetR(stressW$Scale_PSS10_UCLA_8)))
    stressW$Scale_PSS10_UCLA_9 <- as.numeric(as.character(recodet(stressW$Scale_PSS10_UCLA_9)))
    stressW$Scale_PSS10_UCLA_10 <- as.numeric(as.character(recodet(stressW$Scale_PSS10_UCLA_10)))
    TstressW <- stressW [,29:38]%>%rowSums()

## Loneliness scale
    stressW$Scale_PSS10_UCLA_11 <- as.numeric(as.character(recodet(stressW$Scale_PSS10_UCLA_11)))
    stressW$Scale_PSS10_UCLA_12 <- as.numeric(as.character(recodet(stressW$Scale_PSS10_UCLA_12)))
    stressW$Scale_PSS10_UCLA_13 <- as.numeric(as.character(recodet(stressW$Scale_PSS10_UCLA_13)))
    TloneW <- stressW [,39:41] %>% rowSums()

## Trust in people
    stressW$OECD_people_1 <- as.numeric(as.character(stressW$OECD_people_1))
    stressW$OECD_people_2 <- as.numeric(as.character(stressW$OECD_people_2))
    TpeopleW <- stressW [,42:43] %>% rowSums()

## Trust in institutions
    stressW$OECD_insititutions_1 <- as.numeric(as.character(stressW$OECD_insititutions_1))
    stressW$OECD_insititutions_2 <- as.numeric(as.character(stressW$OECD_insititutions_2))
    stressW$OECD_insititutions_3 <- as.numeric(as.character(stressW$OECD_insititutions_3))
    stressW$OECD_insititutions_4 <- as.numeric(as.character(stressW$OECD_insititutions_4))
    stressW$OECD_insititutions_5 <- as.numeric(as.character(stressW$OECD_insititutions_5))
    stressW$OECD_insititutions_6 <- as.numeric(as.character(stressW$OECD_insititutions_6))
    TinstW <- stressW [,44:49] %>% rowSums()

## Coronavirus concerns
    stressW$Corona_concerns_1<- as.numeric(as.character(recodea(stressW$Corona_concerns_1)))
    stressW$Corona_concerns_2<- as.numeric(as.character(recodea(stressW$Corona_concerns_2)))
    stressW$Corona_concerns_3<- as.numeric(as.character(recodea(stressW$Corona_concerns_3)))
    stressW$Corona_concerns_4<- as.numeric(as.character(recodea(stressW$Corona_concerns_4)))
    stressW$Corona_concerns_5<- as.numeric(as.character(recodea(stressW$Corona_concerns_5)))
    TconcernW <- stressW [,50:54] %>% rowSums()

## Country measures (though I would go for a strictness measures index)
    levels(stressW$Trust_countrymeasure) <- sub("Too much", "11", levels(stressW$Trust_countrymeasure))
    levels(stressW$Trust_countrymeasure) <- sub("Appropriate", "5", levels(stressW$Trust_countrymeasure))
    levels(stressW$Trust_countrymeasure) <- sub("Too little", "0", levels(stressW$Trust_countrymeasure))
    stressW$Trust_countrymeasure<- as.numeric(as.character(stressW$Trust_countrymeasure))

## Compliance
### The alpha of the scale is low, so I only added the items regarding social distance Compliance_2 & Compliance_3. With these two items, the alpha of the scale improves above 0.7, but now the scale is only about compliance with social distancing
    stressW$Compliance_1<- as.numeric(as.character(recodea(stressW$Compliance_1)))
    stressW$Compliance_2<- as.numeric(as.character(recodea(stressW$Compliance_2)))
    stressW$Compliance_3<- as.numeric(as.character(recodea(stressW$Compliance_3)))
    stressW$Compliance_4<- as.numeric(as.character(recodeaR(stressW$Compliance_4)))
    stressW$Compliance_5<- as.numeric(as.character(recodea(stressW$Compliance_5)))
    stressW$Compliance_6<- as.numeric(as.character(recodea(stressW$Compliance_6)))
    TcomplW <- stressW [,57:58] %>% rowSums()

# Each of the Big 5. Alphas may not be good, as expected in B5 scales with few items
## neurosis 1, 2, 3R
    stressW$BFF_15_1<- as.numeric(as.character(recodea(stressW$BFF_15_1)))
    stressW$BFF_15_2<- as.numeric(as.character(recodea(stressW$BFF_15_2)))
    stressW$BFF_15_3<- as.numeric(as.character(recodeaR(stressW$BFF_15_3)))
    TneurosisW <- stressW [,62:64] %>% rowSums()

## extroversion 4, 5, 6R
    stressW$BFF_15_4<- as.numeric(as.character(recodea(stressW$BFF_15_4)))
    stressW$BFF_15_5<- as.numeric(as.character(recodea(stressW$BFF_15_5)))
    stressW$BFF_15_6<- as.numeric(as.character(recodeaR(stressW$BFF_15_6)))
    TextroverW <- stressW [,65:67] %>% rowSums()

## openness 7, 8, 9
    stressW$BFF_15_7<- as.numeric(as.character(recodea(stressW$BFF_15_7)))
    stressW$BFF_15_8<- as.numeric(as.character(recodea(stressW$BFF_15_8)))
    stressW$BFF_15_9<- as.numeric(as.character(recodea(stressW$BFF_15_9)))
    TopenW <- stressW [,68:70] %>% rowSums()

## agreeableness 10R, 11, 12
    stressW$BFF_15_10<- as.numeric(as.character(recodeaR(stressW$BFF_15_10)))
    stressW$BFF_15_11<- as.numeric(as.character(recodea(stressW$BFF_15_11)))
    stressW$BFF_15_12<- as.numeric(as.character(recodea(stressW$BFF_15_12)))
    TagreeW <- stressW [,71:73] %>% rowSums()

## consciousness 13, 14R, 15
    stressW$BFF_15_13<- as.numeric(as.character(recodea(stressW$BFF_15_13)))
    stressW$BFF_15_14<- as.numeric(as.character(recodeaR(stressW$BFF_15_14)))
    stressW$BFF_15_15<- as.numeric(as.character(recodea(stressW$BFF_15_15)))
    TconcW <- stressW [,74:76] %>% rowSums()

# Sources of stress. Alpha is not bad, but maybe not a scale, as it admits na's.
## A summatory value is included, you know, just in case
    stressW$Expl_Distress_1<- as.numeric(as.character(recodistress(stressW$Expl_Distress_1)))
    stressW$Expl_Distress_2<- as.numeric(as.character(recodistress(stressW$Expl_Distress_2)))
    stressW$Expl_Distress_3<- as.numeric(as.character(recodistress(stressW$Expl_Distress_3)))
    stressW$Expl_Distress_4<- as.numeric(as.character(recodistress(stressW$Expl_Distress_4)))
    stressW$Expl_Distress_5<- as.numeric(as.character(recodistress(stressW$Expl_Distress_5)))
    stressW$Expl_Distress_6<- as.numeric(as.character(recodistress(stressW$Expl_Distress_6)))
    stressW$Expl_Distress_7<- as.numeric(as.character(recodistress(stressW$Expl_Distress_7)))
    stressW$Expl_Distress_8<- as.numeric(as.character(recodistress(stressW$Expl_Distress_8)))
    stressW$Expl_Distress_9<- as.numeric(as.character(recodistress(stressW$Expl_Distress_9)))
    stressW$Expl_Distress_10<- as.numeric(as.character(recodistress(stressW$Expl_Distress_10)))
    stressW$Expl_Distress_11<- as.numeric(as.character(recodistress(stressW$Expl_Distress_11)))
    stressW$Expl_Distress_12<- as.numeric(as.character(recodistress(stressW$Expl_Distress_12)))
    stressW$Expl_Distress_13<- as.numeric(as.character(recodistress(stressW$Expl_Distress_13)))
    stressW$Expl_Distress_14<- as.numeric(as.character(recodistress(stressW$Expl_Distress_14)))
    stressW$Expl_Distress_15<- as.numeric(as.character(recodistress(stressW$Expl_Distress_15)))
    stressW$Expl_Distress_16<- as.numeric(as.character(recodistress(stressW$Expl_Distress_16)))
    stressW$Expl_Distress_17<- as.numeric(as.character(recodistress(stressW$Expl_Distress_17)))
    stressW$Expl_Distress_18<- as.numeric(as.character(recodistress(stressW$Expl_Distress_18)))
    stressW$Expl_Distress_19<- as.numeric(as.character(recodistress(stressW$Expl_Distress_19)))
    stressW$Expl_Distress_20<- as.numeric(as.character(recodistress(stressW$Expl_Distress_20)))
    stressW$Expl_Distress_21<- as.numeric(as.character(recodistress(stressW$Expl_Distress_21)))
    stressW$Expl_Distress_22<- as.numeric(as.character(recodistress(stressW$Expl_Distress_22)))
    stressW$Expl_Distress_23<- as.numeric(as.character(recodistress(stressW$Expl_Distress_23)))
    stressW$Expl_Distress_24<- as.numeric(as.character(recodistress(stressW$Expl_Distress_24)))
    TdistressW <- stressW [,77:100] %>% rowSums()

# Social provisions
    stressW$SPS_1<- as.numeric(as.character(recodea(stressW$SPS_1)))
    stressW$SPS_2<- as.numeric(as.character(recodea(stressW$SPS_2)))
    stressW$SPS_3<- as.numeric(as.character(recodea(stressW$SPS_3)))
    stressW$SPS_4<- as.numeric(as.character(recodea(stressW$SPS_4)))
    stressW$SPS_5<- as.numeric(as.character(recodea(stressW$SPS_5)))
    stressW$SPS_6<- as.numeric(as.character(recodea(stressW$SPS_6)))
    stressW$SPS_7<- as.numeric(as.character(recodea(stressW$SPS_7)))
    stressW$SPS_8<- as.numeric(as.character(recodea(stressW$SPS_8)))
    stressW$SPS_9<- as.numeric(as.character(recodea(stressW$SPS_9)))
    stressW$SPS_10<- as.numeric(as.character(recodea(stressW$SPS_10)))
    TspsW <- stressW [,102:111] %>% rowSums()

# Coping. Again, probably not a scale. Yet, a total is included
    stressW$Expl_Coping_1<- as.numeric(as.character(recodea(stressW$Expl_Coping_1)))
    stressW$Expl_Coping_2<- as.numeric(as.character(recodea(stressW$Expl_Coping_2)))
    stressW$Expl_Coping_3<- as.numeric(as.character(recodea(stressW$Expl_Coping_3)))
    stressW$Expl_Coping_4<- as.numeric(as.character(recodea(stressW$Expl_Coping_4)))
    stressW$Expl_Coping_5<- as.numeric(as.character(recodea(stressW$Expl_Coping_5)))
    stressW$Expl_Coping_6<- as.numeric(as.character(recodea(stressW$Expl_Coping_6)))
    stressW$Expl_Coping_7<- as.numeric(as.character(recodea(stressW$Expl_Coping_7)))
    stressW$Expl_Coping_8<- as.numeric(as.character(recodea(stressW$Expl_Coping_8)))
    stressW$Expl_Coping_9<- as.numeric(as.character(recodea(stressW$Expl_Coping_9)))
    stressW$Expl_Coping_10<- as.numeric(as.character(recodea(stressW$Expl_Coping_10)))
    stressW$Expl_Coping_11<- as.numeric(as.character(recodea(stressW$Expl_Coping_11)))
    stressW$Expl_Coping_12<- as.numeric(as.character(recodea(stressW$Expl_Coping_12)))
    stressW$Expl_Coping_13<- as.numeric(as.character(recodea(stressW$Expl_Coping_13)))
    stressW$Expl_Coping_14<- as.numeric(as.character(recodea(stressW$Expl_Coping_14)))
    stressW$Expl_Coping_15<- as.numeric(as.character(recodea(stressW$Expl_Coping_15)))
    stressW$Expl_Coping_16<- as.numeric(as.character(recodea(stressW$Expl_Coping_16)))
    TcopingW <- stressW [,112:127] %>% rowSums()

# Media
## Alfa increases greatly if last item is dropped. The sentence of the last item refers to attitude, while the previous items regard only consumption. Thus, the value of the scale considers only the first five items
    stressW$Expl_media_1<- as.numeric(as.character(recodea(stressW$Expl_media_1)))
    stressW$Expl_media_2<- as.numeric(as.character(recodea(stressW$Expl_media_2)))
    stressW$Expl_media_3<- as.numeric(as.character(recodea(stressW$Expl_media_3)))
    stressW$Expl_media_4<- as.numeric(as.character(recodea(stressW$Expl_media_4)))
    stressW$Expl_media_5<- as.numeric(as.character(recodea(stressW$Expl_media_5)))
    stressW$Expl_media_6<- as.numeric(as.character(recodea(stressW$Expl_media_6)))
    TmediaW <- stressW [,129:133] %>% rowSums()

# Now we make a tibble with the scale totals
    ScalesW <- tibble(TstressW, TpeopleW, TinstW,
                  TconcernW, TcomplW, TneurosisW,
                  TextroverW, TopenW, TagreeW,
                  TconcW, TdistressW, TspsW,
                  TcopingW,TmediaW)

# And we paste the scale results with the general database
    clean_world <- stressW %>% bind_cols(ScalesW)

## Just remember demographics and very small scales were not cleaned. And LO! You're ready to perform your magic on analyses and visualizations. Good luck!

# Limpieza para MX
## Limpiamos para MX y traducimos
    clean_mx <- filter(clean_world, Country == "Mexico")
    levels(clean_mx$Dem_gender) <- sub("Female", "Mujeres", levels(clean_mx$Dem_gender))
    levels(clean_mx$Dem_gender) <- sub("Male", "Hombres", levels(clean_mx$Dem_gender))
    levels(clean_mx$Dem_gender) <- sub("Other/would rather not say", "Otro/Prefiere no decir", levels(clean_mx$Dem_gender))

    levels(clean_mx$Dem_edu) <- sub("- College degree, bachelor, master", "Licenciatura", levels(clean_mx$Dem_edu))
    levels(clean_mx$Dem_edu) <- sub("- None", "Ninguna", levels(clean_mx$Dem_edu))
    levels(clean_mx$Dem_edu) <- sub("- PhD/Doctorate", "Posgrado", levels(clean_mx$Dem_edu))
    levels(clean_mx$Dem_edu) <- sub("- Some College, short continuing education or equivalent", "Licenciatura trunca",    levels(clean_mx$Dem_edu))
    levels(clean_mx$Dem_edu) <- sub("- Up to 12 years of school", "Bachillerato", levels(clean_mx$Dem_edu))
    levels(clean_mx$Dem_edu) <- sub("- Up to 9 years of school", "Secundaria", levels(clean_mx$Dem_edu))
    levels(clean_mx$Dem_edu) <- sub("- Up to 6 years of school", "Primaria", levels(clean_mx$Dem_edu))

    levels(clean_mx$Dem_edu_mom) <- sub("- College degree", "Licenciatura", levels(clean_mx$Dem_edu_mom))
    levels(clean_mx$Dem_edu_mom) <- sub("- None", "Ninguna", levels(clean_mx$Dem_edu_mom))
    levels(clean_mx$Dem_edu_mom) <- sub("- PhD/Doctorate", "Posgrado", levels(clean_mx$Dem_edu_mom))
    levels(clean_mx$Dem_edu_mom) <- sub("- Some College or equivalent", "Licenciatura trunca", levels(clean_mx$Dem_edu_mom))
    levels(clean_mx$Dem_edu_mom) <- sub("- Up to 12 years of school", "Bachillerato", levels(clean_mx$Dem_edu_mom))
    levels(clean_mx$Dem_edu_mom) <- sub("- Up to 9 years of school", "Secundaria", levels(clean_mx$Dem_edu_mom))
    levels(clean_mx$Dem_edu_mom) <- sub("- Up to 6 years of school", "Primaria", levels(clean_mx$Dem_edu_mom))

    levels(clean_mx$Dem_employment) <- sub("Full time employed", "Empleo de tiempo completo", levels(clean_mx$Dem_employment))
    levels(clean_mx$Dem_employment) <- sub("Not employed", "Sin empleo", levels(clean_mx$Dem_employment))
    levels(clean_mx$Dem_employment) <- sub("Part time employed", "Empleo de tiempo parcial", levels(clean_mx$Dem_employment))
    levels(clean_mx$Dem_employment) <- sub("Retired", "Retirado-Jubilado", levels(clean_mx$Dem_employment))
    levels(clean_mx$Dem_employment) <- sub("Self-employed", "Auto empleo", levels(clean_mx$Dem_employment))
    levels(clean_mx$Dem_employment) <- sub("Student", "Estudiante", levels(clean_mx$Dem_employment))

    levels(clean_mx$Dem_state) <- sub(".*Ciudad de M.*|.*CDMX.*|.*cd de m.*|.*mexico city.*|.*df.*|.*d.f.*|.*distrito f.*|.*azcapo.*|.*benito j.*|.*alvaro o.*|.*iztapalap.*|.*cuidad de m.*|.*tlalpan.*|.*ciudad mex.*|.*cd mex.*|.*cd mx.*|.*coyoacan.*|.*cdmex.*", "CDMX", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*Aguascal.*", "Aguascalientes", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*baja california s.*|.*bcs.*", "Baja California Sur", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*Jalis.*|.*guadalaj.*", "Jalisco", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*nuevo l.*|.*monterr.*", "Nuevo León", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*estado de m.*|.*edomex.*|.*edo de m.*|.*atizap.*", "Estado de México", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*verac.*|.*coatepec.*|.*boca del r.*|.*xalapa.*|.*Jalapa.*|.*varacruz.*", "Veracruz", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*chiap.*", "Chiapas", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*chihua.*", "Chihuahua", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*coah.*", "Coahuila", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*colim.*", "Colima", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*durang.*", "Durango", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*michoa.*|.*morelia.*", "Michoacán", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*puebla.*", "Puebla", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*sonor.*|.*hermosillo.*", "Sonora", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*guanaj.*|.*gto.*", "Guanajuato", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*hidalgo.*|.*hgo.*|.*pachuca.*", "Hidalgo", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*queretaro.*|.*QuerÃ©t.*|.*querét.*", "Querétaro", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*oaxaca.*|.*oax.*|.*huatulco.*", "Oaxaca", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*tamaulipas.*|.*tamps.*|.*tamsulipas.*", "Tamaulipas", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*sinal.*", "Sinaloa", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*quintana r.*|.*cancun.*|.*cancún.*|.*roo.*", "Quintana Roo", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*tabasc.*", "Tabasco", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*yucat.*|.*mérida.*|.*merida.*", "Yucatán", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*san luis.*", "San Luis Potosí", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*guerrero.*|.*ayutla de los.*", "Guerrero", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*nayar.*", "Nayarit", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*tlaxca.*", "Tlaxcala", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*zacatec.*", "Zacatecas", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*cuernav.*|.*morelos.*", "Morelos", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("baja california", "Baja California", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("mexicali", "Baja California", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("slp", "San Luis Potosí", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*chih.*", "Chihuahua", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*cd méx.*|.*ciuda de me.*|.*cuauht.*|.*xico city.*|.*xixo city.*|.*ciudad méx.*|.*gustavo a.*", "CDMX", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*tlalnepant.*|.*valle de ch.*|.*nezahu.*|.*naucalpan.*|.*mexico.*|.*méxico.*|.*cuautitl.*|.*edo. mex.*|.*edo.mex.*|.*edo mex.*", "Estado de México", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*metepec.*|.*toluc.*|.*coacalco.*|.*huixquil.*|.*mexiqu.*|.*mexixo.*|.*mex.*|.*méx.*", "Estado de México", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*mor.*", "Morelos", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("bc", "Baja California", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("coshuila", "Coahuila", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("^bleon$|^león$", "Guanajuato", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("^nay$", "Nayarit", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("^Baja California $", "Baja California", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("^tijuana$", "Baja California", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("^nl$", "Nuevo León", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("^torreon$", "Coahuila", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("^ver$", "Veracruz", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub("^orizaba $", "Veracruz", levels(clean_mx$Dem_state), ignore.case = TRUE)
    levels(clean_mx$Dem_state) <- sub(".*zapopan.*", "Jalisco", levels(clean_mx$Dem_state), ignore.case = TRUE)

# Monitoreo
    view(count(clean_mx, Dem_state))

    estmx <- c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
               "Chihuahua", "Coahuila", "Colima", "CDMX", "Durango", "Guanajuato",
               "Guerrero", "Hidalgo", "Jalisco", "Estado de México", "Michoacán", "Morelos",
               "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", 
              "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala",
              "Veracruz", "Yucatán", "Zacatecas")

# Poner en NA todos los que no son estado
    clean_mx$Dem_state[!clean_mx$Dem_state %in% estmx ] <- NA

    levels(clean_mx$Dem_maritalstatus) <- sub("Married/cohabiting", "Casada(o)/en unión libre", levels(clean_mx$Dem_maritalstatus))
    levels(clean_mx$Dem_maritalstatus) <- sub("Single", "Soltera(o)", levels(clean_mx$Dem_maritalstatus))
    levels(clean_mx$Dem_maritalstatus) <- sub("Divorced/widowed", "Divorciada(o)/viuda(o)", levels(clean_mx$Dem_maritalstatus))
    levels(clean_mx$Dem_maritalstatus) <- sub("Other or would rather not say", "Otro/prefiere no decir", levels(clean_mx$Dem_maritalstatus))

    levels(clean_mx$Dem_islolation) <- sub("Isolated in medical facility of similar location", "En aislamiento en unidad médica o similar", levels(clean_mx$Dem_islolation))
    levels(clean_mx$Dem_islolation) <- sub("Isolated", "En aislamiento", levels(clean_mx$Dem_islolation))
    levels(clean_mx$Dem_islolation) <- sub("Life carries on as usual", "La vida sigue sin cambios", levels(clean_mx$Dem_islolation))
    levels(clean_mx$Dem_islolation) <- sub("Life carries on with minor changes", "La vida sigue con cambios menores",    levels(clean_mx$Dem_islolation))

    clean_mx$Dem_age <- as.numeric(as.character(clean_mx$Dem_age))
    clean_mx$dem_ageG <- case_when(clean_mx$Dem_age <= 20 ~ "20 o menos",
                                  clean_mx$Dem_age > 20 & clean_mx$Dem_age < 31 ~ "21 - 30",
                                  clean_mx$Dem_age > 30 & clean_mx$Dem_age < 41 ~ "31 - 40",
                                  clean_mx$Dem_age > 40 & clean_mx$Dem_age < 51 ~ "41 - 50",
                                  clean_mx$Dem_age > 50 & clean_mx$Dem_age < 61 ~ "51 - 60",
                                  clean_mx$Dem_age > 60 & clean_mx$Dem_age < 71 ~ "61 - 70",
                                  clean_mx$Dem_age > 70 ~ "70 o más")

    clean_mx$Dem_isolation_adults <- as.numeric(as.character(clean_mx$Dem_isolation_adults))
    clean_mx$Dem_isolation_kids <- as.numeric(as.character(clean_mx$Dem_isolation_kids))

    clean_mx$isoladult <- ifelse(clean_mx$Dem_isolation_adults==0, "No", "Sí" )
    clean_mx$isolkids <- ifelse(clean_mx$Dem_isolation_kids==0, "No", "Sí" )
    count(clean_mx, isolkids)

    write.csv(clean_mx, "clean_mx27abr.csv")
    write.csv(clean_world, "cleanwrld27abr.csv")
