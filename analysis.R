## Crystal Xue and Aurash Vatan
## HODP Comp Project: Q Guide Scraping & Analysis

## Setup:
rm(list=ls())
sink(file = "analysis.txt", split=TRUE)
setwd("~/Downloads/HODP")
raw <- read.csv(file = 'raw.csv', stringsAsFactors = FALSE)

## install.packages("dplyr")
## install.packages("tidyverse")
## install.packages("robustbase")

## Data Cleaning:

## Keeping only courses that exist in both Spring 2018 and Spring 2019:
library(dplyr)
both <- raw %>% 
  group_by(Course) %>%
  filter(n() > 1)

## Converting datatypes to numeric for regressions ("None" and empty strings forced to NAs):
both$Workload <- as.numeric(both$Workload)
both$Rating <- as.numeric(both$Rating)
both$Elective <- as.numeric(both$Elective)
both$Concentration <- as.numeric(both$Concentration)
both$Secondary.Citation <- as.numeric(both$Secondary.Citation)
both$Core.GENED <- as.numeric(both$Core.GENED)
both$EXPOS <- as.numeric(both$EXPOS)
both$Language <- as.numeric(both$Language)
both$Pre.med <- as.numeric(both$Pre.med)

## Renaming columns so I don't go crazy:
both <- both %>% 
  rename(
    term = Term,
    dept = Department,
    course = Course,
    workload = Workload,
    rating = Rating,
    enroll = Enrollment,
    rec = Recommendation,
    elective = Elective,
    concentration = Concentration,
    secondary = Secondary.Citation,
    gened = Core.GENED,
    expos = EXPOS,
    lang = Language,
    premed = Pre.med
  )

## Creating final dataframe:
courses_2018 <- subset(both, term=="Spring 2018")
courses_2019 <- subset(both, term=="Spring 2019")
courses_2018 <- courses_2018 %>% 
  rename(
    workload_2018 = workload,
    rating_2018 = rating,
    enroll_2018 = enroll,
    rec_2018 = rec,
    elective_2018 = elective,
    concentration_2018 = concentration,
    secondary_2018 = secondary,
    gened_2018 = gened,
    expos_2018 = expos,
    lang_2018 = lang,
    premed_2018 = premed
  )
courses_2019 <- courses_2019 %>% 
  rename(
    workload_2019 = workload,
    rating_2019 = rating,
    enroll_2019 = enroll,
    rec_2019 = rec,
    elective_2019 = elective,
    concentration_2019 = concentration,
    secondary_2019 = secondary,
    gened_2019 = gened,
    expos_2019 = expos,
    lang_2019 = lang,
    premed_2019 = premed
  )
combined <- full_join(courses_2019, courses_2018, by = "course")
combined<-combined[!(is.na(combined$term.x) | is.na(combined$term.y)),]

## Enrollment change in percentage points
combined$enroll_chg_pct <- (combined$enroll_2019 - combined$enroll_2018)/combined$enroll_2018
combined <- select (combined,-c(enroll_2018, enroll_2019))

## Find most common reason taken
library(tidyverse)
combined <- select (combined,-c(elective_2019, concentration_2019, secondary_2019, gened_2019, lang_2019, premed_2019, expos_2019, term.y, dept.y))
combined <- combined %>% 
  rename(
    elective = elective_2018,
    concentration = concentration_2018,
    secondary = secondary_2018,
    gened = gened_2018,
    expos = expos_2018,
    lang = lang_2018,
    premed = premed_2018
  )
combined$common_reason <- names(combined)[10:16][max.col(combined[10:16])]
combined <- select (combined,-c(elective, concentration, secondary, gened, lang, premed, expos))

## Other random cleaning:
combined <- select (combined,-c(term.x,workload_2019, rating_2019, rec_2019))
combined <- combined %>% 
  rename(
    dept = dept.x,
    workload = workload_2018,
    rating = rating_2018,
    rec = rec_2018
  )

## Creating indicator variables for most common reason taken and department:
## Common reason taken:
combined$concentration <- ifelse(combined$common_reason == "concentration", 1, 0)
combined$elective <- ifelse(combined$common_reason == "elective", 1, 0)
combined$expos <- ifelse(combined$common_reason == "expos", 1, 0)
combined$gened <- ifelse(combined$common_reason == "gened", 1, 0)
combined$lang <- ifelse(combined$common_reason == "lang", 1, 0)
combined$premed <- ifelse(combined$common_reason == "premed", 1, 0)
combined$secondary <- ifelse(combined$common_reason == "secondary", 1, 0)

## Department/Field:
combined$afam <- ifelse(combined$dept == "African & African Amer Studies", 1, 0)
combined$anthro <- ifelse(combined$dept == "Anthropology", 1, 0)
combined$ac <- ifelse(combined$dept == "Applied Computation", 1, 0)
combined$am <- ifelse(combined$dept == "Applied Mathematics", 1, 0)
combined$ap <- ifelse(combined$dept == "Applied Physics", 1, 0)
combined$astro <- ifelse(combined$dept == "Astronomy", 1, 0)
combined$biopub <- ifelse(combined$dept == "Bio Sciences in Public Health", 1, 0)
combined$bioeng <- ifelse(combined$dept == "Biomedical Engineering", 1, 0)
combined$biophy <- ifelse(combined$dept == "Biophysics", 1, 0)
combined$biostat <- ifelse(combined$dept == "Biostatistics", 1, 0)
combined$celtic <- ifelse(combined$dept == "Celtic Languages & Literatures", 1, 0)
combined$chemphysbio <- ifelse(combined$dept == "Chemical & Physical Biology", 1, 0)
combined$chembio <- ifelse(combined$dept == "Chemical Biology", 1, 0)
combined$chemchembio <- ifelse(combined$dept == "Chemistry & Chemical Biology", 1, 0)
combined$classics <- ifelse(combined$dept == "Classics, The", 1, 0)
combined$complit <- ifelse(combined$dept == "Comparative Literature", 1, 0)
combined$cs <- ifelse(combined$dept == "Computer Science", 1, 0)
combined$earth <- ifelse(combined$dept == "Earth & Planetary Sciences", 1, 0)
combined$eastasian <- ifelse(combined$dept == "East Asian Langs & Civ", 1, 0)
combined$ec <- ifelse(combined$dept == "Economics", 1, 0)
combined$engsci <- ifelse(combined$dept == "Engineering Sciences", 1, 0)
combined$eng <- ifelse(combined$dept == "English", 1, 0)
combined$epp <- ifelse(combined$dept == "Envi Science & Public Policy", 1, 0)
combined$ese <- ifelse(combined$dept == "Environmental Sci & Engineer", 1, 0)
combined$emr <- ifelse(combined$dept == "Ethnicity, Migration, & Rights", 1, 0)
combined$expos <- ifelse(combined$dept == "Expository Writing", 1, 0)
combined$fm <- ifelse(combined$dept == "Folklore & Mythology", 1, 0)
combined$fresh <- ifelse(combined$dept == "Freshman Seminars", 1, 0)
combined$gened <- ifelse(combined$dept == "General Education", 1, 0)
combined$german <- ifelse(combined$dept == "Germanic Languages & Lit", 1, 0)
combined$ghhp <- ifelse(combined$dept == "Global Health & Health Policy", 1, 0)
combined$gov <- ifelse(combined$dept == "Government", 1, 0)
combined$hp <- ifelse(combined$dept == "Health Policy", 1, 0)
combined$hist <- ifelse(combined$dept == "History", 1, 0)
combined$histlit <- ifelse(combined$dept == "History & Literature", 1, 0)
combined$haa <- ifelse(combined$dept == "History of Art & Architecture", 1, 0)
combined$histsci <- ifelse(combined$dept == "History of Science", 1, 0)
combined$heb <- ifelse(combined$dept == "Human Evolutionary Biology", 1, 0)
combined$humanities <- ifelse(combined$dept == "Humanities", 1, 0)
combined$ling <- ifelse(combined$dept == "Linguistics", 1, 0)
combined$math <- ifelse(combined$dept == "Mathematics", 1, 0)
combined$medic <- ifelse(combined$dept == "Medical Sciences", 1, 0)
combined$medieval <- ifelse(combined$dept == "Medieval Studies", 1, 0)
combined$me <- ifelse(combined$dept == "Middle Eastern Studies", 1, 0)
combined$mbb <- ifelse(combined$dept == "Mind Brain & Behavior", 1, 0)
combined$mcb <- ifelse(combined$dept == "Molecular & Cellular Biology", 1, 0)
combined$music <- ifelse(combined$dept == "Music", 1, 0)
combined$neareast <- ifelse(combined$dept == "Near Eastern Languages & Civ", 1, 0)
combined$neuro <- ifelse(combined$dept == "Neuroscience", 1, 0)
combined$oeb <- ifelse(combined$dept == "Organismic & Evolutionary Bio", 1, 0)
combined$phil <- ifelse(combined$dept == "Philosophy", 1, 0)
combined$phys <- ifelse(combined$dept == "Physics", 1, 0)
combined$pop <- ifelse(combined$dept == "Population Health Sciences", 1, 0)
combined$psych <- ifelse(combined$dept == "Psychology", 1, 0)
combined$religion <- ifelse(combined$dept == "Religion, The Study of", 1, 0)
combined$rll <- ifelse(combined$dept == "Romance Languages & Lit", 1, 0)
combined$russia <- ifelse(combined$dept == "Russia, E Europe, Central Asia", 1, 0)
combined$slavic <- ifelse(combined$dept == "Slavic Languages & Literatures", 1, 0)
combined$soc <- ifelse(combined$dept == "Social Studies", 1, 0)
combined$southasia <- ifelse(combined$dept == "South Asian Studies", 1, 0)
combined$stat <- ifelse(combined$dept == "Statistics", 1, 0)
combined$scrb <- ifelse(combined$dept == "Stem Cell & Regenerative Biol", 1, 0)
combined$sysbio <- ifelse(combined$dept == "Systems Biology", 1, 0)
combined$tdm <- ifelse(combined$dept == "Theater, Dance, & Media", 1, 0)
combined$ves <- ifelse(combined$dept == "Visual & Environmental Studies", 1, 0)
combined$wgs <- ifelse(combined$dept == "Women, Gender & Sexuality", 1, 0)

## Export the dataset:
write.csv(combined, "clean.csv")

## Data Analysis:

## CORRELATION BW WORKLOAD AND % CHANGE IN ENROLLMENT
## Regression 1: Bare bones regression
enroll_work <- lm(combined$enroll_chg_pct ~ combined$workload)
summary(enroll_work)
## Regression 2: Interaction and indicator variables for most common reason taken
enroll_work_reason <- lm(combined$enroll_chg_pct ~ combined$workload + 
                           combined$elective + combined$expos + combined$gened + combined$lang + combined$premed + 
                           combined$elective*combined$workload + combined$expos*combined$workload + combined$gened*combined$workload + combined$lang*combined$workload + combined$premed*combined$workload
                         )
summary(enroll_work_reason)
## Regression 3: Interaction and indicator variables for department
enroll_work_dept <- lm(combined$enroll_chg_pct ~ combined$workload +
                         combined$afam + combined$anthro + combined$ac + combined$am + combined$ap + combined$astro + combined$biopub + combined$bioeng + combined$biophy + combined$biostat + combined$celtic + combined$chemphysbio + combined$chembio + combined$chemchembio + combined$classics + combined$complit + combined$cs + combined$earth + combined$eastasian + combined$ec + combined$engsci + combined$eng + combined$epp + combined$ese + combined$emr + combined$fm + combined$fresh + combined$german + combined$ghhp + combined$gov + combined$hp + combined$hist + combined$histlit + combined$haa + combined$heb + combined$humanities + combined$ling + combined$math + combined$medic + combined$medieval + combined$me + combined$mbb + combined$mcb + combined$music + combined$neareast + combined$neuro + combined$oeb + combined$phil + combined$phys + combined$pop + combined$psych + combined$religion + combined$rll + combined$russia + combined$slavic + combined$soc + combined$southasia + combined$southasia + combined$stat + combined$scrb + combined$sysbio + combined$tdm + combined$ves + combined$wgs +
                         combined$workload*combined$afam + combined$workload*combined$anthro + combined$workload*combined$ac + combined$workload*combined$am + combined$workload*combined$ap + combined$workload*combined$astro + combined$workload*combined$biopub + combined$workload*combined$bioeng + combined$workload*combined$biophy + combined$workload*combined$biostat + combined$workload*combined$celtic + combined$workload*combined$chemphysbio + combined$workload*combined$chembio + combined$workload*combined$chemchembio + combined$workload*combined$classics + combined$workload*combined$complit + combined$workload*combined$cs + combined$workload*combined$earth + combined$workload*combined$eastasian + combined$workload*combined$ec + combined$workload*combined$engsci + combined$workload*combined$eng + combined$workload*combined$epp + combined$workload*combined$ese + combined$workload*combined$emr + combined$workload*combined$fm + combined$workload*combined$fresh + combined$workload*combined$german + combined$workload*combined$ghhp + combined$workload*combined$gov + combined$workload*combined$hp + combined$workload*combined$hist + combined$workload*combined$histlit + combined$workload*combined$haa + combined$workload*combined$heb + combined$workload*combined$humanities + combined$workload*combined$ling + combined$workload*combined$math + combined$workload*combined$medic + combined$workload*combined$medieval + combined$workload*combined$me +combined$workload* combined$mbb + combined$workload*combined$mcb + combined$workload*combined$music + combined$workload*combined$neareast + combined$workload*combined$neuro + combined$workload*combined$oeb + combined$workload*combined$phil + combined$workload*combined$phys + combined$workload*combined$pop + combined$workload*combined$psych + combined$workload*combined$religion + combined$workload*combined$rll + combined$workload*combined$russia + combined$workload*combined$slavic + combined$workload*combined$soc + combined$workload*combined$southasia + combined$workload*combined$southasia + combined$workload*combined$stat + combined$workload*combined$scrb + combined$workload*combined$sysbio + combined$workload*combined$tdm + combined$workload*combined$ves + combined$workload*combined$wgs
                      )
summary(enroll_work_dept)

## CORRELATION BW RATING AND % CHANGE IN ENROLLMENT
## Regression 1: Bare bones regression
enroll_rating <- lm(combined$enroll_chg_pct~combined$rating)
summary(enroll_rating)
## Regression 2: Interaction and indicator variables for most common reason taken
enroll_rating_reason <- lm(combined$enroll_chg_pct ~ combined$rating + 
                             combined$elective + combined$expos + combined$gened + combined$lang + combined$premed + 
                             combined$elective*combined$rating + combined$expos*combined$rating + combined$gened*combined$rating + combined$lang*combined$rating + combined$premed*combined$rating
                           )
summary(enroll_rating_reason)
## Regression 3: Interaction and indicator variables for department
enroll_rating_dept <- lm(combined$enroll_chg_pct ~ combined$rating + 
                           combined$afam + combined$anthro + combined$ac + combined$am + combined$ap + combined$astro + combined$biopub + combined$bioeng + combined$biophy + combined$biostat + combined$celtic + combined$chemphysbio + combined$chembio + combined$chemchembio + combined$classics + combined$complit + combined$cs + combined$earth + combined$eastasian + combined$ec + combined$engsci + combined$eng + combined$epp + combined$ese + combined$emr + combined$fm + combined$fresh + combined$german + combined$ghhp + combined$gov + combined$hp + combined$hist + combined$histlit + combined$haa + combined$heb + combined$humanities + combined$ling + combined$math + combined$medic + combined$medieval + combined$me + combined$mbb + combined$mcb + combined$music + combined$neareast + combined$neuro + combined$oeb + combined$phil + combined$phys + combined$pop + combined$psych + combined$religion + combined$rll + combined$russia + combined$slavic + combined$soc + combined$southasia + combined$southasia + combined$stat + combined$scrb + combined$sysbio + combined$tdm + combined$ves + combined$wgs + 
                           combined$rating*combined$afam + combined$rating*combined$anthro + combined$rating*combined$ac + combined$rating*combined$am + combined$rating*combined$ap + combined$rating*combined$astro + combined$rating*combined$biopub + combined$rating*combined$bioeng + combined$rating*combined$biophy + combined$rating*combined$biostat + combined$rating*combined$celtic + combined$rating*combined$chemphysbio + combined$rating*combined$chembio + combined$rating*combined$chemchembio + combined$rating*combined$classics + combined$rating*combined$complit + combined$rating*combined$cs + combined$rating*combined$earth + combined$rating*combined$eastasian + combined$rating*combined$ec + combined$rating*combined$engsci + combined$rating*combined$eng + combined$rating*combined$epp + combined$rating*combined$ese + combined$rating*combined$emr + combined$rating*combined$fm + combined$rating*combined$fresh + combined$rating*combined$german + combined$rating*combined$ghhp + combined$rating*combined$gov + combined$rating*combined$hp + combined$rating*combined$hist + combined$rating*combined$histlit + combined$rating*combined$haa + combined$rating*combined$heb + combined$rating*combined$humanities + combined$rating*combined$ling + combined$rating*combined$math + combined$rating*combined$medic + combined$rating*combined$medieval + combined$rating*combined$me +combined$rating* combined$mbb + combined$rating*combined$mcb + combined$rating*combined$music + combined$rating*combined$neareast + combined$rating*combined$neuro + combined$rating*combined$oeb + combined$rating*combined$phil + combined$rating*combined$phys + combined$rating*combined$pop + combined$rating*combined$psych + combined$rating*combined$religion + combined$rating*combined$rll + combined$rating*combined$russia + combined$rating*combined$slavic + combined$rating*combined$soc + combined$rating*combined$southasia + combined$rating*combined$southasia + combined$rating*combined$stat + combined$rating*combined$scrb + combined$rating*combined$sysbio + combined$rating*combined$tdm + combined$rating*combined$ves + combined$rating*combined$wgs
                         )
summary(enroll_rating_dept)

## CORRELATION BETWEEN RATING AND WORKLOAD AND % CHANGE IN ENROLLMENT
## Regression 1: Bare bones regresion
enroll_rating_work <- lm(combined$enroll_chg_pct~combined$rating + combined$workload)
summary(enroll_rating_work)
## Regression 2: Interaction and indicator variables for reason taken
enroll_rating_work_reason <- lm(combined$enroll_chg_pct ~ combined$rating + combined$workload + 
                                  combined$elective + combined$expos + combined$gened + combined$lang + combined$premed + 
                                  combined$elective*combined$rating + combined$expos*combined$rating + combined$gened*combined$rating + combined$lang*combined$rating + combined$premed*combined$rating + 
                                  combined$elective*combined$workload + combined$expos*combined$workload + combined$gened*combined$workload + combined$lang*combined$workload + combined$premed*combined$workload
                                )
summary(enroll_rating_work_reason)
## Regression 3: Interaction and indicator variables for department
enroll_rating_work_dept <- lm(combined$enroll_chg_pct ~ combined$rating + combined$workload +
                                combined$afam + combined$anthro + combined$ac + combined$am + combined$ap + combined$astro + combined$biopub + combined$bioeng + combined$biophy + combined$biostat + combined$celtic + combined$chemphysbio + combined$chembio + combined$chemchembio + combined$classics + combined$complit + combined$cs + combined$earth + combined$eastasian + combined$ec + combined$engsci + combined$eng + combined$epp + combined$ese + combined$emr + combined$fm + combined$fresh + combined$german + combined$ghhp + combined$gov + combined$hp + combined$hist + combined$histlit + combined$haa + combined$heb + combined$humanities + combined$ling + combined$math + combined$medic + combined$medieval + combined$me + combined$mbb + combined$mcb + combined$music + combined$neareast + combined$neuro + combined$oeb + combined$phil + combined$phys + combined$pop + combined$psych + combined$religion + combined$rll + combined$russia + combined$slavic + combined$soc + combined$southasia + combined$southasia + combined$stat + combined$scrb + combined$sysbio + combined$tdm + combined$ves + combined$wgs +
                                combined$rating*combined$afam + combined$rating*combined$anthro + combined$rating*combined$ac + combined$rating*combined$am + combined$rating*combined$ap + combined$rating*combined$astro + combined$rating*combined$biopub + combined$rating*combined$bioeng + combined$rating*combined$biophy + combined$rating*combined$biostat + combined$rating*combined$celtic + combined$rating*combined$chemphysbio + combined$rating*combined$chembio + combined$rating*combined$chemchembio + combined$rating*combined$classics + combined$rating*combined$complit + combined$rating*combined$cs + combined$rating*combined$earth + combined$rating*combined$eastasian + combined$rating*combined$ec + combined$rating*combined$engsci + combined$rating*combined$eng + combined$rating*combined$epp + combined$rating*combined$ese + combined$rating*combined$emr + combined$rating*combined$fm + combined$rating*combined$fresh + combined$rating*combined$german + combined$rating*combined$ghhp + combined$rating*combined$gov + combined$rating*combined$hp + combined$rating*combined$hist + combined$rating*combined$histlit + combined$rating*combined$haa + combined$rating*combined$heb + combined$rating*combined$humanities + combined$rating*combined$ling + combined$rating*combined$math + combined$rating*combined$medic + combined$rating*combined$medieval + combined$rating*combined$me +combined$rating* combined$mbb + combined$rating*combined$mcb + combined$rating*combined$music + combined$rating*combined$neareast + combined$rating*combined$neuro + combined$rating*combined$oeb + combined$rating*combined$phil + combined$rating*combined$phys + combined$rating*combined$pop + combined$rating*combined$psych + combined$rating*combined$religion + combined$rating*combined$rll + combined$rating*combined$russia + combined$rating*combined$slavic + combined$rating*combined$soc + combined$rating*combined$southasia + combined$rating*combined$southasia + combined$rating*combined$stat + combined$rating*combined$scrb + combined$rating*combined$sysbio + combined$rating*combined$tdm + combined$rating*combined$ves + combined$rating*combined$wgs +
                                combined$workload*combined$afam + combined$workload*combined$anthro + combined$workload*combined$ac + combined$workload*combined$am + combined$workload*combined$ap + combined$workload*combined$astro + combined$workload*combined$biopub + combined$workload*combined$bioeng + combined$workload*combined$biophy + combined$workload*combined$biostat + combined$workload*combined$celtic + combined$workload*combined$chemphysbio + combined$workload*combined$chembio + combined$workload*combined$chemchembio + combined$workload*combined$classics + combined$workload*combined$complit + combined$workload*combined$cs + combined$workload*combined$earth + combined$workload*combined$eastasian + combined$workload*combined$ec + combined$workload*combined$engsci + combined$workload*combined$eng + combined$workload*combined$epp + combined$workload*combined$ese + combined$workload*combined$emr + combined$workload*combined$fm + combined$workload*combined$fresh + combined$workload*combined$german + combined$workload*combined$ghhp + combined$workload*combined$gov + combined$workload*combined$hp + combined$workload*combined$hist + combined$workload*combined$histlit + combined$workload*combined$haa + combined$workload*combined$heb + combined$workload*combined$humanities + combined$workload*combined$ling + combined$workload*combined$math + combined$workload*combined$medic + combined$workload*combined$medieval + combined$workload*combined$me +combined$workload* combined$mbb + combined$workload*combined$mcb + combined$workload*combined$music + combined$workload*combined$neareast + combined$workload*combined$neuro + combined$workload*combined$oeb + combined$workload*combined$phil + combined$workload*combined$phys + combined$workload*combined$pop + combined$workload*combined$psych + combined$workload*combined$religion + combined$workload*combined$rll + combined$workload*combined$russia + combined$workload*combined$slavic + combined$workload*combined$soc + combined$workload*combined$southasia + combined$workload*combined$southasia + combined$workload*combined$stat + combined$workload*combined$scrb + combined$workload*combined$sysbio + combined$workload*combined$tdm + combined$workload*combined$ves + combined$workload*combined$wgs
                              )
summary(enroll_rating_work_dept)

## CORRELATION BETWEEN WORKLOAD AND RATING
## Regression 1: Bare bones regressions
rating_workload <- lm(combined$rating ~ combined$workload)
summary(rating_workload)
## Regression 2: Interaction and indicator variables for reason taken
rating_workload_reason <- lm(combined$rating ~ combined$workload +
                               combined$elective + combined$expos + combined$gened + combined$lang + combined$premed + 
                               combined$elective*combined$workload + combined$expos*combined$workload + combined$gened*combined$workload + combined$lang*combined$workload + combined$premed*combined$workload
                             )
summary(rating_workload_reason)
## Regression 3: Interaction and indicator variables for department
rating_workload_dept <- lm(combined$rating ~ combined$workload +
                             combined$afam + combined$anthro + combined$ac + combined$am + combined$ap + combined$astro + combined$biopub + combined$bioeng + combined$biophy + combined$biostat + combined$celtic + combined$chemphysbio + combined$chembio + combined$chemchembio + combined$classics + combined$complit + combined$cs + combined$earth + combined$eastasian + combined$ec + combined$engsci + combined$eng + combined$epp + combined$ese + combined$emr + combined$fm + combined$fresh + combined$german + combined$ghhp + combined$gov + combined$hp + combined$hist + combined$histlit + combined$haa + combined$heb + combined$humanities + combined$ling + combined$math + combined$medic + combined$medieval + combined$me + combined$mbb + combined$mcb + combined$music + combined$neareast + combined$neuro + combined$oeb + combined$phil + combined$phys + combined$pop + combined$psych + combined$religion + combined$rll + combined$russia + combined$slavic + combined$soc + combined$southasia + combined$southasia + combined$stat + combined$scrb + combined$sysbio + combined$tdm + combined$ves + combined$wgs +
                             combined$workload*combined$afam + combined$workload*combined$anthro + combined$workload*combined$ac + combined$workload*combined$am + combined$workload*combined$ap + combined$workload*combined$astro + combined$workload*combined$biopub + combined$workload*combined$bioeng + combined$workload*combined$biophy + combined$workload*combined$biostat + combined$workload*combined$celtic + combined$workload*combined$chemphysbio + combined$workload*combined$chembio + combined$workload*combined$chemchembio + combined$workload*combined$classics + combined$workload*combined$complit + combined$workload*combined$cs + combined$workload*combined$earth + combined$workload*combined$eastasian + combined$workload*combined$ec + combined$workload*combined$engsci + combined$workload*combined$eng + combined$workload*combined$epp + combined$workload*combined$ese + combined$workload*combined$emr + combined$workload*combined$fm + combined$workload*combined$fresh + combined$workload*combined$german + combined$workload*combined$ghhp + combined$workload*combined$gov + combined$workload*combined$hp + combined$workload*combined$hist + combined$workload*combined$histlit + combined$workload*combined$haa + combined$workload*combined$heb + combined$workload*combined$humanities + combined$workload*combined$ling + combined$workload*combined$math + combined$workload*combined$medic + combined$workload*combined$medieval + combined$workload*combined$me +combined$workload* combined$mbb + combined$workload*combined$mcb + combined$workload*combined$music + combined$workload*combined$neareast + combined$workload*combined$neuro + combined$workload*combined$oeb + combined$workload*combined$phil + combined$workload*combined$phys + combined$workload*combined$pop + combined$workload*combined$psych + combined$workload*combined$religion + combined$workload*combined$rll + combined$workload*combined$russia + combined$workload*combined$slavic + combined$workload*combined$soc + combined$workload*combined$southasia + combined$workload*combined$southasia + combined$workload*combined$stat + combined$workload*combined$scrb + combined$workload*combined$sysbio + combined$workload*combined$tdm + combined$workload*combined$ves + combined$workload*combined$wgs
                           )
summary(rating_workload_dept)
## Close and save log file

sink()