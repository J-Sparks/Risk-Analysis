### Data shaping for applicants to APR 2017 to 2020 
# prediction for risk APR 2021

# applied FTIC/ removed DEC_CODE=NA
FTIC_ID20172021 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/Jay's Space/2021 Active Projects/08/ACCEPTANCE RATE/all_applicants20172021/FTIC_ID.csv")
ADMIT =c("UD","UI","UR")
STUD  =c("B","E") #FTIC
DEC =c("C")
col_names <- c("UWFID","OFFER_CODE","STUDTYP","ADMIT_TYPE", "DEC_CODE", "APP_TERM","APP_NO","APP_DT",
               "CURRICULUM_COLL","CURRICULUM_DEPT","HS_FL_YRS","ENROLLED_AP_TERM","MY_UWF_STATUS","HS_FOREIGN_LANG",
               "PROGRAM_DESC","PROGRAM_CODE",  "HS_OFFER_GPA","TR_GPA","TIER_SCORE","TIER_RANK",
               "PELL_ELGBL","PELL_VERF","GENDER","RACE")

library(readr)
app_2017 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/DATA 202108/Application_Admit Data 2017-2022/Banner Application Load 9.1.2017.csv") %>% 
    select(col_names, contains("HS"),contains("SAT"),contains("ACT"), contains("NAME"),
           -HS_RANK,-HS_FINAL_GPA,-HS_NUMBER_IN_CLASS, -HS_PERCENTILE,-HS_RECALC_GPA) %>%  
    #filter(!is.na(DEC_CODE)) %>% 
    filter(ADMIT_TYPE %in% ADMIT) %>% 
    filter( STUDTYP %in% STUD) %>% #5481
    mutate(APP_YEAR=rep("2017"))

app_2018 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/DATA 202108/Application_Admit Data 2017-2022/Banner Application Load 9.1.2018.csv") %>% 
    select(col_names, contains("HS"),contains("SAT"),contains("ACT"),contains("NAME"),
           -HS_NUMBER_IN_CLASS, -HS_PERCENTILE,-HS_RECALC_GPA,-HS_RANK,-HS_FINAL_GPA) %>%  
    #filter(!is.na(DEC_CODE)) %>% 
    filter(ADMIT_TYPE %in% ADMIT) %>% 
    filter( STUDTYP %in% STUD) %>% #5523
    mutate(APP_YEAR=rep("2018"))
addmargins(table(app_2018$STUDTYP, app_2018$TIER_RANK ))

app_2019 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/DATA 202108/Application_Admit Data 2017-2022/Banner Application Load 9.01.2019.csv") %>% 
    select(col_names, contains("HS"),contains("SAT"),contains("ACT"),contains("NAME"),
           -SAT15_RE_SUB, -SAT15_WL_SUB,-SAT15_MA_SUB ) %>%  
    #filter(!is.na(DEC_CODE)) %>% 
    filter(ADMIT_TYPE %in% ADMIT) %>% 
    filter( STUDTYP %in% STUD) %>%  #7976
    mutate(APP_YEAR=rep("2019"))
colSums(is.na(app_2019))
addmargins(table(app_2019$STUDTYP, app_2019$TIER_RANK ))

app_2020 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/DATA 202108/Application_Admit Data 2017-2022/Banner Application Load 9.01.2020.csv") %>% 
    select(col_names, contains("HS"),contains("SAT"),contains("ACT"),contains("NAME"),
           -SAT15_RE_SUB, -SAT15_WL_SUB,-SAT15_MA_SUB) %>%  
    #filter(!is.na(DEC_CODE)) %>% 
    filter(ADMIT_TYPE %in% ADMIT) %>% 
    filter( STUDTYP %in% STUD) %>% #8563
    mutate(APP_YEAR=rep("2020"))

addmargins(table(app_2020$STUDTYP, app_2020$TIER_RANK ))

app_2021 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/DATA 202108/Application_Admit Data 2017-2022/Banner Application Load 9.1.2021.csv") %>%
    select(col_names, contains("HS"),contains("SAT"),contains("ACT"),contains("NAME"),-HS_CORE_GPA,
           -SAT15_RE_SUB, -SAT15_WL_SUB,-SAT15_MA_SUB) %>%  
    #filter(!is.na(DEC_CODE)) %>% 
    filter(ADMIT_TYPE %in% ADMIT) %>% 
    filter( STUDTYP %in% STUD) %>% #9475
    mutate(APP_YEAR=rep("2021"))
names(app_2021)[names(app_2021) == "ACT_EN_20"] <- "ACT_EN"
names(app_2021)[names(app_2021) == "ACT_MA_20"] <- "ACT_MA"
names(app_2021)[names(app_2021) == "ACT_RE_20"] <- "ACT_RE"
names(app_2021)[names(app_2021) == "ACT_SR_20"] <- "ACT_SR"
names(app_2021)[names(app_2021) == "ACT_CM_20"] <- "ACT_CM"



addmargins(table(app_2021$STUDTYP, app_2021$TIER_RANK ))
colnames(app_2021)
app_1718 <- rbind(app_2017, app_2018)
app_1920 <- rbind(app_2019, app_2020)
colnames(app_1718)=colnames(app_1920)
app_1720 <- rbind(app_1718, app_1920)
app_1721 <- rbind(app_1720, app_2021) #37018
addmargins(table(app_1721$APP_YEAR, app_1721$APP_TERM))
colSums(is.na(app_1721))
write.csv(app_1721, "applicants_2017to2021V1.csv", row.names = F)
 

 
library(readxl)
EABA_2810_2021_FTIC_Admitted_Students_Tier_and_Offer_GPA_Data <- read_excel("G:/Shared drives/HMCSE-PAM Lab/DATA 202108/Application_Admit Data 2017-2022/EABA-2810 - 2021 FTIC Admitted Students Tier and Offer GPA - Data.xlsx") #4725
dim(EABA_2810_2021_FTIC_Admitted_Students_Tier_and_Offer_GPA_Data)
 
library(readr)
applicants_2017to2021V1 <- read_csv("applicants_2017to2021V1.csv")

applicants_2017to2021_eaba_gpa <- merge(applicants_2017to2021V1, EABA_2810_2021_FTIC_Admitted_Students_Tier_and_Offer_GPA_Data, by="UWFID", all.x = T)
FTIC_ID20172021 <- read_csv("G:/Shared drives/HMCSE-PAM Lab/Jay's Space/2021 Active Projects/08/ACCEPTANCE RATE/all_applicants20172021/FTIC_ID.csv")
addmargins(table(FTIC_ID20172021$Cohort))

all_app_1 <- merge(applicants_2017to2021_eaba_gpa, FTIC_ID20172021, 
                   by.x="UWFID",by.y="UNIV_ROW_ID", all.x = T) %>% 
    group_by(UWFID) %>% unique() %>% 
    mutate(REP_ID=row_number())
addmargins(table(all_app_1$OFFER_CODE, all_app_1$APP_YEAR))
addmargins(table(all_app_1$Cohort, all_app_1$APPLICANT_TIER))

write.csv(all_app_1, "applicants_2017to2021_COM_HSGPA2021V0.csv", row.names = F) #include reject before offer
 