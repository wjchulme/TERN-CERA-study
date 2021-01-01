
source(here::here("R","0 preliminaries.R"))
library("tidyverse")
library('lubridate')


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# import raw data files ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# import raw redcap export
# DATA contains short variables names and coded values
# DATA_LABELS contains long (and duplicate) variable names and uncoded values
# can't export short variable names and uncoded values ¯\_(ツ)_/¯


## contains UK surveys 1 2 and 3

raw_dataUK <- readr::read_csv(here::here("data","UK", "COVID19EmergencyResp_DATA_2020-06-30_2117.csv"))

raw_dataUK_labels <- readr::read_csv(here::here("data", "UK", "COVID19EmergencyResp_DATA_LABELS_2020-06-30_2117.csv"))

#check column number and order matches
stopifnot(nrow(raw_dataUK) == nrow(raw_dataUK_labels))
cbind(names(raw_dataUK), substr(names(raw_dataUK_labels), 1, 30))


## contains Irish surveys 1 and 3

raw_dataRI <- readr::read_csv(here::here("data","Ireland","COVID19EmergencyResp_DATA_2020-06-30_2158.csv"))

raw_dataRI_labels <- readr::read_csv(here::here("data","Ireland","COVID19EmergencyResp_DATA_LABELS_2020-06-30_2158.csv"))

#check column number and order matches
stopifnot(nrow(raw_dataRI) == nrow(raw_dataRI_labels))
cbind(names(raw_dataRI), substr(names(raw_dataRI_labels), 1, 30))


#contains Irish survey 2

raw_dataRI2 <- readr::read_csv(here::here("data","Ireland","COVID19EmergencyResp_DATA_2020-06-30_2116 survey2.csv"))
raw_dataRI2_labels <- readr::read_csv(here::here("data","Ireland","COVID19EmergencyResp_DATA_LABELS_2020-06-30_2116 survey2.csv"))

#check column number and order matches
stopifnot(nrow(raw_dataRI2) == nrow(raw_dataRI2_labels))
cbind(names(raw_dataRI2), substr(names(raw_dataRI2_labels), 1, 30))





# import column name lookup table
collookupUK <- readr::read_csv(here::here("dictionaries", "UK", "colname lookup 2020-09-10.csv"))
raw_dictionaryUK <- readr::read_csv(here::here("dictionaries", "UK", "COVID19EmergencyResponseAssess_DataDictionary_2020-06-30.csv"))

collookupRI <- readr::read_csv(here::here("dictionaries", "Ireland", "colname lookup 2020-06-30.csv"))
collookupRI2 <- readr::read_csv(here::here("dictionaries", "Ireland", "colname lookup s2 2020-06-30.csv"))
#raw_dictionaryRI <- readr::read_csv(here::here("dictionaries", "RI", "COVID19EmergencyResponseAssess_DataDictionary_2020-05-04.csv"))



# rename columns to R-friendly names, as per look-up
dataUK0 <- raw_dataUK_labels %>% set_names(collookupUK$R[match(colnames(raw_dataUK), collookupUK$redcap)]) %>% mutate(country = "UK")
dataRI0_13 <- raw_dataRI_labels %>% set_names(collookupRI$R[match(colnames(raw_dataRI), collookupRI$redcap)]) %>% mutate(country = "Ireland") %>% select(-ends_with("_s2"))
dataRI0_2 <- raw_dataRI2_labels %>% set_names(collookupRI2$R[match(colnames(raw_dataRI2), collookupRI2$redcap)]) %>% mutate(country = "Ireland") %>% select(ptid, ends_with("_s2"), -email_s2, -resend_s2)

dataRI0 <- full_join(dataRI0_13, dataRI0_2, by="ptid")

# are there any column names in UK (RoI) that aren't in RoI (UK) ?
names(dataRI0)[names(dataRI0) %ni% names(dataUK0)]
names(dataUK0)[names(dataUK0) %ni% names(dataRI0)]
# should just be specialty, specialty_fy and department
# specialty and department were asked "properly" in irish data collection as checkboxes (allowing multiple answers), 
# not as single answer qs as in first few responses in survey 1 before being changed
# specialty_fy was not an option in ireland

# combine all data
data0 <- bind_rows(dataUK0, dataRI0) %>%
  mutate(
    ptid=if_else(country=="Ireland", ptid+2000000, ptid+1000000)
  ) 



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# import data dictionaries ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# wrangle redcap dictionary to give factor codes and levels
#using uk dictionary and then adding Irish grades to grade variable
# adding country variable for UK / RI
dictionary0 <- raw_dictionaryUK %>%
  mutate(
    colname = collookupUK$R[match(`Variable / Field Name`, collookupUK$redcap)]
  ) %>%
  select(colname, redcapchrlevels=`Choices, Calculations, OR Slider Labels`) %>%
  mutate(
    #redcapchrlevels=if_else(colname=='hosp', NA_character_, redcapchrlevels),
    code=map(redcapchrlevels, ~splitlevs(., take="code")),
    level=map(redcapchrlevels, ~splitlevs(., take="level"))
  )

# yes/no questions
yn_fct <- c(
  "selfisolate_s1", "selfisolate_s2", "selfisolate_s3",
  "redeployed_s1", "redeployed_s2",
  "feelings_contribution_s2", "feelings_accomplish_s2",   "feelings_confidence_s2",
  "feelings_compassion_s2",   "feelings_purpose_s2", "feelings_satisfaction_s2", "feelings_cohesion_s2",
  "feelings_contribution_s3", "feelings_accomplish_s3",  "feelings_confidence_s3", 
  "feelings_compassion_s3", "feelings_purpose_s3", "feelings_satisfaction_s3", "feelings_cohesion_s3",
  NULL
)

# symptom questions
symp_fct <- c(
  "increasesympmedical_s1", "increasesympmedical_s2", "increasesympmedical_s1", 
  "increasesympmental_s1", "increasesympmental_s2", "increasesympmental_s3",
  "covidadmit_s2", "covidadmit_s3"
)


# data dictionary containing:
# colname in R
# colname in redcap
# short name
# full name
# type of plot to produce
# variable type
# redcap code-level pair as long string
# redcap code
# redcap level
dictionary <- left_join(collookupUK, dictionary0, by=c("R"="colname")) %>%
  mutate(
    level = map(level, ~{if(is.null(.)) NA_character_ else .}),
    level = map2(R, level, ~{if(.x %in% yn_fct) c("Yes", "No") else .y}),
    level = map2(R, level, ~{if(.x=="grade") # standarise grades
      unique(c(.y, c("Intern", "Senior House Officer (Training scheme)", "Senior House Officer (Standalone)", 
                     "Registrar (Training scheme)", "Registrar (Standalone)", "Specialist Registrar", "Senior Registrar",
                     "Associate Specialist", "Consultant",
                     "Other")))
      else 
        .y
    }),
    code = map2(R, code, ~{if (.x == "grade") NA else .y})
  ) %>% 
  add_row( #add row for country variable created earlier
    R = "country",
    name =" Country",
    code= NA,
    redcap = NA,
    plottype= "bar",
    full = "Country",
    level = NA,
  )



# create factor dictionary and add extra factors as needed
# this is extended with derived variables later on
# create initial dictionary of factor variables only
# this is extended with derived variables later on
factor_dct0 <- dictionary %>%
  filter(!is.na(level)) %>%
  mutate(
    short = NA,
    short = ifelse(R %in% "specialty", list(c("ed", "anaes", "icu", "paeds", "gp", "surg", "fy", "acute", "other")), short),
    short = ifelse(R == "dept", list(c("ed", "anaes", "icu", "amu", "ward", "other")), short),
    full = ifelse(R == "supportcol_s3", "In the last 2 weeks I have felt well supported by the senior clinical leadership team", full),
  )




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# import hospital and grade corrections ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# need to make Irish hospital code list manually since no dictionary

hosp_RI <- 
  tibble(
    hosp_code = raw_dataRI$hosp+2000, 
    hosp_level = raw_dataRI_labels$`What is the name of the hospital where you currently work?  Please type and your hospital should appear, if not present select 'other'`
  ) %>%
  distinct() %>%
  arrange(hosp_code) %>%
  filter(!is.na(hosp_code), hosp_level!="Other")


# recode hospitals to eg merge trusts and decode private hospitals
dct_hosp <- dictionary %>% 
  filter(dictionary$R=="hosp") %>%
  {tibble(hosp_code=.$code[[1]], hosp_level=.$level[[1]])} %>%
  bind_rows(hosp_RI) %>%
  add_row(hosp_code=414, hosp_level="Ashford and St Peter's Hospital") %>%
  add_row(hosp_code=415, hosp_level="Singleton hospital") %>%
  add_row(hosp_code=416, hosp_level="Victoria Hospital, kirkcaldy") %>%
  add_row(hosp_code=2051, hosp_level="Coombe") %>%
  add_row(hosp_code=2052, hosp_level="Midlands Regional Hospital Mullingar") %>%
  add_row(hosp_code=2054, hosp_level="National Maternity Hospital") %>%
  add_row(hosp_code=2056, hosp_level="Bantry General Hospital") %>%
  add_row(hosp_code=2057, hosp_level="Royal Victoria Eye and Ear Hospital") %>%
  add_row(hosp_code=2058, hosp_level="South Infirmary Victoria University Hospital") %>%
  add_row(hosp_code=2060, hosp_level="Urgent care centre connolly") %>%
  filter(!is.na(hosp_code)) %>%
  left_join((readxl::read_xlsx(here::here("dictionaries", "remapping", "remapped hospitals.xlsx"))), by = "hosp_code") %>%
  select(-hosp) %>%
  filter(!is.na(new_hosp_code))


# import recodes for free text hospitals
corrections_ft_hosp <- read_csv(here::here("dictionaries", "remapping", "corrected ft hospitals.csv")) %>%
  left_join(dct_hosp %>% select(hosp_code, hosp_level), by="hosp_code")

# import recodes for free text grades
corrections_ft_grade <- read_csv(here::here("dictionaries", "remapping", "corrected ft grades.csv")) %>%
  mutate(
    new_grade = str_extract(new_grade, "[a-zA-Z](.*)"), #extract everything from first letter onwards
  )

# check no duplicates
stopifnot(
  nrow(corrections_ft_hosp %>% janitor::get_dupes(ft_hosp))==0
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# wrangling ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# check number of rows in dictionary matches number of variables in data0
stopifnot(nrow(dictionary)==ncol(data0))
# dictionary$R[dictionary$R %ni% names(data0)]
# names(data0)[names(data0) %ni% dictionary$R]

stopifnot(nrow(dct_hosp %>% janitor::get_dupes(hosp_level))==0)

# redo hospital levels to remove duplicates and add region
corrections_hosp <- data0 %>%
  select(ptid, hosp, ft_hosp) %>%
  left_join(dct_hosp %>% select(hosp_level, hosp_code), by=c("hosp"="hosp_level")) %>%
  left_join(corrections_ft_hosp %>% rename(ft_hosp_code=hosp_code), by="ft_hosp") %>%
  transmute(
    ptid,
    new_hosp_code = case_when(
      !is.na(ft_hosp) ~ as.integer(ft_hosp_code),
      !is.na(hosp) ~ as.integer(hosp_code),
      TRUE ~ NA_integer_
    )
  ) %>%
  left_join(dct_hosp %>% select(hosp_code, hosp_level, region), by=c("new_hosp_code"="hosp_code")) %>%
  mutate(
    region = factor(
      region, 
      levels=c(
        "East Midlands", "East of England", "London", "North East", "North West",
        "South East", "South West", "West Midlands", "Yorkshire and the Humber",
        "Northern Ireland", "Scotland", "Wales", "Dublin", "Rest of Ireland"
      )
    ),
    nation = case_when(
      region %in% c("Dublin", "Rest of Ireland") ~ "Republic of Ireland",
      region == "Wales" ~ "Wales",
      region == "Scotland" ~ "Scotland",
      region %in% c("East Midlands", "East of England", "London", "North East", 
                    "North West", "South East", "South West", "West Midlands", 
                    "Yorkshire and the Humber") ~ "England",
      region %in% c("Northern Ireland") ~ "Northern Ireland"
    ),
  )



# convert free text grades to correct levels
# convert free text and duplicate hospitals to correct levels
# convert character columns to factors as appropriate
# convert "Checked"/"Unchecked" into TRUE/FALSE
# convert timestamps to datetime format
# clean up hospital duplicates
# covert "prefer not to say" gender to missing

data1 <- data0 %>% 
  left_join(corrections_ft_grade, by="ft_grade") %>% 
  mutate(grade = if_else(!is.na(ft_grade), new_grade, grade)) %>% 
  select(-new_grade) %>%
  left_join(corrections_hosp %>% select(ptid, hosp_level), by="ptid") %>% 
  mutate(hosp = hosp_level) %>% select(-hosp_level) %>%
  imap_dfc(
    function(col, name){
      level <- dictionary$level[dictionary$R == name][[1]]
      if(name=="hosp"){col}
      else if(length(level)>1){factor(x=col, levels=level)}
      else if(all(col %in% c("Checked", "Unchecked", NA))) {col=="Checked"}
      else  {col}
    }
  ) %>%
  mutate(
    timestamp_s1 = as_datetime(timestamp_s1),
    timestamp_s2 = as_datetime(timestamp_s2),
    timestamp_s3 = as_datetime(timestamp_s3),
    gender = factor(gender, levels=levels(gender)[-4]),
  ) 

## error-checking ####
data_checks <- data1 %>%
  transmute(
    ptid,
    
    # create indicators for "no response" for multiresponse questions
    # and indicators for if responses to multiresponse questions are inconsistent
    
    specialty_NA_s1 = (specialty_acute+specialty_anaes+specialty_ed+specialty_icu+specialty_paeds+specialty_gp+specialty_surg+specialty_fy+specialty_other==0),
    outbreak_NA_s1 = (outbreak_none+outbreak_ebola+outbreak_mers+outbreak_sars+outbreak_chikungunya+outbreak_cholera+outbreak_flu+outbreak_zika+outbreak_other==0),
    ppe.dondof_NA_s1 = (ppe.dondof_none_s1+ppe.dondof_vid_s1+ppe.dondof_written_s1+ppe.dondof_sim_s1+ppe.dondof_dept_s1+ppe.dondof_other_s1==0),
    ppe.fit_NA_s1 = (ppe.fit_none_s1+ppe.fit_vid_s1+ppe.fit_written_s1+ppe.fit_sim_s1+ppe.fit_dept_s1+ppe.fit_other_s1==0),
    ppe.exp_NA_s1 = (ppe.exp_none_s1+ppe.exp_vid_s1+ppe.exp_written_s1+ppe.exp_sim_s1+ppe.exp_dept_s1+ppe.exp_other_s1==0),
    practicaled_NA_s1 = (practicaled_none_s1+practicaled_case_s1+practicaled_aerosol_s1+practicaled_other_s1==0),
    
    outbreak_incon = (outbreak_none & (outbreak_ebola+outbreak_mers+outbreak_sars+outbreak_chikungunya+outbreak_cholera+outbreak_flu+outbreak_zika+outbreak_other>0)),
    ppe.dondof_incon_s1 = (ppe.dondof_none_s1 & (ppe.dondof_vid_s1+ppe.dondof_written_s1+ppe.dondof_sim_s1+ppe.dondof_dept_s1+ppe.dondof_other_s1>0)),
    ppe.fit_incon_s1 = (ppe.fit_none_s1 & (ppe.fit_vid_s1+ppe.fit_written_s1+ppe.fit_sim_s1+ppe.fit_dept_s1+ppe.fit_other_s1>0)),
    ppe.exp_incon_s1 = (ppe.exp_none_s1 & (ppe.exp_vid_s1+ppe.exp_written_s1+ppe.exp_sim_s1+ppe.exp_dept_s1+ppe.exp_other_s1>0)),
    practicaled_incon_s1 = (practicaled_none_s1 & (practicaled_case_s1+practicaled_aerosol_s1+practicaled_other_s1>0)),
    
    specialty_NA_s2 = (specialty_acute+specialty_anaes+specialty_ed+specialty_icu+specialty_paeds+specialty_gp+specialty_surg+specialty_fy+specialty_other==0),
    outbreak_NA_s2 = (outbreak_none+outbreak_ebola+outbreak_mers+outbreak_sars+outbreak_chikungunya+outbreak_cholera+outbreak_flu+outbreak_zika+outbreak_other==0),
    ppe.dondof_NA_s2 = (ppe.dondof_none_s2+ppe.dondof_vid_s2+ppe.dondof_written_s2+ppe.dondof_sim_s2+ppe.dondof_dept_s2+ppe.dondof_other_s2==0),
    ppe.fit_NA_s2 = (ppe.fit_none_s2+ppe.fit_vid_s2+ppe.fit_written_s2+ppe.fit_sim_s2+ppe.fit_dept_s2+ppe.fit_other_s2==0),
    ppe.exp_NA_s2 = (ppe.exp_none_s2+ppe.exp_vid_s2+ppe.exp_written_s2+ppe.exp_sim_s2+ppe.exp_dept_s2+ppe.exp_other_s2==0),
    practicaled_NA_s2 = (practicaled_none_s2+practicaled_case_s2+practicaled_aerosol_s2+practicaled_other_s2==0),
    
    ppe.dondof_incon_s2 = (ppe.dondof_none_s2 & (ppe.dondof_vid_s2+ppe.dondof_written_s2+ppe.dondof_sim_s2+ppe.dondof_dept_s2+ppe.dondof_other_s2>0)),
    ppe.fit_incon_s2 = (ppe.fit_none_s2 & (ppe.fit_vid_s2+ppe.fit_written_s2+ppe.fit_sim_s2+ppe.fit_dept_s2+ppe.fit_other_s2>0)),
    ppe.exp_incon_s2 = (ppe.exp_none_s2 & (ppe.exp_vid_s2+ppe.exp_written_s2+ppe.exp_sim_s2+ppe.exp_dept_s2+ppe.exp_other_s2>0)),
    practicaled_incon_s2 = (practicaled_none_s2 & (practicaled_case_s2+practicaled_aerosol_s2+practicaled_other_s2>0)),
    
    specialty_NA_s3 = (specialty_acute+specialty_anaes+specialty_ed+specialty_icu+specialty_paeds+specialty_gp+specialty_surg+specialty_fy+specialty_other==0),
    outbreak_NA_s3 = (outbreak_none+outbreak_ebola+outbreak_mers+outbreak_sars+outbreak_chikungunya+outbreak_cholera+outbreak_flu+outbreak_zika+outbreak_other==0),
    # ppe.dondof_NA_s3 = (ppe.dondof_none_s3+ppe.dondof_vid_s3+ppe.dondof_written_s3+ppe.dondof_sim_s3+ppe.dondof_dept_s3+ppe.dondof_other_s3==0),
    # ppe.fit_NA_s3 = (ppe.fit_none_s3+ppe.fit_vid_s3+ppe.fit_written_s3+ppe.fit_sim_s3+ppe.fit_dept_s3+ppe.fit_other_s3==0),
    # ppe.exp_NA_s3 = (ppe.exp_none_s3+ppe.exp_vid_s3+ppe.exp_written_s3+ppe.exp_sim_s3+ppe.exp_dept_s3+ppe.exp_other_s3==0),
    #practicaled_NA_s3 = (practicaled_none_s3+practicaled_case_s3+practicaled_aerosol_s3+practicaled_other_s3==0),
    
    # ppe.dondof_incon_s3 = (ppe.dondof_none_s3 & (ppe.dondof_vid_s3+ppe.dondof_written_s3+ppe.dondof_sim_s3+ppe.dondof_dept_s3+ppe.dondof_other_s3>0)),
    # ppe.fit_incon_s3 = (ppe.fit_none_s3 & (ppe.fit_vid_s3+ppe.fit_written_s3+ppe.fit_sim_s3+ppe.fit_dept_s3+ppe.fit_other_s3>0)),
    # ppe.exp_incon_s3 = (ppe.exp_none_s3 & (ppe.exp_vid_s3+ppe.exp_written_s3+ppe.exp_sim_s3+ppe.exp_dept_s3+ppe.exp_other_s3>0)),
    #practicaled_incon_s3 = (practicaled_none_s3 & (practicaled_case_s3+practicaled_aerosol_s3+practicaled_other_s3>0)),
    
  )

## create derived variables ####

# fix single/multiple answer specialty issue
# fix single/multiple answer department issue
# identify questions where all boxes unchecked and convert to NA
# add some new variables
# add regions
# 

data2 <- data1 %>%
  mutate(
    
    # can only run with access to emails
    email = tolower(email),
    
    datesince_s1 = interval(ymd_hms("2020-03-18T00:00:00"), timestamp_s1) %/% days(1),
    datesince_s2 = interval(ymd_hms("2020-03-18T00:00:00"), timestamp_s2) %/% days(1),
    datesince_s3 = interval(ymd_hms("2020-03-18T00:00:00"), timestamp_s3) %/% days(1),
    
    age2 = fct_collapse(age, `>60` = c("61-65", "66-70", ">70")),
    
    specialty_ed = if_else(specialty %in% "Emergency Medicine", TRUE, specialty_ed),
    specialty_anaes = if_else(specialty %in% "Anaesthetics", TRUE, specialty_anaes),
    specialty_icu = if_else(specialty %in% "Intensive Care Medicine", TRUE, specialty_icu),
    specialty_paeds = if_else(specialty %in% "Paediatrics", TRUE, specialty_paeds),
    specialty_gp = if_else(specialty %in% "General Practice", TRUE, specialty_gp),
    specialty_surg = if_else(specialty %in% "Surgery", TRUE, specialty_surg),
    specialty_fy = if_else(specialty %in% "Foundation Programme", TRUE, specialty_fy),
    specialty_acute = if_else(specialty %in% "Acute Internal Medicine", TRUE, specialty_acute),
    specialty_other = if_else(specialty %in% "Other", TRUE, specialty_other),
    specialty_NA = (specialty_acute+specialty_anaes+specialty_ed+specialty_icu+specialty_paeds+specialty_gp+specialty_surg+specialty_fy+specialty_other==0),
    
    comb_specialty = paste0(
      if_else(specialty_ed, "ED+", ""),
      if_else(specialty_anaes, "anaes+", ""),
      if_else(specialty_icu, "ICU+", ""),
      if_else(specialty_paeds, "paeds+", ""),
      if_else(specialty_gp, "GP+", ""),
      if_else(specialty_surg, "surgery+", ""),
      if_else(specialty_fy, "FY+", ""),
      if_else(specialty_acute, "acute+", ""),
      if_else(specialty_other, "other+", "")
    ) %>% str_remove("\\+$"),
    comb_specialty = if_else(specialty_fy, "FY", comb_specialty),
    
    comb_specialty2 = fct_other(comb_specialty, keep=c("ED", "anaes", "ICU", "anaes+ICU", "paeds", "FY")),
    
    comb_specialty3 = case_when(
      specialty_fy ~ "FY",
      specialty_gp ~ "GP",
      specialty_ed & specialty_anaes & specialty_icu  ~ "ED+anaes+ICU",
      specialty_ed & specialty_anaes  ~ "ED+anaes",
      specialty_ed & specialty_icu ~ "ED+ICU",
      specialty_ed ~ "ED",
      specialty_anaes & specialty_icu ~ "anaes+ICU",
      specialty_anaes ~ "anaes",
      specialty_icu ~ "ICU",
      specialty_paeds ~ "paeds",
      specialty_surg ~ "surgery",
      TRUE ~ "other"
    ),
    
    
    dept_ed = if_else(dept %in% "Emergency Department (adult or paediatric)", TRUE, dept_ed),
    dept_anaes = if_else(dept %in% "Anaesthetic Department (adult or paediatric)", TRUE, dept_anaes),
    dept_icu = if_else(dept %in% "Intensive Care Department (adult or paediatric)", TRUE, dept_icu),
    dept_amu = if_else(dept %in% "Acute Medical Unit", TRUE, dept_amu),
    dept_ward = if_else(dept %in% "Hospital ward (adult or paediatric)", TRUE, dept_ward),
    dept_other = if_else(dept %in% "Other", TRUE, dept_other),
    dept_NA = (dept_ed+dept_anaes+dept_icu+dept_amu+dept_ward+dept_other==0),
    
    comb_deptedcc = (dept_ed | dept_anaes | dept_icu),
    
    comb_dept = paste0(
      if_else(dept_ed, "ED+", ""),
      if_else(dept_anaes, "anaes+", ""),
      if_else(dept_icu, "ICU+", ""),
      if_else(dept_amu, "AMU+", ""),
      if_else(dept_ward, "ward+", ""),
      if_else(dept_other, "other+", "")
    ) %>% str_remove("\\+$"),
    
    comb_dept2 = fct_other(comb_dept, keep=c("ED", "anaes", "ICU", "AMU", "anaes+ICU")),
    
    comb_dept3 = case_when(
      dept_ed & dept_anaes & dept_icu  ~ "ED+anaes+ICU",
      dept_ed & dept_anaes  ~ "ED+anaes",
      dept_ed & dept_icu ~ "ED+ICU",
      dept_ed ~ "ED",
      dept_anaes & dept_icu ~ "anaes+ICU",
      dept_anaes ~ "anaes",
      dept_icu ~ "ICU",
      TRUE ~ "other"
    ),
    
    seniority = case_when(
      grade %in% c(
        "F1", "F2", "GP Trainee","ST1","ST2", "ST3",
        "Clinical Fellow (F2-ST3 Level)", "Intern", 
        "Senior House Officer (Training scheme)", "Senior House Officer (Standalone)"
      ) ~ "Junior Doctor",
      grade %in% c(
        "ST4", "ST5", "ST6", "ST7", "ST8", "Clinical Fellow (>=ST4 Level)", 
        "Registrar (Training scheme)", "Registrar (Standalone)", "Specialist Registrar", "Senior Registrar"
      ) ~ "Middle Grade Doctor",
      grade %in% c(
        "Associate Specialist", "Staff Grade", "GP", "CESR Doctor"
      ) ~ "Other Senior Doctor",
      grade %in% c("Consultant") ~ "Senior Doctor (Consultant Grade)",
      TRUE ~ "other"
    ) %>% 
      factor(levels=c(
        "Junior Doctor","Middle Grade Doctor",
        "Other Senior Doctor", "Senior Doctor (Consultant Grade)"
      )),
    
    
    outbreak_NA = (outbreak_none+outbreak_ebola+outbreak_mers+outbreak_sars+outbreak_chikungunya+outbreak_cholera+outbreak_flu+outbreak_zika+outbreak_other==0),
    outbreak_none = if_else(outbreak_NA, NA, outbreak_none),
    outbreak_ebola = if_else(outbreak_NA, NA, outbreak_ebola),
    outbreak_mers = if_else(outbreak_NA, NA, outbreak_mers),
    outbreak_sars = if_else(outbreak_NA, NA, outbreak_sars),
    outbreak_chikungunya = if_else(outbreak_NA, NA, outbreak_chikungunya),
    outbreak_cholera = if_else(outbreak_NA, NA, outbreak_cholera),
    outbreak_flu = if_else(outbreak_NA, NA, outbreak_flu),
    outbreak_zika = if_else(outbreak_NA, NA, outbreak_zika),
    outbreak_other = if_else(outbreak_NA, NA, outbreak_other),
    comb_outbreak_any = if_else(!outbreak_none, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    
    # survey 1
    
    ppe.dondof_NA_s1 = (ppe.dondof_none_s1+ppe.dondof_vid_s1+ppe.dondof_written_s1+ppe.dondof_sim_s1+ppe.dondof_dept_s1+ppe.dondof_other_s1==0),
    ppe.dondof_none_s1 = if_else(ppe.dondof_NA_s1, NA, ppe.dondof_none_s1),
    ppe.dondof_vid_s1 = if_else(ppe.dondof_NA_s1, NA, ppe.dondof_vid_s1),
    ppe.dondof_written_s1 = if_else(ppe.dondof_NA_s1, NA, ppe.dondof_written_s1),
    ppe.dondof_sim_s1 = if_else(ppe.dondof_NA_s1, NA, ppe.dondof_sim_s1),
    ppe.dondof_other_s1 = if_else(ppe.dondof_NA_s1, NA, ppe.dondof_other_s1),
    comb_ppe.dondof_any_s1 = if_else(!ppe.dondof_none_s1, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    ppe.fit_NA_s1 = (ppe.fit_none_s1+ppe.fit_vid_s1+ppe.fit_written_s1+ppe.fit_sim_s1+ppe.fit_dept_s1+ppe.fit_other_s1==0),
    ppe.fit_none_s1 = if_else(ppe.fit_NA_s1, NA, ppe.fit_none_s1),
    ppe.fit_vid_s1 = if_else(ppe.fit_NA_s1, NA, ppe.fit_vid_s1),
    ppe.fit_written_s1 = if_else(ppe.fit_NA_s1, NA, ppe.fit_written_s1),
    ppe.fit_sim_s1 = if_else(ppe.fit_NA_s1, NA, ppe.fit_sim_s1),
    ppe.fit_other_s1 = if_else(ppe.fit_NA_s1, NA, ppe.fit_other_s1),
    comb_ppe.fit_any_s1 = if_else(!ppe.fit_none_s1, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    ppe.exp_NA_s1 = (ppe.exp_none_s1+ppe.exp_vid_s1+ppe.exp_written_s1+ppe.exp_sim_s1+ppe.exp_dept_s1+ppe.exp_other_s1==0),
    ppe.exp_none_s1 = if_else(ppe.exp_NA_s1, NA, ppe.exp_none_s1),
    ppe.exp_vid_s1 = if_else(ppe.exp_NA_s1, NA, ppe.exp_vid_s1),
    ppe.exp_written_s1 = if_else(ppe.exp_NA_s1, NA, ppe.exp_written_s1),
    ppe.exp_sim_s1 = if_else(ppe.exp_NA_s1, NA, ppe.exp_sim_s1),
    ppe.exp_other_s1 = if_else(ppe.exp_NA_s1, NA, ppe.exp_other_s1),
    comb_ppe.exp_any_s1 = if_else(!ppe.exp_none_s1, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    practicaled_NA_s1 = (practicaled_none_s1+practicaled_case_s1+practicaled_aerosol_s1+practicaled_other_s1==0),
    practicaled_none_s1 = if_else(practicaled_NA_s1, NA, practicaled_none_s1),
    practicaled_case_s1 = if_else(practicaled_NA_s1, NA, practicaled_case_s1),
    practicaled_aerosol_s1 = if_else(practicaled_NA_s1, NA, practicaled_aerosol_s1),
    practicaled_other_s1 = if_else(practicaled_NA_s1, NA, practicaled_other_s1),
    comb_practicaled_any_s1 = if_else(!practicaled_none_s1, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    selfisolatereason_NA_s1 = if_else(
      (selfisolate_s1=="Yes"),
      (selfisolatereason_symptomspersonal_s1+selfisolatereason_diagnosispersonal_s1+
         selfisolatereason_symptomshousehold_s1+selfisolatereason_diagnosiswork_s1+
         selfisolatereason_diagnosisplay_s1+selfisolatereason_other_s1==0),
      NA
    ),
    selfisolatereason_symptomspersonal_s1 = if_else(selfisolatereason_NA_s1, NA, selfisolatereason_symptomspersonal_s1),
    selfisolatereason_diagnosispersonal_s1 = if_else(selfisolatereason_NA_s1, NA, selfisolatereason_diagnosispersonal_s1),
    selfisolatereason_symptomshousehold_s1 = if_else(selfisolatereason_NA_s1, NA, selfisolatereason_symptomshousehold_s1),
    selfisolatereason_diagnosiswork_s1 = if_else(selfisolatereason_NA_s1, NA, selfisolatereason_diagnosiswork_s1),
    selfisolatereason_diagnosisplay_s1 = if_else(selfisolatereason_NA_s1, NA, selfisolatereason_diagnosisplay_s1),
    selfisolatereason_other_s1 = if_else(selfisolatereason_NA_s1, NA, selfisolatereason_other_s1),
    
    redeployed_where2_s1 = 
      case_when(
        redeployed_s1=="Yes" ~ as.character(redeployed_where_s1),
        redeployed_s1=="No" ~ "Not redeployed",
        is.na(redeployed_s1) ~ NA_character_
      ) %>% 
      factor(levels = c("Not redeployed", factor_dct0$level[factor_dct0$R=="redeployed_where_s1"][[1]])),
    
    
    # because for survey 2 there was no "i do not have an established medical condition" option so simplifying to "any relevant health worries" or not.
    increasesympmedical_s1 = case_when(
      increasesympmedical_s1=="Yes" ~ "Yes",
      increasesympmedical_s1 %in% c("No", "I do not have an established medical condition") ~ "No",
      TRUE ~ NA_character_
    ) %>% factor(levels=c("No", "Yes")),
    increasesympmental_s1 = case_when(
      increasesympmental_s1=="Yes" ~ "Yes",
      increasesympmental_s1 %in% c("No", "I do not have an established mental health condition") ~ "No",
      TRUE ~ NA_character_
    ) %>% factor(levels=c("No", "Yes")),
    
    exposure_confirmed2_s1 = fct_collapse(exposure_confirmed_s1, ">25" = c("26-30","31-35","> 36")),
    
    ghqsum0123_s1 = (select(., matches("(^ghq)\\_(.+)(\\_s1$)")) %>% mutate_all(as.integer) %>% rowSums()) - 12,
    ghqsum0011_s1 = (select(., matches("(^ghq)\\_(.+)(\\_s1$)")) %>% mutate_all(as.integer) %>%
                       mutate_all(.funs=~case_when(
                         .==1 ~ 0L,
                         .==2 ~ 0L,
                         .==3 ~ 1L,
                         .==4 ~ 1L,
                         TRUE ~ NA_integer_
                       )) %>%
                       rowSums()),
    ghqcase_s1 = cut(ghqsum0011_s1, breaks=c(-Inf, 3, Inf), labels=c("GHQ12-0011 <= 3","GHQ12-0011 > 3")),
    ghqsum0123_cat_s1 = cut_interval(ghqsum0123_s1, length=6, right=FALSE),
    
    
    # survey 2
    
    
    ppe.dondof_NA_s2 = (ppe.dondof_none_s2+ppe.dondof_vid_s2+ppe.dondof_written_s2+ppe.dondof_sim_s2+ppe.dondof_dept_s2+ppe.dondof_other_s2==0),
    ppe.dondof_none_s2 = if_else(ppe.dondof_NA_s2, NA, ppe.dondof_none_s2),
    ppe.dondof_vid_s2 = if_else(ppe.dondof_NA_s2, NA, ppe.dondof_vid_s2),
    ppe.dondof_written_s2 = if_else(ppe.dondof_NA_s2, NA, ppe.dondof_written_s2),
    ppe.dondof_sim_s2 = if_else(ppe.dondof_NA_s2, NA, ppe.dondof_sim_s2),
    ppe.dondof_other_s2 = if_else(ppe.dondof_NA_s2, NA, ppe.dondof_other_s2),
    ppe.dondof_any_s2 = if_else(!ppe.dondof_none_s2, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    ppe.fit_NA_s2 = (ppe.fit_none_s2+ppe.fit_vid_s2+ppe.fit_written_s2+ppe.fit_sim_s2+ppe.fit_dept_s2+ppe.fit_other_s2==0),
    ppe.fit_none_s2 = if_else(ppe.fit_NA_s2, NA, ppe.fit_none_s2),
    ppe.fit_vid_s2 = if_else(ppe.fit_NA_s2, NA, ppe.fit_vid_s2),
    ppe.fit_written_s2 = if_else(ppe.fit_NA_s2, NA, ppe.fit_written_s2),
    ppe.fit_sim_s2 = if_else(ppe.fit_NA_s2, NA, ppe.fit_sim_s2),
    ppe.fit_other_s2 = if_else(ppe.fit_NA_s2, NA, ppe.fit_other_s2),
    ppe.fit_any_s2 = if_else(!ppe.fit_none_s2, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    ppe.exp_NA_s2 = (ppe.exp_none_s2+ppe.exp_vid_s2+ppe.exp_written_s2+ppe.exp_sim_s2+ppe.exp_dept_s2+ppe.exp_other_s2==0),
    ppe.exp_none_s2 = if_else(ppe.exp_NA_s2, NA, ppe.exp_none_s2),
    ppe.exp_vid_s2 = if_else(ppe.exp_NA_s2, NA, ppe.exp_vid_s2),
    ppe.exp_written_s2 = if_else(ppe.exp_NA_s2, NA, ppe.exp_written_s2),
    ppe.exp_sim_s2 = if_else(ppe.exp_NA_s2, NA, ppe.exp_sim_s2),
    ppe.exp_other_s2 = if_else(ppe.exp_NA_s2, NA, ppe.exp_other_s2),
    ppe.exp_any_s2 = if_else(!ppe.exp_none_s2, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    practicaled_NA_s2 = (practicaled_none_s2+practicaled_case_s2+practicaled_aerosol_s2+practicaled_other_s2==0),
    practicaled_none_s2 = if_else(practicaled_NA_s2, NA, practicaled_none_s2),
    practicaled_case_s2 = if_else(practicaled_NA_s2, NA, practicaled_case_s2),
    practicaled_aerosol_s2 = if_else(practicaled_NA_s2, NA, practicaled_aerosol_s2),
    practicaled_other_s2 = if_else(practicaled_NA_s2, NA, practicaled_other_s2),
    practicaled_any_s2 = if_else(!practicaled_none_s2, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    selfisolatereason_NA_s2 = if_else((selfisolate_s2=="Yes"),
                                      (selfisolatereason_symptomspersonal_s2+selfisolatereason_diagnosispersonal_s2+
                                         selfisolatereason_symptomshousehold_s2+selfisolatereason_diagnosiswork_s2+
                                         selfisolatereason_diagnosisplay_s2+selfisolatereason_other_s2==0),
                                      NA),
    selfisolatereason_symptomspersonal_s2 = if_else(selfisolatereason_NA_s2, NA, selfisolatereason_symptomspersonal_s2),
    selfisolatereason_diagnosispersonal_s2 = if_else(selfisolatereason_NA_s2, NA, selfisolatereason_diagnosispersonal_s2),
    selfisolatereason_symptomshousehold_s2 = if_else(selfisolatereason_NA_s2, NA, selfisolatereason_symptomshousehold_s2),
    selfisolatereason_diagnosiswork_s2 = if_else(selfisolatereason_NA_s2, NA, selfisolatereason_diagnosiswork_s2),
    selfisolatereason_diagnosisplay_s2 = if_else(selfisolatereason_NA_s2, NA, selfisolatereason_diagnosisplay_s2),
    selfisolatereason_other_s2 = if_else(selfisolatereason_NA_s2, NA, selfisolatereason_other_s2),
    
    
    unwellfam_NA_s2 = (unwellfam_none_s2+unwellfam_home_s2+unwellfam_ward_s2+unwellfam_icu_s2+unwellfam_died_s2==0),
    unwellfam_none_s2 = if_else(unwellfam_NA_s2, NA, unwellfam_none_s2),
    unwellfam_home_s2 = if_else(unwellfam_NA_s2, NA, unwellfam_home_s2),
    unwellfam_ward_s2 = if_else(unwellfam_NA_s2, NA, unwellfam_ward_s2),
    unwellfam_icu_s2 = if_else(unwellfam_NA_s2, NA, unwellfam_icu_s2),
    unwellfam_died_s2 = if_else(unwellfam_NA_s2, NA, unwellfam_died_s2),
    comb_unwellfam_any_s2 = if_else(!unwellfam_none_s2, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    unwellcol_NA_s2 = (unwellcol_none_s2+unwellcol_home_s2+unwellcol_ward_s2+unwellcol_icu_s2+unwellcol_died_s2==0),
    unwellcol_none_s2 = if_else(unwellcol_NA_s2, NA, unwellcol_none_s2),
    unwellcol_home_s2 = if_else(unwellcol_NA_s2, NA, unwellcol_home_s2),
    unwellcol_ward_s2 = if_else(unwellcol_NA_s2, NA, unwellcol_ward_s2),
    unwellcol_icu_s2 = if_else(unwellcol_NA_s2, NA, unwellcol_icu_s2),
    unwellcol_died_s2 = if_else(unwellcol_NA_s2, NA, unwellcol_died_s2),
    comb_unwellcol_any_s2 = if_else(!unwellcol_none_s2, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    comb_unwellfam_max_s2 = case_when(
      unwellfam_none_s2 ~ "No",
      unwellfam_died_s2 ~ "Yes, died",
      unwellfam_icu_s2 ~ "Yes, unwell and required ICU",
      unwellfam_ward_s2 ~ "Yes, unwell and required hospital",
      unwellfam_home_s2 ~ "Yes, unwell at home",
      unwellfam_none_s2 ~ "No",
      unwellfam_NA_s2 ~ NA_character_
    ) %>% factor(c("No",  "Yes, unwell at home", "Yes, unwell and required hospital", "Yes, unwell and required ICU", "Yes, died")),
    
    comb_unwellcol_max_s2 = case_when(
      unwellcol_none_s2 ~ "No",
      unwellcol_died_s2 ~ "Yes, died",
      unwellcol_icu_s2 ~ "Yes, unwell and required ICU",
      unwellcol_ward_s2 ~ "Yes, unwell and required hospital",
      unwellcol_home_s2 ~ "Yes, unwell at home",
      unwellcol_none_s2 ~ "No",
      unwellcol_NA_s2 ~ NA_character_
    ) %>% factor(c("No",  "Yes, unwell at home", "Yes, unwell and required hospital", "Yes, unwell and required ICU", "Yes, died")),
    
    comb_covidpersonal_s2 = case_when(
      covidadmit_s2 == "Yes" ~ "Yes, admitted",
      coviddiag_s2 == "Yes" ~ "Yes, diagnosed",
      coviddiag_s2 == "No" ~ "No",
      TRUE ~ NA_character_
    ) %>% factor(c("No", "Yes, diagnosed", "Yes, admitted")),
    
    
    redeployed_where2_s2 = 
      case_when(
        redeployed_s2=="Yes" ~ as.character(redeployed_where_s2),
        redeployed_s2=="No" ~ "Not redeployed",
        is.na(redeployed_s2) ~ NA_character_
      ) %>% 
      factor(levels = c("Not redeployed", factor_dct0$level[factor_dct0$R=="redeployed_where_s2"][[1]])),
    
    
    increasesympmedical_s2 = case_when(
      increasesympmedical_s2=="Yes" ~ "Yes",
      increasesympmedical_s2 %in% c("No") ~ "No",
      TRUE ~ NA_character_
    ) %>% factor(levels=c("No", "Yes")),
    increasesympmental_s2 = case_when(
      increasesympmental_s2=="Yes" ~ "Yes",
      increasesympmental_s2 %in% c("No", "I do not have an established mental health condition") ~ "No",
      TRUE ~ NA_character_
    ) %>% factor(levels=c("No", "Yes")),
    
    feelings_NA_s2 = (
      (is.na(feelings_contribution_s2))+(is.na(feelings_accomplish_s2))+
        (is.na(feelings_purpose_s2))+(is.na(feelings_confidence_s2))+
        (is.na(feelings_compassion_s2))+(is.na(feelings_satisfaction_s2))+
        (is.na(feelings_cohesion_s2))==0
    ),
    #feelings_contribution_s2 = if_else(feelings_NA_s2, NA_character_, feelings_contribution_s2),
    #feelings_accomplish_s2 = if_else(feelings_NA_s2, NA_character_, feelings_accomplish_s2),
    #feelings_confidence_s2 = if_else(feelings_NA_s2, NA_character_, feelings_confidence_s2),
    #feelings_compassion_s2 = if_else(feelings_NA_s2, NA_character_, feelings_compassion_s2),
    #feelings_purpose_s2 = if_else(feelings_NA_s2, NA_character_, feelings_purpose_s2),
    #feelings_satisfaction_s2 = if_else(feelings_NA_s2, NA_character_, feelings_satisfaction_s2),
    #feelings_cohesion_s2 = if_else(feelings_NA_s2, NA_character_, feelings_cohesion_s2),
    
    exposure_confirmed2_s2 = fct_collapse(exposure_confirmed_s2, ">25" = c("26-30","31-35","> 36")),
    
    ghqsum0123_s2 = (select(., matches("(^ghq)\\_(.+)(\\_s2$)")) %>% mutate_all(as.integer) %>% rowSums()) - 12,
    ghqsum0011_s2 = (select(., matches("(^ghq)\\_(.+)(\\_s2$)")) %>% mutate_all(as.integer) %>%
                       mutate_all(.funs=~case_when(
                         .==1 ~ 0L,
                         .==2 ~ 0L,
                         .==3 ~ 1L,
                         .==4 ~ 1L,
                         TRUE ~ NA_integer_
                       )) %>%
                       rowSums()
    ),
    ghqsum0123_cat_s2 = cut_interval(ghqsum0123_s2, length=6, right=FALSE),
    ghqcase_s2 = cut(ghqsum0011_s2, breaks=c(-Inf, 3, Inf), labels=c("GHQ12-0011 <= 3","GHQ12-0011 > 3")),
    
    iesrsum0123_s2 = (select(., matches("(^iesr)\\_(.+)(\\_s2$)")) %>% mutate_all(as.integer) %>% rowSums()) - 22,
    iesrsum0123_cat_s2 = cut(iesrsum0123_s2, breaks=c(-1,0,8,16,24,32,49,48,56,64,72,80), 
                             labels=c("0", "1-8","9-16","17-24","25-32","33-40","41-48", "49-56", "57-64", "65-72", "73-80"), 
                             right=FALSE),
    iesrsum0123_log_s2 = log(iesrsum0123_s2+1),
    iesrcase24_s2 = cut(iesrsum0123_s2, breaks=c(-Inf, 24, Inf), labels=c("IES-R-0123 <= 24","IES-R-0123 > 24")),
    iesrcase33_s2 = cut(iesrsum0123_s2, breaks=c(-Inf, 33, Inf), labels=c("IES-R-0123 <= 33","IES-R-0123 > 33")),
    
    
    
    # survey 3
    
    selfisolatereason_NA_s3 = if_else((selfisolate_s3=="Yes"),
                                      (selfisolatereason_symptomspersonal_s3+selfisolatereason_diagnosispersonal_s3+
                                         selfisolatereason_symptomshousehold_s3+selfisolatereason_diagnosiswork_s3+
                                         selfisolatereason_diagnosisplay_s3+selfisolatereason_other_s3==0),
                                      NA),
    selfisolatereason_symptomspersonal_s3 = if_else(selfisolatereason_NA_s3, NA, selfisolatereason_symptomspersonal_s3),
    selfisolatereason_diagnosispersonal_s3 = if_else(selfisolatereason_NA_s3, NA, selfisolatereason_diagnosispersonal_s3),
    selfisolatereason_symptomshousehold_s3 = if_else(selfisolatereason_NA_s3, NA, selfisolatereason_symptomshousehold_s3),
    selfisolatereason_diagnosiswork_s3 = if_else(selfisolatereason_NA_s3, NA, selfisolatereason_diagnosiswork_s3),
    selfisolatereason_diagnosisplay_s3 = if_else(selfisolatereason_NA_s3, NA, selfisolatereason_diagnosisplay_s3),
    selfisolatereason_other_s3 = if_else(selfisolatereason_NA_s3, NA, selfisolatereason_other_s3),
    
    unwellfam_NA_s3 = ((!is.na(unwellfam_home_s3)) + (!is.na(unwellfam_ward_s3)) + (!is.na(unwellfam_icu_s3)) + (!is.na(unwellfam_died_s3)) ==0),
    unwellfam_none_s3 = (unwellfam_home_s3+unwellfam_ward_s3+unwellfam_icu_s3+unwellfam_died_s3==0),
    unwellfam_home_s3 = if_else(unwellfam_NA_s3, NA, unwellfam_home_s3),
    unwellfam_ward_s3 = if_else(unwellfam_NA_s3, NA, unwellfam_ward_s3),
    unwellfam_icu_s3 = if_else(unwellfam_NA_s3, NA, unwellfam_icu_s3),
    unwellfam_died_s3 = if_else(unwellfam_NA_s3, NA, unwellfam_died_s3),
    comb_unwellfam_any_s3 = if_else(!unwellfam_none_s3, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    unwellcol_NA_s3 = ((!is.na(unwellcol_home_s3)) + (!is.na(unwellcol_ward_s3)) + (!is.na(unwellcol_icu_s3)) + (!is.na(unwellcol_died_s3))==0),
    unwellcol_none_s3 = if_else(unwellcol_NA_s3, NA, (unwellcol_home_s3+unwellcol_ward_s3+unwellcol_icu_s3+unwellcol_died_s3==0)),
    unwellcol_home_s3 = if_else(unwellcol_NA_s3, NA, unwellcol_home_s3),
    unwellcol_ward_s3 = if_else(unwellcol_NA_s3, NA, unwellcol_ward_s3),
    unwellcol_icu_s3 = if_else(unwellcol_NA_s3, NA, unwellcol_icu_s3),
    unwellcol_died_s3 = if_else(unwellcol_NA_s3, NA, unwellcol_died_s3),
    comb_unwellcol_any_s3 = if_else(!unwellcol_none_s3, "Yes", "No") %>% factor(levels=c("Yes", "No")),
    
    comb_unwellfam_max_s3 = case_when(
      unwellfam_none_s3 ~ "No",
      unwellfam_died_s3 ~ "Yes, died",
      unwellfam_icu_s3 ~ "Yes, unwell and required ICU",
      unwellfam_ward_s3 ~ "Yes, unwell and required hospital",
      unwellfam_home_s3 ~ "Yes, unwell at home",
      unwellfam_none_s3 ~ "No",
      unwellfam_NA_s3 ~ NA_character_
    ) %>% factor(c("No",  "Yes, unwell at home", "Yes, unwell and required hospital", "Yes, unwell and required ICU", "Yes, died")),
    
    comb_unwellcol_max_s3 = case_when(
      unwellcol_none_s3 ~ "No",
      unwellcol_died_s3 ~ "Yes, died",
      unwellcol_icu_s3 ~ "Yes, unwell and required ICU",
      unwellcol_ward_s3 ~ "Yes, unwell and required hospital",
      unwellcol_home_s3 ~ "Yes, unwell at home",
      unwellcol_none_s3 ~ "No",
      unwellcol_NA_s3 ~ NA_character_
    ) %>% factor(c("No",  "Yes, unwell at home", "Yes, unwell and required hospital", "Yes, unwell and required ICU", "Yes, died")),
    
    
    comb_covidpersonal_s3 = case_when(
      covidadmit_s3 == "Yes" ~ "Yes, admitted",
      coviddiag_s3 == "Yes" ~ "Yes, diagnosed",
      coviddiag_s3 == "No" ~ "No",
      TRUE ~ NA_character_
    ) %>% factor(c("No", "Yes, diagnosed", "Yes, admitted")),
    increasesympmedical_s3 = case_when(
      increasesympmedical_s3=="Yes" ~ "Yes",
      increasesympmedical_s3 %in% c("No", "I do not have an established physical health condition") ~ "No",
      TRUE ~ NA_character_
    ) %>% factor(levels=c("No", "Yes")),
    increasesympmental_s3 = case_when(
      increasesympmental_s3=="Yes" ~ "Yes",
      increasesympmental_s3 %in% c("No", "I do not have an established mental health condition") ~ "No",
      TRUE ~ NA_character_
    ) %>% factor(levels=c("No", "Yes")),
    
    feelings_NA_s3 = (
      (is.na(feelings_contribution_s3))+(is.na(feelings_accomplish_s3))+
        (is.na(feelings_purpose_s3))+(is.na(feelings_confidence_s3))+
        (is.na(feelings_compassion_s3))+(is.na(feelings_satisfaction_s3))+
        (is.na(feelings_cohesion_s3)) ==0
    ),
    
    #feelings_contribution_s3 = if_else(feelings_NA_s3, NA_character_, feelings_contribution_s3) %>% factor(levels=c("No", "Yes")),
    #feelings_accomplish_s3 = if_else(feelings_NA_s3, NA_character_, if_else(feelings_accomplish_s3) %>% factor(levels=c("No", "Yes")),
    #feelings_confidence_s3 = if_else(feelings_NA_s3, NA_character_, feelings_confidence_s3) %>% factor(levels=c("No", "Yes")),
    #feelings_compassion_s3 = if_else(feelings_NA_s3, NA_character_, feelings_compassion_s3) %>% factor(levels=c("No", "Yes")),
    #feelings_purpose_s3 = if_else(feelings_NA_s3, NA_character_, feelings_purpose_s3) %>% factor(levels=c("No", "Yes")),
    #feelings_satisfaction_s3 = if_else(feelings_NA_s3, NA_character_, feelings_satisfaction_s3) %>% factor(levels=c("No", "Yes")),
    #feelings_cohesion_s3 = if_else(feelings_NA_s3, NA_character_, feelings_cohesion_s3) %>% factor(levels=c("No", "Yes")),
    
    exposure_confirmed2_s3 = fct_collapse(exposure_confirmed_s3, ">25" = c("26-30","31-35","> 36")),
    
    
    ghqsum0123_s3 = (select(., matches("(^ghq)\\_(.+)(\\_s3$)")) %>% mutate_all(as.integer) %>% rowSums()) - 12,
    ghqsum0011_s3 = (select(., matches("(^ghq)\\_(.+)(\\_s3$)")) %>% mutate_all(as.integer) %>%
                       mutate_all(.funs=~case_when(
                         .==1 ~ 0L,
                         .==2 ~ 0L,
                         .==3 ~ 1L,
                         .==4 ~ 1L,
                         TRUE ~ NA_integer_
                       )) %>%
                       rowSums()),
    
    ghqsum0123_cat_s3 = cut_interval(ghqsum0123_s3, length=6, right=FALSE),
    ghqcase_s3 = cut(ghqsum0011_s3, breaks=c(-Inf, 3, Inf), labels=c("GHQ12-0011 <= 3","GHQ12-0011 > 3")),
    
    iesrsum0123_s3 = (select(., matches("(^iesr)\\_(.+)(\\_s3$)")) %>% mutate_all(as.integer) %>% rowSums()) - 22,
    
    iesrsum0123_cat_s3 = cut(iesrsum0123_s3, breaks=c(-1,0,8,16,24,32,49,48,56,64,72,80), 
                             labels=c("0", "1-8","9-16","17-24","25-32","33-40","41-48", "49-56", "57-64", "65-72", "73-80"), 
                             right=FALSE),
    iesrsum0123_log_s3 = log(iesrsum0123_s3+1),
    iesrcase24_s3 = cut(iesrsum0123_s3, breaks=c(-Inf, 24, Inf), labels=c("IES-R-0123 <= 24","IES-R-0123 > 24")),
    iesrcase33_s3 = cut(iesrsum0123_s3, breaks=c(-Inf, 33, Inf), labels=c("IES-R-0123 <= 33","IES-R-0123 > 33")),
    
    
    ghqsum0123_jit_s1 = ghqsum0123_s1 + runif(n(), -0.2,0.2),
    ghqsum0123_jit_s2 = ghqsum0123_s2 + runif(n(), -0.2,0.2),
    ghqsum0123_jit_s3 = ghqsum0123_s3 + runif(n(), -0.2,0.2),
    
    iesrsum0123_jit_s2 = iesrsum0123_s2 + runif(n(), -0.2,0.2),
    iesrsum0123_jit_s3 = iesrsum0123_s3 + runif(n(), -0.2,0.2),
    
    ethnicity2 = fct_case_when(
      ethnicity_s2 %in% c("English / Welsh / Scottish / Northern Irish / British", "Any other White background", "Irish") ~ "White",
      ethnicity_s2 %in% c(NA, "", "Prefer not to disclose")  ~ NA_character_,
      TRUE ~ "Not white"
    ),
    
    ethnicity3 = fct_case_when(
      ethnicity_s2 %in% c("English / Welsh / Scottish / Northern Irish / British") ~ "White British",
      ethnicity_s2 %in% c("Irish") ~ "Irish",
      #ethnicity_s2 %in% c("Any other White background") ~ "White other",
      ethnicity_s2 %in% c(NA, "", "Prefer not to disclose")  ~ NA_character_,
      TRUE ~ "Ethnic minority (including white other)"
    ),
    
    
  ) %>% 
  left_join(corrections_hosp %>% select(ptid, region, nation), by="ptid") %>%
  mutate(
    compl_hosp = (!is.na(hosp)) & (hosp!="Other") & (!(country=="UK" & region %in% c("Dublin", "Rest of Ireland"))),
    compl_grade = (!is.na(grade)) & (grade!="Other"), # because if they've put something in free text they've been reassigned, and if they've put nothing then then need to be removed
    compl_dept = (!dept_NA) & comb_deptedcc,
    compl_hospgradedept = (compl_hosp)*(compl_grade)*(compl_dept),
    compl_specialty = !specialty_NA,
    compl_ghq_s1 = !is.na(ghqsum0123_s1),
    compl_ghq_s2 = !is.na(ghqsum0123_s2),
    compl_ghq_s3 = !is.na(ghqsum0123_s3),
    compl_ghq_s123 = compl_ghq_s1 & compl_ghq_s2 & compl_ghq_s3,
    compl_iesr_s2 = !is.na(iesrsum0123_s2),
    compl_iesr_s3 = !is.na(iesrsum0123_s3),
    compl_iesr_s23 = compl_iesr_s2 & compl_iesr_s3,
    consent_s2 = consent_s2 %in% "Yes",
    consent_s3 = consent_s3 %in% "Yes",
    consent_s123 = consent_s1 & consent_s2 &consent_s3,
    
    ptid = paste0("p", ptid),
    
  ) %>%
  rename(ethnicity = ethnicity_s2, trauma = trauma_s2) %>%
  select(-dept, -specialty)



write_rds(x=data2, file=here::here("processed-data", "data_all.rds"))










factor_dct <- factor_dct0 %>%
  add_row(
    R="age2",
    name = "Age",
    full=dictionary %>% filter(R=="age") %>% pull(full),
    vartype="personal",
    code=dictionary %>% filter(R=="age") %>% pull(code),
    level=dictionary %>% filter(R=="age") %>% pull(level),
    short=NULL
  ) %>%
  add_row(
    R="ethnicity2",
    name = "Ethnicity",
    full=dictionary %>% filter(R=="ethnicity_s2") %>% pull(full),
    vartype="personal",
    #code=dictionary %>% filter(R=="age") %>% pull(code),
    #level=dictionary %>% filter(R=="age") %>% pull(level),
    short=NULL
  ) %>% 
  add_row(
    R="ethnicity3",
    name = "Ethnicity",
    vartype="personal",
    full=dictionary %>% filter(R=="ethnicity_s2") %>% pull(full),
    #code=dictionary %>% filter(R=="age") %>% pull(code),
    #level=dictionary %>% filter(R=="age") %>% pull(level),
    short=NULL
  ) %>%
  add_row(
    R="comb_specialty2",
    name = "Specialty",
    full="Specialty",
    vartype="professional",
    code=NULL,
    level=list(c("ED", "anaes", "ICU", "anaes+ICU", "paeds", "FY", "Other")),
    short=NULL
  ) %>%
  add_row(
    R="comb_specialty3",
    name = "Specialty",
    full="Specialty",
    vartype="professional",
    code=NULL,
    level=list(c("FY", "GP", "ED_anaes+ICU", "ED+anaes", "ED+ICU", "ED", "anaes+ICU", "anaes", "ICU", "paeds", "surgery", "other")),
    short=NULL
  ) %>%
  add_row(
    R="comb_dept2",
    name = "Department",
    full="Department",
    vartype="professional",
    code=NULL,
    level=list(c("ED", "anaes", "ICU", "AMU", "anaes+ICU", "Other")),
    short=NULL
  ) %>%
  # add_row(
  #   R="outbreak",
  #   full="Have you provided direct clinical care to any patients affected by these infectious disease outbreaks",
  #   name="outbreak experience",
  #   vartype="professional",
  #   code=list(1:9),
  #   level=list(c("none", "Ebola virus", "MERS-CoV", "SARS", "Chikungunya", "Cholera", "Influenza (swine, avian, zoonotic)", "Zika virus", "Other")),
  #   short=list(c("none", "ebola", "mers", "sars", "chikungunya", "cholera", "flu", "zika", "other"))
  # ) %>%
  add_row(
    R="ghq",
    name = "GHQ-12",
    level=list(c("Concentration",
                 "Losing Sleep",
                 "Playing a useful part",
                 "Capable of making decisions",
                 "Constantly under strain",
                 "Couldn't overcome your difficulties",
                 "Enjoying activities",
                 "Facing up to problems",
                 "Unhappy or depressed",
                 "Losing confidence",
                 "Feeling worthless",
                 "Feeling happy"
    )),
    short=list(c("conc","sleep", "use", "capable", "strain", "diffs", "day", "face", "depressed", "loseconf", "worthless", "happy"))
  ) %>%
  # add_row(
  #   R="ppe",
  #   name = "PPE training since COVID-19 outbreak",
  #   full="What training have you received in regards to PPE since the COVID-19 outbreak was declared",
  #   vartype="professional",
  #   code=list(1:6),
  #   level=list(c("none", "Instructional video", "Written instruction", "Simulation training", "Departmental guidance", "Other")),
  #   short=list(c("none", "vid", "written", "sim", "dept", "other"))
  # ) %>%
  add_row(
    R="ppe.dondof",
    name = "Training for donning and doffing",
    full="Training for donning and doffing",
    vartype="professional",
    code=list(1:6),
    level=list(c("none", "Instructional video", "Written instruction", "Simulation training", "Departmental guidance", "Other")),
    short=list(c("none", "vid", "written", "sim", "dept", "other"))
  ) %>%
  add_row(
    R="ppe.fit",
    name = "Training for fit testing for mask",
    full="Training for fit testing for mask",
    vartype="professional",
    code=list(1:6),
    level=list(c("none", "Instructional video", "Written instruction", "Simulation training", "Departmental guidance", "Other")),
    short=list(c("none", "vid", "written", "sim", "dept", "other"))
  ) %>%
  add_row(
    R="ppe.exp",
    name ="Training for exposure to aerosol generating procedure",
    full="Training for exposure to aerosol generating procedure",
    vartype="professional",
    code=list(1:6),
    level=list(c("none", "Instructional video", "Written instruction", "Simulation training", "Departmental guidance", "Other")),
    short=list(c("none", "vid", "written", "sim", "dept", "other"))
  ) %>%
  # add_row(
  #   R="ppeaspect",
  #   name = "",
  #   vartype="professional",
  #   code=list(1:3),
  #   level=list(c("donning and doffing", "fit testing for mask", "exposure to aerosol generating procedure")),
  #   short=list(c("dondof", "fit", "exp"))
  # ) %>%
  add_row(
    R="practicaled",
    name = "Practical education for COVID-19 care",
    full="What practical education have you received in regards to the clinical care of patients presenting with suspected/diagnosed COVID-19?",
    vartype="professional",
    code=list(1:4),
    level=list(c("none", "simulation training of a possible case", "simulation training of a case requiring aerosol procedure", "other")),
    short=list(c("none", "case", "aerosol", "other"))
  ) %>%
  add_row(
    R="guid",
    name = "information access frequency",
    full="How frequently do you access the following sources of information regarding policy and clinical aspects of COVID-19?",
    vartype="professional",
    code=list(1:7),
    level=list(c("Hourly", "Up to twice a day", "Daily", "Several times a week", "Weekly", "Less than weekly", "Never"))
  ) %>%
  add_row(
    R="guidetype",
    name = "information access frequency",
    full="How frequently do you access the following sources of information regarding policy and clinical aspects of COVID-19?",
    vartype="professional",
    code=list(1:7),
    level=list(c("Government guidance","College guidance","Trust guidance","Departmental guidance","Social media","Online blogs and podcasts","Peer review literature")),
    short=list(c("govt", "college", "trust", "dept", "sm", "blogs", "lit"))
  ) %>%
  add_row(
    R="feelings",
    name = "Post-trauma growth",
    full="During your time working in the COVID-19 pandemic have you experienced any of the following?",
    vartype="personal",
    code=list(1:7),
    level=list(c(
      "Feelings that you made a contribution", "A sense of personal accomplishment", 
      "Improved confidence and self esteem", "Increased compassion",
      "Re-evaluation of self and purpose", "Work satisfaction", 
      "A sense of team cohesion")
    ),
    short=list(c(
      "contribution", "accomplish", "confidence", 
      "compassion", "purpose", "satisfaction", "cohesion")
    )
  ) %>%
  add_row(
    R="selfisolatereason",
    name="self-isolation reason",
    full="If you had to self-isolate, why?",
    vartype="personal",
    code=list(1:6),
    level=list(c(
      "Personal symptoms","Personal diagnosis of COVID-19",
      "Symptoms of a member of the household",
      "Exposure to a positive case of COVID-19 in the work environment",
      "Exposure to a positive case of COVID-19 in your personal environment",
      "Other")
    ),
    short=list(c(
      "symptomspersonal", "diagnosispersonal", "symptomshousehold", 
      "diagnosiswork", "diagnosisplay", "other")
    )
  ) %>%
  add_row(
    R="redeployed_where2",
    name="Redeployment",
    full="Have you been redeployed due to Covid-19 and if so where",
    vartype="professional",
    code=dictionary %>% filter(R=="dept") %>% pull(code),
    level=dictionary %>% filter(R=="dept") %>% pull(level),
    short=list(c("ed", "anaes", "icu", "amu", "ward", "other"))
  )  %>%
  add_row(
    R="region",
    name = "Region",
    full="Region",
    vartype="professional",
    code=NULL,
    level=list(c("East Midlands", "East of England", "London", "North East", "North West", 
                 "South East", "South West", "West Midlands", "Yorkshire and the Humber",
                 "Northern Ireland", "Scotland", "Wales", "Dublin", "Rest of Ireland")),
    short=NULL
  ) %>%
  add_row(
    R="seniority",
    name = "Seniority",
    full="Seniority",
    vartype="professional",
    code=NULL,
    level=list(c("Junior Doctor","Middle Grade Doctor","Other Senior Doctor", "Senior Doctor (Consultant Grade)")),
    short=NULL
  ) %>%
  add_row(
    R="comb_outbreak_any",
    name = "Any previous outbreak experience",
    full="Any previous outbreak experience",
    vartype="professional",
    code=NULL,
    level=list(c("No", "Yes")),
    short=NULL
  ) %>%
  add_row(
    R="comb_ppe.dondof_any",
    name = "Any training for PPE donning and doffing",
    full="Any training for PPE donning and doffing",
    vartype="professional",
    code=NULL,
    level=list(c("No", "Yes")),
    short=NULL
  ) %>%
  add_row(
    R="comb_ppe.fit_any",
    name = "Any training for fit testing for mask",
    full="Any training for fit testing for mask",
    vartype="professional",
    code=NULL,
    level=list(c("No", "Yes")),
    short=NULL
  ) %>%
  add_row(
    R="comb_ppe.exp_any",
    name="Any training for exposure to aerosol generating procedure",
    full="Any training for exposure to aerosol generating procedure",
    vartype="professional",
    code=NULL,
    level=list(c("No", "Yes")),
    short=NULL
  ) %>%
  add_row(
    R="comb_practicaled_any",
    name = "Practical education for COVID-19 care",
    full="Any practical education received in regards to the clinical care of patients presenting with suspected/diagnosed COVID-19?",
    vartype="professional",
    code=NULL,
    level=list(c("No", "Yes")),
    short=NULL
  ) %>%
  add_row(
    R="exposure_confirmed2",
    name = "Exposure to confirmed case",
    full=dictionary %>% filter(R=="exposure_confirmed_s1") %>% pull(full),
    vartype="professional",
    code=dictionary %>% filter(R=="exposure_confirmed_s1") %>% pull(code),
    level=dictionary %>% filter(R=="exposure_confirmed_s1") %>% pull(level),
    short=NULL
  ) %>%
  add_row(
    R="comb_unwellfam_any",
    name="Any family members unwell due to COVID-19",
    full="Any family members unwell due to COVID-19",
    vartype="personal",
    code=NULL,
    level=list(c("No", "Yes")),
    short=NULL
  ) %>%
  add_row(
    R="comb_unwellcol_any",
    name="Any colleagues unwell due to COVID-19",
    full="Any colleagues unwell due to COVID-19",
    vartype="personal",
    code=NULL,
    level=list(c("No", "Yes")),
    short=NULL
  ) %>%
  add_row(
    R="comb_unwellfam_max",
    name = "Any family unwell due to COVID-19",
    #full=dictionary %>% filter(R=="comb_unwellfam_max_s2") %>% pull(full),
    vartype="personal",
    #code=dictionary %>% filter(R=="age") %>% pull(code),
    #level=dictionary %>% filter(R=="age") %>% pull(level),
    short=NULL
  ) %>%
  add_row(
    R="comb_unwellcol_max",
    name = "Any colleagues unwell due to COVID-19",
    full="",
    vartype="personal",
    #code=dictionary %>% filter(R=="age") %>% pull(code),
    #level=dictionary %>% filter(R=="age") %>% pull(level),
    short=NULL
  ) %>%
  add_row(
    R="comb_covidpersonal",
    name = "Personal COVID-19 diagnosis or hospital admission",
    full="",
    vartype="personal",
    #code=dictionary %>% filter(R=="age") %>% pull(code),
    #level=dictionary %>% filter(R=="age") %>% pull(level),
    short=NULL
  ) %>%
  add_row(
    R="comb_dept2",
    name = "Department",
    full="Department",
    plottype="bar",
    vartype="professional",
    code=NULL,
    level=list(c("ED", "anaes", "ICU", "AMU", "anaes+ICU", "Other")),
    short=NULL
  ) %>%
  add_row(
    R="comb_dept3",
    name = "Department",
    full="Department",
    plottype="bar",
    vartype="professional",
    code=NULL,
    level=list(c("anaes", "anaes+ICU", "ED", "ED+anaes", "ICU", "ED+ICU", "ED+anaes+ICU")),
    short=NULL
  ) %>%
  add_row(
    R="ghqcase_s1",
    name = "GHQ12-0011 case identification",
    full="",
    plottype="bar",
    vartype="personal",
  ) %>%
  add_row(
    R="redeployed_satisfaction_s1",
    name="If you have been redeployed, how satisfied are you with this redeployment?",
    full="",
    plottype="bar",
    vartype="personal",
  ) %>%
  add_row(
    R="missedshifts_s1",
    name="How many clinical shifts in your rota have you missed due to self-isolation?",
    full="",
    plottype="bar",
    vartype="personal",
  ) 

write_rds(factor_dct, here::here("processed-data", "factor_dictionary.rds"))

