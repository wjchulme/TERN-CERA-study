
library("tidyverse")


data_all <- read_rds(file = here::here("processed-data", "data_all.rds"))


# filter duplicates / incomplete responses etc ----
# remove email duplicates by keeping last record according to timestamp1
# remove unconsented entries
# remove when hospital field missing


# can only run with access to emails
data_distinctemails <- data_all %>%
  arrange(
    email, desc(consent_s1),
    desc(compl_hosp), desc(compl_grade), desc(compl_dept), desc(complete_s1 == "Complete"), desc(timestamp_s1)
  ) %>%
  filter(!is.na(email)) %>%
  distinct(email, .keep_all = TRUE) %>%
  arrange(timestamp_s1)

# survey 1 data for all those consenting to survey 1, with complete hospital/grade/dept info

data_s1 <- data_distinctemails %>%
  filter(
    consent_s1, # consent_s2=="Yes", #consent_s3 =="Yes",
    compl_hospgradedept == 1,
    # complete1=="Complete"
  ) %>%
  select(c(-ends_with("_s2"), -ends_with("_s3")), consent_s1, consent_s2, consent_s3)

# all survey data for all those consenting to at least survey 1, regardless of drop-out status, with complete hospital/grade/dept info
data_sany <- data_distinctemails %>%
  filter(
    consent_s1, # consent_s2=="Yes", #consent_s3 =="Yes",
    compl_hospgradedept == 1,
    # complete1=="Complete"
  )

# all survey data from those who responded to all surveys
data_s123 <- data_sany %>%
  filter(
    consent_s1, consent_s2, consent_s3,
  )


## long-form: all responses from survey 1 consented participants, regardless of drop-out status (to use for imputation)
local({
  data_baseline <- data_sany %>%
    select(-ends_with("_s1"), -ends_with("_s2"), -ends_with("_s3"))

  data_s1 <- data_sany %>%
    select(ptid, ends_with("_s1")) %>%
    mutate(survey = 1) %>%
    rename_at(
      .vars = vars(ends_with("_s1")),
      .funs = ~ str_remove(., "\\_s1$")
    )

  data_s2 <- data_sany %>%
    select(ptid, ends_with("_s2")) %>%
    mutate(survey = 2) %>%
    rename_at(
      .vars = vars(ends_with("_s2")),
      .funs = ~ str_remove(., "\\_s2$")
    )

  data_s3 <- data_sany %>%
    select(ptid, ends_with("_s3")) %>%
    mutate(survey = 3) %>%
    rename_at(
      .vars = vars(ends_with("_s3")),
      .funs = ~ str_remove(., "\\_s3$")
    )

  data_longany <<- bind_rows(data_s1, data_s2, data_s3) %>%
    left_join(data_baseline, ., by = "ptid") %>%
    mutate(
      surveychr = paste0("s", survey),
      survey_chr = paste0("Survey ", survey),
    )
})




## long-form: all survey responses for those who consented to all surveys
data_long123 <- data_longany %>%
  filter(
    consent_s123
  )

## extracting only participants with complete s1 s2 and s3 ghq data
data_long123_ghq <- data_long123 %>% filter(compl_ghq_s123)
## extracting only participants with complete s1 s2 and s3 iesr data
data_long123_iesr <- data_long123 %>% filter(compl_iesr_s23)


write_rds(x = data_distinctemails, file = here::here("processed-data", "data_distinctemails.rds"))
write_rds(x = data_s1, file = here::here("processed-data", "data_s1.rds"))
write_rds(x = data_s123, file = here::here("processed-data", "data_s123.rds"))
write_rds(x = data_sany, file = here::here("processed-data", "data_sany.rds"))
write_rds(x = data_longany, file = here::here("processed-data", "data_longany.rds"))
write_rds(x = data_long123, file = here::here("processed-data", "data_long123.rds"))
write_rds(x = data_long123_ghq, file = here::here("processed-data", "data_long123_ghq.rds"))
write_rds(x = data_long123_iesr, file = here::here("processed-data", "data_long123_iesr.rds"))

write_csv(x = data_distinctemails, file = here::here("processed-data", "data_distinctemails.csv"))
write_csv(x = data_s1, file = here::here("processed-data", "data_s1.csv"))
write_csv(x = data_s123, file = here::here("processed-data", "data_s123.csv"))
write_csv(x = data_sany, file = here::here("processed-data", "data_sany.csv"))
write_csv(x = data_longany, file = here::here("processed-data", "data_longany.csv"))
write_csv(x = data_long123, file = here::here("processed-data", "data_long123.csv"))
write_csv(x = data_long123_ghq, file = here::here("processed-data", "data_long123_ghq.csv"))
write_csv(x = data_long123_iesr, file = here::here("processed-data", "data_long123_iesr.csv"))

