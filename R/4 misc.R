# distress by discipline, controlling for age, seniority


data_s1 <- read_rds(file=here::here("processed-data",  "data_s1.rds")) 


mod0_dept <- lm(formula = ghqsum0123_s1 ~ dept_ed + dept_anaes+ dept_icu, data = data_s1)
mod1_dept <- lm(formula = ghqsum0123_s1 ~ dept_ed + dept_anaes+ dept_icu + age + seniority, data = data_s1)

summary(mod0_dept)
summary(mod1_dept)


mod0_specialty <- lm(formula = ghqsum0123_s1 ~ comb_specialty2, data = data_s1)
mod1_specialty <- lm(formula = ghqsum0123_s1 ~ comb_specialty2 + age + seniority, data = data_s1)

summary(mod0_specialty)
summary(mod1_specialty)

