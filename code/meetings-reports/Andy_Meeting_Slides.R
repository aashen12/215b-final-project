library(tidyverse)
library(kableExtra)
library(htmltools)
library(xtable)
library(nnet)


## --------------------------------------------------------------------------------------------------------------------------------------------------
# reading in data and recoding it
df_first_study <- read_csv("../processed-data/intox_sex_file.csv")

df_first_study$sex <- as.character(df_first_study$sex)

df_first_study$intoxication <- factor(df_first_study$intoxication)
df_first_study$intoxication <- relevel(df_first_study$intoxication, ref = "never")

df_first_study$intercourse_dummy <- factor(df_first_study$intercourse_dummy)

df_first_study$intox_most_recent_sex_words <- factor(df_first_study$intox_most_recent_sex_words)
df_first_study$intox_most_recent_sex_words <- relevel(df_first_study$intox_most_recent_sex_words, ref = "never")


df_1st_logistic <- df_first_study # %>% na.omit()

# creating dummy variables for intoxication in case we need it later.
# df_1st_logistic <- df_first_study %>% 
#   dplyr::mutate(
#     never_intox = ifelse(intoxication == "never", 1, 0),
#     occasion_intox = ifelse(intoxication == "occasionally", 1, 0),
#     frequently_intox = ifelse(intoxication == "frequently", 1, 0)
#   )


## ---- eval=TRUE------------------------------------------------------------------------------------------------------------------------------------
# running inividual logistic regressions
tassoc_1st <- glm(intercourse_dummy ~ intoxication, 
                  data = df_1st_logistic, 
                  family = "binomial")
# summary(tassoc_1st)
odds_ratio_mf <- exp(tassoc_1st$coefficients) #%>% log()

tassoc_male_1st <- glm(intercourse_dummy ~ intoxication, 
                  data = df_1st_logistic %>% dplyr::filter(sex == 1), 
                  family = "binomial")
# summary(tassoc_male_1st)
odds_ratio_m <- exp(tassoc_male_1st$coefficients) #%>% log()

tassoc_female_1st <- glm(intercourse_dummy ~ intoxication, 
                  data = df_1st_logistic %>% dplyr::filter(sex == 2), 
                  family = "binomial")
# summary(tassoc_female_1st)
odds_ratio_f <- exp(tassoc_female_1st$coefficients) #%>% log()


## --------------------------------------------------------------------------------------------------------------------------------------------------
# inputting our numbers into a table for display
df_1st_OR <- data.frame(
  all_gender = odds_ratio_mf[-1] %>% rev(),
  male = odds_ratio_m[-1] %>% rev(),
  female = odds_ratio_f[-1] %>% rev() # do not need intercept coef
) %>% t() %>% data.frame() %>% tibble()
rownames(df_1st_OR) <- c("all_gender", "male", "female")
names(df_1st_OR) <- c("Occasionally", "Frequently")

df_1st_OR_kbl <- df_1st_OR %>% 
  rownames_to_column("Gender")
df_1st_OR_kbl
print(xtable::xtable(df_1st_OR_kbl, digits = 1, 
                     caption = "Our replicated odds ratios from regressing intercourse on alcohol.", 
                     label = "table:our_or_1st"), 
      booktabs = TRUE, 
      align ="|c|c|c|")


## --------------------------------------------------------------------------------------------------------------------------------------------------
# inputting felson's numbers
df_1st_Felson <- data.frame(
  all_gender = c(3.98, 8.48),
  male = c(3.57, 8.69),
  female = c(4.41, 7.69)
) %>% t() %>% data.frame() %>% tibble()
rownames(df_1st_Felson) <- c("all_gender", "male", "female")
names(df_1st_Felson) <- c("Occasionally", "Frequently")

# display purposes
df_1st_Felson_kbl <- df_1st_Felson %>% 
  rownames_to_column("Gender")
print(xtable::xtable(df_1st_Felson_kbl, digits = 1, 
                     caption = "Felson's reported odds ratios from regressing intercourse on alcohol.", 
                     label = "table:felson_or_1st"), 
      booktabs = TRUE, 
      align ="|c|c|c|")


## --------------------------------------------------------------------------------------------------------------------------------------------------
spurious_assoc_1st <- nnet::multinom(intox_most_recent_sex_words ~ intoxication,
                                     data = df_1st_logistic, trace = FALSE)
# summary(spurious_assoc_1st)


## --------------------------------------------------------------------------------------------------------------------------------------------------
spur_coefs_all <- coef(spurious_assoc_1st)[, -1][-1, ] %>% rev()
tot_coefs_all <- tassoc_1st$coefficients[-1] %>% rev()
# spur_coefs_all; tot_coefs_all
spuriousness_all <- spur_coefs_all / tot_coefs_all * 100


## --------------------------------------------------------------------------------------------------------------------------------------------------
spurious_assoc_male_1st <- nnet::multinom(intox_most_recent_sex_words ~ intoxication,
                                     data = df_1st_logistic %>% dplyr::filter(sex == 1),
                                     trace = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------
spur_coefs_male <- coef(spurious_assoc_male_1st)[, -1][-1, ] %>% rev()
tot_coefs_male <- tassoc_male_1st$coefficients[-1] %>% rev()
spuriousness_male <- spur_coefs_male / tot_coefs_male * 100


## --------------------------------------------------------------------------------------------------------------------------------------------------
spurious_assoc_female_1st <- nnet::multinom(intox_most_recent_sex_words ~ intoxication,
                                     data = df_1st_logistic %>% dplyr::filter(sex == 2),
                                     trace = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------
spur_coefs_female <- coef(spurious_assoc_female_1st)[, -1][-1, ] %>% rev()
tot_coefs_female <- tassoc_female_1st$coefficients[-1] %>% rev()
spuriousness_female <- spur_coefs_female / tot_coefs_female * 100


## --------------------------------------------------------------------------------------------------------------------------------------------------
spur_df <- data.frame(
  "all_gender"  = spuriousness_all,
  "males" = spuriousness_male,
  "females" = spuriousness_female
) %>% t() %>% data.frame()
names(spur_df) <- c("Occasionally", "Frequently")
spur_df
print(xtable::xtable(spur_df, digits = 1, 
                     caption = "Felson's reported odds ratios from regressing intercourse on alcohol.", 
                     label = "table:felson_or_1st"), 
      booktabs = TRUE, 
      align ="|c|c|")

## --------------------------------------------------------------------------------------------------------------------------------------------------
felson_1st_spur <- data.frame(
  "Occasionally" = c(95.7, 95.3, 97.3),
  "Frequently" = c(91.6, 91.2, 93.6)
)
rownames(felson_1st_spur) <- c("all_gender", "males", "females")
felson_1st_spur %>% 
  knitr::kable(booktabs = TRUE,
               format = 'latex',
               caption = "Felson's spuriousness values") %>% 
  kableExtra::kable_styling(font_size = 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------
df_second_study <- read_csv("../processed-data/intox_contraception_file.csv") %>% 
  dplyr::select(-intox_most_recent_sex, -intercourse_dummy, -contraception_use, -intox_most_recent_sex_words) %>% 
  dplyr::mutate_at(vars(sex, intoxication, contraception_use_words, contraception_sex), factor)


## --------------------------------------------------------------------------------------------------------------------------------------------------
df_second_study$contraception_sex <- relevel(df_second_study$contraception_sex, ref = "use")
df_second_study$contraception_use_words <- relevel(df_second_study$contraception_use_words, ref = "use")
df_second_study$intoxication <- relevel(df_second_study$intoxication, ref = "never")

df_2nd_logistic <- df_second_study


## ---- eval=TRUE------------------------------------------------------------------------------------------------------------------------------------
# running inividual logistic regressions
tassoc_2nd <- glm(contraception_use_words ~ intoxication, 
                  data = df_2nd_logistic, 
                  family = "binomial")
# summary(tassoc_2nd)
odds_ratio_mf <- exp(tassoc_2nd$coefficients) #%>% log()

tassoc_male_2nd <- glm(contraception_use_words ~ intoxication, 
                  data = df_2nd_logistic %>% dplyr::filter(sex == 1), 
                  family = "binomial")
# summary(tassoc_male_2nd)
odds_ratio_m <- exp(tassoc_male_2nd$coefficients) #%>% log()

tassoc_female_2nd <- glm(contraception_use_words ~ intoxication, 
                  data = df_2nd_logistic %>% dplyr::filter(sex == 2), 
                  family = "binomial")
# summary(tassoc_female_2nd)
odds_ratio_f <- exp(tassoc_female_2nd$coefficients) #%>% log()


## --------------------------------------------------------------------------------------------------------------------------------------------------
# inputting our numbers into a table for display
df_2nd_OR <- data.frame(
  all_gender = odds_ratio_mf[-1] %>% rev(),
  male = odds_ratio_m[-1] %>% rev(),
  female = odds_ratio_f[-1] %>% rev() # do not need intercept coef
) %>% t() %>% data.frame() %>% tibble()
rownames(df_2nd_OR) <- c("all_gender", "male", "female")
names(df_2nd_OR) <- c("Occasionally", "Frequently")

df_2nd_OR_kbl <- df_2nd_OR %>% 
  rownames_to_column("Gender") %>% 
  dplyr::mutate(OR_diff = abs(Occasionally - Frequently)) %>% 
  kable(format = "latex", booktabs = TRUE, digits = 2,
      caption = "Total association logistic regression odds ratios (Andy).") %>%
  kableExtra::kable_styling(font_size = 10)
df_2nd_OR_kbl


## --------------------------------------------------------------------------------------------------------------------------------------------------
# inputting felson's numbers
df_2nd_Felson <- data.frame(
  all_gender = c(1.05, 1.38),
  male = c(1.00, 1.40),
  female = c(1.07, 1.51)
) %>% t() %>% data.frame() %>% tibble()
rownames(df_2nd_Felson) <- c("all_gender", "male", "female")
names(df_2nd_Felson) <- c("Occasionally", "Frequently")

# display purposes
df_2nd_Felson_kbl <- df_2nd_Felson %>% 
  rownames_to_column("Gender") %>% 
  dplyr::mutate(OR_diff = abs(Occasionally - Frequently)) %>% 
  kable(format = "latex", booktabs = TRUE, digits = 2,
      caption = "Total association logistic regression odds ratios (Felson et al.).") %>% 
  kableExtra::kable_styling(font_size = 10)
df_2nd_Felson_kbl


## --------------------------------------------------------------------------------------------------------------------------------------------------
spurious_assoc_2nd <- nnet::multinom(contraception_sex ~ intoxication,
                                     data = df_2nd_logistic, trace = FALSE)
# summary(spurious_assoc_2nd)


## --------------------------------------------------------------------------------------------------------------------------------------------------
spur_coefs_all <- coef(spurious_assoc_2nd)[, -1][-1, ] %>% rev()
tot_coefs_all <- tassoc_2nd$coefficients[-1] %>% rev()
# spur_coefs_all; tot_coefs_all
spuriousness_all <- spur_coefs_all / tot_coefs_all * 100


## --------------------------------------------------------------------------------------------------------------------------------------------------
spurious_assoc_male_2nd <- nnet::multinom(contraception_sex ~ intoxication,
                                     data = df_2nd_logistic %>% dplyr::filter(sex == 1),
                                     trace = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------
spur_coefs_male <- coef(spurious_assoc_male_2nd)[, -1][-1, ] %>% rev()
tot_coefs_male <- tassoc_male_2nd$coefficients[-1] %>% rev()
spuriousness_male <- spur_coefs_male / tot_coefs_male * 100


## --------------------------------------------------------------------------------------------------------------------------------------------------
spurious_assoc_female_2nd <- nnet::multinom(contraception_sex ~ intoxication,
                                     data = df_2nd_logistic %>% dplyr::filter(sex == 2),
                                     trace = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------
spur_coefs_female <- coef(spurious_assoc_female_2nd)[, -1][-1, ] %>% rev()
tot_coefs_female <- tassoc_female_2nd$coefficients[-1] %>% rev()
spuriousness_female <- spur_coefs_female / tot_coefs_female * 100


## --------------------------------------------------------------------------------------------------------------------------------------------------
spur_df <- data.frame(
  "all_gender"  = spuriousness_all,
  "males" = spuriousness_male,
  "females" = spuriousness_female
) %>% t() %>% data.frame()
names(spur_df) <- c("Occasionally", "Frequently")
spur_df %>% 
  knitr::kable(digits = 1, 
               booktabs = TRUE,
               format = 'latex',
               caption = "Our spuriousness values") %>% 
  kableExtra::kable_styling(font_size = 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------
felson_2nd_spur <- data.frame(
  "Occasionally" = rep(NA, 3),
  "Frequently" = c(46.9, 41.1, 68.3)
)
rownames(felson_2nd_spur) <- c("all_gender", "males", "females")
felson_2nd_spur %>% 
  knitr::kable(booktabs = TRUE,
               format = 'latex',
               caption = "Felson's spuriousness values") %>% 
  kableExtra::kable_styling(font_size = 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------
knitr::purl("Andy_Meeting_Slides.R")

