library(tidyverse)
library(nnet)
library(xtable)
library(vcd)
library(patchwork)
library(latex2exp)
set.seed(12)

user <- "andy"

if (user == "andy") {
  setwd("~/Desktop/215b-final-project/code")
}

df_iv <- read_csv("processed-data/iv_analysis_file.csv") %>% 
  #dplyr::select(-smoke_household) %>% 
  dplyr::mutate_at(
    vars(intoxication, 
         intercourse_dummy, intox_most_recent_sex_words, 
         contraception_use_words, contraception_sex,
         parent_drink_freq_words, parent_drunk_words, smoke_household,
         alc_in_home, adult_care), 
    factor
  )

df_iv$sex <- as.character(df_iv$sex)

df_iv$intoxication <- relevel(df_iv$intoxication %>% factor(levels = c("never", "occasionally", "frequently")), 
                              ref = "never")
df_iv$intox_most_recent_sex_words <- relevel(df_iv$intox_most_recent_sex_words %>% factor(levels = c("never", "sober", "intoxicated")), 
                                             ref = "never")

df_iv$contraception_use_words <- relevel(df_iv$contraception_use_words, 
                                         ref = "use")


df_iv$parent_drink_freq_words <- relevel(df_iv$parent_drink_freq_words %>% factor(levels = c("never", "occasionally", "frequently")), 
                                         ref = "never")
df_iv$parent_drunk_words <- relevel(df_iv$parent_drunk_words, 
                                    ref = "never")


df_iv$contraception_sex <- relevel(df_iv$contraception_sex, 
                                   ref = "use")



df_iv %>% sample_n(12)

df_iv$intoxication_bin <- ifelse(df_iv$intoxication %in% c("never"), "no", "yes") %>% factor()

df_iv$alc_in_home <- ifelse(df_iv$alc_in_home== 0, "no", "yes") %>% factor()



#### Parent alcoholism instrument ####

chisq.test(table(df_iv$parent_drink_freq_words, df_iv$intoxication_bin))
rcompanion::cramerV(df_iv_sex$parent_drink_freq_words, df_iv_sex$intoxication_bin)




#### Parent drunk instrument ####

chisq.test(table(df_iv_sex$parent_drunk_words, df_iv_sex$intoxication_bin))
rcompanion::cramerV(df_iv_sex$parent_drunk_words, df_iv_sex$intoxication_bin)



#### Alc in home instrument ####

chisq.test(table(df_iv$smoke_household, df_iv$intoxication_bin))
rcompanion::cramerV(df_iv$alc_in_home, df_iv$intoxication_bin)



fs <- 14


drink <- data.frame(
  parent_drink = df_iv$alc_in_home,
  child_drink = df_iv$intoxication_bin
) %>%
  count(parent_drink, child_drink) %>%
  group_by(parent_drink) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>% tibble

instr_exposure <- data.frame(
  parent_drink = df_iv$alc_in_home,
  child_drink = df_iv$intoxication_bin
) %>%
  count(parent_drink, child_drink) %>%
  group_by(parent_drink) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>% 
  ggplot(aes(x = parent_drink, y = proportion, fill = child_drink)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Alcohol in Home", y = "Proportion", 
       fill = "Resp. Alcohol Use") +
  theme_bw() + theme(
    text = element_text(size = fs),
    axis.title = element_text(size = fs + 2),
    axis.text = element_text(size = fs),
    legend.text = element_text(size = fs),
    legend.title = element_text(size = fs + 1),
    legend.position = "top",
    plot.caption = element_text(size = fs)
  )
instr_exposure
ggsave(filename = "images/parent_child_alc.png")



# instr_outcome <- data.frame(
#   parent_drink = df_iv$parent_drink_freq_words,
#   contraception = df_iv$contraception_use_words
# ) %>%
#   count(parent_drink, contraception) %>%
#   group_by(parent_drink) %>%
#   mutate(proportion = n / sum(n)) %>%
#   ungroup() %>% 
#   ggplot(aes(x = parent_drink, y = proportion, fill = contraception)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "Parent Alcohol Use", y = "Proportion", 
#        fill = "Resp. Contraception Use") +
#   theme_bw() + theme(
#     text = element_text(size = fs),
#     axis.title = element_text(size = fs + 2),
#     axis.text = element_text(size = fs),
#     legend.text = element_text(size = fs),
#     legend.title = element_text(size = fs + 1),
#     legend.position = "top",
#     plot.caption = element_text(size = fs)
#   )
# instr_outcome
# ggsave(filename = "images/parent_contraception.png")


# 2SRI method

# First-stage multinomial logistic regression: 
#   Regress the categorical treatment variable on the categorical instrument and 
#   other exogenous variables using multinomial logistic regression. 
#   Save the predicted probabilities and calculate the residuals 
#   for each category of the treatment variable.

# Second-stage logistic regression: 
#   Include the residuals from the first-stage multinomial logistic regression as 
#   additional covariates in the original logistic regression model with the 
#   binary outcome variable, the categorical treatment variable, and other control variables.


instruments <- c("parent_drink_freq_words", "alc_in_home", "smoke_household")
df_iv_sex <- df_iv %>% dplyr::filter(intercourse_dummy == 1) %>% na.omit()

coef_sd_mat <- matrix(nrow = 3, ncol = 3)
rownames(coef_sd_mat) <- instruments
colnames(coef_sd_mat) <- c("est", "sd", "p_val")




for (i in seq_along(instruments)) {
  instrument <- instruments[i]
  ##################################################################
  ##                      Sexual Intercourse                      ##
  ##################################################################
  
  df_iv_intercourse <- df_iv[!is.na(df_iv[[instrument]]), ]
  
  #### Parent alcoholism instrument ####
  
  chisq.test(table(df_iv_intercourse$smoke_household, df_iv_intercourse$intoxication_bin))
  rcompanion::cramerV(df_iv_intercourse$parent_drink_freq_words, df_iv_intercourse$intoxication_bin)
  
  first_stage <- glm(intoxication_bin ~ smoke_household, data = df_iv_intercourse, family = "binomial")
  pred_probs <- predict(first_stage, type = "response")
  first_stage_preds <- ifelse(
    pred_probs > min(pred_probs) + 0.001,
    "yes", 
    "no"
  ) %>% factor()
  summary(first_stage)
  first_stage_fitted <- fitted(first_stage) %>% data.frame() %>% tibble()
  first_stage_resids <- first_stage$residuals
  df_2nd <- bind_cols(df_iv_intercourse, first_stage_preds = first_stage_preds, first_stage_resids = first_stage_resids)
  second_stage_tot <- glm(intercourse_dummy ~ first_stage_preds + first_stage_resids,
                          data = df_2nd, family = "binomial")
  summary(second_stage_tot)
  
  #################################################################
  ##                      Contraception Use                      ##
  #################################################################
  
  
  df_iv_sex <- df_iv_sex[!is.na(df_iv_sex[[instrument]]), ]
  df_iv_sex$contraception_sex <- droplevels(df_iv_sex$contraception_sex)
  levels(df_iv_sex$contraception_sex)
  
  df_iv_sex$contraception_use_words <- droplevels(df_iv_sex$contraception_use_words)
  levels(df_iv_sex$contraception_use_words)
  
  chisq.test(table(df_iv_sex$smoke_household, df_iv_sex$intoxication_bin))
  rcompanion::cramerV(df_iv_sex$alc_in_home, df_iv_sex$intoxication_bin)
  
  first_stage <- glm(intoxication_bin ~ smoke_household, data = df_iv_sex, family = "binomial")
  summary(first_stage)
  pred_probs <- predict(first_stage, type = "response")
  first_stage_preds <- ifelse(
    pred_probs > min(pred_probs) + 0.001,
    "yes", 
    "no"
  ) %>% factor()
  first_stage_fitted <- fitted(first_stage) %>% data.frame() %>% tibble()
  first_stage_resids <- first_stage$residuals
  df_2nd <- bind_cols(df_iv_sex, first_stage_preds = first_stage_preds, first_stage_resids = first_stage_resids)
  second_stage_tot <- glm(contraception_use_words ~ first_stage_preds + first_stage_resids,
                          data = df_2nd, family = "binomial")
  summary(second_stage_tot)
  
  
}


##################################################################
##                      Sexual Intercourse                      ##
##################################################################

df_iv_intercourse <- df_iv[!is.na(df_iv$smoke_household), ]

#### Parent alcoholism instrument ####

chisq.test(table(df_iv_intercourse$smoke_household, df_iv_intercourse$intoxication_bin))
rcompanion::cramerV(df_iv_intercourse$parent_drink_freq_words, df_iv_intercourse$intoxication_bin)

first_stage <- glm(intoxication_bin ~ smoke_household, data = df_iv_intercourse, family = "binomial")
first_stage_preds <- ifelse(
  predict(first_stage, type = "response") > 0.24,
  "yes",
  "no"
) %>% factor()
summary(first_stage)
first_stage_fitted <- fitted(first_stage) %>% data.frame() %>% tibble()
first_stage_resids <- first_stage$residuals
df_2nd <- bind_cols(df_iv_intercourse, first_stage_preds = first_stage_preds, first_stage_resids = first_stage_resids)
second_stage_tot <- glm(intercourse_dummy ~ first_stage_preds + first_stage_resids,
                    data = df_2nd, family = "binomial")
summary(second_stage_tot)


### Males ###


first_stage <- glm(intoxication_bin ~ parent_drink_freq_words, data = df_iv_intercourse %>% filter(sex == "1"), family = "binomial")
first_stage_preds <- ifelse(
  predict(first_stage, type = "response") > 0.24,
  "yes",
  "no"
) %>% factor()
first_stage_fitted <- fitted(first_stage) %>% data.frame() %>% tibble()
first_stage_resids <- first_stage$residuals
df_2nd <- bind_cols(df_iv_intercourse %>% filter(sex == "1"), first_stage_preds = first_stage_preds, first_stage_resids = first_stage_resids)
second_stage_tot <- glm(intercourse_dummy ~ first_stage_preds + first_stage_resids,
                        data = df_2nd, family = "binomial")
summary(second_stage_tot)


### Females ###


first_stage <- glm(intoxication_bin ~ parent_drink_freq_words, data = df_iv_intercourse %>% filter(sex == "2"), family = "binomial")
first_stage_preds <- ifelse(
  predict(first_stage, type = "response") > 0.24,
  "yes",
  "no"
) %>% factor()
first_stage_fitted <- fitted(first_stage) %>% data.frame() %>% tibble()
first_stage_resids <- first_stage$residuals
df_2nd <- bind_cols(df_iv_intercourse %>% filter(sex == "2"), first_stage_preds = first_stage_preds, first_stage_resids = first_stage_resids)
second_stage_tot <- glm(intercourse_dummy ~ first_stage_preds + first_stage_resids,
                        data = df_2nd, family = "binomial")
summary(second_stage_tot)



# second_stage_spur <- nnet::multinom(intox_most_recent_sex_words ~ first_stage_preds, data = df_2nd, family = "binomial")
# summary(second_stage_spur)
# spur_m_us <- coefficients(second_stage_spur)[2,2] / coefficients(second_stage_tot)[2]
# 
# 
# first_stage <- glm(intoxication_bin ~ alc_in_home, data = df_iv_sex %>% filter(sex == "2"), family = "binomial")
# first_stage_preds <- ifelse(
#   predict(first_stage, type = "response") > 0.5,
#   "yes",
#   "no"
# ) %>% factor()
# first_stage_fitted <- fitted(first_stage) %>% data.frame() %>% tibble()
# first_stage_resids <- first_stage$residuals
# df_2nd <- bind_cols(df_iv_sex %>% filter(sex == "2"), first_stage_preds = first_stage_preds, first_stage_resids = first_stage_resids)
# second_stage_tot <- glm(intercourse_dummy ~ first_stage_preds + first_stage_resids,
#                         data = df_2nd, family = "binomial")
# summary(second_stage_tot)
# second_stage_spur <- nnet::multinom(intox_most_recent_sex_words ~ first_stage_preds, data = df_2nd, family = "binomial")
# summary(second_stage_spur)
# spur_f_us <- coefficients(second_stage_spur)[2,2] / coefficients(second_stage_tot)[2]
# 
# spur_us <- c(spur_mf_us, spur_m_us, spur_f_us) * 100
# names(spur_us) <- c("all_gender", "males", "females")
# spur_us 
# 
# 
# felson_iv_1 <- glm(contraception_use_words ~ intoxication_bin,
#                  data = df_iv_sex, family = "binomial")
# summary(felson_iv_1)
# felson_iv_2 <- nnet::multinom(contraception_sex ~ intoxication_bin, 
#                               data = df_iv_sex, family = "binomial")
# summary(felson_iv_2)
# spur_mf_fel <- coefficients(felson_iv_2)[2,2] / coefficients(felson_iv_1)[2]
# 
# felson_iv_1 <- glm(contraception_use_words ~ intoxication_bin,
#                    data = df_iv_sex %>% filter(sex == "1"), family = "binomial")
# summary(felson_iv_1)
# felson_iv_2 <- nnet::multinom(contraception_sex ~ intoxication_bin, 
#                               data = df_iv_sex %>% filter(sex == "1"), family = "binomial")
# summary(felson_iv_2)
# spur_m_fel <- coefficients(felson_iv_2)[2,2] / coefficients(felson_iv_1)[2]
# 
# felson_iv_1 <- glm(contraception_use_words ~ intoxication_bin,
#                    data = df_iv_sex %>% filter(sex == "2"), family = "binomial")
# summary(felson_iv_1)
# felson_iv_2 <- nnet::multinom(contraception_sex ~ intoxication_bin, 
#                               data = df_iv_sex %>% filter(sex == "2"), family = "binomial")
# summary(felson_iv_2)
# spur_f_fel <- coefficients(felson_iv_2)[2,2] / coefficients(felson_iv_1)[2]
# 
# spur_fel <- c(spur_mf_fel, spur_m_fel, spur_f_fel) * 100
# spur_fel
# 
# df_iv <- data.frame(
#   "IV_Analysis" = spur_us,
#   "Felson" = spur_fel
# ) %>% tibble()
# rownames(df_iv) <- c("all_gender", "males", "females")
# 
# print(xtable::xtable(df_iv %>% rownames_to_column("Gender"), digits = 1, 
#                      caption = "Spuriousness coefficients rho from using 2SPS/2SRI and Felson's original method.", 
#                      label = "table:iv_spuriousness"), 
#       booktabs = TRUE, 
#       align ="|c|c|")



#################################################################
##                      Contraception Use                      ##
#################################################################


df_iv_sex <- df_iv_sex[!is.na(df_iv_sex$smoke_household), ]
df_iv_sex$contraception_sex <- droplevels(df_iv_sex$contraception_sex)
levels(df_iv_sex$contraception_sex)

df_iv_sex$contraception_use_words <- droplevels(df_iv_sex$contraception_use_words)
levels(df_iv_sex$contraception_use_words)

chisq.test(table(df_iv_sex$smoke_household, df_iv_sex$intoxication_bin))
rcompanion::cramerV(df_iv_sex$alc_in_home, df_iv_sex$intoxication_bin)

first_stage <- glm(intoxication_bin ~ smoke_household, data = df_iv_sex, family = "binomial")
summary(first_stage)
first_stage_preds <- ifelse(
  predict(first_stage, type = "response") > 0.45,
  "yes",
  "no"
) %>% factor()
first_stage_fitted <- fitted(first_stage) %>% data.frame() %>% tibble()
first_stage_resids <- first_stage$residuals
df_2nd <- bind_cols(df_iv_sex, first_stage_preds = first_stage_preds, first_stage_resids = first_stage_resids)
second_stage_tot <- glm(contraception_use_words ~ first_stage_preds + first_stage_resids,
                        data = df_2nd, family = "binomial")
summary(second_stage_tot)





felson_iv_1 <- glm(contraception_use_words ~ intoxication_bin,
                   data = df_iv_sex, family = "binomial")
summary(felson_iv_1)
felson_iv_2 <- nnet::multinom(contraception_sex ~ intoxication_bin, 
                              data = df_iv_sex, family = "binomial")
summary(felson_iv_2)
spur_mf_fel <- coefficients(felson_iv_2)[2,2] / coefficients(felson_iv_1)[2]

felson_iv_1 <- glm(contraception_use_words ~ intoxication_bin,
                   data = df_iv_sex %>% filter(sex == "1"), family = "binomial")
summary(felson_iv_1)
felson_iv_2 <- nnet::multinom(contraception_sex ~ intoxication_bin, 
                              data = df_iv_sex %>% filter(sex == "1"), family = "binomial")
summary(felson_iv_2)
spur_m_fel <- coefficients(felson_iv_2)[2,2] / coefficients(felson_iv_1)[2]

felson_iv_1 <- glm(contraception_use_words ~ intoxication_bin,
                   data = df_iv_sex %>% filter(sex == "2"), family = "binomial")
summary(felson_iv_1)
felson_iv_2 <- nnet::multinom(contraception_sex ~ intoxication_bin, 
                              data = df_iv_sex %>% filter(sex == "2"), family = "binomial")
summary(felson_iv_2)
spur_f_fel <- coefficients(felson_iv_2)[2,2] / coefficients(felson_iv_1)[2]

spur_fel <- c(spur_mf_fel, spur_m_fel, spur_f_fel)
spur_fel

df_iv <- data.frame(
  "ivSD" = spur_us,
  "Felson_Binary" = spur_fel
)
rownames(df_iv) <- c("all", "males", "females")

print(xtable::xtable(df_iv %>% rownames_to_column("Gender"), digits = 1, 
                     caption = "Spuriousness coefficients rho from using 2SPS/2SRI and Felson's original method.", 
                     label = "table:iv_spuriousness"), 
      booktabs = TRUE, 
      align ="|c|c|")


all_rho_df <- df_iv %>% 
  dplyr::mutate(
    Our_Reproduced = c(13.2, -1.9, 42.6) / 100,
    Felson_Reported = c(46.9, 41.1, 68.3) / 100
  )

all_rho_df %>% 
  rownames_to_column("Gender") %>% 
  dplyr::relocate(Gender, .before = "ivSD") %>% 
  tidyr::pivot_longer(cols = ivSD:Felson_Reported, names_to = "Method", values_to = "rho") %>% 
  dplyr::mutate(Method = factor(Method, levels = c("ivSD", "Felson_Reported", "Felson_Binary", "Our_Reproduced"))) %>% 
  ggplot(aes(x = Method, y = rho, fill = Gender)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(x = "Model", y = TeX("$\\widehat{\\rho}$"), title = "Spuriousness coefficients with various situational decomposition methods") +
  theme_bw()



