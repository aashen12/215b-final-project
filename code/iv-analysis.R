library(tidyverse)
library(nnet)
library(xtable)
library(vcd)
library(patchwork)
library(fastDummies)
set.seed(12)

user <- "andy"

if (user == "andy") {
  setwd("~/Desktop/215b-final-project/code")
}

df_iv <- read_csv("processed-data/iv_analysis_file.csv") %>% 
  dplyr::select(-smoke_household) %>% 
  dplyr::mutate_at(
    vars(intoxication, 
         intercourse_dummy, intox_most_recent_sex_words, 
         contraception_use_words, contraception_sex, 
         parent_drink_freq_words, parent_drunk_words, 
         alc_in_home, adult_care), 
    factor
  ) %>% na.omit()

df_iv$sex <- as.character(df_iv$sex)

df_iv$intoxication <- relevel(df_iv$intoxication %>% factor(levels = c("never", "occasionally", "frequently")), 
                              ref = "never")
df_iv$intox_most_recent_sex_words <- relevel(df_iv$intox_most_recent_sex_words %>% factor(levels = c("never", "sober", "intoxicated")), 
                                             ref = "never")

df_iv$contraception_use_words <- relevel(df_iv$contraception_use_words, 
                                         ref = "use")
df_iv$contraception_sex <- relevel(df_iv$contraception_sex, 
                                   ref = "use")

df_iv$parent_drink_freq_words <- relevel(df_iv$parent_drink_freq_words %>% factor(levels = c("never", "occasionally", "frequently")), 
                                         ref = "never")
df_iv$parent_drunk_words <- relevel(df_iv$parent_drunk_words, 
                                    ref = "never")

df_iv %>% sample_n(12)

df_iv_sex <- df_iv %>% dplyr::filter(intercourse_dummy == 1)

df_iv_sex$contraception_sex <- droplevels(df_iv_sex$contraception_sex)
levels(df_iv_sex$contraception_sex)

df_iv_sex$contraception_use_words <- droplevels(df_iv_sex$contraception_use_words)
levels(df_iv_sex$contraception_use_words)

#### Parent alcoholism instrument ####

chisq.test(table(df_iv_sex$parent_drink_freq_words, df_iv_sex$intoxication))
rcompanion::cramerV(df_iv_sex$parent_drink_freq_words, df_iv_sex$intoxication)

chisq.test(df_iv_sex$parent_drink_freq_words, df_iv_sex$contraception_use_words)
rcompanion::cramerV(df_iv_sex$parent_drink_freq_words, df_iv_sex$contraception_use_words)

chisq.test(df_iv_sex$parent_drink_freq_words, df_iv_sex$contraception_sex)
rcompanion::cramerV(df_iv_sex$parent_drink_freq_words, df_iv_sex$contraception_sex)


#### Alc in home instrument ####

chisq.test(table(df_iv_sex$alc_in_home, df_iv_sex$intoxication))
rcompanion::cramerV(df_iv_sex$alc_in_home, df_iv_sex$intoxication)

chisq.test(df_iv_sex$alc_in_home, df_iv_sex$contraception_use_words)
rcompanion::cramerV(df_iv_sex$alc_in_home, df_iv_sex$contraception_use_words)

chisq.test(df_iv_sex$alc_in_home, df_iv_sex$contraception_sex)
rcompanion::cramerV(df_iv_sex$alc_in_home, df_iv_sex$contraception_sex)



fs <- 14

instr_exposure <- data.frame(
  parent_drink = df_iv$parent_drink_freq_words,
  child_drink = df_iv$intoxication
) %>%
  count(parent_drink, child_drink) %>%
  group_by(parent_drink) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>% 
  ggplot(aes(x = parent_drink, y = proportion, fill = child_drink)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Parent Alcohol Use", y = "Proportion", 
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
# ggsave(filename = "images/parent_child_alc.png")



instr_outcome <- data.frame(
  parent_drink = df_iv$parent_drink_freq_words,
  contraception = df_iv$contraception_use_words
) %>%
  count(parent_drink, contraception) %>%
  group_by(parent_drink) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>% 
  ggplot(aes(x = parent_drink, y = proportion, fill = contraception)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Parent Alcohol Use", y = "Proportion", 
       fill = "Resp. Contraception Use") +
  theme_bw() + theme(
    text = element_text(size = fs),
    axis.title = element_text(size = fs + 2),
    axis.text = element_text(size = fs),
    legend.text = element_text(size = fs),
    legend.title = element_text(size = fs + 1),
    legend.position = "top",
    plot.caption = element_text(size = fs)
  )
instr_outcome
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


first_stage <- nnet::multinom(intoxication ~ parent_drink_freq_words, data = df_iv_sex)
first_stage_preds <- first_stage$residuals %>% abs()

dummy_exposure <- data.frame(model.matrix(~df_iv_sex$intoxication - 1)) %>% tibble()
names(dummy_exposure) <- c("never", "occasionally", "frequently")

resids <- (dummy_exposure - first_stage_preds) %>% tibble()



