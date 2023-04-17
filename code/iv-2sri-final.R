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

df_iv$alc_in_home <- ifelse(df_iv$alc_in_home == 0, "no", "yes") %>% factor()





#### Smoking home instrument ####

chisq.test(table(df_iv$smoke_household, df_iv$intoxication_bin))
rcompanion::cramerV(df_iv$smoke_household, df_iv$intoxication_bin)


#### Alc in home instrument ####

chisq.test(table(df_iv$alc_in_home, df_iv$intoxication_bin))
rcompanion::cramerV(df_iv$alc_in_home, df_iv$intoxication_bin)



#### Parent alcoholism instrument ####

chisq.test(table(df_iv$parent_drink_freq_words, df_iv$intoxication_bin))
rcompanion::cramerV(df_iv$parent_drink_freq_words, df_iv$intoxication_bin)





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
# ggsave(filename = "images/parent_child_alc.png")



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
df_sex <- df_iv %>% dplyr::filter(intercourse_dummy == 1) #%>% na.omit()

iv2SRI <- function(int_df = df_iv, cont_df = df_sex) {
  intercourse_iv <- contraception_iv <- matrix(nrow = 3, ncol = 3)
  rownames(intercourse_iv) <- rownames(contraception_iv) <- instruments
  colnames(intercourse_iv) <- colnames(contraception_iv) <- c("est", "sd", "p_val")
  for (i in seq_along(instruments)) {
    instrument <- instruments[i]
    # print(instrument)
    ##################################################################
    ##                      Sexual Intercourse                      ##
    ##################################################################
    
    df_iv_intercourse <- int_df[!is.na(int_df[[instrument]]), ]
    
    #### Parent alcoholism instrument ####
    
    chisq.test(table(df_iv_intercourse$smoke_household, df_iv_intercourse$intoxication_bin))
    rcompanion::cramerV(df_iv_intercourse$parent_drink_freq_words, df_iv_intercourse$intoxication_bin)
    
    first_stage <- glm(df_iv_intercourse$intoxication_bin ~ df_iv_intercourse[[instrument]], family = "binomial")
    pred_probs <- predict(first_stage, newdata = df_iv_intercourse, type = "response")
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
    summ <- summary(second_stage_tot)
    summ
    intercourse_iv[i, ] <- summ$coefficients[2, c(1:2, 4)]
    
    #################################################################
    ##                      Contraception Use                      ##
    #################################################################
    
    
    df_iv_sex <- cont_df[!is.na(cont_df[[instrument]]), ]
    df_iv_sex$contraception_sex <- droplevels(df_iv_sex$contraception_sex)
    levels(df_iv_sex$contraception_sex)
    
    df_iv_sex$contraception_use_words <- droplevels(df_iv_sex$contraception_use_words)
    levels(df_iv_sex$contraception_use_words)
    
    chisq.test(table(df_iv_sex$smoke_household, df_iv_sex$intoxication_bin))
    rcompanion::cramerV(df_iv_sex$alc_in_home, df_iv_sex$intoxication_bin)
    
    first_stage <- glm(df_iv_sex$intoxication_bin ~ df_iv_sex[[instrument]], family = "binomial")
    summary(first_stage)
    pred_probs <- predict(first_stage, newdata = df_iv_sex, type = "response")
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
    summ <- summary(second_stage_tot)
    summ
    contraception_iv[i, ] <- summ$coefficients[2, c(1:2, 4)]
    
  }
  list(intercourse = intercourse_iv, contraception = contraception_iv)
}

iv_mf <- iv2SRI(int_df = df_iv, cont_df = df_sex)
iv_m <- iv2SRI(df_iv %>% filter(sex == "1"), df_sex %>% filter(sex == "1"))
iv_f <- iv2SRI(df_iv %>% filter(sex == "2"), df_sex %>% filter(sex == "2"))

int_mf <- iv_mf$intercourse %>% data.frame() %>% rownames_to_column("instrument") %>% mutate(gender = "all_gender", study = "intercourse")
con_mf <- iv_mf$contraception %>% data.frame() %>% rownames_to_column("instrument") %>% mutate(gender = "all_gender", study = "contraception")

int_m <- iv_m$intercourse %>% data.frame() %>% rownames_to_column("instrument") %>% mutate(gender = "males", study = "intercourse")
con_m <- iv_m$contraception %>% data.frame() %>% rownames_to_column("instrument") %>% mutate(gender = "males", study = "contraception")

int_f <- iv_f$intercourse %>% data.frame() %>% rownames_to_column("instrument") %>% mutate(gender = "females", study = "intercourse")
con_f <- iv_f$contraception %>% data.frame() %>% rownames_to_column("instrument") %>% mutate(gender = "females", study = "contraception")


intercourse_sd <- contraception_sd <- matrix(NA, 3, 3)
rownames(intercourse_sd) <- rownames(contraception_sd) <- c("all_gender", "males", "females")
colnames(intercourse_sd) <- colnames(contraception_sd) <- c("est", "sd", "p_val")

felson_intercourse_all <- glm(intercourse_dummy ~ intoxication_bin,
                              data = df_iv_intercourse, family = "binomial")
intercourse_sd[1, ] <- summary(felson_intercourse_all)$coefficients[2, c(1:2, 4)]

felson_cont_all <- glm(contraception_use_words ~ intoxication_bin,
                       data = df_iv_sex, family = "binomial")
contraception_sd[1, ] <- summary(felson_cont_all)$coefficients[2, c(1:2, 4)]


felson_intercourse_m <- glm(intercourse_dummy ~ intoxication_bin,
                            data = df_iv_intercourse %>% filter(sex == "1"), family = "binomial")
intercourse_sd[2, ] <- summary(felson_intercourse_m)$coefficients[2, c(1:2, 4)]

felson_cont_m <- glm(contraception_use_words ~ intoxication_bin,
                     data = df_iv_sex %>% filter(sex == "1"), family = "binomial")
contraception_sd[2, ] <- summary(felson_cont_m)$coefficients[2, c(1:2, 4)]


felson_intercourse_f <- glm(intercourse_dummy ~ intoxication_bin,
                            data = df_iv_intercourse %>% filter(sex == "2"), family = "binomial")
intercourse_sd[3, ] <- summary(felson_intercourse_f)$coefficients[2, c(1:2, 4)]

felson_cont_f <- glm(contraception_use_words ~ intoxication_bin,
                     data = df_iv_sex %>% filter(sex == "2"), family = "binomial")
contraception_sd[3, ] <- summary(felson_cont_f)$coefficients[2, c(1:2, 4)]


intercourse_sd <- intercourse_sd %>% data.frame() %>% rownames_to_column("gender") %>% mutate(study = "intercourse", instrument = "none")
contraception_sd <- contraception_sd %>% data.frame() %>% rownames_to_column("gender") %>% mutate(study = "contraception", instrument = "none")


intercourse_sd
int_mf


intercourse_full <- bind_rows(intercourse_sd, int_mf, int_m, int_f) %>% 
  dplyr::mutate(instrument = factor(instrument, levels = c("none", "smoke_household", "alc_in_home", "parent_drink_freq_words")))
contraception_full <- bind_rows(contraception_sd, con_mf, con_m, con_f) %>% 
  dplyr::mutate(instrument = factor(instrument, levels = c("none",  "smoke_household", "alc_in_home", "parent_drink_freq_words")))


capitalize_first <- function(labels) {
  sapply(labels, function(label) {
    paste0(toupper(substr(label, 1, 1)), substr(label, 2, nchar(label)))
  })
}

fs <- 17 # font

intercourse_full %>% 
  bind_rows(contraception_full) %>% 
  dplyr::mutate(lower = est - qnorm(0.975) * sd,
                upper = est + qnorm(0.975) * sd) %>% 
  dplyr::mutate(study = factor(study, levels = c("intercourse", "contraception"))) %>% 
  ggplot(aes(x = gender, y = est, fill = instrument)) + 
  theme_bw() + 
  # geom_point(aes(color = instrument), size = 6) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  scale_fill_manual(values = c("steelblue", "tomato2", "violet", "seagreen2"),
                    labels = c("None", "Smoking Household", "Alcohol in Home", "Parent Alcohol Use")) + 
  scale_x_discrete(labels = c("All Gender", "Females", "Males")) + 
  labs(x = "Gender", y = TeX("Regression Coefficient $\\widehat{\\gamma}$"), fill = "Instrument") + 
  facet_grid(~study, labeller = labeller(study = capitalize_first)) + 
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        text = element_text(size = fs),
        axis.title = element_text(size = fs + 2),
        axis.text = element_text(size = fs),
        legend.text = element_text(size = fs),
        legend.title = element_text(size = fs + 1),
        plot.caption = element_text(size = fs),
        plot.title = element_text(hjust = 0.5)) + 
  #ggtitle(TeX(" 2SRI $\\widehat{\\gamma}$ coefficients by instrument")) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.24, position = position_dodge(0.9))
ggsave("images/iv_coefs.png")



