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


## Instrument vs instrument
chisq.test(table(df_iv$parent_drink_freq_words, df_iv$alc_in_home))
chisq.test(table(df_iv$parent_drink_freq_words, df_iv$smoke_household))
chisq.test(table(df_iv$smoke_household, df_iv$alc_in_home))

# Parent drinking frequency shows the most statistical significance with the other 


# for joint IV analysis, drop parent_drink_freq_words

df_sex <- df_iv %>% dplyr::filter(intercourse_dummy == 1) #%>% na.omit()
# int_df = df_iv; cont_df = df_sex
# i <- 2


ivCombo <- function(int_df = df_iv, cont_df = df_sex) {
  all_instruments <- c("parent_drink_freq_words", "alc_in_home", "smoke_household")
  instruments <- all_instruments[c(2, 3)]
  
  df_iv_intercourse <- int_df[!is.na(int_df[[instruments[1]]]) & !is.na(int_df[[instruments[2]]]), ]
  
  first_stage <- glm(df_iv_intercourse[["intoxication_bin"]] ~ df_iv_intercourse[[instruments[1]]] + df_iv_intercourse[[instruments[2]]], 
                     family = "binomial")
  pred_probs <- predict(first_stage, newdata = df_iv_intercourse, type = "response")
  first_stage_resids <- as.numeric(df_iv_intercourse$intoxication_bin) - 1 - pred_probs
  df_2nd <- bind_cols(df_iv_intercourse, first_stage_resids = first_stage_resids)
  second_stage_tot <- glm(intercourse_dummy ~ intoxication_bin + first_stage_resids,
                          data = df_2nd, family = "binomial")
  summ <- summary(second_stage_tot)
  summ
  intercourse_combo_iv <- summ$coefficients[2, c(1:2, 4)]
  names(intercourse_combo_iv) <- c("est", "sd", "p_val")
  #################################################################
  ##                      Contraception Use                      ##
  #################################################################
  
  
  df_iv_sex <- cont_df[!is.na(cont_df[[instruments[1]]]) & !is.na(cont_df[[instruments[2]]]), ]
  df_iv_sex$contraception_sex <- droplevels(df_iv_sex$contraception_sex)
  levels(df_iv_sex$contraception_sex)
  
  df_iv_sex$contraception_use_words <- droplevels(df_iv_sex$contraception_use_words)
  levels(df_iv_sex$contraception_use_words)
  
  chisq.test(table(df_iv_sex$smoke_household, df_iv_sex$intoxication_bin))
  rcompanion::cramerV(df_iv_sex$alc_in_home, df_iv_sex$intoxication_bin)
  
  first_stage <- glm(df_iv_sex[["intoxication_bin"]] ~ df_iv_sex[[instruments[1]]] + df_iv_sex[[instruments[2]]], 
                     family = "binomial")
  summary(first_stage)
  pred_probs <- predict(first_stage, newdata = df_iv_sex, type = "response")
  first_stage_resids <- as.numeric(df_iv_sex$intoxication_bin) - 1 - pred_probs
  df_2nd <- bind_cols(df_iv_sex, first_stage_resids = first_stage_resids)
  second_stage_tot <- glm(contraception_use_words ~ intoxication_bin + first_stage_resids,
                          data = df_2nd, family = "binomial")
  summ <- summary(second_stage_tot)
  summ
  contraception_combo_iv <- summ$coefficients[2, c(1:2, 4)]
  names(contraception_combo_iv) <- c("est", "sd", "p_val")
  data_combo_iv <- rbind(intercourse_combo_iv, contraception_combo_iv)
  return(list(intercourse = intercourse_combo_iv, contraception = contraception_combo_iv))
}


iv_mf <- ivCombo(int_df = df_iv, cont_df = df_sex)
iv_m <- ivCombo(df_iv %>% dplyr::filter(sex == "1"), df_sex %>% filter(sex == "1"))
iv_f <- ivCombo(df_iv %>% filter(sex == "2"), df_sex %>% filter(sex == "2"))

df_mf <- data.frame(
  intercourse = iv_mf$intercourse,
  contraception = iv_mf$contraception
) %>% t() %>% 
  data.frame() %>% 
  rownames_to_column("study") %>% 
  dplyr::mutate(gender = "all_gender")

df_m <- data.frame(
  intercourse = iv_m$intercourse,
  contraception = iv_m$contraception
) %>% t() %>% 
  data.frame() %>% 
  rownames_to_column("study") %>% 
  dplyr::mutate(gender = "males")

df_f <- data.frame(
  intercourse = iv_f$intercourse,
  contraception = iv_f$contraception
) %>% t() %>% 
  data.frame() %>% 
  rownames_to_column("study") %>% 
  dplyr::mutate(gender = "females")

df_full <- bind_rows(df_mf, df_m, df_f) %>% tibble() %>% 
  dplyr::mutate(instrument = "two")

intercourse_sd <- contraception_sd <- matrix(NA, 3, 3)
rownames(intercourse_sd) <- rownames(contraception_sd) <- c("all_gender", "males", "females")
colnames(intercourse_sd) <- colnames(contraception_sd) <- c("est", "sd", "p_val")

df_iv_intercourse <- df_iv
df_iv_sex <- df_sex

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
contraception_sd

df_full



capitalize_first <- function(labels) {
  sapply(labels, function(label) {
    paste0(toupper(substr(label, 1, 1)), substr(label, 2, nchar(label)))
  })
}

fs <- 19 # font

df_full %>% 
  bind_rows(intercourse_sd, contraception_sd) %>% 
  dplyr::mutate(instrument = factor(instrument, levels = c("two", "none") %>% rev())) %>% 
  dplyr::mutate(lower = est - qnorm(0.975) * sd,
                upper = est + qnorm(0.975) * sd) %>% 
  dplyr::mutate(study = factor(study, levels = c("intercourse", "contraception"))) %>% 
  ggplot(aes(x = gender, y = est, fill = instrument)) + 
  theme_bw() + 
  # geom_point(aes(color = instrument), size = 6) + 
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  scale_fill_manual(values = c("steelblue", "orange"),
                    labels = c("None", "Smoking + Alc. Availability")) + 
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
ggsave("images/iv_combo_coefs_avail_smoke.png")






