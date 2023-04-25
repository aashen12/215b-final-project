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


fs <- 22 # font

pi<- df_iv %>% 
  select(intoxication_bin, intercourse_dummy) %>% 
  group_by(intoxication_bin) %>% 
  summarise(Sex = mean(intercourse_dummy == "1"),
            Virgin = mean(intercourse_dummy == "0")) %>% 
  pivot_longer(cols = c("Sex", "Virgin"), names_to = "intercourse", values_to = "rel_prop") %>% 
  ggplot(aes(x = intoxication_bin, y = rel_prop, fill = intercourse)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = scales::percent(rel_prop)), position = position_stack(vjust = 0.5)) + 
  theme_minimal() + 
  labs(x = "Alcohol Use", y = "Relative Proportion", fill = "Sex Status") + 
  scale_fill_manual(values = c("deepskyblue1", "tomato2") %>% rev()) + 
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        text = element_text(size = fs),
        axis.title = element_text(size = fs + 2),
        axis.text = element_text(size = fs),
        legend.text = element_text(size = fs),
        legend.title = element_text(size = fs + 1),
        plot.caption = element_text(size = fs),
        plot.title = element_text(hjust = 0.5))
ggsave("images/eda-intercourse.png")



pc <- df_iv %>% 
  dplyr::filter(intercourse_dummy == 1) %>% 
  select(intoxication_bin, contraception_use_words) %>% 
  na.omit() %>% 
  group_by(intoxication_bin) %>% 
  summarise(Yes = mean(contraception_use_words == "use"),
            No = mean(contraception_use_words == "no_use")) %>% 
  pivot_longer(cols = c("Yes", "No"), names_to = "intercourse", values_to = "rel_prop") %>% 
  ggplot(aes(x = intoxication_bin, y = rel_prop, fill = intercourse)) + 
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = scales::percent(rel_prop)), position = position_stack(vjust = 0.5)) + 
  theme_minimal() + 
  labs(x = "Alcohol Use", y = "Relative Proportion", fill = "Used contraception?") + 
  scale_fill_manual(values = c("deepskyblue1", "tomato2") %>% rev()) + 
  theme(legend.position = "top", 
        legend.direction = "horizontal", 
        text = element_text(size = fs),
        axis.title = element_text(size = fs + 2),
        axis.text = element_text(size = fs),
        legend.text = element_text(size = fs),
        legend.title = element_text(size = fs + 1),
        plot.caption = element_text(size = fs),
        plot.title = element_text(hjust = 0.5))
ggsave("images/eda-contraception.png")


patchwork <- pi + pc
# Remove title from second subplot
patchwork[[2]] = patchwork[[2]] + theme(axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.title.y = element_blank() )
patchwork

ggsave("images/eda-all.png")

