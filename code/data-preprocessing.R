library(tidyverse)
library(prettyR)
library(bannerCommenter)

#################################################################
##         Loading data and identifying question codes         ##
#################################################################

load("../data/wave-1/DS1.rda")
load("../data/wave-1/DS2.rda")
load("../data/wave-1/DS3.rda")
load("../data/wave-1/DS4.rda")

ds1 <- da21600.0001
ds2 <- da21600.0002
ds3 <- da21600.0003
ds4 <- da21600.0004

demographic_codes <- c(
  "H1GI4", # Hisp/Latino
  "H1GI6A", # White
  "H1GI6B", # Black
  "H1GI6C", # Native American
  "H1GI6D", # Asian
  "BIO_SEX" # Gender, 1 = male, 2 = female
)

alcohol_code <- "H1TO" # section 28
sex_drunk_code <- "H1JO" 
contraception_code <- "H1CO" # section 24
knowledge_quiz_code <- "H1KQ1A"

adult_care_code <- "H1PR1"
alc_in_home_code <- "H1TO51"


codes <- c("AID", demographic_codes, alcohol_code, sex_drunk_code, contraception_code,
           adult_care_code, alc_in_home_code)


df_raw <- ds1 %>% 
  dplyr::select(contains(codes))

coerceResponse <- function(MYVAR) {
  # Function to remove unnecessary labels from response entries
  # essentially 'cleans' the entries
  lbls <- sort(levels(MYVAR))
  lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
  out <- add.value.labels(
    as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", MYVAR)), 
    lbls
  )
  out
}



df <- apply(df_raw, 2, coerceResponse) %>% data.frame() %>% tibble()
# cleaning the dataset and converting to a tibble for easy viewing

############################################################################
############################################################################


#################################################################
##              CREATING INTOXICATION-SEX DATASET              ##
#################################################################


intox_freq_code <- "H1TO18"
# Frequency of intoxication was measured by asking participants 
#   how often they had gotten drunk in the past 12 months.
#   1: every day or almost every day
#   2: 3-5 days a week
#   3: 1-2 days a week
#   4: 2-3 days a month
#   5: once a month
#   6: 1-2 days in the past 12 months
#   7: never
sex_intercourse_code <- "H1CO1"
 # 0: no
 # 1: yes

sex_drink_subcode <- 3

drunk_most_recent_sex <- paste0("H1JO", sex_drink_subcode)
# H1JO1: did you drink during your first time?
# H1JO2: were you drunk during your first time?
# H1JO3: did you drink during your last sexual encounter?
#   (I think Felson used this for the first study multinom. logisic regression)
#.  0: no --> sober
#.  1: yes --> intoxicated
#.  2: You have had sexual intercourse only once. --> sober
#.  NA: count as 0 --> never
# H1JO4: were you drunk during your last sexual encounter?
#.  0: no --> sober
#.  1: yes --> intoxicated
#.  NA: legitimate skip --> never

df_first_study <- df %>% 
  dplyr::select(AID, BIO_SEX, all_of(c(intox_freq_code, sex_intercourse_code, drunk_most_recent_sex))) %>% 
  dplyr::rename(sex = BIO_SEX,
                intoxication = H1TO18,
                intercourse_dummy = H1CO1,
                intox_most_recent_sex = paste0("H1JO", sex_drink_subcode)) #%>%  
  # dplyr::mutate_at(.vars = c("sex", "intoxication", "intercourse_dummy", "intox_most_recent_sex"), 
  #                  factor)
# converts categorical columns to factors
# remove NA's




df_first_study$intoxication <- ifelse(
  df_first_study$intoxication %in% c(1, 2, 3),
  "frequently", # frequently
  ifelse(
    df_first_study$intoxication %in% c(4, 5, 6),
    "occasionally", # occasionally
    "never" # never. NA included in this group
  )
) %>% factor() 


############################################################################
############################################################################

##################################################################
##                Recoding contradictory entries                ##
##################################################################

# Here we look through the entries and fix any contradicting intoxication/sexual intercourse rows
# for instance there are people coded as having sex yet they are "nevers" for intoxication during most recent sex



sober_sex_condition <- (df_first_study$intox_most_recent_sex %in% c(0, 2) & df_first_study$intercourse_dummy %in% c(1)) | 
  (df_first_study$intercourse_dummy %in% c(1) & df_first_study$intoxication %in% c("never"))
# to have sober sex, you must have the proper rating (0 or 2) and have had sex in the first place, OR
#. you must have had sex and never drank at all 

intoxicated_sex_condition <- (df_first_study$intox_most_recent_sex %in% c(1) & df_first_study$intercourse_dummy %in% c(1)) 
# to have intoxicated sex, you must respond to previously having sex and have the proper rating (1)

df_first_study$intox_most_recent_sex_words <- ifelse(
  sober_sex_condition,
  "sober",
  ifelse(
    intoxicated_sex_condition,
    "intoxicated",
    df_first_study$intox_most_recent_sex
  )
)
df_first_study$intox_most_recent_sex_words <- ifelse(
  is.na(df_first_study$intox_most_recent_sex_words) & df_first_study$intercourse_dummy %in% c(0),
  "never",
  df_first_study$intox_most_recent_sex_words
)


df_first_study %>% sample_n(12)

# recoding the data as done by felson 2020
# Felson 2020 do not provide a way to account for missing data. We treat them as nevers here. 
#  This is a judgment call that is subject to change.



############################################################################
############################################################################

dir_name <- "processed-data"

if (!dir.exists(dir_name)) {
  dir.create(dir_name)
}

na_df <- df_first_study[which(is.na(df_first_study)),]
apply(na_df, 1, function(row) {sum(is.na(row))}) %>% unique()



write_csv(df_first_study, paste0(dir_name, "/intox_sex_file.csv"))




############################################################################
############################################################################


#################################################################
##         CREATING INTOXICATION-CONTRACEPTION DATASET         ##
#################################################################


df_intercourse <- df %>% 
  dplyr::filter(H1CO1 == 1) 
# only contains respondents who have had sex


df_virgin <- df %>% 
  dplyr::filter(H1CO1 != 1) 
# only contains respondents who have not had sex. Includes people who do not know or refused









