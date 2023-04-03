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
 # 
sex_intercourse_code <- "H1CO1"
 # 0: no
 # 1: yes


df_first_study <- df %>% 
  dplyr::select(AID, BIO_SEX, all_of(c(intox_freq_code, sex_intercourse_code))) %>% 
  dplyr::rename(sex = BIO_SEX,
                intoxication = H1TO18,
                intercourse_dummy = H1CO1) %>%  
  dplyr::mutate_at(.vars = c("sex", "intoxication", "intercourse_dummy"), factor) #%>% na.omit()
# converts categorical columns to factors
# remove NA's

df_first_study$intoxication <- ifelse(
  df_first_study$intoxication %in% c(1, 2, 3),
  "frequently", # frequently
  ifelse(
    df_first_study$intoxication %in% c(4, 5),
    "occasionally", # occasionally
    "never" # never. NA included in this group
  )
) %>% factor() 
# recoding the data as done by felson 2020
# Felson 2020 do not provide a way to account for missing data. We treat them as nevers here. 
#  This is a judgment call that is subject to change.

dir_name <- "processed-data"

if (!dir.exists(dir_name)) {
  dir.create(dir_name)
}

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









