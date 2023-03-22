library(tidyverse)
library(prettyR)

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

alcohol_code <- "H1TO"
sex_drunk_code <- "H1JO"
contraception_code <- "H1CO"
knowledge_quiz_code <- "H1KQ1A"

adult_care_code <- "H1PR1"
alc_in_home_code <- "H1TO51"

intercourse_code <- "H1CO1"


codes <- c("AID", demographic_codes, alcohol_code, sex_drunk_code, contraception_code,
           adult_care_code, alc_in_home_code)


df_raw <- ds1 %>% 
  dplyr::select(contains(codes))



coerceResponse <- function(MYVAR) {
  lbls <- sort(levels(MYVAR))
  lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
  out <- add.value.labels(
    as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", MYVAR)), 
    lbls
  )
  out
}

df <- apply(df_raw, 2, coerceResponse) %>% data.frame() %>% tibble()

df_intercourse <- df %>% 
  dplyr::filter(H1CO1 == 1)

df_virgin <- df %>% 
  dplyr::filter(H1CO1 == 0)


