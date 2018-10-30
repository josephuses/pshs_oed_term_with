library(readit)
library(tidyverse)
library(data.table)
library(janitor)
library(knitr)
library(kableExtra)

## 2015 RE


re2015 <- readit("./Data/raw/RE/2015 RE Scores.xlsx", skip = 2, col_names = F)

g82015colnames <- c(
  "campus",
  "sn",
  "sname",
  "re_english_mcq",
  "re_english_non-mcq",
  "re_english_total",
  "re_filipino_mcq",
  "re_filipino_non-mcq",
  "re_filipino_total",
  "re_is_mcq",
  "re_is_non-mcq",
  "re_is_total",
  "re_math_mcq",
  "re_math_non-mcq",
  "re_math_total",
  "re_ss_mcq",
  "re_ss_non-mcq",
  "re_ss_total",
  "nce_math",
  "nce_verbal",
  "nce_science",
  "nce_abstract",
  "nce_total",
  "qualitype"
)

colnames(re2015) <- g82015colnames

# fwrite(re2015, "./Data/processed/re2015.csv")
format(object.size(re2015), units = "MB")

re2015$year <- 2015

re2015 <- re2015 %>%
  gather(subject, score, re_english_mcq:nce_total) %>%
  separate(subject, into = c("assessment", "subject", "test_type"), sep = "_")

re2015 <- re2015 %>% mutate_if(is.character, trimws)

maxscore2015 <- tribble(
  ~assessment, ~subject, ~test_type, ~maxscore,
  "re", "english", "mcq", 50,
  "re", "english", "non-mcq", 10,
  "re", "english", "total", 60,
  "re", "filipino", "mcq", 45,
  "re", "filipino", "non-mcq", 15,
  "re", "filipino", "total", 60,
  "re", "is", "mcq", 40,
  "re", "is", "non-mcq", 15,
  "re", "is", "total", 55,
  "re", "math", "mcq", 30,
  "re", "math", "non-mcq", 30,
  "re", "math", "total", 60,
  "re", "ss", "mcq", 45,
  "re", "ss", "non-mcq", 15,
  "re", "ss", "total", 60,
  "nce", "math", NA_character_, 80,
  "nce", "verbal", NA_character_, 60,
  "nce", "science", NA_character_, 40,
  "nce", "abstract", NA_character_, 40,
  "nce", "total", NA_character_, 220
)

re2015 <- re2015 %>% left_join(maxscore2015)


re2015$grade <- 8

## 2016

### Grade 8

re2016g8pilot <- readit("./Data/raw/RE/2016 RE Scores.xlsx", sheet = 1, skip = 3, col_names = F)
re2016g8actual <- readit("./Data/raw/RE/2016 RE Scores.xlsx", sheet = 2, skip = 3, col_names = F)
g82016colnames <- c(
  "campus",
  "sn",
  "sname",
  "set",
  "re_english_mcq",
  "re_english_non-mcq",
  "re_english_total",
  "re_filipino_mcq",
  "re_filipino_non-mcq",
  "re_filipino_total",
  "re_is_mcq",
  "re_is_non-mcq",
  "re_is_total",
  "re_math_mcq",
  "re_math_non-mcq",
  "re_math_total",
  "re_ss_mcq",
  "re_ss_non-mcq",
  "re_ss_total",
  "nce_math",
  "nce_verbal",
  "nce_science",
  "nce_abstract",
  "nce_total",
  "qualitype"
)



colnames(re2016g8pilot) <- g82016colnames
colnames(re2016g8actual) <- g82016colnames[-4]
re2016g8actual$year <- 2016
format(object.size(re2016g8actual), units = "MB")

re2016g8 <- re2016g8actual %>%
  gather(subject, score, re_english_mcq:nce_total) %>%
  separate(subject, into = c("assessment", "subject", "test_type"), sep = "_")
# table(re2016g8$subject)
# table(re2016g8$test_type)
# table(re2016g8$assessment)

re2016g8 <- re2016g8 %>% mutate_if(is.character, trimws)

re2016g8 <- re2016g8 %>% left_join(maxscore2015)
re2016g8$grade <- 8

### Grade 10

re2016g10pilot <- readit("./Data/raw/RE/2016 RE Scores.xlsx", sheet = 3, skip = 3, col_names = F)
re2016g10actual <- readit("./Data/raw/RE/2016 RE Scores.xlsx", sheet = 4, skip = 3, col_names = F)
g102016pilotcol <- c(
  "campus",
  "sn",
  "sname",
  "set",
  "re_english_mcq",
  "re_english_non-mcq",
  "re_english_total",
  "re_filipino_mcq",
  "re_filipino_non-mcq",
  "re_filipino_total",
  "re_math_mcq",
  "re_math_non-mcq",
  "re_math_total",
  "re_ss_mcq",
  "re_ss_non-mcq",
  "re_ss_total",
  "re_bio_mcq",
  "re_bio_non-mcq",
  "re_bio_total",
  "re_chem_mcq",
  "re_chem_non-mcq",
  "re_chem_total",
  "re_phys_mcq",
  "re_phys_non-mcq",
  "re_phys_total",
  "re_stat_mcq",
  "re_stat_non-mcq",
  "re_stat_total",
  "nce_math",
  "nce_verbal",
  "nce_science",
  "nce_abstract",
  "nce_total",
  "qualitype"
)

colnames(re2016g10pilot) <- g102016pilotcol
colnames(re2016g10actual) <- g102016pilotcol[-4]
re2016g10actual$year <- 2016

re2016g10actual <- re2016g10actual %>%
  gather(subject, score, re_english_mcq:nce_total) %>%
  separate(subject, into = c("assessment", "subject", "test_type"), sep = "_")

re2016g10actual <- re2016g10actual %>% mutate_if(is.character, trimws)

maxscore2016 <- tribble(
  ~assessment, ~subject, ~test_type, ~maxscore,
  "re", "english", "mcq", 40,
  "re", "english", "non-mcq", 10,
  "re", "english", "total", 50,
  "re", "filipino", "mcq", 45,
  "re", "filipino", "non-mcq", 10,
  "re", "filipino", "total", 55,
  "re", "math", "mcq", 30,
  "re", "math", "non-mcq", 20,
  "re", "math", "total", 50,
  "re", "ss", "mcq", 40,
  "re", "ss", "non-mcq", 15,
  "re", "ss", "total", 55,
  "re", "bio", "mcq", 40,
  "re", "bio", "non-mcq", 10,
  "re", "bio", "total", 50,
  "re", "chem", "mcq", 35,
  "re", "chem", "non-mcq", 15,
  "re", "chem", "total", 50,
  "re", "phys", "mcq", 35, 
  "re", "phys", "non-mcq", 15,
  "re", "phys", "total", 50,
  "re", "stat", "mcq", 30,
  "re", "stat", "non-mcq", 20,
  "re", "stat", "total", 50,
  "nce", "math", NA_character_, 80,
  "nce", "verbal", NA_character_, 60,
  "nce", "science", NA_character_, 40,
  "nce", "abstract", NA_character_, 40,
  "nce", "total", NA_character_, 220
)

re2016g10actual <- re2016g10actual %>% left_join(maxscore2016)

re2016g10actual$grade <- 10

format(object.size(re2016g10actual), units = "MB")

## 2017

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, skip = 2, col_names = F))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

mysheets2017 <- read_excel_allsheets("./Data/raw/RE/2017 RE Scores.xlsx")
re2017colnames <- c(
  "subject",
  "campus",
  "sn",
  "sname",
  "re_mcq",
  "re_non-mcq",
  "re_total",
  "nce_math",
  "nce_verbal",
  "nce_science",
  "nce_abstract",
  "nce_total",
  "qualitype"
)


re2017 <- mysheets2017 %>% enframe() %>% unnest()
colnames(re2017) <- re2017colnames
re2017$year <- 2017
re2017 <- re2017 %>%
  gather(assessment, score, re_mcq:nce_total) %>%
  separate(assessment, into = c("assessment", "test_type"), sep = "_")


re2017 <- re2017 %>% separate(subject, into = c("subject", "grade"), sep = " ")
re2017$grade <- str_extract_all(re2017$grade, "[0-9]+")

re2017 <- re2017 %>% mutate(grade = as.numeric(str_extract_all(grade, "[0-9]+")))
re2017 <- re2017 %>% mutate(subject = tolower(subject))
re2017 <- re2017 %>% mutate(subject = case_when(subject == "biology"~"bio",
                                                subject == "chemistry"~"chem",
                                                subject == "physics"~"phys",
                                                subject == "statistics"~"stat",
                                                TRUE~subject))

re2017 <- re2017 %>% 
  mutate_if(is.character, trimws) 



maxscore2017 <- tribble(
  ~assessment, ~grade, ~subject, ~test_type, ~maxscore,
  "re", 8, "english", "mcq", 50,
  "re", 8, "english", "non-mcq", 10,
  "re", 8, "english", "total", 60,
  "re", 8, "filipino", "mcq", 44,
  "re", 8, "filipino", "non-mcq", 15,
  "re", 8, "filipino", "total", 59,
  "re", 8, "is", "mcq", 40,
  "re", 8, "is", "non-mcq", 10,
  "re", 8, "is", "total", 50,
  "re", 8, "math", "mcq", 29,
  "re", 8, "math", "non-mcq", 30,
  "re", 8, "math", "total", 59,
  "re", 8, "ss", "mcq", 39,
  "re", 8, "ss", "non-mcq", 16,
  "re", 8, "ss", "total", 55,
  "re", 10, "bio", "mcq", 40,
  "re", 10, "bio", "non-mcq", 10,
  "re", 10, "bio", "total", 50,
  "re", 10, "chem", "mcq", 39,
  "re", 10, "chem", "non-mcq", 10,
  "re", 10, "chem", "total", 49,
  "re", 10, "english", "mcq", 40,
  "re", 10, "english", "non-mcq", 10,
  "re", 10, "english", "total", 50,
  "re", 10, "filipino", "mcq", 45,
  "re", 10, "filipino", "non-mcq", 10,
  "re", 10, "filipino", "total", 55,
  "re", 10, "math", "mcq", 30,
  "re", 10, "math", "non-mcq", 20,
  "re", 10, "math", "total", 50,
  "re", 10, "phys", "mcq", 35, 
  "re", 10, "phys", "non-mcq", 15,
  "re", 10, "phys", "total", 50,
  "re", 10, "ss", "mcq", 40,
  "re", 10, "ss", "non-mcq", 15,
  "re", 10, "ss", "total", 55,
  "re", 10, "stat", "mcq", 30,
  "re", 10, "stat", "non-mcq", 20,
  "re", 10, "stat", "total", 50,
  "nce", 10, "math", NA_character_, 80,
  "nce", 10, "verbal", NA_character_, 60,
  "nce", 10, "science", NA_character_, 40,
  "nce", 10, "abstract", NA_character_, 40,
  "nce", 10, "total", NA_character_, 220,
  "nce", 8, "math", NA_character_, 80,
  "nce", 8, "verbal", NA_character_, 60,
  "nce", 8, "science", NA_character_, 40,
  "nce", 8, "abstract", NA_character_, 40,
  "nce", 8, "total", NA_character_, 220
)

re2017 <- left_join(re2017, maxscore2017) 

format(object.size(re2017), units = "MB")


## 2018

mysheets2018 <- read_excel_allsheets("./Data/raw/RE/2018 RE Scores.xlsx")
re2018 <- mysheets2018 %>% enframe() %>% unnest()
colnames(re2018) <- re2017colnames
re2018
re2018$year <- 2018
re2018 <- re2018 %>%
  gather(assessment, score, re_mcq:nce_total) %>%
  separate(assessment, into = c("assessment", "test_type"), sep = "_")

re2018 <- re2018 %>%
  mutate(grade = case_when(subject %in% c(
    "EnglishG8",
    "FilipinoG8",
    "IS",
    "MathG8",
    "SSG8"
  )~8,
  TRUE~10))

re2018 <- re2018 %>%
  mutate(subject = case_when(
            subject == "EnglishG8"~"english",
            subject == "FilipinoG8"~"filipino",
            subject == "IS"~"is",
            subject == "MathG8"~"math",
            subject == "SSG8"~"ss",
            subject == "Biology"~"bio",
            subject == "Chemistry"~"chem",
            subject == "EnglishG10"~"english",
            subject == "FilipinoG10"~"filipino",
            subject == "MathG10"~"math",
            subject == "Physics"~"phys",
            subject == "SSG10"~"ss",
            subject == "Statistics"~"stat", 
            TRUE~subject))

re2018 <- re2018 %>% mutate_if(is.character, trimws)

maxscore2018 <- tribble(
  ~assessment, ~grade, ~subject, ~test_type, ~maxscore,
  "re", 8, "english", "mcq", 50,
  "re", 8, "english", "non-mcq", 10,
  "re", 8, "english", "total", 60,
  "re", 8, "filipino", "mcq", 45,
  "re", 8, "filipino", "non-mcq", 10,
  "re", 8, "filipino", "total", 55,
  "re", 8, "is", "mcq", 40,
  "re", 8, "is", "non-mcq", 10,
  "re", 8, "is", "total", 50,
  "re", 8, "math", "mcq", 30,
  "re", 8, "math", "non-mcq", 22,
  "re", 8, "math", "total", 52,
  "re", 8, "ss", "mcq", 40,
  "re", 8, "ss", "non-mcq", 10,
  "re", 8, "ss", "total", 50,
  "re", 10, "bio", "mcq", 40,
  "re", 10, "bio", "non-mcq", 9,
  "re", 10, "bio", "total", 49,
  "re", 10, "chem", "mcq", 45,
  "re", 10, "chem", "non-mcq", 5,
  "re", 10, "chem", "total", 49,
  "re", 10, "english", "mcq", 40,
  "re", 10, "english", "non-mcq", 10,
  "re", 10, "english", "total", 50,
  "re", 10, "filipino", "mcq", 45,
  "re", 10, "filipino", "non-mcq", 10,
  "re", 10, "filipino", "total", 55,
  "re", 10, "math", "mcq", 29,
  "re", 10, "math", "non-mcq", 12,
  "re", 10, "math", "total", 41,
  "re", 10, "phys", "mcq", 31, 
  "re", 10, "phys", "non-mcq", 20,
  "re", 10, "phys", "total", 51,
  "re", 10, "ss", "mcq", 38,
  "re", 10, "ss", "non-mcq", 7,
  "re", 10, "ss", "total", 45,
  "re", 10, "stat", "mcq", 23,
  "re", 10, "stat", "non-mcq", 26,
  "re", 10, "stat", "total", 49,
  "nce", 10, "math", NA_character_, 80,
  "nce", 10, "verbal", NA_character_, 60,
  "nce", 10, "science", NA_character_, 40,
  "nce", 10, "abstract", NA_character_, 40,
  "nce", 10, "total", NA_character_, 220,
  "nce", 8, "math", NA_character_, 80,
  "nce", 8, "verbal", NA_character_, 60,
  "nce", 8, "science", NA_character_, 40,
  "nce", 8, "abstract", NA_character_, 40,
  "nce", 8, "total", NA_character_, 220
)

re2018 <- re2018 %>% left_join(maxscore2018)



format(object.size(re2018), units = "MB")

names(re2018)


## Combining

re2015 <- re2015 %>% select(campus,
                            sn,
                            sname,
                            year, 
                            assessment, 
                            subject, 
                            test_type, 
                            grade,
                            score,
                            maxscore,
                            qualitype)

re2016g8 <- re2016g8 %>% select(campus,
                                sn,
                                sname,
                                year, 
                                assessment, 
                                subject, 
                                test_type, 
                                grade,
                                score,
                                maxscore,
                                qualitype)

re2016g10actual <- re2016g10actual %>% select(campus,
                           sn,
                           sname,
                           year, 
                           assessment, 
                           subject, 
                           test_type, 
                           grade,
                           score,
                           maxscore,
                           qualitype)

re2017 <- re2017 %>% select(campus,
                                sn,
                                sname,
                                year, 
                                assessment, 
                                subject, 
                                test_type, 
                                grade,
                                score,
                                maxscore,
                                qualitype)

re2018 <- re2018 %>% select(campus,
                            sn,
                            sname,
                            year, 
                            assessment, 
                            subject, 
                            test_type, 
                            grade,
                            score,
                            maxscore,
                            qualitype)

re2015_18 <- bind_rows(re2015, re2016g8, re2016g10actual, re2017, re2018)
re2015_18 <- re2015_18 %>% mutate(campus = case_when(campus %in% c("BICOL REGION", "BRC")~"BRC",
                                                     campus %in% c("CALABARZON", "CBZRC", "CBZ")~"CBZRC",
                                                     campus %in% c("CENTRAL LUZON", "CLC")~"CLC",
                                                     campus %in% c("CENTRAL MINDANAO", "CMC")~"CMC",
                                                     campus %in% c("CENTRAL VISAYAS", "CVISC")~"CVISC",
                                                     campus %in% c("EASTERN VISAYAS", "EVC")~"EVC",
                                                     campus %in% c("ILOCOS REGION", "IRC")~"IRC",
                                                     campus %in% c("MAIN", "MC")~"MC",
                                                     campus %in% c("MIMAROPA")~"MRC",
                                                     campus %in% c("SOUTHERN MINDANAO", "SMC")~"SMC",
                                                     campus %in% c("SOCCSKSARGEN REGION", "SRC")~"SRC",
                                                     campus %in% c("WESTERN VISAYAS", "WVC")~"WVC",
                                                     campus %in% c("ZAMBOANGA PENINSULA")~"ZRC",
                                                     campus %in% c("CORDILLERA ADMINISTRATIVE", "CAR")~"CARC",
                                                     campus %in% c("CAGAYAN VALLEY", "CVC")~"CVC",
                                                     campus %in% c("CARAGA REGION", "CRC")~"CRC",
                                                     TRUE~campus))

re2015_18 <- re2015_18 %>% mutate(subject = case_when(test_type %in% c("abstract", "math", "science", "verbal")~test_type,
                                                      TRUE~subject))
re2015_18 <- re2015_18 %>% filter(subject != "total", test_type != "total")


format(object.size(re2015_18), units = "MB")

summary(re2015_18)
fwrite(re2015_18, "./Data/processed/re2015_18.csv")


