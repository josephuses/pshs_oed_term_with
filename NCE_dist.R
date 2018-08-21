library(data.table)

nce <- fread("./Data/raw/NCE Data on Examinees with Region (2011-2017).csv")


library(tidyverse)
library(knitr)
library(kableExtra)
library(janitor)
names(nce)
nce_td <- melt(nce, measure.vars = c("MATH (80 pts)", 
                                     "VERBAL (60 pts)", 
                                     "SCIENCE (40 pts)", 
                                     "ABSTRACT (40 pts)"),
               variable.name = "subject", value.name = "score")

nce_td[,`:=`(maxscore = as.numeric(str_extract(subject, "[0-9]+")),
              subject = trimws(gsub("\\([0-9]+.*\\)", "", subject)),
              score = as.numeric(score))]
nce_td[, Median := median(score), by = list(subject)]

nce_td[, Mean := mean(score), by = list(subject)]


nce_td[, principal := ifelse(`PASS FLAG` == 1, 1, 0)]

nce_td[, Mean_Subject := median(score), by = list(`NCE YEAR`, subject)]

nce_td[, pass := ifelse(score > Mean_Subject, 1, 0)]

nce_td[, `REGION NAME` := case_when(
  `REGION NAME`== "AUTONOMOUS REGION IN MUSLIM MINDANAO"~"ARMM",
  `REGION NAME` == "CORDILLERA ADMINISTRATIVE REGION"~"CAR",
  `REGION NAME` == "KINGDOM OF SAUDI ARABIA"~"KSA",
  `REGION NAME` == "NATIONAL CAPITAL REGION"~"NCR",
  `REGION NAME` == "NEGROS ISLAND REGION"~"NIR",
  `REGION NAME` == "UNITED ARAB EMERATES"~"UAE",
  `REGION NAME` == "CENTRAL LUZON"~"CL",
  `REGION NAME` == "DAVAO REGION"~"Davao",
  `REGION NAME` == "BICOL REGION"~"Bicol",
  TRUE~stringi::stri_trans_totitle(tolower(`REGION NAME`)))]

nce_td[, `FIRST NAME` := stringi::stri_trans_totitle(tolower(`FIRST NAME`))]
nce_td[, `LAST NAME` := stringi::stri_trans_totitle(tolower(`LAST NAME`))]
nce_td[, `MIDDLE NAME` := stringi::stri_trans_totitle(tolower(`MIDDLE NAME`))]


passflag <- nce_td %>%
  group_by(`NCE YEAR`, `REGION NAME`, `LAST NAME`, `FIRST NAME`) %>%
  summarise(passflag = sum(pass)) %>%
  mutate(passflag = if_else(passflag == 4, 1, 0))

nce_td <- merge(nce_td, passflag) 

nce_td[, Pct := round(score / maxscore * 100, 2)]

passers_n <- nce_td %>% filter(passflag == 1) %>% tabyl(principal, `NCE YEAR`, `REGION NAME`) 

passers_n %>% adorn_totals("row")

passers_pct <- nce_td %>% filter(passflag == 1) %>% tabyl(principal, `NCE YEAR`, `REGION NAME`) %>%
  adorn_percentages(c("col")) %>%
  enframe() %>% unnest()

names(nce_td) <- sapply(strsplit(stringr::str_to_title(names(nce_td)), " "), paste, collapse = "_")

nce_td[, Subject := str_to_title(Subject)]

