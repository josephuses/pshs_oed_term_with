library(readit)
library(tidyverse)
library(data.table)
library(janitor)
library(knitr)
library(kableExtra)

ncedat <- readit("./Data/raw/RE/NCE Data on Examinees with Campus (2011-2017).csv")
ncetd <- ncedat %>% select(-total) %>%
  gather(subject, score, math:abstract)

ncetd <- ncetd %>% group_by(year, subject) %>% mutate(mean = mean(score)) %>%
  ungroup() %>%
  group_by(year, id, lname, fname, mname) %>%
  mutate(total = sum(score)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(total_mean = mean(total)) %>%
  mutate(pass = as.integer(score > mean)) %>% ungroup() %>%
  group_by(year, id) %>% mutate(pass = sum(pass)) %>%
  mutate(justpass = score < mean & score >= .9 * mean) %>%
  mutate(passflag2 = case_when(pass == 4 & total >= total_mean~"P",
                               pass < 4 & total >= total_mean~"A",
                               TRUE~"F"))

ncetd %>% tabyl(passflag2, year, choice1)

