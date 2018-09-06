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
  mutate(pass = as.integer(score > mean)) %>% ungroup() %>%
  group_by(year, id) %>% mutate(pass = sum(pass)) %>%
  mutate(passflag2 = case_when(pass == 4~"passed", TRUE~"failed"))