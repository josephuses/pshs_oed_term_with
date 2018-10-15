library(readit)
library(tidyverse)
library(data.table)
library(janitor)
library(knitr)
library(kableExtra)
library(TeachingDemos)

ncedat <- readit("./Data/raw/RE/NCE Data on Examinees with Campus (2011-2017).csv")
setDT(ncedat)[, total_mean := mean(total), by = .(year)]
ncedat[, region_name := case_when(
  region_name %in% c("A. R. OF MUSLIM MINDANAO", "AUTONOMOUS REGION IN MUSLIM MINDANAO")~"ARMM",
  region_name %in% c("CARAGA AUTONOMOUS REGION")~"CARAGA",
  region_name %in% c("CORDILLERA ADMINISTRATIVE REGN", "CORDILLERA ADMINISTRATIVE REGION")~"CAR",
  region_name %in% c("SOCCSKSARGEN REGION")~"SOCCSKSARGEN",
  TRUE~region_name
)]
ncedat[, choice1 := case_when(
  choice1 %in% c("BICOL REGION CAMPUS")~"BRC",
  choice1 %in% c("CAGAYAN VALLEY CAMPUS")~"CVC",
  choice1 %in% c("CENTRAL LUZON CAMPUS")~"CLC",
  choice1 %in% c("CENTRAL MINDANAO CAMPUS")~"CMC",
  choice1 %in% c("CENTRAL VISAYAS CAMPUS")~"CVISC",
  choice1 %in% c("CORDILLERA ADMINISTRATIVE REGION CAMPUS")~"CARC",
  choice1 %in% c("EASTERN VISAYAS CAMPUS")~"EVC",
  choice1 %in% c("ILOCOS REGION CAMPUS")~"IRC",
  choice1 %in% c("SOUTHERN MINDANAO CAMPUS")~"SMC",
  choice1 %in% c("WESTERN VISAYAS CAMPUS")~"WVC",
  choice1 %in% c("MAIN CAMPUS")~"MC",
  TRUE~choice1
)]
names(ncedat)
ncedat <- ncedat %>%
  group_by(year) %>%
  mutate(main = case_when(choice1 == "MC" & (total >= total_mean & 
                            science >= mean(science) & 
                            math >= mean(math)) & 
                            (abstract <= mean(abstract) & abstract >= 0.9*mean(abstract) | 
                            (verbal >= 0.9 * mean(verbal) & verbal <= mean(verbal)))~"main A",
                          TRUE~NA_character_))
ncetd <- ncedat %>% select(-total) %>%
  gather(subject, score, math:abstract)

#ncetd <- ncetd %>% group_by(year, subject) %>% mutate(mean = mean(score)) %>%
#  ungroup() %>%
#  group_by(year, id, lname, fname, mname) %>%
#  mutate(total = sum(score)) %>%
#  ungroup() %>%
#  group_by(year) %>%
#  mutate(total_mean = mean(total)) %>%
#  mutate(pass = as.integer(score > mean)) %>% ungroup() %>%
#  group_by(year, id) %>% mutate(pass = sum(pass)) %>%
#  mutate(justpass = as.integer(score < mean & score >= .9 * mean)) %>%
#  mutate(justpass = sum(justpass, na.rm=T)) %>%
#  mutate(passflag2 = case_when(pass == 4 & total >= total_mean~"P",
#                               pass < 4 & total >= total_mean & justpass == 1~"A",
#                               TRUE~"F"))

setDT(ncetd)[, mean := mean(score), by = .(year, subject)]
ncetd[, score := as.numeric(score)]
ncetd[, choice1mean := mean(score), by = .(year, choice1, subject)]
ncetd[, choice2mean := mean(score), by = .(year, choice2, subject)]
ncetd[, regmean := mean(score), by = .(year, subject, region_name)]
ncetd[, total := sum(score, na.rm = TRUE), by = .(year, id, lname, fname, mname)]
ncetd[, total_mean_choice1 := mean(total), by = .(year, choice1)]
ncetd[, total_mean_choice2 := mean(total), by = .(year, choice2)]
# consider definition of pass prior to 2013

ncetd[, total_mean_region := mean(total), by = .(year, region_name)]

# consider definition of pass prior to 2013

ncetd[, pass := case_when(year %in% c(2008:2012) & choice1 != "MC"~as.integer(score > choice1mean), 
                          TRUE~as.integer(score > mean)), by = .(year)]


ncetd[, pass := sum(pass, na.rm = T), by = .(year, id)]
# this is wrong! modify!
ncetd[, justpass := case_when(main == "main A"~as.integer(main == "main A"),
                              TRUE~as.integer(between(score, 0.9*mean, mean) & (total >= total_mean))
                              )]
ncetd[, justpass := sum(justpass, na.rm = T), by = .(year, id)]
ncetd[, passflag2 := case_when(pass == 4 & total >= total_mean~"P",
                               pass < 4 & total >= total_mean & justpass >= 1 ~"A",
                               TRUE~"F")]


alternates <- ncetd %>% distinct(year, id, region_name, passflag2, justpass, region_name, subject, score, mean, total, total_mean) %>% 
  filter(justpass == 4, passflag2 == "A") %>%
  arrange(year, region_name, id)
alternates_wide <- dcast(setDT(alternates), region_name + year + id + total + total_mean + passflag2~subject, value.var = c("score", "mean"))


spt2 <- split(alternates_wide, alternates_wide$year)

lapply(names(spt2), function(x){
  fwrite(spt2[[x]], paste(x, ".csv", sep = ""))
})


ncedat %>% group_by(year) %>% summarise_at(vars(math:abstract), mean)
ncedat %>% group_by(year) %>% summarise_at(vars(total), mean)
ncetd[, passflag2 := factor(passflag2, c("P", "A", "F"))]

APF_by_region_year <- dcast(setDT(ncetd %>% filter(passflag2 != "F") %>% distinct(year, id, pass_flag, passflag2, year, region_name)), region_name ~ year + passflag2, value.var = "passflag2", fun.aggregate = length) %>%
  adorn_totals(where = "row")
APF_by_campus_year <- dcast(setDT(ncetd %>% filter(passflag2 != "F") %>% distinct(year, id, pass_flag, passflag2, year, choice1)), choice1 ~ year + passflag2, value.var = "passflag2", fun.aggregate = length) %>%
  adorn_totals(where = "row")


ncets <- ncetd %>% group_by(year, subject) %>% summarise_at(vars(score), funs(mean, sd))
ncets
fwrite(ncets, "ncets_year.csv")
ncets <- ncetd %>% group_by(year) %>% summarise_at(vars(total), funs(mean, sd))




fwrite(APF_by_campus_year, "APF_by_campus_year.csv")
fwrite(APF_by_region_year, "APF_by_region_year.csv")

APF_by_region_year2 <- fread("APF_by_region_year2.csv")
deped <- fread("deped.csv")
setkey(deped, "regno")
setkey(APF_by_region_year2, "regno")
APF_by_region_year2 <- APF_by_region_year2[deped]

# fwrite(APF_by_region_year2, "regional_pops.csv")
regional <- fread("regional_pops.csv")
