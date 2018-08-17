library(data.table)
nce <- fread("./Data/raw/NCE Data on Examinees (2011-2017).csv")


library(tidyverse)
names(nce)
nce_td <- melt(nce, measure.vars = c("MATH (80 pts)", 
                                     "VERBAL (60 pts)", 
                                     "SCIENCE (40 pts)", 
                                     "ABSTRACT (40 pts)"),
               variable.name = "subject", value.name = "score")

nce_tds <- sample_n(nce_td, 100)
as.numeric(str_extract(nce_tds$subject, "[0-9]+"))
setDT(nce_tds)
nce_tds[,`:=`(maxscore = as.numeric(str_extract(subject, "[0-9]+")),
              subject = trimws(gsub("\\([0-9]+.*\\)", "", subject)),
              score = as.numeric(score))]

nce_td[,`:=`(maxscore = as.numeric(str_extract(subject, "[0-9]+")),
              subject = trimws(gsub("\\([0-9]+.*\\)", "", subject)),
              score = as.numeric(score))]


nce_tds[, Median := median(score), by = list(subject)]

nce_tds[, flag := as.character(ifelse(score > Median, 1, 2))]
nce_tds[, `PASS FLAG` := as.character(`PASS FLAG`)]
nce_tds[, matched := flag == `PASS FLAG`]

nce_tds[matched == FALSE]

str(nce_td)
1 %% 2
nce_tds[,`PASS FLAG` := `PASS FLAG` %% 2]

mod <- glm(`PASS FLAG` ~ `SCHOOL TYPE` + subject, data = nce_tds, family = binomial(link = "logit"))
summary(mod)

schooltype <- nce_td[, list(.N, mean = mean(score), SD = sd(score)), by = list(`SCHOOL TYPE`)]
schooltype

summary(nce_td)

nce_tds[, list(mean = mean(score), SD = sd(score))]
(15.96610-18.72) / 8.72484
(22.68293-18.72) / 8.72484

nce_td %>%
  map_if(is.numeric, ~table(is.na(.)))

filter(nce_td, is.na(score))

nce_td[,`:=`(maxscore = as.numeric(str_extract(subject, "[0-9]+")),
              subject = trimws(gsub("\\([0-9]+.*\\)", "", subject)),
              score = as.numeric(score))]
nce_td[, Median := median(score), by = list(subject)]


ggplot(nce_td, aes(score, fill = as.character(`PASS FLAG`))) +
  geom_histogram() +
  facet_wrap(~`SCHOOL TYPE`)
nce_td[,.N, by = list(`SCHOOL TYPE`)]
