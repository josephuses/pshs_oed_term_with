regions <- nce_td %>% group_by(`REGION NAME`) %>% count() %>% filter(n > 50) %>% select(1) %>% as_vector()
nce_tdphils <- nce_td %>% filter(`REGION NAME` %in% regions)
lapply(sort(unique(nce_tdphils$`REGION NAME`)), function(i){
  lapply(sort(unique(nce_tdphils$`NCE YEAR`)), function(j){
    ggplot(nce_td[nce_td$`REGION NAME` == i & nce_td$`NCE YEAR` == j, ], aes(`TOTAL SCORE (220 pts)`)) +
      geom_histogram(aes(y=..density..), bins = 40, alpha = 0.6) +
      xlim(0,200) + ylim(0,0.6) +
      facet_wrap(~`NCE YEAR` + `SCHOOL TYPE`, ncol = 2, nrow = 7) + 
      theme_bw()
    ggsave(filename = paste0("./img/year/",i,j, ".png"))
  })}) 