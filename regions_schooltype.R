regions <- nce_td %>% group_by(`REGION NAME`) %>% count() %>% filter(n > 50) %>% select(1) %>% as_vector()
lapply(sort(unique(nce_td$`REGION NAME`)), function(i){
  ggplot(nce_td[nce_td$`REGION NAME` == i, ], aes(`TOTAL SCORE (220 pts)`)) +
    geom_histogram(aes(y=..density..), bins = 40, alpha = 0.6) + 
    facet_wrap(~`SCHOOL TYPE`, ncol = 2) + 
    theme_bw()
  ggsave(filename = paste0("./img/",i, ".png"))
})  