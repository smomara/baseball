library(baseballr)
library(tidyverse)

get_expected_runs_matrix=function(base_cd, outs, runs_rest_of_inn){
  tibble(base_cd, outs, runs_rest_of_inn) %>%
    mutate(runs_rest_of_inn = as.integer(runs_rest_of_inn)) %>%
    group_by(base_cd, outs) %>%
    summarize(ERV = round(mean(runs_rest_of_inn) / 2, 3)) %>%
    ungroup() %>%
    mutate(state=paste(base_cd, outs, sep=' ')) %>%
    arrange(base_cd) -> ER
  
  ER = matrix(ER$ERV, ncol = 3, byrow = TRUE)
  rownames(ER)=c('_ _ _','X _ _','_ X _ ','X X _','_ _ X','X _ X','_ X X','X X X')
  colnames(ER)=c('0','1','2')
  
  return (ER)
}