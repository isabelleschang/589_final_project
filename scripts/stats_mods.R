library("papaja")
library("tidyverse")
library("here")
library("ds4ling")
library("lmtest")
library("reshape2")

# load data
data_tidy <- read_csv('./data/data_tidy/589_tidy.csv')
#change data type into factor
data_factor <- data_tidy %>%
  mutate(
    ellipsis_type = as.factor(`ellipsis_type`),
    num_metric = as.factor(`num_metric`)
  ) %>%
  select(ellipsis_type,num_metric,num_score)
data_factor

#get stats and arrange 
ellipsis_stats <- data_factor %>%
  group_by(ellipsis_type,num_metric) %>%
  summarize(avg=mean(num_score)) %>%
  pivot_wider(names_from="num_metric", values_from="avg") %>%
  write_csv('./data/data_tidy/desc_stats')
  #knitr::kable() save this for markdown doc

data_tidy
#constant model - meaning (subset of numerical scores meaning)
meaning_subset <-  subset(data_tidy, num_metric=="meaning")
meaning_subset

mod_meaning <- lm(num_score ~ 1, data = meaning_subset)
summary(mod_meaning)

mod_eltype_m <- lm(num_score ~ ellipsis_type, data = meaning_subset)

mod_add_m <- lm(num_score ~ ellipsis_type + mt_service, data = meaning_subset)

mod_int_m <- lm(num_score ~ ellipsis_type:mt_service, data = meaning_subset)

#constant model - gram (subset of numerical scores corresponding to grammaticality)
gram_subset <-  subset(data_tidy, num_metric=="grammaticality")
gram_subset

mod_gram <- lm(num_score ~ 1, data = gram_subset)
summary(mod_gram)

mod_eltype_g <- lm(num_score ~ ellipsis_type, data = gram_subset)

mod_add_g <- lm(num_score ~ ellipsis_type + mt_service, data = gram_subset)

mod_int_g <- lm(num_score ~ ellipsis_type:mt_service, data = gram_subset)