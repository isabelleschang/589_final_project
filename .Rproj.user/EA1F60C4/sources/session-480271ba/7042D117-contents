library("papaja")
library("tidyverse")
library("here")
library("ds4ling")
library("lmtest")
library("reshape2")

# read data
data_raw = read_csv(file=here("data","data_raw","497_raw.csv"))
data_tidy <- data_raw %>%
  mutate(
    meaning = as.factor(`Meaning Score`),
    grammaticality = as.factor(`Grammaticality Score`)) %>%
  select(meaning, grammaticality, `Ellipsis type`) %>%
  pivot_longer(cols=c("meaning", "grammaticality"),
               names_to="metric",
               values_to = "num_score") %>%
  subset(num_score != "NA") %>%
  mutate(num_score = as.numeric(num_score))

plot_0 <- data_tidy %>%
  ggplot() +
  aes(x = metric, y = num_score, color=`Ellipsis type`) +
  ggtitle("Numerical Scores As A Function Of Metric") +
  theme(legend.position="bottom") +
  geom_jitter(alpha=0.4) +
  stat_summary(fun.data=mean_se,
               geom="pointrange",
               position = position_dodge(0.5))

plot0

ggsave(here("497.png"),
       plot = plot0)