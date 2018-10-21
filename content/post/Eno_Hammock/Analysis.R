library(tidytext)
library(dplyr)
library(ggplot2)
library(reshape2)

#Eno
Eno <- data[data$binary == 1,]
Kammock <- data[data$binary == 2,]

tidy_Eno <- Eno%>%
  unnest_tokens(word, reviews)%>%
  anti_join(stop_words)

tidy_Kammock <- Kammock%>%
  unnest_tokens(word, reviews)%>%
  anti_join(stop_words)

tidy_All <- data%>%
  unnest_tokens(word, reviews)%>%
  anti_join(stop_words)


frequency <- bind_rows(mutate(tidy_Eno, Company = "Eno"),
                       mutate(tidy_Kammock, Company = "Kammock"),
                       mutate(tidy_All, Company = "Both"))%>%
  mutate(word = str_extract(word, "[a-z]+"))%>%
  count(Company, word)%>%
  group_by(Company)%>%
  mutate(proportion = n/sum(n))%>%
  select(-n)%>%
  spread(Company, proportion)%>%
  gather(Company, proportion, c("Eno", "Kammock"))
  

library(scales)



ggplot(frequency, aes(x = proportion, y = `Both`, color = abs(`Both` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~Company, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Both", x = NULL)


library(tm)



corp_Eno <- VCorpus(Eno)

corp_Eno[3]
