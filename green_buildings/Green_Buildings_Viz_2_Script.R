library(tidyverse)
library(gridExtra)
library(grid)
library(gridBase)

green = read.csv('data/greenbuildings.csv')

### Data Cleaning to add the Class in one column
green <- green %>% mutate(
  class = case_when(class_a == 1 ~ "a",
                    class_b == 1 ~ "b"),
  class = ifelse(!is.na(class),class,"c"),
  green_rating = ifelse(green_rating == 1,"Green","Not Green"),
  green_rating= as.factor(green_rating),
  class = as.factor(class))

p1 <- ggplot(green,aes(x=class,y=leasing_rate,color=class)) +
  geom_jitter(aes(color = class), alpha = 0.4) + geom_violin(alpha=0.7) + 
  facet_wrap(~green_rating) + theme_bw() + 
  guides(fill=FALSE) + 
  labs(x="Class",y="Leasing Rate") + guides(color=FALSE)


p2 <- ggplot(green,aes(x=class,y=Rent)) + 
  geom_jitter(aes(color = class), alpha = 0.4) + geom_boxplot(alpha=0.7) +
  facet_wrap( ~ green_rating) + theme_bw() + 
  labs(x = "Class", y = "Rent ($/SqFt)") + 
  guides(color=FALSE)


p3 <- ggplot(green,aes(x=class,y=age)) + 
  geom_jitter(aes(color = class), alpha = 0.4) + geom_boxplot(alpha=0.7) + 
  facet_wrap(~ green_rating) + theme_bw() + 
  guides(color=FALSE) + labs(x="Class",y="Age")

#### Location of Green Rating in different Classes
total_count <- green %>%
  summarize(count = n())
total_count <- as.numeric(total_count)

green_df <- green %>% group_by(green_rating,class) %>% summarize(cnt = n()/total_count) %>% ungroup()

## Green vs Not Green counts in different Classes. 
p4 <- ggplot(green_df,aes(class,cnt)) + 
  geom_bar(aes(fill=class),stat='identity') + 
  facet_wrap( ~ green_rating) + theme_bw() + 
  labs(x="Class",y="Fraction of Counts") + 
  guides(fill=FALSE)

grid.arrange(p4,p1,p2,p3,ncol=2,top = "Challenging the value of Green properties?")