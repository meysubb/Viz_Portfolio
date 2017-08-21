library(tidyverse)
library(grid)
library(gridBase)
library(gridExtra)
library(scales)

cars <- read.csv("data/Cars.csv")
colnames(cars)[1] <- "id"
cars_eda <- cars %>% mutate(
  subTrim = as.factor(subTrim),
  year = year,
  fuel = as.factor(fuel),
  region = toupper(region),
  subTrim = toupper(subTrim),
  soundSystem = as.factor(ifelse(soundSystem == "unsp","unsp","premium")),
  displacement = as.factor(displacement)
) %>% select(-state,-isOneOwner)

cars <- cars_eda %>% mutate(
  year = 2016 - year) 
cars_eda$year <- as.factor(cars_eda$year)


price_density <- ggplot(cars_eda,aes(price)) + 
  geom_histogram(aes(y = ..density..),alpha=0.5,color="black",fill="darkblue") + 
  geom_density(alpha=.7, fill="lightgrey") + 
  labs(x = "Price", y = "Count")  +
  scale_x_continuous(labels = dollar) + 
  guides(color = FALSE) + 
  theme_bw() 


year_pr <- ggplot(cars_eda,aes(year,price,colour = year)) + 
  geom_boxplot(fill="white",alpha=0.2) +
  scale_y_continuous(labels = dollar) + 
  labs(x="Year",y="Price") + 
  guides(color = FALSE) + 
  theme_bw() +
  coord_flip()

region <- ggplot(cars_eda,aes(region,price,fill = region)) + 
  scale_y_continuous(labels = dollar) + 
  geom_boxplot() + 
  labs(x="Region",y="Price") + 
  guides(fill = FALSE) + 
  theme_bw() 

  
mile_scttr <- ggplot(cars_eda,aes(x=mileage,y=price,colour = price)) + 
  geom_point(alpha=0.7) + 
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  guides(color = FALSE) + 
  labs(x="Mileage",y="Price") + 
  scale_y_continuous(labels = dollar) +
  theme_bw() 

plot5 <- grid.arrange(year_pr, arrangeGrob(mile_scttr,price_density,region,ncol=1),
                      ncol=2, widths=c(1,1.2))