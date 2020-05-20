# Final status of the KS project
ks.proj <- final.data

ggplot(ks.proj, aes(status, fill = status)) +
  geom_bar() +
  ylab("# of Projects") + xlab("Final Status") +
  theme(legend.position = "bottom") +
  ggtitle("Final Status of the Kickstarter projects")

colnames(ks.proj)
summary(ks.proj$status)


# 1. Main Categories present in the dataset
library("ggplot2")
library("dplyr")


p2 <- ggplot(ks.proj, aes(x = main_category, fill = ks.proj$status)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("") +
  ggtitle("Main categories of the KS Projects")  

p2

# Main categories of the KS Projects - percent to whole
p1 <- ks.proj %>% 
  count(main_category) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(reorder(main_category, pct), pct)) +
  geom_density(size = 3) + scale_color_gradient() +
  coord_flip() + 
  ylab("% of projects") + xlab("") +
  ggtitle("Main categories of the KS Projects ") 

gridExtra::grid.arrange(p1, p2, ncol = 2)

##2. countries 

# Final status of the KS project
ggplot(ks.proj, aes(x = country, fill = ks.proj$status)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::comma) +
  ylab("Number of projects") + xlab("") +
  ggtitle("KS Projects by country") 


##project duration 
p1 <- ggplot(ks.proj, aes(duration, fill = ks.proj$status)) +
  geom_histogram(binwidth = 5) +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("Duration") +
  ggtitle("Duration of the KS projects (in days)")

p2 <- ggplot(ks.proj, aes(x = status, y = duration, fill = ks.proj$status)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  coord_flip() +
  xlab("") + ylab("Duration") +
  ggtitle("Active Duration of the KS projects")

gridExtra::grid.arrange(p1, p2, ncol = 2)


##8. USD Goal

p1 <- ggplot(ks.proj, aes(log(goal_usd),  fill = ks.proj$status)) +
  geom_density() +
  theme(legend.position = "bottom") +
  xlab("USD Goal (log-transformed)") + ylab("") +
  ggtitle("KS projects' Goal")

# Log-transformed goal_usd
p2 <- ggplot(ks.proj, aes(x = status, y = log(goal_usd), fill = ks.proj$status)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ylab("Goal in USD (log-transformed)") + xlab("") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() + 
  ggtitle(" Goal of the KS projects (Log)")

gridExtra::grid.arrange(p1, p2, ncol = 2)





         
         


