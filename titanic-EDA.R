# Foundations of Data Science
# Exploratory Data Analysis (ggplot2)
# Titanic Exercise

library(ggplot2)
library(readxl)

# load data -------------------------------------------------------------------

# titanic from Harrell
# biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/titanic3.xls
titanic <- read_excel("data/titanic3.xls")

# remove blank row from excel file
titanic <- titanic[-1310, ]

# Check out the structure of titanic
str(titanic)

# plot class, sex, survival ---------------------------------------------------

# convert variables to factor
titanic$pclass <- factor(titanic$pclass)
titanic$sex <- factor(titanic$sex)

# barplot of passenger class with gender encoded by color (`fill`)
ggplot(titanic, aes(pclass, fill = sex)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c( "deepskyblue4", "bisque2")) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title.x = element_text(family = "Times", face = "italic"),
        axis.title.y = element_text(family = "Times", face = "italic")) +
  labs(title = "Titanic: Passenger Class ~ Gender", x = "class", y = "count",
       fill = "gender")

# facetted barplot of passenger class and gender (`sex`)
ggplot(titanic, aes(pclass, fill = sex)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c( "deepskyblue4", "bisque2")) +
  theme_gray(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title.x = element_text(family = "Times", face = "italic"),
        axis.title.y = element_text(family = "Times", face = "italic")) +
  facet_grid(. ~ survived) +
  labs(title = "Titanic Survivors: Passenger Class ~ Gender", 
       x = "class", y = "count", fill = "gender")

# define a jittered position
posn.j <- position_jitter(0.5, 0)

# Use ggplot() for the last instruction
ggplot(titanic, aes(pclass, age, col = sex)) +
  geom_jitter(size = 3, alpha = 0.65, position = posn.j, 
              aes(shape = sex)) +
  scale_color_manual(values = c("deepskyblue4", "bisque3")) +
  theme_gray(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title.x = element_text(family = "Times", face = "italic"),
        axis.title.y = element_text(family = "Times", face = "italic")) +
  facet_grid(. ~ survived) +
  labs(title = "Titanic Survivors: Passenger Class ~ Gender", 
       x = "class", y = "count", fill = "gender")
