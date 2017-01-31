# Foundations of Data Science
# Exploratory Data Analysis (ggplot2)
# CHIS exercise

library(ggplot2)
library(Hmisc)
library(scales)

# load data -------------------------------------------------------------------

adult <- spss.get("data/ADULT.sav")

# Explore the dataset with summary and str
str(adult)
summary(adult)
adult$BM

# Define my own general theme
# assigns GillSans to theme_minimal, 
# makes axis titles italic set in Times.
pd.theme <- theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12,
                                  margin(1, 1, 0, 0)))

# plot age, BMI ---------------------------------------------------------------

# Age histogram
ggplot(adult, aes(SRAGE.P)) + 
    geom_histogram()

# BMI histogram
ggplot(adult, aes(BMI.P)) + geom_histogram()

# Age colored by BMI, default binwidth
ggplot(adult, aes(SRAGE.P, fill = factor(RBMI))) +
  geom_histogram(binwidth = 1, alpha = 0.85, color = "white") +
  scale_fill_manual(values = c("gray32",
                               "steelblue",
                               "plum3",
                               "firebrick3")) +
  labs(x = "age", y = "count", fill = "BMI category",
       title = "Body Mass Index ~ Age") +
  pd.theme

# cleanse variables -----------------------------------------------------------

# Remove individual aboves 84
adult <- adult[adult$SRAGE.P <= 84, ] 

# Remove individuals with a BMI below 16 and above or equal to 52
adult <- adult[adult$BMI.P >= 16 & adult$BMI.P < 52, ]

####
# Relabel the race variable (this couldn't be completed with copypasted datacamp code)
####
adult$RACEHPR2 <- factor(adult$RACEHPR2)
levels(adult$RACEHPR2)

# Relabel the BMI categories variable:
adult$RBMI <- factor(adult$RBMI, 
                     labels = c("Under-weight", "Normal-weight", 
                                "Over-weight", "Obese"))

# Age ~ RBMI plot: cleansed ---------------------------------------------------

# The color scale used in the plot
BMI_fill <- scale_fill_brewer("BMI Category", palette = "Reds")

# Custom theme to fix category display in faceted plot
fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                    strip.background = element_blank(), 
                    legend.position = "none")

# Histogram, add BMI_fill and customizations
ggplot(adult, aes (x = SRAGE.P, fill = factor(RBMI))) + 
  geom_histogram(binwidth = 1, color = "white") +
  facet_grid(RBMI ~ .) +
  theme_classic(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title = element_text(family = "Times", face = "italic",
                                  size = 12)) +
  labs(title = "RBMI ~ Age", x = "age") +
  fix_strips +
  BMI_fill

# six plots: density, faceted counts ------------------------------------------

# Plot 1 - Count histogram
ggplot(adult, aes (x = SRAGE.P, fill = factor(RBMI))) + 
  geom_histogram(binwidth = 1) +
  labs(title = "BMI Category ~ Age", x = "age", y = "") +
  BMI_fill + pd.theme

# Plot 2 - Density histogram
ggplot(adult, aes (x = SRAGE.P, fill = factor(RBMI))) + 
  geom_histogram(binwidth = 1, aes(y = ..density..)) +
  labs(title = "BMI Category ~ Age", x = "age", y = "density") +
  BMI_fill + pd.theme

# Plot 3 - Faceted count histogram
ggplot(adult, aes (x = SRAGE.P, fill = factor(RBMI))) + 
  geom_histogram(binwidth = 1, color = "white") +
  facet_grid(RBMI ~ .) +
  theme_classic(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title = element_text(family = "Times", face = "italic",
                                  size = 12)) +
  labs(x = "age") +
  BMI_fill

# Plot 4 - Faceted density histogram
ggplot(adult, aes (x = SRAGE.P,fill = factor(RBMI))) + 
  geom_histogram(binwidth = 1, aes(y = ..density..), color = "white") +
  facet_grid(RBMI ~ .) +
  theme_classic(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title = element_text(family = "Times", face = "italic",
                                  size = 12)) +
  labs(x = "age") +
  BMI_fill

# Plot 5 - Density histogram with position = "fill"
ggplot(adult, aes (x = SRAGE.P, fill = factor(RBMI))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 1, position = "fill") +
  labs(title = "BMI Category ~ Age", x = "age", y = "density") +
  BMI_fill + pd.theme

# Plot 6 - The accurate histogram
ggplot(adult, aes (x = SRAGE.P, fill = factor(RBMI))) + 
  geom_histogram(aes(y = ..count../sum(..count..)), 
                 binwidth = 1 , position = "fill") +
  labs(title = "BMI Category ~ Age, \"the accurate histogram\"", x = "age") +
  BMI_fill + pd.theme

# facetting -------------------------------------------------------------------

# An attempt to facet the accurate frequency histogram from before (failed)
ggplot(adult, aes (x = SRAGE.P, fill= factor(RBMI))) + 
  geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, position = "fill") +
  BMI_fill +
  facet_grid(RBMI ~ .)

# Create frequency table `DF`` with table()
DF <- table(adult$RBMI, adult$SRAGE.P)

# Use apply on DF to get frequency of each group
# frequency defined as x divided sum(x)
# number of particular observation divided by total number of observations
DF_freq <- apply(DF, 2, function(x) x/sum(x))

# Load reshape2 and use melt on DF to create DF_melted
library(reshape2)
DF_melted <- melt(DF_freq)
str(DF_freq)
str(DF_melted)

# Change names of DF_melted
names(DF_melted) <- c("FILL", "X", "value")

# custom gray theme
pd.gray <- theme_gray(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12,
                                  margin(1, 1, 0, 0)))

# Add code to make this a faceted plot
ggplot(DF_melted, aes(x = X, y = value, fill = FILL)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(FILL ~ .) +
  labs(title = "BMI Frequency ~ Age", x = "age") +
  BMI_fill +
  pd.gray

# Contingency Tables ----------------------------------------------------------

# The initial contingency table
DF <- as.data.frame.matrix(table(adult$SRAGE.P, adult$RBMI))

# Add the columns groupsSum, xmax and xmin. Remove groupSum again.
DF$groupSum <- rowSums(DF)
DF$xmax <- cumsum(DF$groupSum)
DF$xmin <- DF$xmax - DF$groupSum

# The groupSum column needs to be removed
# DF$groupSum <- NULL

# Copy row names to variable X
DF$X <- row.names(DF)

# Melt the dataset
library(reshape2)

DF_melted <- melt(DF, id.vars = c("X", "xmin", "xmax"), variable.name = "FILL")

# dplyr call to calculate ymin and ymax - don't change
library(dplyr)

DF_melted <- DF_melted %>% 
  group_by(X) %>% 
  mutate(ymax = cumsum(value/sum(value)),
         ymin = ymax - value/sum(value))

# Plot rectangles - don't change.
library(ggthemes)

ggplot(DF_melted, aes(ymin = ymin, 
                      ymax = ymax,
                      xmin = xmin, 
                      xmax = xmax, 
                      fill = FILL)) + 
  geom_rect(colour = "white") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  BMI_fill +
  theme_tufte(base_size = 12) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.text = element_text(size = 10))


# Chi-Sq Test + Plot ----------------------------------------------------------

# Perform chi.sq test (RBMI and SRAGE.P)
results <- chisq.test(table(adult$RBMI, adult$SRAGE.P))
summary(results)

#           Length Class  Mode     
# statistic   1    -none- numeric  
# parameter   1    -none- numeric  
# p.value     1    -none- numeric  
# method      1    -none- character
# data.name   1    -none- character
# observed  268    table  numeric  
# expected  268    -none- numeric  
# residuals 268    table  numeric  
# stdres    268    table  numeric 

# Melt results$residuals and store as resid
resid <- melt(results$residuals)

# Change names of resid
names(resid) <- c("FILL", "X", "residual")

# merge the two datasets:
str(DF_melted)
str(resid)

DF_all <- merge(DF_melted, resid)
str(DF_all)
summary(DF_all$residual)

# Update plot command
library(ggthemes)

ggplot(DF_all, aes(ymin = ymin, 
                   ymax = ymax,
                   xmin = xmin, 
                   xmax = xmax, 
                   fill = residual)) + 
  geom_rect() +
  scale_fill_gradient2(low = "firebrick4", mid = "white",
                       high = "deepskyblue4", midpoint = 0.0) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_tufte() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
       axis.text = element_text(size = 10))

# Add text to plot ------------------------------------------------------------

# Position for labels on x axis
DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2

# Position for labels on y axis (don't change)
# all vaules for DF_all$xmax that are maximal
index <- DF_all$xmax == max(DF_all$xmax) 
DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2

# Plot
ggplot(DF_all, aes(ymin = ymin, ymax = ymax, xmin = xmin, 
                   xmax = xmax, fill = residual)) + 
  geom_rect(col = "white") +
  # geom_text for ages (i.e. the x axis)
  geom_text(aes(x = xtext, 
                label = X),
            y = 1,
            size = 3,
            angle = 90,
            hjust = 1,
            show.legend = FALSE) +
  # geom_text for BMI (i.e. the fill axis)
  geom_text(aes(x = max(xmax), 
                y = ytext,
                label = FILL),
            size = 3,
            hjust = 1,
            show.legend  = FALSE) +
  scale_fill_gradient2() +
  theme_tufte() +
  theme(legend.position = "bottom")

# Generalize mosaic plot into a function --------------------------------------

# includes:
# - cleansing/melting raw data
# - performing ChiSq test
# - merging ChiSq residuals with original data
# - plotting results with calculated label positions  

# Load all packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggthemes)

# Script generalized into a function
mosaicGG <- function(data, X, FILL) {
  
  # Proportions in raw data
  DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
  DF$groupSum <- rowSums(DF)
  DF$xmax <- cumsum(DF$groupSum)
  DF$xmin <- DF$xmax - DF$groupSum
  DF$X <- row.names(DF)
  DF$groupSum <- NULL
  DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
  library(dplyr)
  DF_melted <- DF_melted %>% 
    group_by(X) %>% 
    mutate(ymax = cumsum(value/sum(value)),
           ymin = ymax - value/sum(value))
  
  # Chi-sq test
  results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
  resid <- melt(results$residuals)
  names(resid) <- c("FILL", "X", "residual")
  
  # Merge data
  DF_all <- merge(DF_melted, resid)
  
  # Positions for labels
  DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
  index <- DF_all$xmax == max(DF_all$xmax)
  DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
  
  # plot:
  g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin, 
                          xmax = xmax, fill = residual)) + 
    geom_rect(col = "white") +
    geom_text(aes(x = xtext, label = X),
              y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
    geom_text(aes(x = max(xmax),  y = ytext, label = FILL),
              size = 3, hjust = 1, show.legend = FALSE) +
    scale_fill_gradient2("Residuals") +
    scale_x_continuous("Individuals", expand = c(0,0)) +
    scale_y_continuous("Proportion", expand = c(0,0)) +
    theme_tufte() +
    theme(legend.position = "bottom")
  print(g)
}

# using the mosaicGG() function on different variables and datasets -----------

# BMI described by age
mosaicGG(adult, "SRAGE.P", "RBMI")

# Poverty described by age
mosaicGG(adult, "SRAGE.P", "POVLL")

# mtcars: am described by cyl
mosaicGG(mtcars, "cyl", "am")

# Vocab: vocabulary described by education
library(car)
mosaicGG(Vocab, "education", "vocabulary")

