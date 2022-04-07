# Mini-Project 8 Information Visualization

# clear workspace
rm(list = ls())

# load packages and read data
library(tidyverse)
library(magrittr)
library(ggplot2)
Data <- read_excel("Spring 2022/Wearble Bioinstrumentation/Self-Tracking Data.xlsx", col_types = c("date", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
Data$`Type of Workout` <- factor(Data$`Type of Workout`)

# find mean mood improvement for each workout type
mean_data <- Data %>% group_by(`Type of Workout`) %>% 
  summarize(mean_value = mean(`Mood Improvement (after - before)`))

tibble(Data)

# plot date vs mood improvement
ggplot(data = Data, aes(`Date`, `Mood Improvement (after - before)`, group = `Type of Workout`, color = `Type of Workout`)) +
  ggtitle("Mood Improvement for Each Workout Type") + 
  labs(x = "Date", y = "Mood Improvement (after - before)", color = "Type of Workout") +
  theme(legend.position = "top") + 
  geom_hline(aes(yintercept = 2.000000), color = "#DF536B", lwd = 1, linetype = "dashed") +
  geom_hline(aes(yintercept = 1.333333), color = "#61D04F", lwd = 1, linetype = "dashed") +
  geom_hline(aes(yintercept = 1.600000), color = "#28E2E5", lwd = 1, linetype = "dashed") +
  geom_hline(aes(yintercept = 1.086957), color = "mediumpurple1", lwd = 1, linetype = "dashed") +
  geom_point() 

# plot mood improvement vs intensity
# fit linear model to data, y = rr_fft, x = rr)
fit <- lm(Data$Intensity ~ Data$`Mood Improvement (after - before)`)

# combine text for equation
eq <- substitute(italic(y) == a + b %.% italic(x)*", "~~italic(r)^2~"="~r2, 
                 list(a = format(unname(coef(fit)[1]), digits = 2),
                      b = format(unname(coef(fit)[2]), digits = 2),
                      r2 = format(summary(fit)$r.squared, digits = 2)))
text <- as.character(as.expression(eq));

Data %<>% mutate(Intensity = `Average Heart Rate`/`Active Calories Burned`)
ggplot(data = Data, aes(x = Intensity, y = `Mood Improvement (after - before)`)) +
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(x = "Intensity (Calories Burned/ Average Heart Rate)") +
  ggtitle("Intensity vs Mood Improvement") +
  annotate("text", x = 1, y = 1.4, label = text, parse = TRUE)
