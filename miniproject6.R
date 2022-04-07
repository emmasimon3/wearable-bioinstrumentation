# Mini-Project 5 Validation

# clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(magrittr)
library(ggplot2)

# load data
data <- read.csv("C:\\Users\\Emma\\Downloads\\rrData.csv") # data should be 200 obs. x 4 variables
data$participant <- factor(data$participant) # make participant variable a factor
table(data$participant) # should be 10 repeats per participant


# LINE PLOT ----
# reshape the data into long format so that there are 4 columns: participant, time, feature (rr or rr_fft), and value
data_long <- data %>% gather(feature, value, -c(participant, time)) # data_long should be 400 obs. x 4 variables

# line plot
ggplot(data = data_long, aes(time, value, group = feature, color = feature)) +
  geom_line() + geom_point() +
  ggtitle("Figure 1: Line Plot") + 
  facet_wrap(vars(participant)) +
  labs(x = "Elapsed Time (s)", y = "RR (brpm)", color = "Feature (Simon)") +
  scale_color_hue(labels = c("RR", "RR FFT")) +
  theme(legend.position = "top")
  

# BAR PLOT ----
# find the mean and standard deviation within each participant-feature
summary <- data_long %>% group_by(participant, feature) %>% 
  summarize(mean_value = mean(value), sd_value = sd(value)) # summary should be  40 obs. x 4 variables

# bar plot
ggplot(summary, aes(participant, mean_value, fill = feature)) +
  ggtitle("Figure 2: Bar Plot") +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), 
                width = 0.2, position = position_dodge(.9)) +
  labs(x = "Participant", y = "RR (brpm)", fill = "Feature (Simon)") +
  theme(legend.position = "top") +
  scale_fill_discrete(labels = c("RR", "RR FFT"))
  
  
  # SCATTER PLOT ----
# fit linear model to data, y = rr_fft, x = rr)
fit <- lm(data$rr_fft ~ data$rr)

# combine text for equation
eq <- substitute(italic(y) == a + b %.% italic(x)*", "~~italic(r)^2~"="~r2, 
                 list(a = format(unname(coef(fit)[1]), digits = 2),
                      b = format(unname(coef(fit)[2]), digits = 2),
                      r2 = format(summary(fit)$r.squared, digits = 2)))
text <- as.character(as.expression(eq));

# scatter plot
ggplot(data, aes(x = rr, y = rr_fft)) +
  ggtitle("Figure 3: Scatter Plot") +
  annotate("text", x = 30, y = 30, label = text, parse = TRUE) +
  geom_point(alpha = 1/5) + geom_smooth(method = 'lm', formula = y~x) +
  labs(x = "RR (brpm)", y = "RR FFT (brpm)")


# BLAND-ALTMAN PLOT ----
# caclulate and save the differences between the two measures and the averages of the two measures
data %<>% mutate(difference = rr - rr_fft, average = (rr + rr_fft)/2)
mean_bias <- mean(data$difference)
LoA <- 1.96*sd(data$difference)
lower <- mean_bias - LoA
upper <- mean_bias + LoA

# Bland-Altman plot
ggplot(data, aes(x = average, y = difference)) +
  ggtitle("Figure 4: Bland-Altman Plot") +
  geom_point() +
  labs(x = "Average of Measures (brpm)", 
       y = "Difference Between Measures (rr - rr_fft) (brpm)") +
  geom_hline(aes(yintercept = mean_bias), color = "blue", lwd = 1.2) +
  geom_hline(aes(yintercept = lower), color = "pink", linetype = "dashed"
             , lwd = 1.2) +
  geom_hline(aes(yintercept = upper), color = "pink", linetype = "dashed"
             , lwd = 1.2) +
  geom_text(label = paste("Mean (LoA)\n", mean_bias, "(", LoA, ")")
             , x = 35, y = 23)

# BOX PLOT ----
# box plot
ggplot(data, aes(x = participant, y = difference, color = participant)) +
  ggtitle("Figure 5: Box Plot") +
  geom_boxplot() +
  labs(x = "Participant", 
       y = "Difference Between Measures (rr - rr_fft) (brpm)(Simon)") +
  theme(legend.position = "none")
