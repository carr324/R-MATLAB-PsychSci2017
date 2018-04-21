# Carr, Brady, & Winkielman (2017) - Psych Science
# Experiment 1 - Example psychophysics code
# Evan W. Carr, December 2015

require(ggplot2)
require(lmerTest)
require(extrafont)
require(tidyverse)
require(gridExtra)
require(quickpsy)
require(ez)

# Fill in .csv file name with raw data file (set working directory):
rawData <- read.csv(".csv")

# Data file should contain the following columns:
# subject = subject id
# trialnum = trial number
# blocknum = block number
# stimlevel = level of stimulus (e.g., contrast, brightness, emotion, color, etc.), as numeric codes (e.g., -1, 0, 1, etc.)
# resp = response code (e.g., left/right, happy/angry, yes/no, etc.), as numeric code (e.g., 0 vs. 1)
# rt = reaction time, in ms

# Clean trials without responses or where RTs aren't between 200-3000 ms:
cleanData <- filter(morpheus6_rawData, !is.na(resp), rt >= 200, rt <= 3000)

# Log-transform RTs, and make subject id's a factor for modeling:
cleanData$log10rt <- log10(cleanData$rt)
cleanData$subject <- as.factor(cleanData$subject)

# Fit logistic, cumulative gaussian, and/or weibull functions ...

# Defaults:
# prob = 0.5 (probability to calculate threshold)
# B = 100 (number of bootstrap samples)
# guess = 0 (guess rate)
# lapses = 0 (lapse rate)
# ci = 0.95 (confidence intervals)

cleanData_logistic <- quickpsy(d = cleanData, x = stimlevel, k = resp, fun = logistic_fun)
cleanData_gaussian <- quickpsy(d = cleanData, x = stimlevel, k = resp, fun = cum_normal_fun)
cleanData_weibull <- quickpsy(d = cleanData, x = stimlevel, k = resp, fun = weibull_fun)

# A version of the plot code below was used to produce Figure 3 in Carr, Brady, & Winkielman (2017) ...
# (in Experiment 1, the "stimlevel" attribute was "PercentEmotion_num")
# Need to access the following parameters from quickpsy object:
#   - $curvesbootstrap = fitted bootstrap samples
#   - $curves = final estimate of psychometric function
#   - $averages = point estimates for "resp" (k) at each "stimlevel" (x)

ggplot() + 
  geom_line(data = cleanData_logistic$curvesbootstrap, aes(x = x, y = y, group = paste(sample)), color = "deepskyblue3", lwd = .2, alpha = .2) + 
  geom_line(data = cleanData_logistic$curves, aes(x = x, y = y, group = NA), color = "black", lwd = .75) +
  geom_point(data = cleanData_logistic$averages, aes(x = PercentEmotion_num, y = prob), color = "deepskyblue3", size = 5) +
  xlab("\nEmotion Level") +
  ylab("Probability of Selecting the\nTrained Face as Happier\n") +
  geom_hline(yintercept = .50, color = "black", linetype = "dashed") + 
  scale_x_continuous(breaks = c(-2, -1, 0, 1, 2), labels = c("50%\nAngry", "25%\nAngry", "Neutral", "25%\nHappy", "50%\nHappy")) +
  scale_y_continuous(breaks = c(".48"=0.48, ".50"=0.50, ".52"=0.52, ".54"=0.54, ".56"=0.56, ".58"=0.58, ".60"=0.60), limits = c(0.48, 0.60)) +
  theme_classic() +
  theme(text = element_text(family = "Gill Sans MT"), 
        axis.text.x  = element_text(size=16), 
        axis.text.y  = element_text(size=16), 
        axis.title.x = element_text(size=18), 
        axis.title.y = element_text(size=18),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        plot.margin = unit(c(0.5, 1, 0.5, 0.5),"cm"))

