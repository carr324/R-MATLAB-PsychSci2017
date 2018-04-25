# Carr, Brady, & Winkielman (2017) - Psych Science
# Experiment 2 - Example psychophysics code
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
# condition = other manipulated factors (e.g., trained vs. untrained faces, as in Exp. 2 here)

# Clean trials without responses or where RTs aren't between 200-3000 ms:
cleanData <- filter(rawData, !is.na(resp), rt >= 200, rt <= 3000)

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

# NOTE: grouping argument should come from the "condition" column (trained vs. untrained faces in Exp. 2)

cleanData_logistic <- quickpsy(d = cleanData, x = stimlevel, k = resp, fun = logistic_fun, grouping = .(condition))
cleanData_gaussian <- quickpsy(d = cleanData, x = stimlevel, k = resp, fun = cum_normal_fun, grouping = .(condition))
cleanData_weibull <- quickpsy(d = cleanData, x = stimlevel, k = resp, fun = weibull_fun, grouping = .(condition))

# A version of the plot code below was used to produce Figure 6 (panel a) in Carr, Brady, & Winkielman (2017) ...
# (in Exp. 2, stimlevel = "stimulusEmotionPercentage_num"; condition = training_code)
# Need to access the following parameters from quickpsy object:
#   - $curvesbootstrap = fitted bootstrap samples
#   - $curves = final estimate of psychometric function
#   - $averages = point estimates for "resp" (k) at each "stimlevel" (x)

ggplot() + 
  geom_line(data = cleanData_logistic$curvesbootstrap, aes(x = x, y = y, color = factor(training_code, labels = c("Trained Faces", "Untrained Faces")), group = paste(sample, training_code)), lwd = .2, alpha = .1) + 
  geom_line(data = cleanData_logistic$curves, aes(x = x, y = y, group = factor(training_code), linetype = factor(training_code, labels = c("Trained Faces", "Untrained Faces"))), size = 0.75, color = 'black') +
  geom_point(data = cleanData_logistic$averages, aes(x = stimulusEmotionPercentage_num, y = prob, color = factor(training_code, labels = c("Trained Faces", "Untrained Faces")), shape = factor(training_code, labels = c("Trained Faces", "Untrained Faces"))), size = 4) +
  scale_color_manual(values = c("red", "deepskyblue2")) +
  scale_fill_manual(values = c("red", "deepskyblue2")) +
  xlab("Emotion Level") +
  ylab('Average Proportion of\n"Happy" Classifications\n') +
  scale_x_continuous(breaks = c(-2,-1,0,1,2), labels = c("50%\nAngry\n(-2)", "25%\nAngry\n(-1)", "Neutral\n(0)", "25%\nHappy\n(1)", "50%\nHappy\n(2)")) +
  scale_y_continuous(breaks = c("0" = 0, ".25" = .25, ".50" = .5, ".75" = .75, "1" = 1)) +
  theme_classic() +
  theme(text = element_text(family = "Gill Sans MT"), 
        axis.text.x  = element_text(size=16, color="black"), 
        axis.text.y  = element_text(size=16, color="black"), 
        axis.title.x = element_text(size=18, color="black"), 
        axis.title.y = element_text(size=18, color="black"),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        legend.position = "bottom",
        legend.key.width = unit(2.5, "cm"),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        plot.margin = unit(c(0.5, 1, 0.5, 0.5),"cm"))

# A version of the code below was used to do the threshold analyses and produce the plot for
#     Figure 6 (panel b) in Carr, Brady, & Winkielman (2017) ...
# (in Exp. 2, stimlevel = "stimulusEmotionPercentage_num"; condition = training_code)

# We used 4 threshold levels (20%, 40%, 60%, 80%), but this could could be extended to include more.
# We also used logistic functions, but you could change to cumulative gaussian or weibull (as shown above). 

# Need to access the following parameters from quickpsy object:
#   - $curvesbootstrap = fitted bootstrap samples
#   - $curves = final estimate of psychometric function
#   - $averages = point estimates for "resp" (k) at each "stimlevel" (x)
#   - $thresholds = threshold estimates
#   - $thresholdsci = confidence interval around thresholds

cleanData_t.2 <- quickpsy(d = cleanData, x = stimlevel, k = resp, grouping = .(condition), fun = logistic_fun, prob = .2)
cleanData_t.4 <- quickpsy(d = cleanData, x = stimlevel, k = resp, grouping = .(condition), fun = logistic_fun, prob = .4)
cleanData_t.6 <- quickpsy(d = cleanData, x = stimlevel, k = resp, grouping = .(condition), fun = logistic_fun, prob = .6)
cleanData_t.8 <- quickpsy(d = cleanData, x = stimlevel, k = resp, grouping = .(condition), fun = logistic_fun, prob = .8)

cleanData_t.2_plotData <- merge(cleanData_t.2$thresholds, cleanData_t.2$thresholdsci)
cleanData_t.4_plotData <- merge(cleanData_t.4$thresholds, cleanData_t.4$thresholdsci)
cleanData_t.6_plotData <- merge(cleanData_t.6$thresholds, cleanData_t.6$thresholdsci)
cleanData_t.8_plotData <- merge(cleanData_t.8$thresholds, cleanData_t.8$thresholdsci)

cleanData_thresholdPlotData <- rbind(cleanData_t.2_plotData,
                                     cleanData_t.4_plotData,
                                     cleanData_t.6_plotData,
                                     cleanData_t.8_plotData)

cleanData_thresholdPlotData$probPercent <- c('20%', '20%', '40%', '40%', '60%', '60%', '80%', '80%')

ggplot() +
  geom_pointrange(data = cleanData_thresholdPlotData, aes(x = condition, y = thre, ymin = threinf, ymax = thresup, color = condition, shape = condition), size = 1.25) +
  facet_wrap(factor("probPercent"), scales = "free_y", nrow = 1 , ncol = 4) +
  scale_colour_manual(values = c("red", "deepskyblue2")) +
  scale_fill_manual(values = c("red", "deepskyblue2")) +
  xlab("") +
  ylab("Emotion-Level Threshold\n") +
  theme_classic() +
  theme(text = element_text(family = "Gill Sans MT"), 
        axis.text.x  = element_text(size=16, color="black"), 
        axis.text.y  = element_text(size=16, color="black"), 
        axis.title.x = element_text(size=18, color="black"), 
        axis.title.y = element_text(size=18, color="black"),
        legend.position = "none",
        strip.background = element_rect(fill = "white"),
        strip.text.x = element_text(size = 18, color = "black"))
