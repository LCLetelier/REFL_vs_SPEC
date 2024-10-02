# COMPARISON OF METHODS 
#FOR ASSESSMENT OF REACTIVE SOLUBLE PHOSPHORUS (SRP) IN FRESHWATERS

# Leonidas Carrasco-Letelier 
# Natural Resources, Production and Environment, INIA Uruguay, Uruguay
# Forestry System, Production and Environment, INIA Uruguay, Uruguay

# Andrés Hirigoyen
# Forestry System, Production and Environment, INIA Uruguay, Uruguay


# Libraries
{  rm(list = ls()) 
  rm(list = ls(all.names = TRUE))  
library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(BlandAltmanLeh)
library(MethComp)
library(blandr)
library(gridExtra)
library(car)
library(lmtest)
library(gridExtra)
}

# Data are in R_SPEC_REFL.xlsx file that have three sheets:
# cc     : assessment of SRP concentrations in solutions with 
#        known concentrations using spectrophotometric method (SPEC),
#        and Reflectoquant TM system (REFL)
# cc_2   :the same data of cc array in two columns
# rivers : assessment of SRP concentrations in 300 freshwater samples 
#        of different Uruguayan rivers using spectrophotometric method 
#        (SPEC_f), and Reflectoquant TM system (REFL_f)

setwd("~/Dropbox/manus/Concordancia_Met_Pdis/Data_processing")  

# Data importation from Excel file
cc <- read_excel("R_SPEC_REFL.xlsx", sheet = "cc")
cc_2 <- read_excel("R_SPEC_REFL.xlsx", sheet = "cc_2")
rivers <- read_excel("R_SPEC_REFL.xlsx", sheet = "rivers")

# Verificar los datos importados
print(cc)
print(cc_2)
print(rivers)

str(cc)
str(cc_2)
str(rivers)

save(cc, file="cc.RData")
save(cc_2, file="cc_2.RData")
save(rivers, file="rivers.RData")

load("cc.RData")
load("cc_2.RData")
load("rivers.RData")

# CORRELATION ANALYSYS
# Row elimination with NA
str(cc)
str(cc_2)
str(rivers)

cc <- cc %>% drop_na(SPEC_1,SPEC_2,SPEC_3, REFL_1,REFL_2,REFL_3)
cc_2 <- cc_2 %>% drop_na(SPEC, REFL)
rivers <- rivers %>% drop_na(SPEC_f, REFL_f)

str(cc)
str(cc_2)
str(rivers)

save(cc, file="cc.RData")
save(cc_2, file="cc_2.RData")
save(rivers, file="rivers.RData")

#######################################################################
# ASSESSMENT OF SOLUTIONS WITH KNOWN CONCENTRATIONS
#######################################################################
# Figure of linear regression (raw data)
# Soluble reactive phosphorus (micrograms of P per liter)
FIG_model_cc2 <- ggplot(cc_2, aes(x = REFL, y = SPEC)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Reflectoquant",
       y = "Spectrophotometer UV-Vis") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18, margin = margin(t = 10, r = 10, b = 10, l = 10)),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 18)
  )

FIG_model_cc2 
ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/FIG_model_cc2.jpg", plot = FIG_model_cc2, dpi = 300, bg = "white")

# Linear regression model
model_cc2 <- lm(SPEC~ REFL , data = cc_2)
summary(model_cc2)

# Breusch-Pagan Test for homocedasticity
bptest(model_cc2)

# Shapiro-Wilk test
shapiro.test(residuals(model_cc2))

# Q-Q plot 
residuals <- data.frame(residuals = residuals(model_log_cc2))

qq_plot <- ggplot(residuals, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs( x = "Theoretical Quantiles",
       y = "Residual Quantiles") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 18, margin = margin(t = 15)),
    axis.title = element_text(size = 18, margin = margin(t = 15)),
    plot.title = element_text(size = 12, margin = margin(t = 15))
  )
qq_plot 

ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/FIG_QQ.jpg", plot = qq_plot, dpi = 300, bg = "white")

# quantiles of REFL
quantiles_REFL <- quantile(cc_2$REFL, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
print(quantiles_REFL)

#######################################################################
# ASSESSMENT OF FRESHWATER SAMPLES (SAMPLED RIVERS BY INA SA27)
#######################################################################
# rivers
# SPEC_f: SRP concentrations determined by spectrophotometric method
# REFL_f: SRP concentrations determined by reflectoometric method

FIG_model_rivers <- ggplot(rivers, aes(x = REFL_f, y = SPEC_f)) +
  geom_point(shape = 4, size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", show.legend = FALSE, level = 0.95) +  
  labs(x = "Reflectoquant",
       y = "Spectrophotometer") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18, margin = margin(t = 10, r = 10, b = 10, l = 10)),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 18)
  ) + 
  scale_x_continuous(limits = c(0, 2300)) + 
  scale_y_continuous(limits = c(0, 2300)) +  
  geom_vline(xintercept = 25, linetype = "dotted", color = "red") + 
  geom_hline(yintercept = 25, linetype = "dotted", color = "red")
  
FIG_model_rivers

ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/FIG_model_rivers.jpg", plot = FIG_model_rivers, dpi = 300, bg = "white")


# Linear regression model
model_rivers <- lm( SPEC_f~ REFL_f, data = rivers)
summary(model_rivers)
bptest(model_rivers )
shapiro.test(residuals(model_rivers ))

# Q-Q plot 
residuals2 <- data.frame(residuals = residuals(model_rivers))

qq_plot2 <- ggplot(residuals2, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs( x = "Theoretical Quantiles",
        y = "Residual Quantiles") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 18, margin = margin(t = 15)),
    axis.title = element_text(size = 18, margin = margin(t = 15)),
    plot.title = element_text(size = 12, margin = margin(t = 15))
  )
qq_plot2 

ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/FIG_QQ2.jpg", plot = qq_plot2, dpi = 300, bg = "white")


##################################################
# Conversion data to long format and re-assessment
##################################################

rivers_long <- rivers %>%
  pivot_longer(cols = c(SPEC_f, REFL_f), names_to = "Methods", values_to = "value")


#################    HISTOGRAMS 
# Histogram 1
histogram1 <- ggplot(rivers_long, aes(x = value, fill = Methods)) +
  geom_histogram(alpha = 0.5, position = "dodge", bins = 30) +  
  labs(x = "Value", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("SPEC_f" = "blue", "REFL_f" = "red"),
                    labels = c("SPEC_f" = "Spectrophotometer", "REFL_f" = "Reflectoquant")) +
  theme(legend.position = "none") +
  ylim(0, 90)  # Set y-axis limits


histogram1

ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/histogram1.jpg", plot = histogram1, dpi = 300, bg = "white")

# Histogram 2
histogram2 <- ggplot(rivers_long, aes(x = value, fill = Methods)) +
  geom_histogram(alpha = 0.5, position = "dodge", bins = 30) +  
  labs(x = "Value", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("SPEC_f" = "blue", "REFL_f" = "red"),
                    labels = c("SPEC_f" = "Spectrophotometer", "REFL_f" = "Reflectoquant")) +
  scale_x_log10() +  
  theme(legend.position = c(0.60, 0.9))  +
  ylim(0, 90)  # Set y-axis limits

histogram2

ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/histogram2.jpg", plot = histogram2, dpi = 300, bg = "white")

combined_histograms <- grid.arrange(histogram1, histogram2, ncol = 2)
ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/combined_histograms.jpg", 
       plot = combined_histograms, dpi = 300, bg = "white")

#####################################################
# Data of rivers analyzed by ranges
#rivers_0_100    : 0 - 100 micrograms/L
#rivers_300_600  : 300 -600 micrograms/L

# Subset for each range
rivers_0_100 <- subset(rivers, SPEC_f > 0 & SPEC_f <= 100)
rivers_300_600 <- subset(rivers, SPEC_f >= 300 & SPEC_f <= 600)

#linear regression models and residue analysis
model_0_100 <- lm(REFL_f ~ SPEC_f, data = rivers_0_100)
summary(model_0_100)
bptest(model_0_100)  
shapiro.test(residuals(model_0_100))

model_300_600 <- lm(REFL_f ~ SPEC_f, data = rivers_300_600)
summary(model_300_600)
bptest(model_300_600)  
shapiro.test(residuals(model_300_600))


# Create Q-Q plot for model 0-100
qq_plot_model_0_100 <- ggplot(data.frame(residuals = residuals(model_0_100)), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "0-100 micrograms/L",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

qq_plot_model_0_100


qq_plot_model_300_600 <- ggplot(data.frame(residuals = residuals(model_300_600)), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "300-600 micrograms/L",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

qq_plot_model_300_600 

# Display the plots
print(qq_plot_model_0_100)
print(qq_plot_model_300_600)

combined_qq <- grid.arrange(qq_plot_model_0_100, qq_plot_model_300_600, ncol = 2)
ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/combined_qq_plot_models.jpg", 
       plot = combined_qq , dpi = 300, bg = "white")


###############################################################################
#BLAND-ALTMAN analysis (with BlandAltmanLeh)

# means of SPEC and REFL
cc$SPEC_mean <- rowMeans(cbind(cc$SPEC_1, cc$SPEC_2, cc$SPEC_3))
cc$REFL_mean <- rowMeans(cbind(cc$REFL_1, cc$REFL_2, cc$REFL_3))

# mean and difference between two methods
cc$mean<- rowMeans(cbind(cc$SPEC_mean, cc$REFL_mean))
cc$difference <- cc$SPEC_mean - cc$REFL_mean

# difference mean
difference_mean<- mean(cc$difference, na.rm = TRUE) 
print(difference_mean)

head(cc)
str(cc)

# Bland-Altman (with BlandAltmanLeh)
fig_bland.altman.plot<-bland.altman.plot(cc$SPEC_mean, cc$REFL_mean, graph.sys = "ggplot2")
fig_bland.altman.plot <- fig_bland.altman.plot + xlab("Differences") + ylab("Mean of measurements")
fig_bland.altman.plot <- fig_bland.altman.plot + theme(
  text = element_text(size = 16), 
  axis.title = element_text(size = 16),  
  axis.text = element_text(size = 16),  
  plot.title = element_text(size = 14, face = "bold") 
)
fig_bland.altman.plot

ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/fig_bland.altman.plot.jpg", plot = fig_bland.altman.plot, dpi = 300, bg="white")

################### Rivers
# means of SPEC and REFL
rivers$SPEC_mean <- rowMeans(cbind(rivers$SPEC_f))
rivers$REFL_mean <- rowMeans(cbind(rivers$REFL_f))

# mean and difference between two methods
rivers$mean<- rowMeans(cbind(rivers$SPEC_mean, rivers$REFL_mean))
rivers$difference <- rivers$SPEC_mean - rivers$REFL_mean

# difference mean_rivers
difference_mean_riv<- mean(rivers$difference, na.rm = TRUE) 
print(difference_mean_riv)

head(rivers)
str(rivers)

# Bland-Altman (with BlandAltmanLeh)
fig_bland.altman.plot_riv<-bland.altman.plot(rivers$SPEC_mean, rivers$REFL_mean, graph.sys = "ggplot2")
fig_bland.altman.plot_riv <- fig_bland.altman.plot_riv + xlab("Differences") + ylab("Mean of measurements")
fig_bland.altman.plot_riv <- fig_bland.altman.plot_riv + theme(
  text = element_text(size = 16), 
  axis.title = element_text(size = 16),  
  axis.text = element_text(size = 16),  
  plot.title = element_text(size = 14, face = "bold") 
)
fig_bland.altman.plot_riv

ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/fig_bland.altman.plot_riv.jpg", plot = fig_bland.altman.plot_riv, dpi = 300, bg="white")
