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

# REGRESSION MODELS
# Assessment of solutions with known concentrations
model_cc2 <- lm(REFL ~ SPEC, data = cc_2)
# Assessment of freshwater samples (sampled rivers by INA Sa27)
model_rivers <- lm(REFL_f ~ SPEC_f, data = rivers)

# Summary of models
summary(model_cc2)
summary(model_rivers)

# Linear regression of assessment of solutions with known concentrations
FIG_model_cc2<-ggplot(cc_2, aes(x = SPEC, y = REFL)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Soluble reactive phosphorus (micrograms of P per liter)",
       x = "Spectrophotometer UV-Vis",
       y = "Reflectoquant") +
  theme_minimal()

FIG_model_cc2 
ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/FIG_model_cc2.jpg", plot = FIG_model_cc2, dpi = 300, bg = "white")


## SAMPLES FROM RIVERS

# Conversion data to long format
rivers_long <- rivers %>%
  pivot_longer(cols = c(SPEC_f, REFL_f), names_to = "Methods", values_to = "value")

# Histograms
histogram<-ggplot(rivers_long, aes(x = value, fill = Methods)) +
  geom_histogram(alpha = 0.5, position = "dodge", bins = 30) +  
  labs( x = "Value",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("SPEC_f" = "blue", "REFL_f" = "red"),
                    labels = c("SPEC_f" = "Spectrophotometer", "REFL_f" = "Reflectoquant"))+
  scale_x_log10() +  
  theme(legend.position = c(0.80, 0.9))  

histogram
ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/histogram.jpg", plot = histogram, dpi = 300, bg = "white")



FIG_model_rivers <- ggplot(rivers, aes(x = SPEC_f, y = REFL_f)) +
  geom_point(shape = 4, size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", show.legend = FALSE) +  # Confidence interval in light blue
  labs(title = "Soluble reactive phosphorus (micrograms of P per liter)",
       x = "Spectrophotometer",
       y = "Reflectoquant") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 100)) +  # Set X axis limits
  scale_y_continuous(limits = c(0, 100)) +  # Set Y axis limits
  geom_vline(xintercept = 25, linetype = "dotted", color = "red") +  # Red dotted line at X = 25
  geom_hline(yintercept = 25, linetype = "dotted", color = "red") +  # Red dotted line at Y = 25
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

FIG_model_rivers

###################################

# Suponiendo que 'rivers' contiene tus datos
# SPEC_f: valores del método espectrofotométrico
# REFL_f: valores del método reflectoquant

# 1. Filtrar los datos por los tres rangos

rivers_0_2500 <- rivers
rivers_60_2500 <- subset(rivers, SPEC_f >= 60)
rivers_0_100 <- subset(rivers, SPEC_f <= 100)

# 2. Crear modelos de regresión para cada rango

# Modelo 0-2500
model_0_2500 <- lm(REFL_f ~ SPEC_f, data = rivers_0_2500)
summary(model_0_2500)

# Modelo 60-2500
model_60_2500 <- lm(REFL_f ~ SPEC_f, data = rivers_60_2500)
summary(model_60_2500)

# Modelo 0-100
model_0_100 <- lm(REFL_f ~ SPEC_f, data = rivers_0_100)
summary(model_0_100)

# 3. Crear los gráficos de regresión

plot_0_2500 <- ggplot(rivers_0_2500, aes(x = SPEC_f, y = REFL_f)) +
  geom_point(shape = 4, size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", show.legend = FALSE) +  
  labs(title = "0-2500 micrograms/L",
       x = "Spectrophotometer",
       y = "Reflectoquant") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 2500)) +  
  scale_y_continuous(limits = c(0, 2500)) +
  geom_vline(xintercept = 25, linetype = "dotted", color = "red") +  
  geom_hline(yintercept = 25, linetype = "dotted", color = "red") +
  theme(plot.title = element_text(hjust = 0.5))

plot_60_2500 <- ggplot(rivers_60_2500, aes(x = SPEC_f, y = REFL_f)) +
  geom_point(shape = 4, size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", show.legend = FALSE) +  
  labs(title = "60-2500 micrograms/L",
       x = "Spectrophotometer",
       y = "Reflectoquant") +
  theme_minimal() +
  scale_x_continuous(limits = c(60, 2500)) +  
  scale_y_continuous(limits = c(0, 2500)) +
  geom_vline(xintercept = 25, linetype = "dotted", color = "red") +  
  geom_hline(yintercept = 25, linetype = "dotted", color = "red") +
  theme(plot.title = element_text(hjust = 0.5))

plot_0_100 <- ggplot(rivers_0_100, aes(x = SPEC_f, y = REFL_f)) +
  geom_point(shape = 4, size = 3) + 
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue", show.legend = FALSE) +  
  labs(title = "0-100 micrograms/L",
       x = "Spectrophotometer",
       y = "Reflectoquant") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 100)) +  
  scale_y_continuous(limits = c(0, 100)) +
  geom_vline(xintercept = 25, linetype = "dotted", color = "red") +  
  geom_hline(yintercept = 25, linetype = "dotted", color = "red") +
  theme(plot.title = element_text(hjust = 0.5))

# Unified figure with three plots
grid.arrange(plot_0_2500, plot_60_2500, plot_0_100, ncol = 3)


# Breusch-Pagan Test for homocedasticity
# model 0-2500  micrograms/L
bptest(model_0_2500)  
# model 60-2500  micrograms/L
bptest(model_60_2500)
# model 0-100  micrograms/L
bptest(model_0_100)

# Residues vs adjusted values 
#model 0-2500 micrograms/L
plot(fitted(model_0_2500), residuals(model_0_2500),
     xlab = "Adjusted values", ylab = "Residues",
     main = "Residues vs Adjusted values (0-2500)")
abline(h = 0, col = "red")

#model 60-2500 micrograms/L
plot(fitted(model_60_2500), residuals(model_60_2500),
     xlab = "Adjusted values", ylab = "Residues",
     main = "Residues vs Adjusted values (60-2500)")
abline(h = 0, col = "red")

# model 0-100 micrograms/L
plot(fitted(model_0_100), residuals(model_0_100),
     xlab = "Adjusted values", ylab = "Residues",
     main = "Residues vs Adjusted values (0-100)")
abline(h = 0, col = "red")






#ANÁLISIS DE BLAND-ALTMAN

# promedio de las réplicas para SPEC y REFL
cc_2$SPEC_promedio <- rowMeans(cbind(cc_2$SPEC_1, cc_2$SPEC_2, cc_2$SPEC_3))
cc_2$REFL_promedio <- rowMeans(cbind(cc_2$REFL_1, cc_2$REFL_2, cc_2$REFL_3))

# media y la diferencia entre los dos métodos
metodos$media <- rowMeans(cbind(metodos$SPEC_promedio, metodos$REFL_promedio))
metodos$diferencia <- metodos$SPEC_promedio - metodos$REFL_promedio

# media de la diferencia
media_diferencia <- mean(metodos$diferencia, na.rm = TRUE)  # na.rm = TRUE elimina los NA
print(media_diferencia)

# estructura del dataframe
head(metodos)

# análisis de Bland-Altman (with BlandAltmanLeh)
fig3<-bland.altman.plot(metodos$SPEC_promedio, metodos$REFL_promedio, graph.sys = "ggplot2")
fig3 <- fig3 + xlab("Differences") + ylab("Mean of measurements")
fig3 <- fig3 + theme(
  text = element_text(size = 12),  # Tamaño general del texto
  axis.title = element_text(size = 12),  # Tamaño de los títulos de los ejes
  axis.text = element_text(size = 12),  # Tamaño de los textos de los ejes
  plot.title = element_text(size = 12, face = "bold")  # Tamaño del título del gráfico
)

ggsave("~/Dropbox/manus/Concordancia_Met_Pdis/Figures/fig3.png", plot = fig3, dpi = 300)


  

 
