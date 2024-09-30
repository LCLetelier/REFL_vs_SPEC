# COMPARACION DE DOS PROCEDIMENTOS ANALITICOS
# Leonidas Carrasco-Letelier
# Recursos Naturales, Producción y Ambiente
# INIA Uruguay, Uruguay

#COMPARACION DE CURVAS DE CALIBRACION  

{  rm(list = ls()) 
  rm(list = ls(all.names = TRUE))  
library(readr)
library(lattice, pos=16)
library(gridExtra, pos=16)
library(cowplot, pos=17)
library(ggplot2)
library(openxlsx)
}
#setwd(r"(C:/Users/ahirigoyen/Documents/Colaboraciones/Leonidas/Evaluacion P)")
setwd("/home/lcarrasco/Dropbox/manus_PDIS_metodos/datos/")

Curvas <- read.csv("PDIS_2_calibra.csv", header = TRUE, sep = ";", quote = "")
Curvas
save(Curvas, file="/home/lcarrasco/Dropbox/manus_PDIS_metodos/planillas/Curvas.RData")
load("Curvas.RData")

# # AJUSTE DEL MODELO DE REGRESIÓN LINEAL
# modelo1 <- lm(Est_espect ~ Referencia, data = Curvas)
# modelo2 <- lm(Est_fotom ~ Referencia, data = Curvas)
# modelo3 <- lm(Est_fotom ~ Est_espect, data = Curvas)
# modelo4 <- lm(Dif_espec ~ Referencia, data = Curvas)
# modelo5 <- lm(Dif_fotom ~ Referencia, data = Curvas)
# 
# # EVALUACION DE LA SIGNIFICANCIA DE LA REGRESIÓN
# summary(modelo1)  # Proporciona un resumen del modelo con coeficientes, valores p, R-cuadrado, etc.
# summary(modelo2)
# summary(modelo3)
# summary(modelo4)
# summary(modelo5)
# 
# # GRAFICA DE LA REGRESIÓN Y SU INTERVALO DE CONFIANZA
# scatterplot1 <- ggplot(Curvas, aes(x = Referencia, y = Est_espect)) +
#   geom_point() +
#   labs(x = "Estándar de referencia (ug P-PO4/L)", y = "Espectrofotometría (ug P-PO4/L)") +
#   theme_minimal()
# 
# scatterplot2 <- ggplot(Curvas, aes(x = Referencia, y = Est_fotom)) +
#   geom_point() +
#   labs(x = "Estándar de referencia (ug P-PO4/L)", y = "Reflectoquant (ug P-PO4/L)") +
#   theme_minimal()
# 
# scatterplot3 <- ggplot(Curvas, aes(x = Est_espect, y = Est_fotom)) +
#   geom_point() +
#   labs(x = "Método de referencia (ug P-PO4/L)", y = "Reflectoquant (ug P-PO4/L)") +
#   theme_minimal()
# 
# scatterplot4 <- ggplot(Curvas, aes(x = Referencia, y = Dif_espec)) +
#   geom_point() +
#   labs(x = "Estándar de referencia (ug P-PO4/L)", y = "Espectrofotometría (ug P-PO4/L)") +
#   theme_minimal()
# 
# scatterplot5 <- ggplot(Curvas, aes(x = Referencia, y = Dif_fotom)) +
#   geom_point() +
#   labs(x = "Estándar de referencia (ug P-PO4/L)", y = "Reflectoquant (ug P-PO4/L)") +
#   theme_minimal()
# 
# # LÍNEA DE REGRESIÓN Y EL INTERVALO DE CONFIANZA
# regression_plot1 <- scatterplot1 +
#   geom_smooth(method = "lm", se = TRUE, color = "blue")
# regression_plot2 <- scatterplot2 +
#   geom_smooth(method = "lm", se = TRUE, color = "blue")
# regression_plot3 <- scatterplot3 +
#   geom_smooth(method = "lm", se = TRUE, color = "blue")
# 
# regression_plot4 <- scatterplot4 +
#   geom_smooth(method = "lm", se = TRUE, color = "blue")
# regression_plot5 <- scatterplot5 +
#   geom_smooth(method = "lm", se = TRUE, color = "blue")
# 
# # Gráfico
# print(regression_plot1)
# print(regression_plot2)
# print(regression_plot3)
# print(regression_plot4)
# print(regression_plot5)
# 
#   
# # Para comparar los dos procedimientos analíticos y validar el método fotométrico como una metodología alternativa
# # al procedimiento volumétrico, se evaluaran siete pruebas estadisticas:
#   
#   # Medidas del método espectrofotometrico
#   mean_espectro <- mean(Curvas$Est_espect)
#   median_espectro <- median(Curvas$Est_espect)
#   sd_espectro <- sd(Curvas$Est_espect)
#   range_espectro <- range(Curvas$Est_espect)
#   
#   # Medidas del método fotométrico
#   mean_photometric <- mean(Curvas$Est_fotom)
#   median_photometric <- median(Curvas$Est_fotom)
#   sd_photometric <- sd(Curvas$Est_fotom)
#   range_photometric <- range(Curvas$Est_fotom)
#   
# # GRAFICO DE DISPERSION: 
#   # Crea un gráfico que muestra las medidas de concentración de fósforo obtenidas por el método volumétrico en el eje X 
#   # y las medidas obtenidas por el método fotométrico en el eje Y, para visualizar la relación entre los dos métodos y
#   # detectar posibles patrones o discrepancias.
#   
#   plot(Curvas$Est_espect, Curvas$Est_fotom, 
#        xlab = "Spectrometric Method", 
#        ylab = "Photometric Method",
#        main = "Scatter Plot")
#   
#  
# # REGRESION LINEAL: 
# # Para determinar la relación entre las medidas obtenidas por ambos métodos. 
# #  Esto implica ajustar una línea de regresión a los datos y calcular el coeficiente de correlación. El que indicará el
# #  grado de relación lineal entre los dos métodos.
#   class(Curvas)
#   str(Curvas)
#   
#   lm_model <- lm(Curvas$Est_espect~ Curvas$Est_fotom)
#   summary(lm_model)
#   correlation <- cor(Curvas$Est_espect, Curvas$Est_fotom)
#   print(correlation)
#  
# # ERROR MEDIO ABSOLUTO (MAE)
# # Calcula el error medio absoluto entre los valores de concentración de fósforo obtenidos
# # por ambos métodos. El MAE te dará una medida de la diferencia promedio entre las medidas de ambos métodos, sin considerar
# # la dirección del error. Un MAE bajo indicaría una buena concordancia entre los dos métodos.
# 
#   mae1 <- mean(abs(Curvas$Est_espect - Curvas$Referencia))
#   mae2 <- mean(abs(Curvas$Est_fotom - Curvas$Referencia))
#   mae3 <- mean(abs(Curvas$Est_espect - Curvas$Est_fotom))
#   mae1
#   mae2
#   mae3
#   
# # Error cuadrático medio (MSE) y raíz del error cuadrático medio (RMSE): Calcula el error cuadrático medio entre
# # los valores de concentración de fósforo obtenidos por ambos métodos. El MSE es el promedio de los errores al cuadrado, 
# # y el RMSE es la raíz cuadrada del MSE. Estos estadísticos tienen en cuenta tanto la magnitud como la dirección del error.
#   
#   #Error cuadrático medio (MSE) y raíz del error cuadrático medio (RMSE):
#   mse1 <- mean((Curvas$Est_espect - Curvas$Referencia)^2)
#   rmse1 <- sqrt(mse1)
#   mse2 <- mean((Curvas$Est_fotom - Curvas$Referencia)^2)
#   rmse2 <- sqrt(mse2)
#   mse3 <- mean((Curvas$Est_espect - Curvas$Est_fotom)^2)
#   rmse3 <- sqrt(mse3)
#   rmse1
#   rmse2 
#   rmse3
#   
#   
# #MethComp UN PAQUETE PARA COMPARAR METODOS
#   # http://staff.pubhealth.ku.dk/~bxc/MethComp/introMethComp.pdf
  
#requerido para instalar MethComp
 #sudo apt-get install jags 
 # install.packages("MethComp")
  library(MethComp)
 
 #corr.measures: Calcula el coeficiente de correlación de 
 #concordancia entre dos métodos de medición.
 # corr_measures<-corr.measures(Curvas$Est_espect, Curvas$Est_fotom)
 # corr_measures
 # 
 #Passing-Bablok regression - plot method
 #A plot method for the "PBreg" class object, that is a result of Passing-Bablok
 # #regression.
 # a <- data.frame(x=Curvas$Est_espect,y=Curvas$Est_fotom)
 # a
# Instala y# Lee el archivo Excel carga la librería readxl si no está instalada
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)


datos <- read_excel("20240401_Curvas_ah.xlsx", sheet = 1)
datos
###########################################
###########################################
###########################################
# Creo Objeto Meth
datos<-read.xlsx("20240401_Curvas_ah.xlsx",1) # OJO Q CAMBIE EL FORMATO
names(datos)
datos$Metodo<-as.factor(datos$Metodo)

datos1<- Meth(datos, meth=3,item=5,y=4,repl = 1) # El orden es 1:meth, 2:item, 3:repl,4:y

summary(datos1)

PBreg_a<- PBreg(datos1)
PBreg_a
print(PBreg_a)
par(mfrow=c(2,2))
plot(PBreg_a, s=1:4)
################################################################
# Lo importanrte aca es ver los IC de los coef
PBreg_a$coefficients 
# Passing-Bablok :
# Si 1 está dentro del intervalo de confianza del gradiente y 0 dentro del 
# intervalo de confianza del intercepto, entonces los dos métodos son 
# comparables dentro del intervalo de concentración investigado.
# Si 1 no se encuentra en el intervalo de confianza del gradiente,
# existe una diferencia proporcional entre los dos métodos.Si 0 no se
# encuentra en el intervalo de confianza del intervalo, existe una diferencia
# sistemática.
################################################################

 # Ajusta un modelo de regresión Deming para estimar la relación entre dos métodos de medición.
 # Regression with errors in both variables (Deming regression)
 #FitDeming(): -comando antiguo- Ajusta un modelo de regresión Deming ponderado o no ponderado 
 #para estimar la relación entre dos métodos de medición
 
 # deming<-deming(Curvas$Est_espect, Curvas$Est_fotom, vr = sdr^2, sdr = sqrt(vr), boot = FALSE, keep.boot = FALSE, alpha = 0.05)
 # deming
 # summary(deming)
 
 # Intervalo de confianza: Calcula intervalos de confianza para las diferencias entre las medidas obtenidas
 # por ambos métodos. Esto te ayudará a determinar si las diferencias son estadísticamente significativas y
 # si el método fotométrico es equivalente al método volumétrico dentro de un intervalo de confianza específico.
 
 # #Intervalo de confianza (intervalo de confianza del 95%):
 # diff <- Curvas$Est_espect- Curvas$Est_fotom
 # confidence_interval <- t.test(diff)$conf.int
 # confidence_interval 
 
 
 ##### COMPARACIONES DE METODOS QUE NO ME HAN FUNCIONADO AUN
 #### BlandALtman no me esta funcionando
par(mar=c(3,3,3,3), mgp=c(3,1,0)/1.6 )
BA.plot(datos1, repl.conn=FALSE, model=NULL,eqn=F,xaxs="i", yaxs="i")#

BA.plot(datos1, repl.conn=T)
BA.est(datos1)
#           
#           
# x0<-Curvas$Est_espect
# y0<-Curvas$Est_fotom
#   
# BA.plot(Curvas, x0, y0,
#         meth.names = NULL,
#         mean.repl = FALSE,
#         conn.repl = !mean.repl,
#         lwd.conn = 1,
#         col.conn = "black",
#         comp.levels = 2:1)

 # BlandAltman(): Crea un gráfico de Bland-Altman para visualizar la diferencia entre 
  #dos métodos de medición versus su media.
  # BlandAltman<-BlandAltman(Curvas$Est_espect, Curvas$Est_fotom)
  



  

 