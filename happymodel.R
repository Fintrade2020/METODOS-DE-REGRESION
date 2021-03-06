#En las pr�ximas lineas se desarrolla una rutina de regresi�n a partir de una base de datos llamada "happy" que podemos encontrar en la libreria "faraway" 
#AN�LISIS PRELIMINAR
#De la librer�a "faraway" llamamos base de datos "happy" (en ayuda puedes visualizar en qu� consiste la base)
library(faraway)
data(happy)
help(happy)
#Presente la correlaci�n y los gr�ficos de dispersi�n entre todas las variables.
attach(happy)
cor(happy)
plot(happy)
#REGRESI�N LINEAL M�LTIPLE
#Estime un modelo de regresi�n con todas las variables tomando "happy" como variable dependiente. Muestre los principales resultados. 
modelo1<-lm(happy$happy~money+sex+love+work)    
summary(modelo1)
#Calcule los intervalos de confianza de los estimadores.
confint(modelo1)
#Presente la tabla "anova".
anova(modelo1)
#Estime un modelo con las variables que fueron significativas individualmente al 10%. 
modelo2<-lm(happy$happy~money+love+work)    
summary(modelo2)
#Estime un modelo con las variables que fueron significativas al 5%. 
modelo3<-lm(happy$happy~love+work)    
summary(modelo3)
#REGRESI�N CON VARIABLES DUMMY
#Observe que la variable "love" es importante, sin embargo, se ha tomado como si fuera cuantitativa. Reconstruya esta variable mediante dos dummy y estime de nuevo el modelo donde las otras independientes ser�n "money" y "work". 
love2<-as.numeric(love==2)
love3 <- as.numeric(love==3)
modelo4<-lm(happy$happy~money+love2+love3+work)    
summary(modelo4)
#Ahora estime un modelo log lin.
modelo5<-lm(log(happy$happy)~money+love2+love3+work)    
summary(modelo5)
#SELECCI�N DEL MODELO
#Calcule los criterios AIC y BIC para los modelos tres primeros modelos. Determine el mejor modelo.
library(stats4)
AIC(modelo1,modelo2,modelo3)
BIC(modelo1,modelo2,modelo3)
#Utilice el metodo secuencial backward para obtener el mejor modelo
step(modelo1)
#VARIABLE DEPENDIENTE CUALITATIVA
#Modifiquemos el an�lisis y supongamos que tenemos sex como dependiente.
modlog1<-glm(sex~money+love+work,family="binomial")
summary(modlog1)
step(modlog1)
modlog2<-glm(sex~money+work,family="binomial")
summary(modlog2)
#Interpretemos los resultados primero calculando el pronostico y luego la probabilidad.
logratio<-predict(modlog2)
ratio<-exp(logratio)
piest<-ratio/(1+ratio)
plot(piest)
#La interpretaci�n depende de los valores x. Por lo tanto podemos calcular el efecto marginal a partir del promedio de la probabilidad.
modlog2$coefficients*mean(piest)*(1-mean(piest))library(faraway)
