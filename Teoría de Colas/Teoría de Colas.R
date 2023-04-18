#CASO DE ANÁLISIS
  #En una farmacia existe una única caja para los clientes. A esa caja llegan clientes con tiempos entre llegadas que siguen una distribución exponencial con media de 5 clientes por hora. 
  #Los tiempos de cobro siguen una distribución exponencial con una duración media de 10 minutos.
  #El salario y las prestaciones del cajero se estiman en $180 por hora, en una jornada laboral de 8hs. Además, la farmacia dese a asignar un costo de $25 por hora al tiempo de espera de un cliente.
#Primera Parte Se solicita calcular:
#1. -La probabilidad de que un cliente tenga que esperar en la cola.
#. -El tiempo de espera promedio.
#3. -El número promedio de clientes en el sistema.
#4. -Tiempo ocioso durante la jornada de trabajo.
#5. -Costo total para el sistema
#SegundaParte
  #Luego de ver el tiempo promedio de espera en la cola de los clientes y frente a la situación actual del COVID-19
  #, el dueño decide evaluar la alternativa de habilitar una segunda caja para que puedan ser atendidos dos clientes al mismo tiempo manteniendo una única cola y así poder reducir dicho tiempo.
  #Evaluando esta alternativa, ¿Se logra reducir el tiempo promedio de espera de un cliente en la cola?



install.packages("knitr")

##para operar con diferentes formatos de fechas

install.packages("lubridate")

library(knitr)
library(lubridate)



### RESOLUCI�N DE LA PRIMER PARTE

## Seteo de par�metros para la simulaci�n de tiempos de clientes

set.seed(12345678, "Mersenne-Twister")   


lambda <- 5  #media de cantidad de llegada de clientes
mu <- 6 #media de tiempo de servicio por cliente
t <- 0  #tiempo de simulaci�n
ocupado <- FALSE  #indica si el punto de servicio est� ocupado o no
cola <- 0 #el n�mero de clientes en la cola
llegada <- rexp(1, lambda / 60) #simulaci�n de hora de llegada de un cliente
prox_llegada <- t + llegada # es la hora de la pr�xima llegada de un cliente
prox_salida <- 9999 # es la hora de la pr�xima salida de un cliente
tsim <- 480  #duraci�n de la simulaci�n, expresada en minutos (8 hs)

# se definen vectores de eventos: almacenan el estado del sistema cada vez que ocurre una llegada o una salida
tm <- numeric() #el tiempo del evento en minutos
oc <- logical() #si el sistema est� ocupado o no, True or False
cl <- integer() #s el n�mero de clientes en la cola

#se define el dataframe (o tabla de datos) de la trayectoria de cada cliente
clientes <- data.frame("t_ll"=numeric(), # hora de llegada
                       "t_sa"=numeric(), # hora de salida
                       "t_se"=numeric(), # hora del servicio
                       "d_se"=numeric()) # duraci�n del servicio

# Simulaci�n de la trayectoria de los clientes
# BUCLE: utiliza la sentencia WHILE que implica iterar el procesar mientras se cumple determinada condici�n
while (t < tsim){
  #se define como se completaran los vectores de eventos
  tm <- c(tm, t) # el campo tm se llena con la variable t
  oc <- c(oc, ocupado) # el campo oc se llena con la variable ocupado
  cl <- c(cl, cola) # el campo cl se llena con la variable cola
  #condicional para definir el tiempo de arribo a la cola
  if (prox_llegada < prox_salida){
                                  t <- prox_llegada
                                  llegada <- rexp(1, lambda / 60)
                                  prox_llegada <- t + llegada
                                  cliente <- data.frame("t_ll"=t, "t_sa"=NA, "t_se"=NA, "d_se"=NA)
                                  clientes <- rbind(clientes, cliente)
                                     ## condicional para definir la ocupaci�n de la cola, dado que el servicio est� ocupado
                                      if (ocupado == TRUE){
                                                            cola <- cola + 1  #dado que el servicio est� ocupado, el cliente va a la cola
                                                          } else {
                                                                    ocupado <- TRUE
                                                                    servicio <- rexp(1, mu / 60)
                                                                    prox_salida <- t + servicio
                                                                    clientes[nrow(clientes), "t_sa"] = prox_salida
                                                                    clientes[nrow(clientes), "t_se"] = t
                                                                    clientes[nrow(clientes), "d_se"] = servicio
                                                                  }
                                 } else { ## condicional para definir la ocupaci�n de la cola, dado que se va el cliente
                                         t <- prox_salida
                                          if (cola > 0){ #si hab�a clientes en la cola
                                                        cola <- cola - 1
                                                        servicio <- rexp(1, mu / 60)
                                                        prox_salida <- t + servicio
                                                        clientes[nrow(clientes) - cola, "t_sa"] = prox_salida #hora de salida
                                                        clientes[nrow(clientes) - cola, "t_se"] = t # la hora que lo empiezan a atender
                                                        clientes[nrow(clientes) - cola, "d_se"] = servicio #duraci�n del servicio
                                                       } else { #si no hab�a clientes en la cola
                                                                ocupado <- FALSE
                                                                prox_salida <- 9999
                                                              }
                                        }
                                }


#Generaci�n de la tabla de Clientes

#se crea un dataframe con todos los campos que ser�n completados con la informaci�n previamente simulada
cl_table <- data.frame("n_cli"=integer(),
                       "llega"=character(),
                       "sale"=character(),
                       "d_ocio"=numeric(),
                       "d_cola"=numeric(),
                       "d_serv"=numeric(),
                       "d_sis"=numeric())

#se setea el formato fecha hora de inicio

t_ini <- strptime("09:00:00", "%H:%M:%S")
t_ini

#se define una funci�n que calcula hora, minutos y segundos sin la fecha
time_str <- function(t_ini, t_ev){
                                  hr <- hours(t_ev %/% 60)
                                  min <- minutes(round(t_ev %% 60))
                                  sec <- seconds(round((t_ev * 60) %% 60))
                                  as.character(t_ini + hr + min + sec, format = "%H:%M:%S")
                                }

# se llena la tabla de clientes con un loop
#FOR:  recorre cada registro del dataframe
#IF: por cada recorrido del FOR, se establece un condicional para el c�lculo de valor

anterior <- 0
for (i in 1:nrow(clientes)){
                             cliente <- clientes[i,]
                                          if (all(!is.na(cliente))){ #condicional que evita tomar registros vac�os
                                                                    t_ll <- time_str(t_ini, cliente$t_ll)
                                                                    t_sa <- time_str(t_ini, cliente$t_sa)
                                                                    d_ocio <- max(0, cliente$t_ll - anterior)
                                                                    d_cola <- cliente$t_se - cliente$t_ll
                                                                    d_serv <- cliente$d_se
                                                                    d_sis <- cliente$t_sa - cliente$t_ll
                                                                  }
                            anterior <- cliente$t_sa
                            row <- data.frame("n_cli"=i, "llega"=t_ll, "sale"=t_sa,
                                              "d_ocio"=d_ocio, "d_cola"=d_cola, "d_serv"=d_serv, "d_sis"=d_sis)
                            cl_table <- rbind(cl_table, row)
                          }
kable(cl_table, digits = 2, caption = "Traza de los clientes")


#### Optativo: exportamos la tabla de clientes a un csv
####write.csv2(cl_table, "clientes.csv")


### graficos a partir de la tabla anterior

plot(tm, cl + oc, col="green", type="s", main="N�mero de Clientes en el sistema",
     xlab = "tiempo (min)", ylab = "# Clientes")

plot(tm, cl, col="red", type="s", main="N�mero de Clientes en la Cola",
     xlab = "tiempo (min)", ylab = "# Clientes")

### c�lculo de medidas de desempe�o

#N�mero promedio de clientes en el sistema
prod <- numeric(length(tm) - 1)
for (i in 1:length(tm)-1){
  prod[i] <- (tm[i+1] - tm[i]) * (cl[i] + oc[i])
}
L <- sum(prod) / tm[length(tm)]

#N�mero promedio de clientes en la cola
prod <- numeric(length(tm) - 1)
for (i in 1:length(tm)-1){
  prod[i] <- (tm[i+1] - tm[i]) * (cl[i])
}
Lq <- sum(prod) / tm[length(tm)]

#Fracci�n estimada de tiempo ocioso
prod <- numeric(length(tm) - 1)
for (i in 1:length(tm)-1){
  prod[i] <- (tm[i+1] - tm[i]) * (!oc[i])
}
P0 <- sum(prod) / tm[length(tm)]

#Ocupaci�n promedio estimada
prod <- numeric(length(tm) - 1)
for (i in 1:length(tm)-1){
  prod[i] <- (tm[i+1] - tm[i]) * (oc[i])
}
Rho <- sum(prod) / tm[length(tm)]

#Tiempo de espera promedio en el sistema
W <- sum(cl_table$d_sis) / nrow(cl_table)

#Tiempo de espera promedio en la cola
Wq <- sum(cl_table$d_cola) / nrow(cl_table)


CT<- 25*L + 180*1


#se vuelcan los resultados a un data frame para luego obtenerlos ordenados
resultados <- data.frame ('Nro promedio de clientes en el sistema'  = L,
                          'Nro promedio de clientes en la cola' =Lq,
                          'Tiempo ocioso del sistema' = P0,
                          'Ocupaci�n promedio estimada'=Rho,
                          'Tiempo de espera promedio en el sistema'=W,
                          'Tiempo de espera promedio en la cola'=Wq,
                          'Costo total del sistema'=CT)

##Se traspone la matriz(data frame) y se muestra
print(t(resultados))


### RESOLUCI�N DE LA SEGUNDA PARTE

## contin�a siendo un sistema M/M/1 pero ahora lo particioan
## Seteo de par�metros para la simulaci�n de tiempos de clientes

set.seed(12345678, "Mersenne-Twister")   ##seteamos una semilla para obtener siempre la misma simulaci�n


lambda <- 5  #media de cantidad de llegada de clientes
mu <- 6 #media de tiempo de servicio por cliente
t <- 0  #tiempo de simulaci�n
ocupado <- FALSE  #indica si el punto de servicio est� ocupado o no
cola <- 0 #el n�mero de clientes en la cola
llegada <- rexp(1, (lambda / 60)/2) #simulaci�n de hora de llegada de un cliente
prox_llegada <- t + llegada # es la hora de la pr�xima llegada de un cliente
prox_salida <- 9999 # es la hora de la pr�xima salida de un cliente
tsim <- 480  #duraci�n de la simulaci�n, expresada en minutos (8 hs)

# se definen vectores de eventos: almacenan el estado del sistema cada vez que ocurre una llegada o una salida
tm <- numeric() #el tiempo del evento en minutos
oc <- logical() #si el sistema est� ocupado o no, True or False
cl <- integer() #s el n�mero de clientes en la cola

#se define el dataframe (o tabla de datos) de la trayectoria de cada cliente
clientes <- data.frame("t_ll"=numeric(), # hora de llegada
                       "t_sa"=numeric(), # hora de salida
                       "t_se"=numeric(), # hora del servicio
                       "d_se"=numeric()) # duraci�n del servicio

# Simulaci�n de la trayectoria de los clientes
# BUCLE: utiliza la sentencia WHILE que implica iterar el procesar mientras se cumple determinada condici�n
while (t < tsim){
  #se define como se completaran los vectores de eventos
  tm <- c(tm, t) # el campo tm se llena con la variable t
  oc <- c(oc, ocupado) # el campo oc se llena con la variable ocupado
  cl <- c(cl, cola) # el campo cl se llena con la variable cola
  #condicional para definir el tiempo de arribo a la cola
  if (prox_llegada < prox_salida){
    t <- prox_llegada
    llegada <- rexp(1, (lambda / 60)/2)
    prox_llegada <- t + llegada
    cliente <- data.frame("t_ll"=t, "t_sa"=NA, "t_se"=NA, "d_se"=NA)
    clientes <- rbind(clientes, cliente)
    ## condicional para definir la ocupaci�n de la cola, dado que el servicio est� ocupado
    if (ocupado == TRUE){
      cola <- cola + 1  #dado que el servicio est� ocupado, el cliente va a la cola
    } else {
      ocupado <- TRUE
      servicio <- rexp(1, (mu / 60)/2)
      prox_salida <- t + servicio
      clientes[nrow(clientes), "t_sa"] = prox_salida
      clientes[nrow(clientes), "t_se"] = t
      clientes[nrow(clientes), "d_se"] = servicio
    }
  } else { ## condicional para definir la ocupaci�n de la cola, dado que se va el cliente
    t <- prox_salida
    if (cola > 0){ #si hab�a clientes en la cola
      cola <- cola - 1
      servicio <- rexp(1, (mu / 60)/2)
      prox_salida <- t + servicio
      clientes[nrow(clientes) - cola, "t_sa"] = prox_salida #hora de salida
      clientes[nrow(clientes) - cola, "t_se"] = t # la hora que lo empiezan a atender
      clientes[nrow(clientes) - cola, "d_se"] = servicio #duraci�n del servicio
    } else { #si no hab�a clientes en la cola
      ocupado <- FALSE
      prox_salida <- 9999
    }
  }
}


#Generaci�n de la tabla de Clientes

#se crea un dataframe con todos los campos que ser�n completados con la informaci�n previamente simulada
cl_table <- data.frame("n_cli"=integer(),
                       "llega"=character(),
                       "sale"=character(),
                       "d_ocio"=numeric(),
                       "d_cola"=numeric(),
                       "d_serv"=numeric(),
                       "d_sis"=numeric())

#se setea el formato fecha hora de inicio

t_ini <- strptime("09:00:00", "%H:%M:%S")
t_ini

#se define una funci�n que calcula hora, minutos y segundos sin la fecha
time_str <- function(t_ini, t_ev){
  hr <- hours(t_ev %/% 60)
  min <- minutes(round(t_ev %% 60))
  sec <- seconds(round((t_ev * 60) %% 60))
  as.character(t_ini + hr + min + sec, format = "%H:%M:%S")
}

# se llena la tabla de clientes con un loop
#FOR:  recorre cada registro del dataframe
#IF: por cada recorrido del FOR, se establece un condicional para el c�lculo de valor

anterior <- 0
for (i in 1:nrow(clientes)){
  cliente <- clientes[i,]
  if (all(!is.na(cliente))){ #condicional que evita tomar registros vac�os
    t_ll <- time_str(t_ini, cliente$t_ll)
    t_sa <- time_str(t_ini, cliente$t_sa)
    d_ocio <- max(0, cliente$t_ll - anterior)
    d_cola <- cliente$t_se - cliente$t_ll
    d_serv <- cliente$d_se
    d_sis <- cliente$t_sa - cliente$t_ll
  }
  anterior <- cliente$t_sa
  row <- data.frame("n_cli"=i, "llega"=t_ll, "sale"=t_sa,
                    "d_ocio"=d_ocio, "d_cola"=d_cola, "d_serv"=d_serv, "d_sis"=d_sis)
  cl_table <- rbind(cl_table, row)
}
kable(cl_table, digits = 2, caption = "Traza de los clientes")


#### Optativo: exportamos la tabla de clientes a un csv
####write.csv2(cl_table, "clientes.csv")


### graficos a partir de la tabla anterior

plot(tm, cl + oc, col="green", type="s", main="N�mero de Clientes en el sistema",
     xlab = "tiempo (min)", ylab = "# Clientes")

plot(tm, cl, col="red", type="s", main="N�mero de Clientes en la Cola",
     xlab = "tiempo (min)", ylab = "# Clientes")

### c�lculo de medidas de desempe�o

#N�mero promedio de clientes en el sistema
prod <- numeric(length(tm) - 1)
for (i in 1:length(tm)-1){
  prod[i] <- (tm[i+1] - tm[i]) * (cl[i] + oc[i])
}
L <- sum(prod) / tm[length(tm)]

#N�mero promedio de clientes en la cola
prod <- numeric(length(tm) - 1)
for (i in 1:length(tm)-1){
  prod[i] <- (tm[i+1] - tm[i]) * (cl[i])
}
Lq <- sum(prod) / tm[length(tm)]

#Fracci�n estimada de tiempo ocioso
prod <- numeric(length(tm) - 1)
for (i in 1:length(tm)-1){
  prod[i] <- (tm[i+1] - tm[i]) * (!oc[i])
}
P0 <- sum(prod) / tm[length(tm)]

#Ocupaci�n promedio estimada
prod <- numeric(length(tm) - 1)
for (i in 1:length(tm)-1){
  prod[i] <- (tm[i+1] - tm[i]) * (oc[i])
}
Rho <- sum(prod) / tm[length(tm)]

#Tiempo de espera promedio en el sistema
W <- sum(cl_table$d_sis) / nrow(cl_table)

#Tiempo de espera promedio en la cola
Wq <- sum(cl_table$d_cola) / nrow(cl_table)


#se vuelcan los resultados a un data frame para luego obtenerlos ordenados
resultados <- data.frame ('Nro promedio de clientes en el sistema'  = L,
                          'Nro promedio de clientes en la cola' =Lq,
                          'Tiempo ocioso del sistema' = P0,
                          'Ocupaci�n promedio estimada'=Rho,
                          'Tiempo de espera promedio en el sistema'=W,
                          'Tiempo de espera promedio en la cola'=Wq
                          )

##Se traspone la matriz(data frame) y se muestra
print(t(resultados))
