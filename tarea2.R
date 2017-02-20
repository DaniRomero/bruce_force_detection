attackDetector <- function(dataFrame){
  # Ordenamos segun la fecha, descendientemente
  dataFrame <- dataFrame[order(-dataFrame$start_time), ]
  # Pregunta: ¿La columna start time del dataframe nos es util en ese formato?
  # R: No de forma intuitiva, necesita una transformacion para su interpretacion
  # Validacion: 
  dataFrame$new.start_time <- as.POSIXct(dataFrame$start_time, origin="1970-01-01")  
  #Pregunta: ¿Es necesario asignar algun valor a los grupos de request creados a partir del intervalo de tiempo introducido?
  #R: Si, ya que esto nos facilitaria los analisis que realizaremos posteriormente
  #Validacion: 
  dataFrame$interReqTime <- 0
  dataFrame$interval <- 0
  prevTime <- dataFrame$start_time[1]
  intervalStar <- dataFrame$start_time[1]
  start <- 1 #marcar de donde se empezo a contarse en el intervalo
  i <- 1 #variable para recorrer cada row
  intervalCounter <- 1 #numero de request que contiene el grupo al momento de empezar la agrupacion
  probabilitys <- c()
  interReqTimes <- c()
  while (i <= nrow(dataFrame)) {
    parar <- TRUE
    while ((parar) && (i <= nrow(dataFrame))) {
      dataFrame$interReqTime[i] <- intervalCounter
      dataFrame$interReqTime[i] <- prevTime-dataFrame$start_time[i]
      prevTime <- dataFrame$start_time[i]
      if (intervalStar-dataFrame$start_time[i] >= 1800) { #agrupamos todas aquellos request cuyo intervalo de tiempo sea mayor o igual al intervalo introducido
        intervalStar <- dataFrame$start_time[i]
        parar <- FALSE
      }
      i <- i + 1
    }
    intervalCounter <- intervalCounter + 1
    intervalTest <- dataFrame[start:i-1, ]
    interReqTimes <- c(interReqTimes,mean(intervalTest$interReqTime))
    plot(intervalTest$interReqTime, type = "l")
    #Pregunta: ¿como se comportan los ataques de fuerza bruta?
    #R: Se caracterizan por ser request solicitados por un mismo source_ip a distintos destination_ip,
    #donde los campos num_bytes, num_packets, asn y site son iguales en todos los request
    #Validacion: 
    paw <- aggregate(intervalTest, by = list(intervalTest$num_bytes, intervalTest$num_packets, intervalTest$destination_ip, intervalTest$asn, intervalTest$site), FUN = length)
    paw <- paw[paw$source_ip>50, ] #descartamos los grupos que tengas menos de 50 rows
    aux <- sum(paw$source_ip) / nrow(intervalTest) #calculo de la probabilidad
    probabilitys <- c(probabilitys,aux) #almacenamos las probabilidades en un arreglo
    start <- i #actualizar el contador de donde voy a empezar a contar en los intervalos
  }
  
  #Pregunta: ¿cuando se considera que un grupo de request agrupado por intervalo de tiempo es un ataque de fuerza bruta?
  #R: cuando la mediana del tiempo entre request sea menor o igual a la mediana del intervalo introducido. 
  #Validacion: 
  final <- c() 
  for (i in 1:length(probabilitys)) {
    if(interReqTimes[i] <= median(interReqTimes)){
      final <- c(final, probabilitys[i]) 
    }
  }  
  #Retornamos el valor maximo del arreglo creado en el item anterior ya que consideramos el peor de los casos
  return (max(final))
}


