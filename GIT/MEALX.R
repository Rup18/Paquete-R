#' Modelo con efectos aleatorios
#'
#' Realiza una tabla ANOVA para un modelo que contenga efectos fijos y aleatorios.
#' @name Modelo_efect_aleatorios
#' @param respuesta (variable respuesta) datos de la variable respuesta.
#' @param fijo (factor) datos del factor fijo.
#' @param aleatorio (factor) datos del factor aleatorio.
#' @param data (df) dataframes de los datos recolectados.
#' @return una tabla ANOVA con la suma de los cuadrados entre y dentro de grupos, grados de libertad, la media de los cuadrados, la estadistica F y el valor p.
#' @export
TablaAnova_MEA<-function(respuesta, fijo, aleatorio, data){

  # 3 calcular el promedio total

  mean_total<-mean(df$PESO)

  # 4 Calcular el promedio de cada raza
  Promedio_R1 <- aggregate(df$PESO, by = list(df$RAZA), FUN = mean)

  # 4.1 Asignar nombres a las columnas
  colnames(Promedio_R1) <- c("RAZA", "PROMEDIO_PESO")

  print(Promedio_R1)

  # Calcular el promedio de cada dieta
  Promedio_DIETA <- aggregate(df$PESO, by = list(df$DIETA), FUN = mean)
  colnames(Promedio_DIETA) <- c("DIETA", "PROMEDIO_PESO")

  print(Promedio_DIETA)

  # 5 calcular suma de cuadrados total

  ss_total<- sum((df$PESO - mean_total)^2)

  # 6 Calcular las sumas de cuadrados entre dietas  (FIJO)

  DIETA_levels<-unique(df$DIETA)
  n_DIETA<-length(DIETA_levels)

  mean_DIETA<-sapply(DIETA_levels, function(level) mean(df$PESO[df$DIETA == level]))

  ss_DIETA<-sum(n_DIETA*(mean_DIETA-mean_total)^2)

  # 7 Calcular sumas de cuadrados entre razas (ALEATORIO)
  RAZA_levels<-unique(df$RAZA)
  n_RAZA<-length(RAZA_levels)

  mean_RAZA<-sapply(RAZA_levels,function(level) mean(df$PESO[df$RAZA == level]))

  ss_RAZA<-sum(n_RAZA*(mean_RAZA - mean_total)^2)

  ## Calcular la suma de cuadrados dentro de los grupos
  ss_dentro_DIETA <- sum((df$PESO - mean_DIETA[match(df$DIETA, Promedio_DIETA)])^2)
  ss_dentro_RAZA <- sum((df$PESO - mean_RAZA[match(df$RAZA, Promedio_R1)])^2)

  ## Calcular la suma de cuadrados dentro de los grupos del error
  ss_dentro_error<-ss_total-ss_dentro_DIETA-ss_dentro_RAZA

  ## Calcular los grados de libertad dentro de los grupos
  gl_dentro_DIETA <- n_DIETA * (n_RAZA - 1)
  gl_dentro_RAZA <- n_RAZA * (n_DIETA - 1)


  # 8 Calcular las sumas de cuadrado del error
  ss_error<-ss_total - ss_DIETA - ss_RAZA

  # 8 Calcular los grados de libertad entre los grupos
  lb_DIETA <- n_DIETA - 1
  lb_RAZA<-n_RAZA - 1
  lb_error<- length(df$PESO)- n_DIETA + n_RAZA

  # 9 Calcular las medias de cuadrados
  ms_DIETA<- ss_DIETA / lb_DIETA
  ms_RAZA<- ss_RAZA / lb_RAZA
  ms_error<- ss_error / lb_error

  # 10 Calcular la estadistica F y su valor p
  F_DIETA<-ms_DIETA / ms_error
  p_value_DIETA<-pf(F_DIETA, lb_DIETA, lb_error, lower.tail = FALSE)

  F_RAZA<-ms_RAZA / ms_error
  p_value_RAZA<-pf(F_RAZA, lb_RAZA, lb_error, lower.tail = FALSE)

  # 11 Construir tabla ANOVA
  tabla_anova<-data.frame(Fuente= c("DIETA","RAZA","Error"),
                          SSB= c(ss_DIETA, ss_RAZA, ss_error),
                          SSV= c(ss_dentro_DIETA,ss_dentro_RAZA,ss_dentro_error),
                          DF= c(lb_DIETA, lb_RAZA,lb_error),
                          MS= c(ms_DIETA, ms_RAZA, ms_error),
                          F= c(F_DIETA, F_RAZA, NA),
                          "valor p"= c(p_value_DIETA, p_value_RAZA,NA))
  return(tabla_anova)
}
