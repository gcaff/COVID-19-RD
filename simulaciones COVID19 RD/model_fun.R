  ###################################################
  ###################################################
  # Simulaciones de la pandemia del  
  # del COVID-19 en República Dominicana
  #
  # Autor: Gustavo Caffaro (NYU)
  ###################################################
  ###################################################
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(RColorBrewer)
  library(plotly)
  library(latex2exp)
  
model_sim_covid <- function(nrep,n,H,num_ca,tau=0.5){

  ############## Parámetros de la simulación ##############
  #nrep <- 5 # numero de repeticiones (iteraciones)
  dias <- 100 # numero de dias
  
  ############## Supuestos de la simulación ##############
  #n <- 1e3 # numero de personas
  
  # horas de libre tránsito por día
  #H <- 9
  
  # numero de centros de abastecimiento
  # este num es un aproximado del # de supermercados
  # y centros de abastecimiento en RD
  #num_ca <- round(489/2e6*n+44000/10e6*n)
  #num_ca <- 4
  
  n_i <- 1 # numero inicial de infectados
  n_dias_inf <- 14 # duracion de la infeccion (en dias)
  n_dias_compras <- 14 # numero de dias con que duran las compras
  
  # prob. de regresar a casa (la persona solo puede durar entre 1-3 horas en el CA)
  p_rc <- c(0.5,0.75,1)
  
  # prob. de infectar a una persona (transmisibility)
  tau <- tau
  
  # prob. de identificar paciente infectado
  p_dp <- 0.5
  
  # prob. de mostrar sintomas
  p_mostrar_sintomas <- 0.75
  
  # prob. de fallecer teniendo el virus (tasa mortalidad)
  p_d <- 82/1745 #al 3 abril 2020, datos RD MSP
  
  # prob. de ir al centro de abastecimiento (prob. salir de casa)
  # en este modelo las personas necesitan abastecerse y la prob. de salir
  # de su casa es inv. proporcional al tiempo que duran abastecidos
  p_ca <- 1/(n_dias_compras*H)
  
  #############
  
  nper <- H*dias+1 # numero de periodos
  
  
  ###### Probabilidades ###
  ##### Prob. de transicion
  
  
  
  # cada persona solo puede estar en ca+1 lugares: en su casa 1,
  # o en uno de los ca centros de abastecimiento.
  pos <- matrix(0,n,nper)
  # todo el mundo en su casa al principio de la sim
  pos[,1] <- 1
  
  
  ###### Dist. Personas ######
  # cada persona solo puede ser Susceptible, Infectada, Recuperada, o fallecida
  # 1 es susceptible, 2 infectado, 3 recuperado, 4 fallecido
  
  sir_dist <- matrix(1,n,nper)
  
  # distribucion de las personas infectadas
  sir_dist[1:n_i,1] <- 2 # asignar a las primeras n_i personas como infectadas
  
  # contar numero de personas SIR dist
  count_n_SIR <- function(sir_dist){
    S <- colSums(sir_dist == 1)
    I <- colSums(sir_dist == 2)
    R <- colSums(sir_dist == 3)
    D <- colSums(sir_dist == 4)
    data.frame(S,I,R,D)
  }
  
  # numero de horas en centro de acopio
  h_ca <- numeric(n)
  
  # periodos con infeccion
  nper_inf <- matrix(0,n,nper)
  
  # lista para los resultados de las iteraciones
  l_res <- rep(list(""),nrep)
  
  for (it in 1:nrep){
    
    #browser()
    # matriz de decision
    dm <- runif(n*nper) 
    dm <- matrix(dm,n,nper) # convertir en matriz
    sample_centros <- sample(1:num_ca,n*nper,replace = T)+1
    sample_centros <- matrix(sample_centros,n,nper)
    
    # matriz de decision del msp
    dm_msp <- runif(n*nper) 
    dm_msp <- matrix(dm_msp,n,nper) # convertir en matriz
    
    # matriz de personas con sintomas
    m_n_sintomas <- matrix(0,n,nper)
    
    for (h in 1:(nper-1)){
      # si la persona esta infectada y muestra sintomas
      n_isintomatica <- (sir_dist[,h]==2) & (m_n_sintomas[,h] == 1)
      
      for (i in 1:n){
        # si la persona esta infectada y muestra sintomas, se queda en casa (u hospital)
        if (n_isintomatica[i]){
          pos[i,h+1] <- 1
        } else {
          # si la persona esta en su casa
          if (pos[i,h] == 1){
            # decide irse con prob. p_ca
            if (dm[i,h] < p_ca){
              # si la persona decide irse, en el prox. turno estara en el CA
              
              # ahora la persona decide a cual de los centros ir
              # la persona es indiferente entre cual centro asistir
              pos[i,h+1] <- sample_centros[i,h]
              
            } else{
              pos[i,h+1] <- 1
            }
          } else { # si la persona no esta en su casa
            #browser()
            # las horas que estara en el CA aumenta a 1
            h_ca[i] <- h_ca[i]+1
            
            # si la prob de regresar a casa es mayor que la del dm, la pers. vuelve a casa
            if (dm[i,h] < p_rc[h_ca[i]]){
              pos[i,h+1] <- 1
              h_ca[i] <- 0
            } else {
              pos[i,h+1] <- pos[i,h]
            }
          }
        }
      }
      # dinamica de contagio
      #browser()
      # personas en el CA en esta hora
      
      
      for (z in 1:num_ca){
        #browser()
        n_ca <- pos[,h] == z+1
        if (sum(n_ca)>0){
          # personas infectadas en el CA en esta hora
          n_ica <- (sir_dist[,h] == 2) & (pos[,h] == z+1)
          
          # numero de personas infectadas en el CA en esta hora
          num_n_ica <- sum(n_ica)
          
          n_exp <- (1:n)[sir_dist[,h]==1 & n_ca] # personas en exposicion del virus
          if (length(n_exp)>0){
            i_dm <- runif(length(n_exp)) #infection decision matrix
            
            #browser()
            # para cada una de las personas en exposicion, ver si se contagian o no
            # la prob. de contagio es el complemento de la prob. de no contagio
            # P(contagio) = 1-(1-tau)^num_n_ica
            p_no_contagio <- 1-(1-tau)^num_n_ica
            
            for (k in 1:length(n_exp)){
              if (i_dm[k] < p_no_contagio){
                sir_dist[n_exp[k],h] <- 2
              }
            }
          }
        }
        
      }
      
      sir_dist[,h+1] <- sir_dist[,h]
      
      # dinamica de curacion
      #nper_inf[nper_inf[,h] == n_dias_inf*H,h] <- 0
      sir_dist[nper_inf[,h] == n_dias_inf*H,h+1] <- 3 # los pacientes con 14 dias infectados se curan
      nper_inf[,h+1] <- (sir_dist[,h] == 2)*1 + nper_inf[,h] # agregar un dia mas (multiplico por 1 para q de logical pase a numeric)
      
      # si la persona muestra sintomas, se cuarentena
      #m_id[sir_dist[,h] == 2]
      n_pms <- (1:n)[nper_inf[,h] > 1] # personas que pueden mostrar sintomas
      
      for (z in n_pms){
        if (runif(1) < p_mostrar_sintomas/(H*n_dias_inf)){
          m_n_sintomas[z,h:nper] <- 1
        }
      }
      
      # si la persona muestra sintomas, puede fallecer
      n_ms <- (1:n)[sir_dist[,h] == 2 & m_n_sintomas[,h] == 1] # persoas que muestran sintomas
      
      for (z in n_ms){
        if (runif(1) < p_d/(H*n_dias_inf)){
          sir_dist[z,(h+1):nper] <- 4
        }
      }
      
      
      #print(count_n_SIR(sir_dist = sir_dist)[1:nper,])
      #print(count_n_SIR(sir_dist = sir_dist)[nper,])
      #print(h)
    }
    print(it)
    #l_res[[it]] <- sir_dist
    l_res[[it]] <- count_n_SIR(sir_dist)
  }
  
  #num_SIR <- count_n_SIR(sir_dist = sir_dist)
  #l_res2 <- lapply(l_res,count_n_SIR)
  #l_I <- lapply(l_res2,function(x) x$I)
  l_I <- lapply(l_res,function(x) x$I)
  l_I <- as.data.frame(l_I)
  I <- rowMeans(l_I)
  
  #l_S <- lapply(l_res2,function(x) x$S)
  l_S <- lapply(l_res,function(x) x$S)
  l_S <- as.data.frame(l_S)
  S <- rowMeans(l_S)
  
  #l_R <- lapply(l_res2,function(x) x$R)
  l_R <- lapply(l_res,function(x) x$R)
  l_R <- as.data.frame(l_R)
  R <- rowMeans(l_R)
  
  l_D <- lapply(l_res,function(x) x$D)
  l_D <- as.data.frame(l_D)
  D <- rowMeans(l_D)
  
  
  #rm(l_res)
  
  #plot(1:nper,I,type="l")
  #plot(1:nper,I,type="l",ylim=c(0,n))
  #lines(1:nper,S)
  #lines(1:nper,R)
  
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  # convertir en data frame para hacer gráfico
  df <- data.frame(t=1:nper,S,I,R,D)
  
  # tomar última hora del día para graficar
  df <- df[(H*(1:dias))+1 %in% df$t,]
  df$t <- 1:dias
  
  
  df
}

model_sim_covid_plot <- function(df, legend = T, title = F, x.axis = F, y.axis = F,tau=0.5){
  
  # gráfico
  p1 <- df %>%
    gather(key=x, value=value,-t) %>%
    mutate(value = value/n,
           x=factor(x,levels=c("R","S","D","I"))) %>%
    ggplot(aes(x=t, y=value, fill=x)) + 
    geom_area(alpha=0.6 , size=.5, colour="white") +
    scale_fill_manual(values = c("#1A9641","#A6D96A","#D7191C","#FDAE61"),
                      name = "") +
    theme_minimal() + 
    #theme(axis.text.x = element_text(12)) +
    #coord_cartesian(ylim=c(0, 0.5))+
    #ggtitle("Evolución del número de infectados") +
    theme(plot.title = element_text(size=18))
  
  tau <- as.character(tau)
  
  if (legend == F){
    p1 <- p1 +
      theme(legend.position = "none")
  }
  if (title == T){
    p1 <- p1 + ggtitle(TeX(paste0("Evolución de la Distribución de SIRD, $\\tau =$",tau)))
  }
  if (x.axis == T){
    p1 <- p1 + xlab("Núm. dias desde 1er caso")
  } else {
    p1 <- p1 + theme(axis.title.x=element_blank())
  }
  if (y.axis == T){
    p1 <- p1 + ylab("Proporción de la Población")
  } else {
    p1 <- p1 + ylab("")
  }
  
  p1
}
