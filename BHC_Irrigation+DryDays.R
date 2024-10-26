#rm(list = ls())

# OBS: SR in nasa database was calculated through Hargreaves-Samani method
# when not available. 


# --- packages
require(tidyverse)
require(ggplot2)
require(data.table)

# setting working directory
setwd("C:/Users/lomo0003/BHC_Irrigation/met_files/comb/")

g <- dir()
crops <- c("coffee", "grain maize", "soybean", "sweet maize", "sugarcane", "cotton", "common beans",
           "orange", "sugarcane", "irrigated rice")
dayscycle <- c(300, 130, 120, 90, 365, 160, 90, 270, 365, 125)
ini_cycle <- c("1-9", "20-2", "15-10", "20-2", "31-10", "15-2",  "1-6", "15-8", "31-10","10-9")
AFDini <- c(55, 60, 38, 50, 46, 60, 48,54, 46, 25) 
cropinfo <- data.frame(cultura = crops, 
                       diasciclo = dayscycle,
                       inicio = ini_cycle, 
                       AFDi = AFDini,
                       estacao = g, 
                       local = c("Araguari-MG", 
                                 "Balsas-MA",
                                 "CruzAlta-RS",
                                 "Formosa-GO",
                                 "Jatai-GO",
                                 "Posse-GO",
                                 "Poxoreo-MT",
                                 "SaoCarlos-SP",
                                 "Uberaba-MG",
                                 "Uruguaiana-RS"))

for (kr in 1:nrow (cropinfo)){
  A1 <- fread(g[kr])
  A1$crop_days <- paste0(A1$day, "-", A1$month)
  
  ini_days <- A1 [A1$crop_days == cropinfo$inicio[kr], "Data"]
  end_days <- ini_days + cropinfo$diasciclo[kr] 
  
  output_drydays_irrig <- matrix(NA, ncol = 8, nrow = 1)
  colnames(output_drydays_irrig) <- c("yr", "consec_drydays", "consec_NASA_drydays",
                                "n_Irrig", "n_Irrig_NASA", "TotIrri", "TotIrri_NASA", "local")
  # running over the years 
  for(yr in 1:nrow(ini_days)){
    if(any(which(A1$Data == end_days$Data[yr] - 1)) == F){break}
    dias <- which(A1$Data == ini_days$Data[yr]) : which(A1$Data == end_days$Data[yr] - 1)
    per <- A1[dias,  ]
    per$ndc <- 1:nrow(per)  
    per$cycle_nb <- yr
    
    # --- ajustando o valor de kc
    if(cropinfo$cultura[kr] == "coffee"){kc <- 0.9}
    if(cropinfo$cultura[kr] == "orange"){kc <- 0.75}
    if(cropinfo$cultura[kr] == "grain maize"){
      kc <- ifelse(per$ndc < 30, 0.4, ifelse(
                   per$ndc < 60, 0.8, ifelse(
                   per$ndc < 100, 1.15, 0.9)))
    }
    if(cropinfo$cultura[kr] == "sweet maize"){
      kc <- ifelse(per$ndc < 15, 0.4, ifelse(
        per$ndc < 40, 0.8, ifelse(
          per$ndc < 70, 1.15, 1.1)))
    }
    if(cropinfo$cultura[kr] == "sugarcane"){
      kc <- ifelse(per$ndc < 45, 0.5, ifelse(
        per$ndc < 90, 0.85, ifelse(
          per$ndc < 225, 1.1, 0.8)))
    }
    if(cropinfo$cultura[kr] == "soybean"){
      kc <- ifelse(per$ndc < 30, 0.4, ifelse(
        per$ndc < 60, 0.75, ifelse(
          per$ndc < 90, 1.1, 0.75)))
    }
    if(cropinfo$cultura[kr] == "cotton"){
      kc <- ifelse(per$ndc < 50, 0.5, ifelse(
        per$ndc < 110, 0.75, ifelse(
          per$ndc < 130, 1.15, 0.85)))
    }
    if(cropinfo$cultura[kr] == "common beans"){
      kc <- ifelse(per$ndc < 30, 0.4, ifelse(
        per$ndc < 60, 0.75, ifelse(
          per$ndc < 75, 1.15, 0.7)))
    }
    if(cropinfo$cultura[kr] == "irrigated rice"){
      kc <- ifelse(per$ndc < 30, 0.2, ifelse(
        per$ndc < 60, 0.7, ifelse(
          per$ndc < 90, 1.3, 0.5)))
    }
    
    per$kc <- kc 
    
    per$etc <- per$kc * per$EtoPM
    per$etc_NASA <- per$kc * per$EtoPM_NASA
    
    per$prec_etc <- per$Prec - per$etc
    per$prec_etc_NASA <- per$Prec_NASA - per$etc_NASA
    
    # crop-water balance using obs data
    per$AFD <- NA
    per$Irri <- 0
    per$AFD[1] <- cropinfo$AFDi[kr]
    
    for(j in 2:nrow(per)){
      if(per$prec_etc[j] + per$Irri[j] > 0){
        per$Irri[j] <- 0
        per$AFD[j] <- per$AFD[j-1]
      }
      if(per$AFD[j-1] < 5){
        per$Irri[j] <- per$AFD[1] - per$AFD[j-1]
        per$AFD[j] <- per$AFD[j-1] + per$prec_etc[j] + per$Irri[j]
      }else{
        per$AFD[j] <- per$AFD[j-1] + per$prec_etc[j] + per$Irri[j]
      }
      if((per$AFD[j-1] + per$prec_etc[j] + per$Irri[j]) > per$AFD[1]){ 
         per$AFD[j] <- per$AFD[1]
      }
    }
    
   #  crop-water balance using nasa power data
    per$AFD_NASA <- NA
    per$Irri_NASA <- 0 
    per$AFD_NASA[1] <- cropinfo$AFDi[kr]
    
    for(j in 2:nrow(per)){
      if(per$prec_etc_NASA[j] + per$Irri_NASA[j] > 0){
        per$Irri_NASA[j] <- 0
        per$AFD_NASA[j] <- per$AFD_NASA[j-1]
      }
      if(per$AFD_NASA[j-1] < 5){
        per$Irri_NASA[j] <- per$AFD_NASA[1] - per$AFD_NASA[j-1]
        per$AFD_NASA[j] <- per$AFD_NASA[j-1] + per$prec_etc_NASA[j] + per$Irri_NASA[j]
      }else{
        per$AFD_NASA[j] <- per$AFD_NASA[j-1] + per$prec_etc_NASA[j] + per$Irri_NASA[j]
      }
      if((per$AFD_NASA[j-1] + per$prec_etc_NASA[j] + per$Irri_NASA[j]) > per$AFD_NASA[1]){ 
        per$AFD_NASA[j] <- per$AFD_NASA[1]
      }
    }
    
    ini <- nrow(per) - 30
    fim <- ini + 30
    per$Irri[ini:fim] <- 0
    per$Irri_NASA[ini:fim] <- 0
    
    ddays <- ifelse(per$Prec < 1, 1, 0)
    t1 <- rle(ddays)
    t2 <- data.frame(t1$values, t1$lengths)
    t3 <- max(t2[t1$values == 1,2])
    
    ddays_nasa <- ifelse(per$Prec_NASA < 1, 1, 0)
    t1_NASA <- rle(ddays_nasa)
    t2_NASA <- data.frame(t1_NASA$values, t1_NASA$lengths)
    t3_NASA <- max(t2_NASA[t1_NASA$values == 1,2])
    
    n_irri <- sum(per$Irri > 0)
    n_irri_NASA <- sum(per$Irri_NASA > 0)
    
    tot_irri <- sum(per$Irri)
    tot_irri_NASA <- sum(per$Irri_NASA)
    
    output_drydays_irrig[1, 1] <- per$yr[1]
    output_drydays_irrig[1, 2] <- t3
    output_drydays_irrig[1, 3] <- t3_NASA
    output_drydays_irrig[1, 4] <- n_irri
    output_drydays_irrig[1, 5] <- n_irri_NASA
    output_drydays_irrig[1, 6] <- tot_irri
    output_drydays_irrig[1, 7] <- tot_irri_NASA
    output_drydays_irrig[1,8] <- cropinfo$local[kr]
    
    if(yr == 1){
      write.table(per, paste0("C:/Users/lomo0003/BHC_Irrigation/Outputs/BHC/BHC_", 
                           cropinfo$local[kr], "_", cropinfo$cultura[kr], 
                           ".csv"), sep = ";", row.names = F)
      write.table(output_drydays_irrig, paste0("C:/Users/lomo0003/BHC_Irrigation/Outputs/DryDays/DryDays_", 
                            cropinfo$local[kr], "_", cropinfo$cultura[kr], 
                            ".csv"), sep = ";", row.names = F)
    }else{
      write.table(per, paste0("C:/Users/lomo0003/BHC_Irrigation/Outputs/BHC/BHC_", 
                              cropinfo$local[kr], "_", cropinfo$cultura[kr], 
                              ".csv"), sep = ";", row.names = F,
                  append = T, col.names = F)
      write.table(output_drydays_irrig, paste0("C:/Users/lomo0003/BHC_Irrigation/Outputs/DryDays/DryDays_", 
                                               cropinfo$local[kr], "_", cropinfo$cultura[kr], 
                                               ".csv"), sep = ";", row.names = F, col.names = F, append = T)
  }
 }
}  











