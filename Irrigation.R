# --- plotting total irrigation

rm(list = ls())
require(tidyverse)
require(ggplot2)
require(ggExtra)
require(data.table)
require(gridExtra)
require(patchwork)
require(ggpmisc)

setwd("C:/Users/lomo0003/BHC_Irrigation/Outputs/")

d <- grep(".csv", dir("./BHC/"), value = T)
nn <- c("Araguari-MG", "Balsas-MA", "CruzAlta-RS", "Formosa-GO", "Jatai-GO", "Posse-GO", "Poxoreo-MT", "SaoCarlos-SP", "Uberaba-MG", "Uruguaiana-RS")

for(i in 1:length(d)){
  input <- fread(paste0("./BHC/", d[i]))
    irri1 <- input %>% select(Irri, Irri_NASA, cycle_nb) %>% mutate(local = nn[i])
    chuva1 <- input %>% select(Prec, Prec_NASA, cycle_nb) %>% mutate(local = nn[i])
    if(i == 1){
      fwrite(irri1, "./Irrigation/IRRI_cycle.csv", row.names = F, sep = ";")
      fwrite(irri1, "./Precipitation/PREC_cycle.csv", row.names = F, sep = ";")
    }else{
      fwrite(irri1, "./Irrigation/IRRI_cycle.csv", row.names = F, col.names = F, append = T, sep = ";")
      fwrite(chuva1, "./Precipitation/PREC_cycle.csv", row.names = F, col.names = F, append = T, sep = ";")
    }
}
  
irri2 <- fread("./Irrigation/IRRI_cycle.csv")

x = aggregate(irri2$Irri, by = list(irri2$cycle_nb, irri2$local), sum)[,2:3]
y = aggregate(irri2$Irri_NASA, by = list(irri2$cycle_nb, irri2$local), sum)[,3]

dfirri <- data.frame(x, y)  
colnames(dfirri) <- c("local", "Obs", "NASAPOWER")



# -----------
# ----------- plotting total irrigated by site
# -----------


# ====================
# Araguari - MG
# ====================

df0 <- dfirri %>% filter(local == "Araguari-MG")

x1 <- df0$Obs
y1 = df0$NASAPOWER

my.formula <- df0$NASAPOWER ~ df0$Obs

d0 <- ggplot(df0, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 2, size = 3) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 500) + ylim(0, 500) +
  labs(x = "", y = "") +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")
#  labs(x = expression(Total~Irrigation~applied~'-'~Obs~(mm~cycle^-1)))
d0

# ====================
# BALSAS - MA
# ====================

df1 <- dfirri %>% filter(local == "Balsas-MA")
x1 <- df1$Obs
y1 = df1$NASAPOWER

my.formula <- df1$NASAPOWER ~ df1$Obs

d1 <- ggplot(df1, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 2, size = 3) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
xlim(0, 300) + ylim(0, 300) +
  labs(x = "", y = "") +
theme(axis.title = element_text(size = 13),
      axis.text = element_text(size = 13),
      strip.text = element_text(size = 13),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")
#  labs(x = expression(Total~Irrigation~applied~'-'~Obs~(mm~cycle^-1)))
d1

# ====================
# CRUZ ALTA - RS
# ====================
table(dfirri$local)

df2 <- dfirri %>% filter(local == "CruzAlta-RS")
x1 <- df2$Obs
y1 = df2$NASAPOWER

my.formula <- df2$NASAPOWER ~ df2$Obs

d2 <- ggplot(df2, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 2, size = 3) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 300) + ylim(0, 300) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "", y = "")
 # labs(x = expression(Total~Irrigation~applied~'-'~Obs~(mm~cycle^-1)))
d2


# ====================
# Formosa - GO - RS
# ====================
table(dfirri$local)

df3 <- dfirri %>% filter(local == "Formosa-GO")
x1 <- df3$Obs
y1 = df3$NASAPOWER

my.formula <- df3$NASAPOWER ~ df3$Obs

d3 <- ggplot(df3, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 2, size = 3) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 300) + ylim(0, 300) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "", y = "")
  #labs(x = expression(Total~Irrigation~applied~'-'~Obs~(mm~cycle^-1)))
d3

# ====================
# Jatai - GO
# ====================
table(dfirri$local)

df4 <- dfirri %>% filter(local == "Jatai-GO")
x1 <- df4$Obs
y1 = df4$NASAPOWER

my.formula <- df4$NASAPOWER ~ df4$Obs

d4 <- ggplot(df4, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 2, size = 3) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 750) + ylim(0, 750) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(y = expression(Total~Irrigation~'-'~NP~(mm~cycle^-1)),
       x = "")
d4


# ====================
# Posse - GO
# ====================
table(dfirri$local)

df5 <- dfirri %>% filter(local == "Posse-GO")
x1 <- df5$Obs
y1 = df5$NASAPOWER

my.formula <- df5$NASAPOWER ~ df5$Obs

d5 <- ggplot(df5, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 2, size = 3) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 400) + ylim(0, 400) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs( x = "", y = "")
    #labs(x = expression(Total~Irrigation~applied~'-'~Obs~(mm~cycle^-1)))
d5


# ====================
# Poxoreo - MT
# ====================
table(dfirri$local)

df6 <- dfirri %>% filter(local == "Poxoreo-MT")
x1 <- df6$Obs
y1 = df6$NASAPOWER

my.formula <- df6$NASAPOWER ~ df6$Obs

d6 <- ggplot(df6, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 2, size = 3) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 200) + ylim(0, 200) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "", y = "")
    #labs(x = expression(Total~Irrigation~applied~'-'~Obs~(mm~cycle^-1)))
d6



# ====================
# Sao Carlos - SP
# ====================
table(dfirri$local)

df7 <- dfirri %>% filter(local == "SaoCarlos-SP")
x1 <- df7$Obs
y1 = df7$NASAPOWER

my.formula <- df7$NASAPOWER ~ df7$Obs

d7 <- ggplot(df7, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 2, size = 3) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 400) + ylim(0, 400) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs (x = "", y = "")
  #labs(x = expression(Total~Irrigation~applied~'-'~Obs~(mm~cycle^-1)))
d7


# ====================
# Uberaba - MG
# ====================
table(dfirri$local)

df8 <- dfirri %>% filter(local == "Uberaba-MG")
x1 <- df8$Obs
y1 = df8$NASAPOWER

my.formula <- df8$NASAPOWER ~ df8$Obs

d8 <- ggplot(df8, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 2, size = 3) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 800) + ylim(0, 800) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = "",
       y = "")
d8


# ====================
# Uruguaiana - RS
# ====================
table(dfirri$local)

df9 <- dfirri %>% filter(local == "Uruguaiana-RS")
x1 <- df9$Obs
y1 = df9$NASAPOWER

my.formula <- df9$NASAPOWER ~ df9$Obs

d9 <- ggplot(df9, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 2, size = 3) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 300) + ylim(0, 300) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs (x = expression(Total~Irrigation~'-'~Obs~(mm~cycle^-1)), y = "")
  
d9

# ====================
# ALL location
# ====================
dfirri_plot <- dfirri %>% mutate(local1 = "All places") %>% select(-local)

my.formula <- dfirri_plot$NASAPOWER ~ dfirri_plot$Obs

dall <- ggplot(dfirri_plot, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 2, size = 3) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 800) + ylim(0, 800) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs (x = "", y = "")
dall



# --- all plots together 

pfinal <- grid.arrange(d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, dall,
                       ncol = 4)




ggsave ("./plots/Total_Irrigation_Aug26.png", plot = pfinal, width = 15, height = 10, dpi = 300, family = "serif")





# ------------------ indices statisticos - Irrigation


A1 <- dfirri

locais <- names(table(A1$local))

require(caret)
outputs <- matrix(NA, ncol = 5, nrow = 11)
colnames(outputs) <- c("local", "r2", "RMSE", "nRMSE", "d")
for(i in 1:11){
  if(i < 11){
  A2 <- A1 %>% filter(local == locais[i])
  r2 <- caret::R2(A2$Obs, A2$NASAPOWER)
  rmse <- caret::RMSE(A2$Obs, A2$NASAPOWER)
  nRMSE <- 100 * rmse / mean(A2$Obs)
  d1 <- sum(A2$Obs - A2$NASAPOWER)^2
  d2 <- sum(abs(A2$NASAPOWER - mean(A2$Obs)) + abs(A2$Obs - mean(A2$Obs)))^2 
  d <- 1 - d1 / d2
  outputs[i, ] <- c(locais[i], r2, rmse, nRMSE, d)
  }else{
    A2 <- A1
    r2 <- caret::R2(A2$Obs, A2$NASAPOWER)
    rmse <- caret::RMSE(A2$Obs, A2$NASAPOWER)
    nRMSE <- 100 * rmse / mean(A2$Obs)
    d1 <- sum(A2$Obs - A2$NASAPOWER)^2
    d2 <- sum(abs(A2$NASAPOWER - mean(A2$Obs)) + abs(A2$Obs - mean(A2$Obs)))^2 
    d <- 1 - d1 / d2
    outputs[i, ] <- c(locais[i], r2, rmse, nRMSE, d)
}
}
outputs

write.table(outputs, "./Coefficients_irrigacao_Aug26.csv", row.names = F, sep = ";")











