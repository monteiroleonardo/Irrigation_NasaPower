# --- plotting total irrigation

rm(list = ls())
require(tidyverse)
require(ggplot2)
require(ggExtra)
require(data.table)
require(gridExtra)
require(patchwork)
require(ggpmisc)

setwd("C:/Users/lomo0003/BHC_Irrigation/")

d <- grep("^Irri_", dir("./Outputs/Irrigation/"), value = T)[-1]
nn <- c("Balsas-MA", "CruzAlta-RS", "Formosa-GO", "Jatai-GO", "Posse-GO", "Poxoreo-MT", "SaoCarlos-SP", "Uberaba-MG", "Uruguaiana-RS")

for(i in 1:length(d)){
  i = 
  input <- fread(paste0("./Outputs/Irrigation/", d[i])) 
  
  %>% select(Irri, Irri_NASA, cycle_nb) %>% mutate(local = nn[i])
  prec1 <- 
  
  if(i == 1){
    fwrite(w1, "./Outputs/Irrigation/Irrig_cycle.csv", row.names = F, sep = ";")
  }else{
    fwrite(w1, "./Outputs/Irrigation/Irrig_cycle.csv", row.names = F, col.names = F, append = T, sep = ";")
  }
}
  
w2 <- fread("./Irrigation/IRRI_cycle.csv")

x = aggregate(w2$Irri, by = list(w2$cycle_nb, w2$local), sum)[,2:3]
y = aggregate(w2$Irri_NASA, by = list(w2$cycle_nb, w2$local), sum)[,3]

dfirri <- data.frame(x, y)  
colnames(dfirri) <- c("local", "Obs", "NASAPOWER")




w3 = fread("./Precipitation/PREC_cycle.csv")
colnames(w3) = c("Prec", "Prec_NASA", "cycle_nb", "local")

xprec = aggregate(w3$Prec, by = list(w3$cycle_nb, w3$local), sum)[,2:3]
yprec = aggregate(w3$Prec_NASA, by = list(w3$cycle_nb, w3$local), sum)[,3]

dfprec <- data.frame(xprec, yprec)  
colnames(dfprec) <- c("local", "Obs", "NASAPOWER")



# -----------
# ----------- plotting total irrigated by site
# -----------



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
  labs(x = expression(Total~Irrigation~'-'~Obs~(mm~cycle^-1)),
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
  labs (x = "", y = "")
  
d9



# --- all plots together 

pfinal <- grid.arrange(d1, d2, d3, d4, d5, d6, d7, d8, d9,
                       ncol = 3)

ggsave ("./Outputs/plots/Total_Irrigation.png", plot = pfinal, width = 15, height = 10, dpi = 300, family = "serif")











# -----------
# ----------- plotting total Precipitation (on the cycle) by site
# -----------



# ====================
# BALSAS - MA
# ====================

df1 <- dfprec %>% filter(local == "Balsas-MA")
x1 <- df1$Obs
y1 = df1$NASAPOWER

my.formula <- df1$NASAPOWER ~ df1$Obs

d1 <- ggplot(df1, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 1, size = 5) + theme_bw() +
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

df2 <- dfprec %>% filter(local == "CruzAlta-RS")
x1 <- df2$Obs
y1 = df2$NASAPOWER

my.formula <- df2$NASAPOWER ~ df2$Obs

d2 <- ggplot(df2, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 1, size = 5) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 1500) + ylim(0, 1500) +
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

df3 <- dfprec %>% filter(local == "Formosa-GO")
x1 <- df3$Obs
y1 = df3$NASAPOWER

my.formula <- df3$NASAPOWER ~ df3$Obs

d3 <- ggplot(df3, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 1, size = 5) + theme_bw() +
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
  labs(x = "", y = "")
#labs(x = expression(Total~Irrigation~applied~'-'~Obs~(mm~cycle^-1)))
d3

# ====================
# Jatai - GO
# ====================
table(dfirri$local)

df4 <- dfprec %>% filter(local == "Jatai-GO")
x1 <- df4$Obs
y1 = df4$NASAPOWER

my.formula <- df4$NASAPOWER ~ df4$Obs

d4 <- ggplot(df4, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 1, size = 5) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 2100) + ylim(0, 2100) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(y = expression(Total~Precipitation~'-'~NP~(mm~cycle^-1)),
       x = "")
d4


# ====================
# Posse - GO
# ====================
table(dfirri$local)

df5 <- dfprec %>% filter(local == "Posse-GO")
x1 <- df5$Obs
y1 = df5$NASAPOWER

my.formula <- df5$NASAPOWER ~ df5$Obs

d5 <- ggplot(df5, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 1, size = 5) + theme_bw() +
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
  labs( x = "", y = "")
#labs(x = expression(Total~Irrigation~applied~'-'~Obs~(mm~cycle^-1)))
d5


# ====================
# Poxoreo - MT
# ====================
table(dfirri$local)

df6 <- dfprec %>% filter(local == "Poxoreo-MT")
x1 <- df6$Obs
y1 = df6$NASAPOWER

my.formula <- df6$NASAPOWER ~ df6$Obs

d6 <- ggplot(df6, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 1, size = 5) + theme_bw() +
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

df7 <- dfprec %>% filter(local == "SaoCarlos-SP")
x1 <- df7$Obs
y1 = df7$NASAPOWER

my.formula <- df7$NASAPOWER ~ df7$Obs

d7 <- ggplot(df7, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 1, size = 5) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 2000) + ylim(0, 2000) +
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

df8 <- dfprec %>% filter(local == "Uberaba-MG")
x1 <- df8$Obs
y1 = df8$NASAPOWER

my.formula <- df8$NASAPOWER ~ df8$Obs

d8 <- ggplot(df8, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 1, size = 5) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 2500) + ylim(0, 2500) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(x = expression(Total~Precipitation~'-'~Obs~(mm~cycle^-1)),
       y = "")
d8


# ====================
# Uruguaiana - RS
# ====================
table(dfirri$local)

df9 <- dfprec %>% filter(local == "Uruguaiana-RS")
x1 <- df9$Obs
y1 = df9$NASAPOWER

my.formula <- df9$NASAPOWER ~ df9$Obs

d9 <- ggplot(df9, aes(x = Obs, y = NASAPOWER)) + 
  geom_point(shape = 1, size = 5) + theme_bw() +
  geom_smooth(method = "lm", se=FALSE, color="black", size = 1.2) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +
  xlim(0, 1500) + ylim(0, 1500) +
  theme(axis.title = element_text(size = 13),
        axis.text = element_text(size = 13),
        strip.text = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  facet_wrap(~local) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs (x = "", y = "")

d9



# --- all plots together 

pfinal_prec <- grid.arrange(d1, d2, d3, d4, d5, d6, d7, d8, d9, ncol = 3)

ggsave ("./plots/Total_Precipitation.png", plot = pfinal_prec, width = 15, height = 10, dpi = 300, family = "serif")















