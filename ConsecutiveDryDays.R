
rm(list = ls())
require(tidyverse)
require(ggplot2)
require(data.table)

setwd("C:/Users/lomo0003/BHC_Irrigation/Outputs/DryDays/")

# aggregating all files

d <- dir()

for (k in 1:length(d)){
  A1 <- fread(d[k])
  if(k == 1){
    fwrite(A1, "ALL_MaxNumberDryDays.csv", row.names = F, sep = ";")
  }else{
    fwrite(A1, "ALL_MaxNumberDryDays.csv", row.names = F, sep = ";", append = T, col.names = F)
  }
}


e <- fread("ALL_MaxNumberDryDays.csv") %>% select(consec_drydays,consec_NASA_drydays, local )
e 

f <- e %>% gather(key = source, value = ndd, -local)
f$source <- rep(c("Obs", "NP"), each = 330)

p1 <- ggplot(f, aes(x = local, y = ndd, fill = source)) +
  geom_boxplot(aes(x = reorder(local, ndd), y = ndd)) + theme_bw() +
  scale_fill_manual(values = c("gray50", "gray90")) +
  scale_y_continuous(breaks = seq(0,120,20), labels = seq(0,120, 20)) +
  theme(legend.position = "top", 
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = .95, hjust = 1)) +
  labs(y = "Max number of consecutive dry days \n (crop cycle)",
       x = "Sites")
p1

ggsave("C:/Users/lomo0003/BHC_Irrigation/Outputs/plots/ConsecutiveDryDays.png", plot = p1, width = 8, height = 5,
       dpi = 300, family = "serif")








