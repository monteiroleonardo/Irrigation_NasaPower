# ===================================================
# ----- Script to get NASA POWER data 
# ===================================================

# ---- packages ----
pkgs = c('nasapower', 'tidyverse')
lapply(pkgs, require, character.only = T)

# --- if any package is still not installed, please, use:
# install.packages('package_name')
install.packages('nasapower')

# --- weather stations
g <- grep(".csv", dir("./met_files/"), value = T)

# ---- downloading the data from each pair of geocoordinates as in g ----
for(k in 1:length(g)){
  A1 <- read.table(paste0("./met_files/", g[k]), header = T, sep = ";")
  A1$Tmed <- (A1$Tmax  + A1$Tmin) * 0.5
  
  # ---- download nasapower dataset
  ini_date = '2015-01-01'
  end_date = '2015-06-30'

  np <- get_power(lonlat = c(as.numeric(A1$lon[1]), as.numeric(A1$lat[1])),
              dates = c(ini_date, end_date), 
            community = "AG",
            pars = c("PRECTOT", # total precip 
                     "T2M_MAX", # tmax
                     "T2M_MIN", # tmin
                     "ALLSKY_SFC_SW_DWN"),# solar radiation
                     "WS2M", # wind speed at 2 m
                     "RH2M", # relative humidity
                     "T2M", # average air temperature
           temporal_average = "DAILY"
  )

# ---- renaming np dataframe ----
  colnames(np) <- c("lon",
                    "lat",
                    "yr",
                    "month",
                    "day",
                    "doy",
                    "Data",
                    "Prec_NASA",
                    "Tmax_NASA",
                    "Tmin_NASA",
                    "RS_NASA", 
                    "U2_NASA",
                    "UR_NASA",
                    "Tmed_NASA")
  
  write.csv(np, paste0("./met_files/comb/Comb_", g[k]), row.names = F, sep = ";")
print (paste0(k, " stations were downloaded from a total of: ", nrow(g), "stations selected"))  
}  






