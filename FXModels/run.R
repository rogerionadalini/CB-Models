rm(list = ls())

library(ecm)
library(urca)
library(ARDL)
library(dynamac) # https://cran.r-project.org/web/packages/dynamac/vignettes/dynamac-vignette.html
library(nardl)
library(tidyverse)
library(forecast)
library(patchwork)
library(tidyfit) 
library(lubridate)
library(openxlsx)
library(imputeTS)
library(fUnitRoots)
library(strucchange)
library(modelsummary)

setwd('C:/Users/Rogerio Nadalini/OneDrive/Documents/Economia//Fair Value')

source('FX_models.R')

# ***********************************************
# Import data and impute missing  ----
# ***********************************************

data <- read.xlsx(xlsxFile = 'C:/Users/Rogerio Nadalini/OneDrive/Documents/Economia//Fair Value/data.xlsx',
                  sheet = 'Data',
                  startRow = 3,
                  colNames = T,
                  detectDates = T)


data.impute <- data %>% as.ts() %>% na_kalman(smooth = F) # impute missing values with ss model
data.new <- as.data.frame(data.impute)

data.new %>% pivot_longer(cols = !Date, names_to = 'variable', values_to = 'values') %>% 
  ggplot(aes(x = as.Date(Date, origin), y = values)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw()

sum(is.na(data.new)) # check if residual NAs

# ***********************************************
# Set parameters for BRL ----
# ***********************************************

dependent <- 'BRL'
independent <- c('CDS5YBR', 'CRB', 'IBOV_SPX', 'US10Y', 'PRE1Y', 'CESTABR')
model.string <- "BRL ~ CDS5YBR + CRB + IBOV_SPX + US10Y + PRE1Y + CESTABR"
model <- as.formula(model.string)
nobs <-  252*2
nwindows <- 1400
last.obs <- max(data$Date)
outofsample.res <- 21
outofsample.end <- '2022-11-01' # CHOOSE A DATE IN THE DATABASE!

plots.BRL <- estimate_FX_models(data.new, 
                                dependent, 
                                independent, 
                                model.string, 
                                model, 
                                nobs,
                                nwindows,
                                outofsample.res,
                                outofsample.end)

# ***********************************************
# Set parameters for MXN ----
# ***********************************************

dependent <- 'MXN'
independent <- c('CDS5YMX', 'CRB', 'DifUSMXN', 'CESTAMX', 'DXY')
model.string <- "MXN ~ CDS5YMX + CRB + DifUSMXN + CESTAMX + DXY"
model <- as.formula(model.string)
nobs <-  252*2
nwindows <- 1400
last.obs <- max(data$Date)
outofsample.end <- '2022-11-01' # CHOOSE A DATE IN THE DATABASE!

plots.MXN <- estimate_FX_models(data.new, 
                                dependent, 
                                independent, 
                                model.string, 
                                model, 
                                nobs,
                                nwindows,
                                outofsample.res,
                                outofsample.end)


# ***********************************************
# Set parameters for CLP ----
# ***********************************************

dependent <- 'CLP'
independent <- c('CDS5YCL', 'COBRE', 'US10Y', 'IPSA_SPX', 'DifUSCL', 'CESTACL', 'VIX')
model.string <- "CLP ~ CDS5YCL + COBRE + US10Y + IPSA_SPX + DifUSCL + CESTACL + VIX"
model <- as.formula(model.string)
nobs <-  252*2
nwindows <- 1400
last.obs <- max(data$Date)
outofsample.end <- '2022-11-01' # CHOOSE A DATE IN THE DATABASE!

plots.CLP <- estimate_FX_models(data.new, 
                                dependent, 
                                independent, 
                                model.string, 
                                model, 
                                nobs,
                                nwindows,
                                outofsample.res,
                                outofsample.end)


# ***********************************************
# Set parameters for COP ----
# ***********************************************

dependent <- 'COP'
independent <- c('OIL', 'CDS5YCO', 'US10Y', 'VIX', 'DifUSCO', 'CESTACO')
model.string <- "COP ~ OIL + CDS5YCO + US10Y + VIX + DifUSCO + CESTACO"
model <- as.formula(model.string)
nobs <-  252*2
nwindows <- 1400
last.obs <- max(data$Date)
outofsample.end <- '2022-11-01' # CHOOSE A DATE IN THE DATABASE!

plots.COP <- estimate_FX_models(data.new, 
                                dependent, 
                                independent, 
                                model.string, 
                                model, 
                                nobs,
                                nwindows,
                                outofsample.res,
                                outofsample.end)


# ***********************************************
# Export Excel ----
# ***********************************************

# Armazenar como vintage e deletar o arquivo existente.
setwd('C:/Users/Rogerio Nadalini/OneDrive/Documents/Economia/Fair Value/Rolling Results')

file.rename('rolling_results.xlsx',paste0('rolling_results - ', Sys.Date(),'.xlsx'))
file.copy(paste0('rolling_results - ', Sys.Date(),'.xlsx'), 'Vintages/', overwrite = T)
unlink(paste0('rolling_results - ', Sys.Date(),'.xlsx'))

wb <- createWorkbook("rolling_results")

addWorksheet(wb, "BRL")
addWorksheet(wb, "MXN")
addWorksheet(wb, "COP")
addWorksheet(wb, "CLP")

writeData(wb, sheet = "BRL", plots.BRL[[10]])
writeData(wb, sheet = "MXN", plots.MXN[[10]])
writeData(wb, sheet = "COP", plots.COP[[10]])
writeData(wb, sheet = "CLP", plots.CLP[[10]])

saveWorkbook(wb, "rolling_results.xlsx", overwrite = TRUE)


# ***********************************************
# Export PDF ----
# ***********************************************

setwd('C:/Users/Rogerio Nadalini/OneDrive/Documents/Economia/Fair Value/Rolling Results/Graficos')
unlink("Chartbook_FX_Models.pdf")

dev.off()

pdf(
  "Chartbook_FX_Models.pdf",
  width = 12,
  height = 7,
)

plot.new()
txt = paste0('DATA DO ÚLTIMO DADO DISPONÍVEL: ', last.obs)
text(.5, .5, txt, font = 50, cex=2.0)

# BRL
plot.new()
txt = paste0('BRL')
text(.5, .5, txt, font = 50, cex=3.0)

for (i in 1:length(plots.CLP)-1) {
  
  try(print(plots.BRL[[i]]))
  
}

# MXN
plot.new()
txt = paste0('MXN')
text(.5, .5, txt, font = 50, cex=3.0)

for (i in 1:length(plots.CLP)-1) {
  
  try(print(plots.MXN[[i]]))
  
}

# CLP
plot.new()
txt = paste0('CLP')
text(.5, .5, txt, font = 50, cex=3.0)

for (i in 1:length(plots.CLP)-1) {
  
  try(print(plots.CLP[[i]]))
  
}

# COP
plot.new()
txt = paste0('COP')
text(.5, .5, txt, font = 50, cex=3.0)

for (i in 1:length(plots.CLP)-1) {
  
  try(print(plots.COP[[i]]))
  
}

dev.off()
