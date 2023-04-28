rm(list = ls())

# library(ggblanket)
library(imputeTS)     
library(sidrar)       
library(xts)  
library(zoo)
library(rbcb)         
library(tidyverse)
library(lubridate)
library(forecast)
library(devtools)
library(seasonal)     
library(rsoi)         # para baixar o ONI
library(mFilter)      
library(ipeadatar)    
library(Quandl)       # FED FUNDS, see https://docs.data.nasdaq.com/docs/r-installation
library(jtools)
library(openxlsx)

# Quandç API. Create yours here: https://data.nasdaq.com/sign-up
Quandl.api_key("jzxqo6zxXTYzuozYcjDx") 

# escolher data de corte para calcular delta entre copoms ou entre semanas:
corte.anterior <- '2023-01-27'
corte.atual <- '2023-02-03'

# Shortcut tip:
#   - Collapse all: Alt+o
#   - Expand: Alt+Shit+o

# TO DO:
#   - DADOS QUE NÃO ESTÃO AUTOMATIZADOS: CDS e Incerteza FGV -> Atualização manual na planilha, via bloomberg e site da GV
#   - Futuramente calcular o Hiato Mundial. Sugestao: pacote OCDE - http://127.0.0.1:22116/library/OECD/doc/oecd_vignette_main.html
#   - Calcular a variação cambial descontada da parcela explicada pela variação dos preços de commodities. RTI de dez/20.
#   - Adicioanar previsão focus para aberturas: alimentacao...

# NOTE:
#   - Expectativas de Selic já foram corrigidas.
#   - Adicionar as suas projeções de PIB e CAGED manualmente antes de passar o HP.
#   - A planilha raw_base.xlsx deve estar fechada antes de rodar esse código!
#   - É necessário ter o X13 e o Java instalado no computador.
#   - API do BC às vezes fica instável em feriados e fins de semana. Nesses casos é preciso esperar o sistema voltar ou atualizar manualmente.


# Defina o working directory: .../Modelo Semi Estrutural/Excel Files
setwd("XXX")

# Armazenar como vintage e deletar o arquivo existente.
file.rename('raw_base.xlsx',paste0('raw_base - ', Sys.Date(),'.xlsx'))
file.copy(paste0('raw_base - ', Sys.Date(),'.xlsx'), 'vintages/', overwrite = T)
unlink(paste0('raw_base - ', Sys.Date(),'.xlsx'))

wb <- createWorkbook("raw_data")

### IPCA Focus mensal ----
IPCA.m <- rbcb::get_monthly_market_expectations(indic = "IPCA", start_date = Sys.Date()-20)
ADM.m <- rbcb::get_monthly_market_expectations(indic = "IPCA Administrados", start_date = Sys.Date()-20)
LIVRES.m <- rbcb::get_monthly_market_expectations(indic = "IPCA Livres", start_date = Sys.Date()-20)

IPCA.m <- IPCA.m %>% filter(date == as.Date(corte.anterior) | date == as.Date(corte.atual), 
                  base == 1) %>% 
  select(date, reference_date, median) %>% 
  pivot_wider(id_cols = reference_date, names_from = date, values_from = median) %>% 
  mutate(reference_date = gsub(" ", "", as.character(reference_date), fixed = TRUE)) %>% 
  mutate(reference_date = paste0(reference_date,"-01"))


ADM.m <- ADM.m %>% filter(date == as.Date(corte.anterior) | date == as.Date(corte.atual), 
                  base == 1) %>% 
  select(date, reference_date, median) %>% 
  pivot_wider(id_cols = reference_date, names_from = date, values_from = median) %>% 
  mutate(reference_date = gsub(" ", "", as.character(reference_date), fixed = TRUE)) %>% 
  mutate(reference_date = paste0(reference_date,"-01"))


LIVRES.m <- LIVRES.m %>% filter(date == as.Date(corte.anterior) | date == as.Date(corte.atual), 
                  base == 1) %>% 
  select(date, reference_date, median) %>% 
  pivot_wider(id_cols = reference_date, names_from = date, values_from = median) %>% 
  mutate(reference_date = gsub(" ", "", as.character(reference_date), fixed = TRUE)) %>% 
  mutate(reference_date = paste0(reference_date,"-01"))


monthly.inf.expect <- merge(IPCA.m,ADM.m, by = 'reference_date') %>% merge(LIVRES.m, by = 'reference_date') %>% 
  mutate(reference_date = as.Date(reference_date)) %>% arrange(reference_date)

rm(IPCA.m, ADM.m, LIVRES.m)

colnames(monthly.inf.expect) <- c('reference_date',
                                  paste0(corte.atual, "-IPCA"),
                                  paste0(corte.anterior, "-IPCA"),
                                  paste0(corte.atual, "-ADM"),
                                  paste0(corte.anterior, "-ADM"),
                                  paste0(corte.atual, "-LIVRES"),
                                  paste0(corte.anterior, "-LIVRES"))


monthly.inf.expect <- monthly.inf.expect %>% na.omit()

# export
addWorksheet(wb, "IPCA_EXP_MONTHLY")
writeData(wb, sheet = "IPCA_EXP_MONTHLY", monthly.inf.expect)


### IPCA Focus 12 meses a frente (média no periodo) ----------------------------
ipca.exp.raw <- get_twelve_months_inflation_expectations(indic = "IPCA")

ipca.exp <- ipca.exp.raw %>% filter(smoothed == "N" & base == 0) %>% select(date, median)

ipca.exp$mean <- as.numeric(ipca.exp$median)
ipca.exp$ano <- year(ipca.exp$date)
ipca.exp$tri <- quarter(ipca.exp$date)
ipca.exp$dia <- day(ipca.exp$date)
ipca.exp$mes <- month(ipca.exp$date)

ipca.exp %>% 
  group_by(ano, mes) %>% 
  filter(dia == max(dia)) %>% 
  group_by(ano, tri) %>% summarise(media = round(mean(median),2)) -> IPCA.qmean1

ipca.exp %>% 
  group_by(ano, mes) %>% 
  group_by(ano, tri) %>% summarise(media = round(mean(median),2)) -> IPCA.qmean2

ipca.exp %>% 
  group_by(ano, tri) %>% 
  filter(dia == max(dia)) %>% 
  group_by(ano, tri) %>% summarise(media = round(mean(median),2)) -> IPCA.qmean3

ipca.exp %>% 
  group_by(ano, tri) %>% 
  filter(dia == max(dia)) %>% 
  group_by(ano, tri) %>% summarise(media = round(mean(median),2)) -> IPCA.qmean4

ipca.exp <- ipca.exp.raw %>% filter(smoothed == "S" & base == 0) %>% select(date, median)

ipca.exp$mean <- as.numeric(ipca.exp$median)
ipca.exp$ano <- year(ipca.exp$date)
ipca.exp$tri <- quarter(ipca.exp$date)
ipca.exp$dia <- day(ipca.exp$date)
ipca.exp$mes <- month(ipca.exp$date)

ipca.exp %>% 
  group_by(ano, mes) %>% 
  filter(dia == max(dia)) %>% 
  group_by(ano, tri) %>% summarise(media = round(mean(median),2)) -> IPCA.qmean5

ipca.exp %>% 
  group_by(ano, mes) %>% 
  group_by(ano, tri) %>% summarise(media = round(mean(median),2)) -> IPCA.qmean6

ipca.exp %>% 
  group_by(ano, tri) %>% 
  filter(dia == max(dia)) %>% 
  group_by(ano, tri) %>% summarise(media = round(mean(median),2)) -> IPCA.qmean7

ipca.exp %>% 
  group_by(ano, tri) %>% 
  filter(dia == min(dia)) %>% 
  group_by(ano, tri) %>% summarise(media = round(mean(median),2)) -> IPCA.qmean8

cbind(IPCA.qmean1, IPCA.qmean2$media, IPCA.qmean3$media, IPCA.qmean4$media, IPCA.qmean5$media, IPCA.qmean6$media)

IPCA.qmean <- as.data.frame(cbind(IPCA.qmean1, IPCA.qmean2$media, IPCA.qmean3$media, IPCA.qmean4$media, IPCA.qmean5$media, IPCA.qmean6$media, IPCA.qmean7$media, IPCA.qmean8$media))

colnames(IPCA.qmean) <- c('ano','tri','media1','media2','media3','media4','media5','media6','media7','media8')


# export
addWorksheet(wb, "IPCA_EXP")
writeData(wb, sheet = "IPCA_EXP", IPCA.qmean)

### Selic Focus 12 meses a frente -----------------------------

selic.old <- get_monthly_market_expectations("Selic", start_date = NULL, end_date = NULL)

aux.selic <- get_selic_market_expectations()

# Corrigir com o calendário correto do copom
selic <- aux.selic %>% 
  separate(Reuniao, sep = '/', into = c("Meeting","Year")) %>% 
  mutate(month = case_when(Meeting == 'R1' ~ 1,
                           Meeting == 'R2' ~ 3,
                           Meeting == 'R3' ~ 4,
                           Meeting == 'R4' ~ 6,
                           Meeting == 'R5' ~ 7,
                           Meeting == 'R6' ~ 9,
                           Meeting == 'R7' ~ 10,
                           Meeting == 'R8' ~ 12)
         ) %>% 
  mutate(reference_date = paste0(Year,'-',month))

# filter and do a linear interpolation for NAs 12 months ahead
selic_exp_12meses <- selic %>%
  filter(base == 0) %>% 
  select(date, reference_date, median) %>% 
  mutate(reference_date = as.Date(paste0(reference_date,"-01"))) %>% 
  mutate(ano = year(as.Date(.$date)),
         mes = month(as.Date(.$date)),
         tri = quarter(as.Date(.$date)),
         dia = day(as.Date(.$date))) %>%
  group_by(ano, mes, reference_date) %>% 
  filter(dia == max(dia)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  complete(reference_date = seq.Date(min(reference_date), max(reference_date), by="month")) %>% 
  fill(median) %>% ungroup() %>% 
  mutate(ano.ref = year(as.Date(.$reference_date)),
         mes.ref = month(as.Date(.$reference_date)),
         tri.ref = quarter(as.Date(.$reference_date)),
         dia.ref = day(as.Date(.$reference_date))) %>%
  mutate(ano = year(as.Date(.$date)),
         mes = month(as.Date(.$date)),
         tri = quarter(as.Date(.$date)),
         dia = day(as.Date(.$date))) %>%
#  group_by(ano, mes) %>% filter(ano.ref == (ano + 1) & mes.ref == mes) %>%
  group_by(ano, mes) %>% filter(ano.ref == (ano + 1) & (mes.ref == mes | mes.ref == mes + 1| mes.ref == mes - 1)) %>%
  group_by(ano, tri) %>% 
  summarise(expec = round(mean(median),2))

selic_exp_12meses.old <- selic.old %>%
  filter(base == 0) %>% 
  select(date, reference_date, median) %>% 
  mutate(reference_date = as.Date(paste0(reference_date,"-01"))) %>% 
  mutate(ano = year(as.Date(.$date)),
         mes = month(as.Date(.$date)),
         tri = quarter(as.Date(.$date)),
         dia = day(as.Date(.$date))) %>%
  group_by(ano, mes, reference_date) %>% 
  filter(dia == max(dia)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  complete(reference_date = seq.Date(min(reference_date), max(reference_date), by="month")) %>% 
  fill(median) %>% ungroup() %>% 
  mutate(ano.ref = year(as.Date(.$reference_date)),
         mes.ref = month(as.Date(.$reference_date)),
         tri.ref = quarter(as.Date(.$reference_date)),
         dia.ref = day(as.Date(.$reference_date))) %>%
  mutate(ano = year(as.Date(.$date)),
         mes = month(as.Date(.$date)),
         tri = quarter(as.Date(.$date)),
         dia = day(as.Date(.$date))) %>%
  #  group_by(ano, mes) %>% filter(ano.ref == (ano + 1) & mes.ref == mes) %>%
  group_by(ano, mes) %>% filter(ano.ref == (ano + 1) & (mes.ref == mes | mes.ref == mes + 1| mes.ref == mes - 1)) %>%
  group_by(ano, tri) %>% 
  summarise(expec = round(mean(median),2))

selic_exp_12meses <- rbind(selic_exp_12meses.old, selic_exp_12meses)

selic_exp_12meses2 <- selic %>%
  filter(base == 0) %>% 
  select(date, reference_date, median) %>% 
  mutate(reference_date = as.Date(paste0(reference_date,"-01"))) %>% 
  mutate(ano = year(as.Date(.$date)),
         mes = month(as.Date(.$date)),
         tri = quarter(as.Date(.$date)),
         dia = day(as.Date(.$date))) %>%
  group_by(ano, mes, reference_date) %>% 
  filter(dia == max(dia)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  complete(reference_date = seq.Date(min(reference_date), max(reference_date), by="month")) %>% 
  fill(median) %>% ungroup() %>% 
  mutate(ano.ref = year(as.Date(.$reference_date)),
         mes.ref = month(as.Date(.$reference_date)),
         tri.ref = quarter(as.Date(.$reference_date)),
         dia.ref = day(as.Date(.$reference_date))) %>%
  mutate(ano = year(as.Date(.$date)),
         mes = month(as.Date(.$date)),
         tri = quarter(as.Date(.$date)),
         dia = day(as.Date(.$date))) %>%
  group_by(ano, mes) %>% filter(ano.ref == (ano + 1) & mes.ref == mes) %>%
  filter(mes %in% c(3,6,9,12)) %>% 
  group_by(ano, tri) %>% 
  summarise(expec = round(mean(median),2))

selic_exp_12meses2 <- rbind(selic_exp_12meses.old, selic_exp_12meses2)

selic_exp_12meses <- merge(selic_exp_12meses, selic_exp_12meses2, by = c('ano', 'tri'))

selic_exp_12meses <- as.data.frame(selic_exp_12meses)

colnames(selic_exp_12meses) <- c('ano','tri','expec1','expec2')

addWorksheet(wb, "SELIC_EXP")
writeData(wb, sheet = "SELIC_EXP", selic_exp_12meses)


# see if it's ok
plot(selic_exp_12meses$expec1, type = "l")

### Selic Focus Mensal e Anual (para projs. condicionais) -------------------------------

#traj.selic <- get_monthly_market_expectations(indic = "Selic")
traj.selic <- selic

max.date <- max(as.Date(traj.selic$date))
traj.selic.last <- traj.selic %>% arrange(Year, month) %>% filter(date == as.Date(max.date)) %>% arrange(reference_date) %>% filter(base == 0)
traj.selic.last.COPOM <- traj.selic %>% filter(date == as.Date(corte.atual)) %>% arrange(reference_date) %>% filter(base == 0)
traj.selic.before <- traj.selic %>% filter(date == as.Date(corte.anterior)) %>% arrange(reference_date) %>% filter(base == 0)

before <- traj.selic.before %>% arrange(Year, month) %>% select(reference_date, median, Year, month) 
last.COPOM <- traj.selic.last.COPOM %>% arrange(Year, month) %>% select(reference_date, median) 
delta.selic <- merge(before,last.COPOM, by = 'reference_date', all.y = T) %>% arrange(Year, month) %>% select(reference_date, median.x, median.y)

colnames(delta.selic) <- c('Date','Traj.Anterior','Traj.Atual')

selic.anual <- get_annual_market_expectations(indic = "Selic")
selic.anual.before <- selic.anual %>% filter(date == as.Date("2021-12-02")) %>% arrange(reference_date) %>% filter(base == 0)
selic.anual.last <- selic.anual %>% filter(date == as.Date(max.date)) %>% arrange(reference_date) %>% filter(base == 0)

traj.selic.mensal <- as.data.frame(traj.selic.last)
traj.selic.mensal <- traj.selic.mensal %>% arrange(Year, month)
traj.selic.mensal.COPOM <- as.data.frame(delta.selic) %>% mutate(Delta = Traj.Atual - Traj.Anterior)
traj.selic.anual <- as.data.frame(selic.anual.last)

addWorksheet(wb, "TRAJ_SELIC_MENSAL")
addWorksheet(wb, "TRAJ_SELIC_ANUAL")
addWorksheet(wb, "TRAJ_SELIC_MENSAL_COPOM")
writeData(wb, sheet = "TRAJ_SELIC_MENSAL", traj.selic.mensal)
writeData(wb, sheet = "TRAJ_SELIC_ANUAL", traj.selic.anual)
writeData(wb, sheet = "TRAJ_SELIC_MENSAL_COPOM", traj.selic.mensal.COPOM)

### IC-Br em reais ----------------------------------
IC.br <- get_series(27574)
IC.br <- as.data.frame(IC.br)

addWorksheet(wb, "IC_br")
writeData(wb, sheet = "IC_br", IC.br)

Sys.sleep(1)

### IC-Br Agro em reais ----------------------------------
IC.br.Agro <- get_series(27575)
IC.br.Agro <- as.data.frame(IC.br.Agro)

addWorksheet(wb, "IC_br_Agro")
writeData(wb, sheet = "IC_br_Agro", IC.br.Agro)

Sys.sleep(1)

### IC-Br Metais em reais ----------------------------------
IC.br.Metal <- get_series(27576)
IC.br.Metal <- as.data.frame(IC.br.Metal)

addWorksheet(wb, "IC_br_metal")
writeData(wb, sheet = "IC_br_metal", IC.br.Metal)

Sys.sleep(1)

### IC-Br Energia em reais ----------------------------------
IC.br.Energia <- get_series(27577)
IC.br.Energia <- as.data.frame(IC.br.Energia)

addWorksheet(wb, "IC_br_energia")
writeData(wb, sheet = "IC_br_energia", IC.br.Energia)

write.xlsx(IC.br.Energia, 
           file="raw_base.xlsx", 
           sheetName="IC_br_energia", 
           append = T, 
           row.names=FALSE)

Sys.sleep(1)

### IPCA - Administrados -------------------------------
ipca.monitorados <- get_series(4449)
colnames(ipca.monitorados) <- c("Data","monitorados")
ipca.monitorados$monitorados <- as.numeric(ipca.monitorados$monitorados)

proj.dates <- data.frame(reference_date = as.Date(c(tail(ipca.monitorados$Data, 1) %m+% months(1),
                                                    tail(ipca.monitorados$Data, 1) %m+% months(2),
                                                    tail(ipca.monitorados$Data, 1) %m+% months(3),
                                                    tail(ipca.monitorados$Data, 1) %m+% months(4),
                                                    tail(ipca.monitorados$Data, 1) %m+% months(5),
                                                    tail(ipca.monitorados$Data, 1) %m+% months(6))))

aux <- left_join(proj.dates, monthly.inf.expect)

proj <- data.frame(Data = proj.dates[,], 
                   monitorados = c(aux[1,4],
                                   aux[2,4],
                                   aux[3,4],
                                   aux[4,4],
                                   aux[5,4],
                                   aux[6,4]))

ipca.monitorados <-
  ipca.monitorados %>% 
  rbind(proj) %>% 
  filter(Data >= as.Date('2001-01-01')) %>% 
  distinct(Data, .keep_all= TRUE)

ADM.ts.subset <- ts(ipca.monitorados$monitorados, frequency = 12, start = c(2001, 1))

plot(ADM.ts.subset)

# dessaz
Sys.sleep(1)
ADM.sa <- seas(ADM.ts.subset)
ADM.final <- series(ADM.sa, "s11")
ADM.final.df <- as.data.frame(ADM.final)
ADM.final.df$date <- ipca.monitorados$Data

ADM.final.df$indic <- cumprod(1+ADM.final.df$x/100)
ADM.final.df$ADM <- (((ADM.final.df$indic/lag(ADM.final.df$indic,3))^4)-1)*100
ADM.final.df$ano <- year(ADM.final.df$date)
ADM.final.df$mes <- month(ADM.final.df$date)

ADM.quarter <- ADM.final.df %>% filter(mes %in% c(3,6,9,12)) %>% select(date, ano, mes, ADM)
ADM.quarter <- as.data.frame(ADM.quarter)

addWorksheet(wb, "ADM_sa")
writeData(wb, sheet = "ADM_sa", ADM.quarter)

### IPCA-LIVRES Dessaz -------------------------------
ipca.livre <- get_series(11428)
colnames(ipca.livre) <- c("Data","Livres")
ipca.livre$Livres <- as.numeric(ipca.livre$Livres)

proj.dates <- data.frame(reference_date = as.Date(c(tail(ipca.livre$Data, 1) %m+% months(1),
                                                    tail(ipca.livre$Data, 1) %m+% months(2),
                                                    tail(ipca.livre$Data, 1) %m+% months(3),
                                                    tail(ipca.livre$Data, 1) %m+% months(4),
                                                    tail(ipca.livre$Data, 1) %m+% months(5),
                                                    tail(ipca.livre$Data, 1) %m+% months(6))))

aux <- left_join(proj.dates, monthly.inf.expect)

proj <- data.frame(Data = proj.dates[,], 
                   Livres = c(aux[1,6],
                              aux[2,6],
                              aux[3,6],
                              aux[4,6],
                              aux[5,6],
                              aux[6,6]))

ipca.livre <-
  ipca.livre %>% 
  rbind(proj) %>% 
  filter(Data >= as.Date('2001-01-01')) %>% 
  distinct(Data, .keep_all= TRUE)

ipca.ts.subset <- ts(ipca.livre$Livres, frequency = 12, start = c(2001, 1))
plot(ipca.ts.subset)

Sys.sleep(1)
ipca.livre.sa <- seas(ipca.ts.subset)
summary(ipca.livre.sa)
plot(ipca.livre.sa)

IPCA.final <- series(ipca.livre.sa, "s11")
IPCA.final.df <- as.data.frame(ipca.livre.sa)[,1:2]

# frequencias sazonais e de dias úteis 
#freq <- c((1:6)/12, 0.3482, 0.4326) 

# spectral plot
#spec_orig <- series(ipca.livre.sa,"sp0") 
#spec_sa <- series(ipca.livre.sa,"s1s") 

#plot(spec_orig, type = "l") 
#abline(v = freq, col = c(rep("red",6),"blue","blue"), lty = c(rep(2,6),3,3)) 
#legend("topleft", legend = c("Original", "S. Effects", "TD. Effects"), cex = 0.6, lty = 1:3, bty = "n", col = c(1,2,4))

#plot(spec_sa, type = "l") 
#abline(v = freq, col = c(rep("red",6),"blue","blue"), lty = c(rep(2,6),3,3)) 
#legend("topleft", legend = c("Seasonally Adjused", "S. Effects", "TD. Effects"), cex = 0.6, lty = 1:3, bty = "n", col = c(1,2,4))

addWorksheet(wb, "IPCA_Livres_sa")
writeData(wb, sheet = "IPCA_Livres_sa", IPCA.final.df)

### IPCA-CHEIO Dessaz -----------------------------------
ipca <- get_series(433)
colnames(ipca) <- c("Data","IPCA")
ipca$IPCA <- as.numeric(ipca$IPCA)

proj.dates <- data.frame(reference_date = as.Date(c(tail(ipca$Data, 1) %m+% months(1),
                                                    tail(ipca$Data, 1) %m+% months(2),
                                                    tail(ipca$Data, 1) %m+% months(3),
                                                    tail(ipca$Data, 1) %m+% months(4),
                                                    tail(ipca$Data, 1) %m+% months(5),
                                                    tail(ipca$Data, 1) %m+% months(6))))

aux <- left_join(proj.dates, monthly.inf.expect)

proj <- data.frame(Data = proj.dates[,], 
                   IPCA = c(aux[1,2],
                            aux[2,2],
                            aux[3,2],
                            aux[4,2],
                            aux[5,2],
                            aux[6,2]))

ipca <-
  ipca %>% 
  rbind(proj) %>% 
  filter(Data >= as.Date('2001-01-01')) %>% 
  distinct(Data, .keep_all= TRUE)

ipca.cheio.ts.subset <- ts(ipca$IPCA, frequency = 12, start = c(2001, 1))
plot(ipca.cheio.ts.subset)

Sys.sleep(1)
ipca.cheio.sa <- seas(ipca.cheio.ts.subset)
summary(ipca.cheio.sa)
plot(ipca.cheio.sa)
#view(ipca.cheio.sa)

IPCA.cheio.final <- series(ipca.cheio.sa, "s11")
IPCA.cheio.final.df <- as.data.frame(ipca.cheio.sa)[,1:2]

# IPCA 12 Meses NSA
ipca$indic <- cumprod(1+ipca$IPCA/100)
ipca$IPCA12 <- ((ipca$indic/lag(ipca$indic,12))-1)*100
ipca$ano <- year(ipca$Data)
ipca$mes <- month(ipca$Data)
ipca.12m <- ipca %>% filter(mes %in% c(3,6,9,12)) %>% select(Data, ano, mes, IPCA12)
ipca.12m <- as.data.frame(ipca.12m)

# alternative dessaz: x11
ipca.cheio.x11 <- seas(ipca.cheio.ts.subset, x11 = '')
summary(ipca.cheio.x11)
plot(ipca.cheio.x11)
IPCA.cheio.final.x11 <- series(ipca.cheio.x11, "d11")
IPCA.cheio.final.df.x11 <- as.data.frame(ipca.cheio.x11)[,1:2]


addWorksheet(wb, "IPCA_Cheio_x11")
addWorksheet(wb, "IPCA_Cheio_sa")
addWorksheet(wb, "IPCA_Cheio_12m")

writeData(wb, sheet = "IPCA_Cheio_x11", IPCA.cheio.final.df.x11)
writeData(wb, sheet = "IPCA_Cheio_sa", IPCA.cheio.final.df)
writeData(wb, sheet = "IPCA_Cheio_12m", ipca.12m)

### IPCA-Alim. no Domicilio Dessaz -----------------------------------

alim.dom <- get_series(27864)
colnames(alim.dom) <- c("Data","alim.dom")
alim.dom$alim.dom <- as.numeric(alim.dom$alim.dom)

proj <- data.frame(Data = as.Date(c(tail(alim.dom$Data, 1) %m+% months(1),
                                    tail(alim.dom$Data, 1) %m+% months(2))), 
                   alim.dom = c(0.64,
                                0.50))

alim.dom <-
  alim.dom %>% 
  rbind(proj) %>% 
  filter(Data >= as.Date('2001-01-01')) %>% 
  distinct(Data, .keep_all= TRUE)

alim.dom.cheio.ts.subset <- ts(alim.dom$alim.dom, frequency = 12, start = c(2001, 1))
plot(alim.dom.cheio.ts.subset)

Sys.sleep(1)
alim.dom.cheio.sa <- seas(alim.dom.cheio.ts.subset)
summary(alim.dom.cheio.sa)
plot(alim.dom.cheio.sa)
#view(alim.dom.cheio.sa)

alim.dom.cheio.final <- series(alim.dom.cheio.sa, "s11")
alim.dom.cheio.final.df <- as.data.frame(alim.dom.cheio.sa)[,1:2]
alim.dom.cheio.final.df$date <- alim.dom$Data 

# alim.dom 12 Meses NSA
alim.dom$indic <- cumprod(1+alim.dom$alim.dom/100)
alim.dom$alim.dom12 <- ((alim.dom$indic/lag(alim.dom$indic,12))-1)*100
alim.dom$ano <- year(alim.dom$Data)
alim.dom$mes <- month(alim.dom$Data)
alim.dom.12m <- alim.dom %>% filter(mes %in% c(3,6,9,12)) %>% select(Data, ano, mes, alim.dom12)
alim.dom.12m <- as.data.frame(alim.dom.12m)

addWorksheet(wb, "alim.dom_sa")
writeData(wb, sheet = "alim.dom_sa", alim.dom.cheio.final.df)

addWorksheet(wb, "alim.dom_12m")
writeData(wb, sheet = "alim.dom_12m", alim.dom.12m)

### IPCA-Servicos Dessaz -----------------------------------

servicos <- get_series(10844)
colnames(servicos) <- c("Data","servicos")
servicos$servicos <- as.numeric(servicos$servicos)

proj <- data.frame(Data = as.Date(c(tail(servicos$Data, 1) %m+% months(1),
                                    tail(servicos$Data, 1) %m+% months(2))), 
                   servicos = c(0.64,
                                0.50))

servicos <-
  servicos %>% 
  rbind(proj) %>% 
  filter(Data >= as.Date('2001-01-01')) %>% 
  distinct(Data, .keep_all= TRUE)

servicos.cheio.ts.subset <- ts(servicos$servicos, frequency = 12, start = c(2001, 1))
plot(servicos.cheio.ts.subset)

Sys.sleep(1)
servicos.cheio.sa <- seas(servicos.cheio.ts.subset)
summary(servicos.cheio.sa)
plot(servicos.cheio.sa)
#view(servicos.cheio.sa)

servicos.cheio.final <- series(servicos.cheio.sa, "s11")
servicos.cheio.final.df <- as.data.frame(servicos.cheio.sa)[,1:2]

# servicos 12 Meses NSA
servicos$indic <- cumprod(1+servicos$servicos/100)
servicos$servicos12 <- ((servicos$indic/lag(servicos$indic,12))-1)*100
servicos$ano <- year(servicos$Data)
servicos$mes <- month(servicos$Data)
servicos.12m <- servicos %>% filter(mes %in% c(3,6,9,12)) %>% select(Data, ano, mes, servicos12)
servicos.12m <- as.data.frame(servicos.12m)


addWorksheet(wb, "servicos_sa")
writeData(wb, sheet = "servicos_sa", servicos.cheio.final.df)

addWorksheet(wb, "servicos_12m")
writeData(wb, sheet = "servicos_12m", servicos.12m)

### IPCA-Industriais Dessaz -----------------------------------

industriais <- get_series(27863)
colnames(industriais) <- c("Data","industriais")
industriais$industriais <- as.numeric(industriais$industriais)

proj <- data.frame(Data = as.Date(c(tail(industriais$Data, 1) %m+% months(1),
                                    tail(industriais$Data, 1) %m+% months(2))), 
                   industriais = c(0.64,
                                   0.50))

industriais <-
  industriais %>% 
  rbind(proj) %>% 
  filter(Data >= as.Date('2001-01-01')) %>% 
  distinct(Data, .keep_all= TRUE)

industriais.cheio.ts.subset <- ts(industriais$industriais, frequency = 12, start = c(2001, 1))
plot(industriais.cheio.ts.subset)

Sys.sleep(1)
industriais.cheio.sa <- seas(industriais.cheio.ts.subset)
summary(industriais.cheio.sa)
plot(industriais.cheio.sa)
#view(industriais.cheio.sa)

industriais.cheio.final <- series(industriais.cheio.sa, "s11")
industriais.cheio.final.df <- as.data.frame(industriais.cheio.sa)[,1:2]
industriais.cheio.final.df$date <- industriais$Data # CONFERIR O QUE ESTÁ ACONTECENDO

# industriais 12 Meses NSA
industriais$indic <- cumprod(1+industriais$industriais/100)
industriais$industriais12 <- ((industriais$indic/lag(industriais$indic,12))-1)*100
industriais$ano <- year(industriais$Data)
industriais$mes <- month(industriais$Data)
industriais.12m <- industriais %>% filter(mes %in% c(3,6,9,12)) %>% select(Data, ano, mes, industriais12)
industriais.12m <- as.data.frame(industriais.12m)

addWorksheet(wb, "industriais_sa")
writeData(wb, sheet = "industriais_sa", industriais.cheio.final.df)

addWorksheet(wb, "industriais_12m")
writeData(wb, sheet = "industriais_12m", industriais.12m)

### ONI ---------------------------------------------

oni <- download_oni()
oni <- as.data.frame(oni)
plot(oni)

addWorksheet(wb, "ONI")
writeData(wb, sheet = "ONI", oni)

### Primario Ajustado Dessaz -----------------------

primario <- na.omit(get_series(5364))
Sys.sleep(1)
colnames(primario) <- c("Data","primario")
plot(-primario$primario, type = "l")

primario$primario <- -as.numeric(primario$primario)
primario.ts <- ts(primario$primario, frequency = 12, start = c(2001, 12))
plot(primario.ts)

Sys.sleep(1)
primario.sa <- seas(primario.ts)
summary(primario.sa)
plot(primario.sa)

primario$SA <- as.data.frame(primario.sa)[,2]
primario <- as.data.frame(primario)

# primario.ts.sa <- ts(primario.sa.df$final, frequency = 12, start = c(2001, 12))
# primario.filter <- cffilter(primario.ts.sa)
# primario.bind <- as.data.frame(cbind(primario.filter[["trend"]],primario.filter[["x"]]))

addWorksheet(wb, "Primario")
writeData(wb, sheet = "Primario", primario)

### Hiato - PIB Dessaz índice ---------------------------

# PIB <- get_sidra(api = "/t/1621/n1/all/v/all/p/all/c11255/90707/d/v584%201")
# PIB <- PIB[,c("Trimestre","Valor")]

# alternativa via IPEA
PIB <- ipeadata('SCN104_PIBPMAS104')

PIB <- PIB %>% select(date, value) %>% 
  mutate(tri = quarter(as.Date(date)), ano = year(as.Date(date))) %>%
  rename(Valor = value) %>% 
  as.data.frame()

addWorksheet(wb, "PIB.Nivel")
writeData(wb, sheet = "PIB.Nivel", PIB)

# esticar a série antes de passar o HP com a sua previsão qoqsa
last <- tail(PIB$Valor,1)

previsao <- c(1+0.2/100, # 1q/2022
              1+0.2/100, # 2q/2022
              1+0.2/100, # 3q/2022
              1+0.2/100, # 4q/2022
              1+0.4/100, # 1q/2023
              1+0.4/100, # 2q/2023
              1+0.4/100, # 3q/2023
              1+0.4/100, # 4q/2023
              1+0.4/100, # 1q/2024
              1+0.4/100, # 2q/2024
              1+0.4/100, # 3q/2024
              rep(1+0.4/100,8)) 

previsao.cum <- cumprod(previsao)*last
previsao.encadeada <- c(PIB[,2], previsao.cum)
plot(previsao.encadeada, type = 'l')

ln.PIB <- log(previsao.encadeada)
PIB.filter <- hpfilter(ln.PIB, freq = 1600, type=c("lambda"), drift=FALSE)
PIB.cycle <- as.data.frame(PIB.filter[["cycle"]])
PIB.trend <- as.data.frame(PIB.filter[["trend"]])
PIB.obs <- as.data.frame(PIB.filter[["x"]])


PIB.HP <- as.data.frame(c(PIB.cycle, PIB.trend, PIB.obs))
PIB.HP$data <- c(paste0(PIB$ano,'-',PIB$tri), rep(NA, nrow(PIB.HP)-nrow(PIB)))
colnames(PIB.HP) <- c("Ciclo","Tendencia","Observado","Data")

addWorksheet(wb, "PIB")
writeData(wb, sheet = "PIB", PIB.HP)

### CAGED ---------------------------------------------
#search_series(terms = c('Empregados - saldo'), fields = c('name'))

Sys.sleep(1); CAGED1 <- ipeadata('CAGED12_SALDO12')
Sys.sleep(1); CAGED2 <- ipeadata('CAGED12_SALDON12')

CAGED <- rbind(CAGED1, CAGED2)

paste0('Último mês disponível: ', month(tail(CAGED$date,1)))

ultimo.mais.um <- month(tail(CAGED$date,1)) + 1

CAGED.ts <- ts(CAGED$value, frequency = 12, start = c(1999,5))

Sys.sleep(1)
CAGED.sa <- seas(CAGED.ts)
plot(CAGED.sa)

CAGED.final <- series(CAGED.sa, "s11")

# esticar a série antes de passar o HP com a sua previsão qoqsa
previsao <- ts(c(50000,  # jan/22 
                 50000,  # fev/22 
                 50000,  # mar/22 
                 50000,  # abr/22 
                 50000,  # mai/22 
                 50000,  # jun/22 
                 50000,  # jul/22 
                 50000,  # ago/22 
                 50000,  # set/22 
                 50000,  # out/22 
                 50000,  # nov/22 
                 50000,  # dez/22 
                 rep(30000,24)
), 
start = c(2022, ultimo.mais.um),  #' _CHECK HERE_ AS DATA COMES IN!
frequency = 12)

plot(previsao, type = "b")

CAGED.final <- ts(rbind.zoo(CAGED.final, previsao),  frequency = 12, start = c(1999,5))

# acumular estoque e passar o HP
cum.CAGED <- cumsum(CAGED.final)
plot(cum.CAGED, type = "l")
estoque.out20 <- 38743472
base <- estoque.out20 - tail(cum.CAGED,1)
CAGED.estoque <- log(cum.CAGED + base)
plot(CAGED.estoque, type = 'l')

CAGED.ts <- ts(CAGED.estoque, frequency = 12, start = c(1999,5))
CAGED.ts <- subset(CAGED.ts, month = c("March","June","September","December"))

CAGED.filter <- hpfilter(CAGED.ts, freq = 1600, type=c("lambda"), drift=FALSE)

CAGED.cycle <- as.data.frame(CAGED.filter[["cycle"]])
CAGED.trend <- as.data.frame(CAGED.filter[["trend"]])
CAGED.obs <- as.data.frame(CAGED.filter[["x"]])

CAGED.HP <- as.data.frame(c(CAGED.cycle, CAGED.trend, CAGED.obs))
CAGED.HP$data <- c(seq.Date(from = as.Date("1999/6/1"), by = 'quarter', length.out = nrow(CAGED.HP)))
colnames(CAGED.HP) <- c("Ciclo","Tendencia","Observado","Data")

addWorksheet(wb, "CAGED")
writeData(wb, sheet = "CAGED", CAGED.HP)

### NUCI Dessaz --------------------------------------

#search_series(terms = c('Utilização da capacidade instalada'), fields = c('name'))
NUCI <- ipeadata('CE12_CUTIND12')
seq <- seq(as.Date("1970/1/1"), Sys.Date(), "months")
size <- length(seq)

NUCI2month <- data.frame(date = seq, teste = seq(1,size))
NUCI2month <- merge(NUCI2month, NUCI, all.x = TRUE)
NUCI2month <- NUCI2month[,c("date","value")]
NUCI2month$value <- na_interpolation(NUCI2month$value)
NUCI2month$mm3m <- c(NA, NA, rollmean(NUCI2month$value, k = 3, align = "right"))

NUCI2quarter <- data.frame(date = seq(as.Date("1980/3/1"), Sys.Date(), "quarters"))
NUCI2quarter <- merge(NUCI2quarter, NUCI2month)

NUCI.ts <- ts(NUCI2quarter$mm3m, frequency = 4, start = c(1980,1))
plot(NUCI.ts)

Sys.sleep(1)
NUCI.sa <- seas(NUCI.ts)
summary(NUCI.sa)
plot(NUCI.sa)

NUCI.final <- as.data.frame(series(NUCI.sa, "s11"))
NUCI.final$date <- NUCI2quarter$date

addWorksheet(wb, "NUCI")
writeData(wb, sheet = "NUCI", NUCI.final)

### FED FUNDS --------------------------------------------
FF <-  Quandl("FRED/DFEDTARU", start_date = "2001-01-01", end_date = Sys.Date(), collapse="monthly")
FF <- as.data.frame(FF)

addWorksheet(wb, "FED_FUNDS")
writeData(wb, sheet = "FED_FUNDS", FF)

### Brent Oil --------------------------------------------
brent <- Quandl("FRED/DCOILBRENTEU", start_date = "2001-01-01", end_date = Sys.Date(), collapse="monthly")

brent <- as.data.frame(brent)

addWorksheet(wb, "BRENT_OIL_USDOLARS")
writeData(wb, sheet = "BRENT_OIL_USDOLARS", brent)

# daily brent
brent.daily <- Quandl("FRED/DCOILBRENTEU", start_date = "2001-01-01", end_date = Sys.Date())

brent.daily <- as.data.frame(brent.daily)

addWorksheet(wb, "Brent_Daily")
writeData(wb, sheet = "Brent_Daily", brent.daily)


### Câmbio: BRL/USD -------------------------------------
BRL <- get_series(1)
Sys.sleep(1)
colnames(BRL) <- c("Data","BRL")
BRL$BRL <- as.numeric(BRL$BRL)
BRL$ano <- year(BRL$Data)
BRL$tri <- quarter(BRL$Data)
BRL.qmean <- BRL %>% group_by(ano, tri) %>% summarise(media = round(mean(BRL),2))
BRL.qmean <- as.data.frame(BRL.qmean)

addWorksheet(wb, "BRL")
writeData(wb, sheet = "BRL", BRL.qmean)

BRL <- BRL %>% filter(ano >= 2003)

tail(BRL,5) %>% summarise(mean = mean(BRL))

addWorksheet(wb, "BRL_diario")
writeData(wb, sheet = "BRL_diario", BRL)

### Selic -----------------------------------------------
Selic <- get_series(432)
Sys.sleep(1)
colnames(Selic) <- c("Data","Selic")
Selic$Selic <- as.numeric(Selic$Selic)
Selic$ano <- year(Selic$Data)
Selic$tri <- quarter(Selic$Data)
Selic$mes <- month(Selic$Data)
Selic$dia <- day(Selic$Data)
Selic.qend <- Selic %>% group_by(ano, mes) %>% filter(dia == max(dia))
Selic.qmean <- Selic.qend %>% group_by(ano, tri) %>% summarise(media = round(mean(Selic),2))
Selic.qmean <- as.data.frame(Selic.qmean)

addWorksheet(wb, "Selic")
writeData(wb, sheet = "Selic", Selic.qmean)

### Desemprego Retropolado --------------------------------------

#search <- search_series(terms = c('desocupação'), fields = c('name'))
PME <- ipeadata('PMEN12_TD12')
Sys.sleep(1)

PNAD <- ipeadata('PNADC12_TDESOC12')
Sys.sleep(1)

PME.df <- data.frame(data = PME$date, value.PME = c(NA, NA, rollmean(PME$value, k = 3, align = 'right')))
PNAD.df <- data.frame(data = PNAD$date, value.PNAD = PNAD$value)
desemprego <- merge(PME.df, PNAD.df)

ggplot(desemprego, aes(x = data)) +
  geom_line(aes(y = value.PME)) +
  geom_line(aes(y = value.PNAD))

fit <- lm(value.PNAD ~ value.PME, data = desemprego)
summ(fit)

PME.df$predicted <- coef(fit)[1] + coef(fit)[2]*PME.df$value.PME 

desemprego.retro <- merge(PME.df, PNAD.df, all = T)
desemprego.retro$value.PNAD[is.na(desemprego.retro$value.PNAD)] <- desemprego.retro$predicted

PNAD.retro <- ts(desemprego.retro$value.PNAD, frequency = 12, start = c(2002, 5))

Sys.sleep(1)
PNAD.retro.sa <- seas(PNAD.retro)

PNAD.final <- as.data.frame(series(PNAD.retro.sa, "s11")) # - mean(series(PNAD.retro.sa, "s11"))
PNAD.final$date <- desemprego.retro[3:dim(desemprego.retro)[1],1]
PNAD.final <- as.data.frame(PNAD.final[,c("date","x")])

plot(PNAD.final$x)

addWorksheet(wb, "PNAD")
writeData(wb, sheet = "PNAD", PNAD.final)

### EXTRAS -----------------------------------

## Swap DI-pré 360 dias, media do período
# pre.di <- get_series(7806)

## Expextativas + longas
ipca.12m <- get_twelve_months_inflation_expectations(indic = "IPCA")
Sys.sleep(1)

ipca.12m <- ipca.12m %>% filter(smoothed == "N" & base == 0) %>% select(date, median)
ipca.12m <- as.data.frame(ipca.12m)

IPCA.focus <- get_annual_market_expectations(indic = "IPCA")
IPCA.focus <- IPCA.focus %>% filter(base == 0) %>% select(date, reference_date, median) %>% arrange(reference_date)
IPCA.focus <- as.data.frame(IPCA.focus)


addWorksheet(wb, "exp_longas_ipca12m")
writeData(wb, sheet = "exp_longas_ipca12m", ipca.12m)

addWorksheet(wb, "exp_longas_IPCA")
writeData(wb, sheet = "exp_longas_IPCA", IPCA.focus)

## Inputs Phillips para administrados - DEPRECATED

# IGP-M 
# IGPM <- get_series(189)[-c(1:65),]
# colnames(IGPM) <- c("Data","IGPM")
# IGPM$IGPM <- as.numeric(IGPM$IGPM)
# 
# IGPM.ts <- ts(IGPM$IGPM, frequency = 12, start = c(1994, 11))
# plot(IGPM.ts)
# 
# Sys.sleep(1)
# IGPM.sa <- seas(IGPM.ts)
# IGPM.final <- series(IGPM.sa, "s11")
# IGPM.final.df <- as.data.frame(IGPM.final)
# IGPM.final.df$date <- IGPM$Data
# 
# IGPM.final.df$indic <- cumprod(1+IGPM.final.df$x/100)
# IGPM.final.df$IGPM <- (((IGPM.final.df$indic/lag(IGPM.final.df$indic,3))^4)-1)*100
# 
# IGPM.final.df$ano <- year(IGPM.final.df$date)
# IGPM.final.df$mes <- month(IGPM.final.df$date)
# IGPM.quarter <- IGPM.final.df %>% filter(mes == c(3,6) | mes == c(9,12)) %>% select(date, ano, mes, IGPM)


### SAVE -------------------------
saveWorkbook(wb, "raw_base.xlsx", overwrite = TRUE)


