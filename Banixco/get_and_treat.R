library(tidyverse)
library(openxlsx)

cutoff.date <- '2000-01-01'
outDir <- paste0("Unzip-",Sys.Date())
dir.create(outDir)

# download microdata link: https://www.banxico.org.mx/publicaciones-y-prensa/encuestas-sobre-las-expectativas-de-los-especialis/encuestas-expectativas-del-se.html
# unzip and add to folder
zipF <- "YOR FOLDER"
unzip(zipF, exdir = outDir)

# import
data.old <- read.csv(file = paste0(getwd(),"/",outDir,"/Microdatos_1999_01.csv"))
data.new <- read.csv(file = paste0(getwd(),"/",outDir,"/Microdatos_2020_01.csv"))
data.merge <- rbind(data.old, data.new)

# generate dictionary
dictionary <- data.new %>% 
  select(NombreAbsolutoLargo, NombreAbsolutoCorto, NombreRelativoLargo, NombreRelativoCorto) %>% 
  distinct(NombreAbsolutoLargo, .keep_all = T) %>% 
  arrange(NombreAbsolutoLargo)


# ***********************************************
# SELECT VARIABLES ----
# ***********************************************

# inflation
inf.rel.vars  <- c('infgen12mesesmestmas1', # Inflaciongeneral_12m_próximos_mestmas1
                   'infgen12meses', # Inflaciongeneral_12m_próximos
                   'infgent14', # Inflacion general para los próximos uno a cuatro años
                   'infgent58', # Inflacion general para los próximos uno a cuatro años
                   'infsub12mesesmestmas1', # Inflacionsubyacente_12m_próximos_mestmas1
                   'infsub12meses', # Inflacionsubyacente_12m_próximos
                   'infsub14', # Inflacion subjacente para los próximos uno a cuatro años
                   'infsub58', # Inflacion subjacente para los próximos uno a cuatro años
                   'infgent', # Inflacion general al cierre del año en curso (año t)
                   'infgentmas1', 
                   'infgentmas2', 
                   'infgentmas3', # Inflacion general al cierre dentro de tres años (año t+3)
                   'infsubt', 
                   'infsubtmas1', 
                   'infsubtmas2', 
                   'infsubtmas3'
                   )


# inflacion corto plazo relativa
inf.corto <- c('infgenmest',     # Inflacion general para el mes en curso (mes t)
               'infgenmestmas1', # Inflacion general para el siguiente mes (mes t+1)
               'infgenmestmas2',
               'infgenmestmas3',
               'infgenmestmas4',
               'infgenmestmas5',
               'infgenmestmas6',
               'infgenmestmas7',
               'infgenmestmas8',
               'infgenmestmas9',
               'infgenmestmas10',
               'infgenmestmas11',
               'infgenmestmas12',
               'infsubmest',     # Inflacion subyacente para el mes en curso (mes t)
               'infsubmestmas1', # Inflacion subyacente para el siguiente mes (mes t+1)
               'infsubmestmas2',
               'infsubmestmas3',
               'infsubmestmas4',
               'infsubmestmas5',
               'infsubmestmas6',
               'infsubmestmas7',
               'infsubmestmas8',
               'infsubmestmas9',
               'infsubmestmas10',
               'infsubmestmas11',
               'infsubmestmas12')

# Tasa de fondeo interbacaria, bonos y cetes, relativo
tasas <- c('fondeot',     # Nivel de la tasa de fondeo interbancaria al cierre del trimestre actual (trimestre t)
           'fondeotmas1', # Nivel de la tasa de fondeo interbancaria al cierre del siguiente trimestre (trimestre t+1)
           'fondeotmas2', 
           'fondeotmas3', 
           'fondeotmas4', 
           'fondeotmas5', 
           'fondeotmas6', 
           'fondeotmas7', 
           'fondeotmas8', 
           'bonost',
           'bonost1',
           'bonost2',
           'bonost3',
           'cetest',
           'cetest1',
           'cetest2',
           'cetest3'
           )

# Desocupacion y PIB, relativos
atividad <- c('desocupcierret', # desocupacion
             'desocupcierretmas1',
             'desocupcierretmas2',
             'desocupromt',
             'desocupromtmas1',
             'desocupromtmas2',
             'varpibtmenos1', # PIB anual
             'varpibt',
             'varpibtmas1',
             'varpibtmas2',
             'varpibtmas3',
             'pibeutmenos1', # US
             'pibeut',
             'pibeutmas1',
             'pibeutmas2',
             'varpibdesesttrimtmenos1', # Variacion desestacionalizada del PIB en el trimestre anterior
             'varpibdesesttrimt',
             'varpibdesesttrimtmas1',
             'varpibdesesttrimtmas2',
             'varpibdesesttrimtmas3',
             'varpibdesesttrimtmas4',
             'varpibdesesttrimtmas5',
             'varpibdesesttrimtmas6',
             'varpibdesesttrimtmas7',
             'varpibdesesttrimtmas8',
             'varpibtrimtmenos1', #Variación porcentual anual del PIB, trimestre anterior al correspondiente del levantamiento de la Encuesta (trimestre t-1)
             'varpibtrimt',
             'varpibtrimtmas1',
             'varpibtrimtmas2',
             'varpibtrimtmas3',
             'varpibtrimtmas4',
             'varpibtrimtmas5',
             'varpibtrimtmas6',
             'varpibtrimtmas7',
             'varpibtrimtmas8')


# Tipo de cambio, relativo
cambio <- c('tccierret',
            'tcmestmas1',
            'tcmestmas2',
            'tcmestmas3'
            )


# ***********************************************
# FILTER AND COMPUTE SUMMARY STATS ----
# ***********************************************

df.agg <- data.merge %>% 
  filter(FechaEncuesta >= as.Date(cutoff.date)) %>% 
  filter(NombreRelativoCorto %in% inf.rel.vars | 
           NombreRelativoCorto %in% inf.corto | 
           NombreRelativoCorto %in% tasas | 
           NombreRelativoCorto %in% cambio | 
           NombreRelativoCorto %in% atividad) %>%
  group_by(FechaEncuesta, NombreAbsolutoCorto, NombreRelativoCorto, NombreAbsolutoLargo, NombreRelativoLargo) %>% 
  summarise(mean = mean(Dato),
            median = median(Dato),
            sd = sd(Dato),
            max = max(Dato),
            min = min(Dato),
            inter.25 = quantile(Dato, probs = 0.25),
            inter.75 = quantile(Dato, probs = 0.75),
            count = n())


# check for duplicates
df.agg %>% duplicated() %>% sum()


# export to excel





