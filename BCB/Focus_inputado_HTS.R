## Script para imputação de missing values por kalman

## 0. Preliminar

library(tidyverse)
library(lubridate)
library(fable)
library(tsibble)

## 1. Importar dados de expectativas

exp_ipca_mensal <- rbcb::get_monthly_market_expectations(indic = 'IPCA')

exp_ipca_ano <- rbcb::get_annual_market_expectations(indic = 'IPCA') %>% 
  dplyr::filter(date == max(date), base == 0) %>% 
  dplyr::mutate(exp_ipca_ano = 1+median/100) %>% 
  dplyr::select(reference_date, exp_ipca_ano)

# Substituir isso pelas expectativas anuais do focus quando voltarem a ser divulgadas

# exp_ipca_ano <- tibble::tibble(
#   reference_date = 2022:2026,
#   exp_ipca_ano = 1+c(8.5,4.7,3.25,3.0,3.0)/100
# )

## 2. Pegar a mediana das projeções feitas no mês t para o mês t (nowcasting) e a partir daí sempre a última data

# Isso vai ser usado p/ estimar os valores futuros

exp_ipca_nowcasting <- exp_ipca_mensal %>% 
  dplyr::filter(base == 0) %>% 
  dplyr::select(date, reference_date, exp_ipca_mensal = median) %>% 
  dplyr::mutate(yearmon = zoo::as.yearmon(date)) %>% 
  dplyr::group_by(reference_date, yearmon) %>% 
  dplyr::filter(date == max(date)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(-yearmon) %>% 
  dplyr::mutate(across(contains('date'), ~ .x %>% zoo::as.yearmon())) %>% 
  dplyr::group_by(date, reference_date) %>% 
  dplyr::summarise(across(exp_ipca_mensal, ~ mean(.x))) %>% 
  dplyr::ungroup() %>%
  dplyr::filter(date == reference_date | date == max(date) & date <= reference_date) %>%
  dplyr::filter(reference_date <= '2023-06-09') %>%  
  dplyr::select(-date) %>% 
  dplyr::mutate(across(exp_ipca_mensal, ~ 1+.x/100))

# Aqui estou dedando os valores que já são observados
# Quando o Focus voltar a ser divulgado, não será necessário pois as expectativas já incorporarão os valores
# realizados anteriores

#exp_ipca_nowcasting[262:266,2] <- 1+c(0.54, 1.01, 1.62, 1.06, 0.47)/100

exp_ipca_ano_nowcasting <- exp_ipca_ano %>% 
  dplyr::mutate(across(reference_date, ~ zoo::as.yearmon(make_date(year = .x, month = 12))))

## 3. Realizar projeções h-step-ahead p/ série mensal

# Criar tibble com meses projetados e primeiro mês de cada ano. 
# Isso será útil à frente porque a fórmula para gerar o fator aplicado para ajuste das projeções
# leva em conta o número de meses que são projetados dentro do ano

fc_dates <-  tibble(
  month = seq.Date(
    from = zoo::as.Date.yearmon(max(exp_ipca_nowcasting$reference_date)) %m+% months(1),
    to   = zoo::as.Date.yearmon(max(exp_ipca_ano_nowcasting$reference_date)),
    by   = '1 month') %>% 
    zoo::as.yearmon(),
  year = year(month)
) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(first_month = first(month)) %>% 
  dplyr::ungroup()

exp_ipca_nowcasting_fc <- exp_ipca_nowcasting %>% 
  dplyr::mutate(across(reference_date, ~ yearmonth(.x))) %>% 
  as_tsibble() %>% 
  model(ETS(exp_ipca_mensal ~ season('M'))) %>% # !!!
  forecast(h = nrow(fc_dates)) %>% 
  as_tibble() 

## Reunir com os dados observados

exp_ipca_nowcasting_obs_fc <- exp_ipca_nowcasting_fc %>% 
  dplyr::select(reference_date, exp_ipca_mensal = .mean) %>% 
  dplyr::mutate(across(reference_date, ~ zoo::as.yearmon(as.Date(.x)))) %>% 
  dplyr::bind_rows(
    exp_ipca_nowcasting
  ) %>% 
  dplyr::arrange(reference_date)

## 4. Computar as projeções coerentes 

# Reter somente os anos para os quais queremos as projeções

exp_ipca_mensal_filtrado <- exp_ipca_nowcasting_obs_fc %>%
  dplyr::left_join(exp_ipca_ano_nowcasting) %>% 
  dplyr::mutate(year = year(reference_date)) %>% 
  dplyr::filter(year >= year(min(fc_dates$month))) %>% 
  dplyr::group_by(year) %>% 
  tidyr::fill(exp_ipca_ano, .direction = 'updown')  %>% 
  dplyr::left_join(
    fc_dates %>% dplyr::select(year, first_month)
  ) %>% 
  dplyr::distinct()

# Devemos multiplicar as projeções mensais por um fator k de modo que o acumulado delas no ano some o valor
# do ano fechado. Esse fator não se aplica, obviamente, ao que já foi realizado.
# A conta é: k = (acumulado_ano_fechado/acumulado_ano_dos_meses)^(1/(12-meses_projetados + 1))
# No teste, o acumulado das projeções ajustadas devem bater com o ano fechado

exp_ipca_mensal_coherent <- exp_ipca_mensal_filtrado %>% 
  dplyr::mutate(
    fc    = ifelse(reference_date >= first_month, 1, 0),
    month = month(first_month),
  ) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(acum_ano = prod(exp_ipca_mensal)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
   k = ((exp_ipca_ano/acum_ano))^(1/(12-month+1)),
   k = ifelse(fc == 1, k, 1),
   exp_ipca_mensal_coherent = k*exp_ipca_mensal
  ) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(teste = cumprod(exp_ipca_mensal_coherent)) %>% 
  dplyr::ungroup()

## 6. Exportar

df_export <- exp_ipca_mensal_coherent %>% 
  dplyr::select(reference_date, exp_ipca_adj = exp_ipca_mensal_coherent) %>% 
  dplyr::mutate(across(exp_ipca_adj, ~ (.x-1)*100))

write_csv2(df_export, 'C:/Users/Rogerio Nadalini/OneDrive/Documents/Economia/Paper Replications/Modelos Rafa/output.csv')

## 5. Plotar

df_plot <- exp_ipca_mensal_coherent %>%
  dplyr::select(reference_date, exp_ipca_mensal = exp_ipca_mensal_coherent) %>%
  dplyr::bind_rows(
    exp_ipca_nowcasting %>%
      dplyr::filter(reference_date < min(fc_dates$month))
    ) %>%
  dplyr::distinct() %>%
  dplyr::arrange(reference_date) %>% 
  dplyr::mutate(across(exp_ipca_mensal, ~ (.x-1)*100))

# Valores mensais

df_plot %>%
  dplyr::mutate(fcst = ifelse(reference_date %in% exp_ipca_nowcasting$reference_date, 1, 0)) %>%
  ggplot(aes(x = reference_date)) +
  geom_rect(aes(xmin = min(fc_dates$month), xmax = max(fc_dates$month) + 0.5,
                ymin = -Inf, ymax = Inf), alpha = 0.1, fill = 'lightgrey') +
  geom_line(aes(y = exp_ipca_mensal), lwd = 1.2, color = 'steelblue3') +
  theme_minimal() +
  labs(title = 'Focus - Expectativas para o IPCA (% a.m)',
       x = '', y = 'Expectativa para o IPCA (% a.m)') +
  zoo::scale_x_yearmon(format = '%Y', n = length(unique(year(df_plot$reference_date)))) +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +
  geom_hline(yintercept = 0, lwd = 1)

# Valores acumulados em 12 meses

df_plot12m <- df_plot %>%
  dplyr::left_join(
    exp_ipca_ano_nowcasting
  ) %>%
  dplyr::mutate(exp_ipca12m = RcppRoll::roll_prodr((1+exp_ipca_mensal/100), 12),
                exp_ipca12m  = (exp_ipca12m-1)*100)


df_plot12m %>%
  dplyr::mutate(fcst = ifelse(reference_date %in% exp_ipca_nowcasting$reference_date, 1, 0)) %>%
  ggplot(aes(x = reference_date)) +
  geom_rect(aes(xmin = min(fc_dates$month), xmax = max(fc_dates$month),
                ymin = -Inf, ymax = Inf), alpha = 0.1, fill = 'lightgrey') +
  geom_line(aes(y = exp_ipca12m), lwd = 1.2, color = 'steelblue3') +
  geom_point(aes(y = ((exp_ipca_ano-1)*100)), size = 3, color = 'red') +
  theme_minimal() +
  labs(title = 'Focus - Expectativas para o IPCA (% 12m)',
       x = '', y = 'Expectativa para o IPCA (% 12m)') +
  zoo::scale_x_yearmon(format = '%Y', n = length(unique(year(df_plot$reference_date)))) +
  scale_y_continuous(labels = function(x) paste0(x, '%'), breaks = seq(0,15,3)) +
  geom_hline(yintercept = 0, lwd = 1)

