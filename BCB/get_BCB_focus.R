
library(tidyverse)
library(rbcb)
library(gridExtra)
library(grid)
library(openxlsx)

# Set path for results
results_path = "YOUR PATH"

# Set ggplot theme
theme_set(theme_bw())

# ***********************************************
# Import data and save previous database ----
# ***********************************************

IPCA <- rbcb::get_market_expectations(type = c("annual"), indic = 'IPCA', start_date = "2023-01-01")

IPCA <- rbcb::get_market_expectations(type = c("annual"), indic = 'IPCA')


# Last data point
last.date.LONG <- format(IPCA$Data %>% unique() %>% max(), "%A, %B %d")
last.date <- IPCA$Data %>% unique() %>% max()


# save current, file previous
file.rename('raw_annual_IPCA.xlsx', paste0('raw_annual_IPCA - ', Sys.Date(),'.xlsx'))
file.copy(paste0('raw_annual_IPCA - ', Sys.Date(),'.xlsx'), 'Vintage/', overwrite = T)
unlink(paste0('raw_annual_IPCA - ', Sys.Date(),'.xlsx'))

wb <- createWorkbook("raw_annual_IPCA")
addWorksheet(wb, "IPCA_Annual")
writeData(wb, sheet = "IPCA_Annual", IPCA)
saveWorkbook(wb, "raw_annual_IPCA.xlsx", overwrite = TRUE)


# Alternative: import data manualy


# ***********************************************
# Prepare data to plot ----
# ***********************************************

# Format dates and years
IPCA <- IPCA %>% mutate(DataReferencia = as.numeric(DataReferencia)) %>% 
                        dplyr::arrange(DataReferencia, Data)

# Create dataframe with inflation target and merge
Target <- data.frame(DataReferencia = c(2020,2021,2022,2023,2024,2025,2026,2027),
                   Target = c(4.0,3.75,3.5, 3.25, 3, 3, 3, 3))


IPCA <- IPCA %>% left_join(Target)

# set colort and plot
colors <- c("Median" = "red", "Mean" = "#1E6AA4", "Target" = "black")
linetype = c("Median" = "solid", "Mean" = "solid", "Target" = "dashed")

# ***********************************************
# Plots ----
# ***********************************************

plot1 <- IPCA %>% 
  dplyr::filter(Data >= Sys.Date() - 360,
                DataReferencia > 2022, 
                baseCalculo == 0) %>% 
  arrange(as.Date(Data)) %>% 
  ggplot() + 
  geom_ribbon(aes(x = Data, ymin = Media - DesvioPadrao, ymax = Media + DesvioPadrao), fill = "grey50", alpha = 0.2) +
  geom_line(aes(x = as.Date(Data), y = Mediana, color = 'Median', linetype = 'Median')) +
  geom_line(aes(x = as.Date(Data), y = Media, color = 'Mean', linetype = 'Mean')) +
#  geom_line(aes(x = as.Date(Data), y = Minimo), color  = 'red', linetype = 3) +
#  geom_line(aes(x = as.Date(Data), y = Maximo), color  = 'red', linetype = 3) +
  geom_line(aes(x = as.Date(Data), y = Target, color = 'Target', linetype = 'Target')) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2, colour = "grey80")) +
        #panel.grid.major = element_blank()) + 
  scale_y_continuous(breaks=seq(2,8,0.5), position = "right") +
  scale_x_date(expand = c(0,2), breaks=seq(2,8,0.5),
               date_breaks = "1 month", date_labels = "%b-%y",
               guide = guide_axis(angle = 45)) +
  scale_fill_discrete(name=NULL) +
  labs(x = "",
       y = "",
       color = "",
       linetype = "",
       title = 'Brazil: Annual Inflation Expectations (%)',
       subtitle = paste0('Last data point: ', format(IPCA$Data %>% unique() %>% max(), format = "%b %d, %Y"))) +
  scale_color_manual(breaks = c("Median", "Mean", "Target"),
                     values = colors) +
  scale_linetype_manual(breaks = c("Median", "Mean", "Target"),
                        values = linetype) + 
  theme(legend.position="bottom") +
  facet_wrap( ~ DataReferencia, scales = 'fixed', ncol = 2) 

filename = paste(results_path,"IPCA_anual.pdf", sep = "")
ggsave(filename, plot = plot1, device = "pdf", height = 6 , width = 10)

  
plot2 <- IPCA %>% 
  dplyr::filter(Data >= Sys.Date() - 120,
                DataReferencia > 2022, 
                baseCalculo == 0) %>% 
  arrange(as.Date(Data)) %>% 
  ggplot() + 
  geom_ribbon(aes(x = Data, ymin = Media - DesvioPadrao, ymax = Media + DesvioPadrao), fill = "grey50", alpha = 0.2) +
  geom_line(aes(x = as.Date(Data), y = Mediana, color = 'Median', linetype = 'Median')) +
  geom_line(aes(x = as.Date(Data), y = Media, color = 'Mean', linetype = 'Mean')) +
  #  geom_line(aes(x = as.Date(Data), y = Minimo), color  = 'red', linetype = 3, size = 0.8) +
  #  geom_line(aes(x = as.Date(Data), y = Maximo), color  = 'red', linetype = 3, size = 0.8) +
  geom_line(aes(x = as.Date(Data), y = Target, color = 'Target', linetype = 'Target')) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2, colour = "grey80")) +
  #panel.grid.major = element_blank()) + 
  scale_y_continuous(breaks=seq(2,8,0.25), position = "right") +
  scale_x_date(expand = c(0,2), breaks=seq(2,8,0.5),
               date_breaks = "1 month", date_labels = "%b-%y",
               guide = guide_axis(angle = 45)) +
  scale_fill_discrete(name=NULL) +
  labs(x = "",
       y = "",
       color = "",
       linetype = "",
       title = 'ZOOM IN - Brazil: Annual Inflation Expectations (%)',
       subtitle = paste0('Last data point: ', format(IPCA$Data %>% unique() %>% max(), format = "%b %d, %Y"))) +
  scale_color_manual(breaks = c("Median", "Mean", "Target"),
                     values = colors) +
  scale_linetype_manual(breaks = c("Median", "Mean", "Target"),
                        values = linetype) + 
  theme(legend.position="bottom") +
  facet_wrap( ~ DataReferencia, scales = 'free_y', ncol = 2) 

filename = paste(results_path,"IPCA_anual_ZOOM.pdf", sep = "")
ggsave(filename, plot = plot2, device = "pdf", height = 6 , width = 10)

# |- Updated last 5 days ----

plot3 <- IPCA %>% 
  dplyr::filter(Data >= Sys.Date() - 360,
                DataReferencia > 2022, 
                baseCalculo == 1) %>% 
  arrange(as.Date(Data)) %>% 
  ggplot() + 
  geom_ribbon(aes(x = Data, ymin = Media - DesvioPadrao, ymax = Media + DesvioPadrao), fill = "grey50", alpha = 0.2) +
  geom_line(aes(x = as.Date(Data), y = Mediana, color = 'Median', linetype = 'Median')) +
  geom_line(aes(x = as.Date(Data), y = Media, color = 'Mean', linetype = 'Mean')) +
  #  geom_line(aes(x = as.Date(Data), y = Minimo), color  = 'red', linetype = 3) +
  #  geom_line(aes(x = as.Date(Data), y = Maximo), color  = 'red', linetype = 3) +
  geom_line(aes(x = as.Date(Data), y = Target, color = 'Target', linetype = 'Target')) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2, colour = "grey80")) +
  #panel.grid.major = element_blank()) + 
  scale_y_continuous(breaks=seq(2,8,0.5), position = "right") +
  scale_x_date(expand = c(0,2), breaks=seq(2,8,0.5),
               date_breaks = "1 month", date_labels = "%b-%y",
               guide = guide_axis(angle = 45)) +
  scale_fill_discrete(name=NULL) +
  labs(x = "",
       y = "",
       color = "",
       linetype = "",
       title = 'Brazil: Annual Inflation Expectations (%)',
       subtitle = paste0('Last data point: ', format(IPCA$Data %>% unique() %>% max(), format = "%b %d, %Y"))) +
  scale_color_manual(breaks = c("Median", "Mean", "Target"),
                     values = colors) +
  scale_linetype_manual(breaks = c("Median", "Mean", "Target"),
                        values = linetype) + 
  theme(legend.position="bottom") +
  facet_wrap( ~ DataReferencia, scales = 'fixed', ncol = 2) 

filename = paste(results_path,"IPCA_anual_5d.pdf", sep = "")
ggsave(filename, plot = plot3, device = "pdf", height = 6 , width = 10)


plot4 <- IPCA %>% 
  dplyr::filter(Data >= Sys.Date() - 120,
                DataReferencia > 2022, 
                baseCalculo == 1) %>% 
  arrange(as.Date(Data)) %>% 
  ggplot() + 
  geom_ribbon(aes(x = Data, ymin = Media - DesvioPadrao, ymax = Media + DesvioPadrao), fill = "grey50", alpha = 0.2) +
  geom_line(aes(x = as.Date(Data), y = Mediana, color = 'Median', linetype = 'Median')) +
  geom_line(aes(x = as.Date(Data), y = Media, color = 'Mean', linetype = 'Mean')) +
  #  geom_line(aes(x = as.Date(Data), y = Minimo), color  = 'red', linetype = 3, size = 0.8) +
  #  geom_line(aes(x = as.Date(Data), y = Maximo), color  = 'red', linetype = 3, size = 0.8) +
  geom_line(aes(x = as.Date(Data), y = Target, color = 'Target', linetype = 'Target')) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.2, colour = "grey80")) +
  #panel.grid.major = element_blank()) + 
  scale_y_continuous(breaks=seq(2,8,0.25), position = "right") +
  scale_x_date(expand = c(0,2), breaks=seq(2,8,0.5),
               date_breaks = "1 month", date_labels = "%b-%y",
               guide = guide_axis(angle = 45)) +
  scale_fill_discrete(name=NULL) +
  labs(x = "",
       y = "",
       color = "",
       linetype = "",
       title = 'ZOOM IN - Brazil: Annual Inflation Expectations (%) - UPDATED LAST 5 DAYS',
       subtitle = paste0('Last data point: ', format(IPCA$Data %>% unique() %>% max(), format = "%b %d, %Y"))) +
  scale_color_manual(breaks = c("Median", "Mean", "Target"),
                     values = colors) +
  scale_linetype_manual(breaks = c("Median", "Mean", "Target"),
                        values = linetype) + 
  theme(legend.position="bottom") +
  facet_wrap( ~ DataReferencia, scales = 'free_y', ncol = 2) 

filename = paste(results_path,"IPCA_anual_5d_ZOOM.pdf", sep = "")
ggsave(filename, plot = plot4, device = "pdf", height = 6 , width = 10)


# ***********************************************
# Tables ----
# ***********************************************

# https://stackoverflow.com/questions/26590985/reduce-space-margin-between-2-tables-in-gridextra-with-r
# https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html#faster-tables-an-alternative-grid-function

# |- Median ----
tab.mediana <- IPCA %>% 
  select(Data, Mediana, DataReferencia, baseCalculo) %>% 
  dplyr::filter(Data >= Sys.Date() - 120,
                DataReferencia > 2022, 
                baseCalculo == 0) %>% 
  mutate(Mediana = format(round(Mediana, 2), nsmall = 2)) %>% 
  select(!baseCalculo) %>% 
  filter(Data >= last.date - 7) %>% 
  pivot_wider(names_from = 'DataReferencia', values_from = 'Mediana')

t1 <- ttheme_default(core=list(
  fg_params = list(fontface = c(rep("plain", 5), "bold")),
  bg_params = list(fill = c("#6BAED6", "grey95", "grey90", "grey95", "grey90", "#6BAED6"),
                   alpha = rep(c(0.5, 0.5), each = 2))
))

tab <- tableGrob(tab.mediana, rows = NULL, theme = t1)

tab$widths <- unit(rep(1/ncol(tab)-0.02, ncol(tab)), "npc")

plot.new()
grid.newpage()
grid.draw(tab)
grid.text("Median IPCA Expectations", x = 0.5, y = 0.85, gp = gpar(fontsize = 16))


# |- Mean ----
tab.mean <- IPCA %>% 
  select(Data, Media, DataReferencia, baseCalculo) %>% 
  dplyr::filter(Data >= Sys.Date() - 120,
                DataReferencia > 2022, 
                baseCalculo == 0) %>% 
  mutate(Media = format(round(Media, 2), nsmall = 2)) %>% 
  select(!baseCalculo) %>% 
  filter(Data >= last.date - 7) %>% 
  pivot_wider(names_from = 'DataReferencia', values_from = 'Media')

tab <- tableGrob(tab.mean, rows = NULL, theme = t1)

tab$widths <- unit(rep(1/ncol(tab)-0.02, ncol(tab)), "npc")

plot.new()
grid.newpage()
grid.draw(tab)
grid.text("Mean IPCA Expectations", x = 0.5, y = 0.85, gp = gpar(fontsize = 16))

# |- Median - Last 5 days ----
tab.mediana.5d <- IPCA %>% 
  select(Data, Mediana, DataReferencia, baseCalculo) %>% 
  dplyr::filter(Data >= Sys.Date() - 120,
                DataReferencia > 2022, 
                baseCalculo == 1) %>% 
  mutate(Mediana = format(round(Mediana, 2), nsmall = 2)) %>% 
  select(!baseCalculo) %>% 
  filter(Data >= last.date - 7) %>% 
  pivot_wider(names_from = 'DataReferencia', values_from = 'Mediana')

t2 <- ttheme_default(core=list(
  fg_params = list(fontface = c(rep("plain", 5), "bold")),
  bg_params = list(fill = c("#FF8282", "grey95", "grey90", "grey95", "grey90", "#FF8282"),
                   alpha = rep(c(0.5, 0.5), each = 2))
))

tab <- tableGrob(tab.mediana.5d, rows = NULL, theme = t2)

tab$widths <- unit(rep(1/ncol(tab)-0.02, ncol(tab)), "npc")

plot.new()
grid.newpage()
grid.draw(tab)
grid.text("Median IPCA Expectations - Updated last 5 days", x = 0.5, y = 0.85, gp = gpar(fontsize = 16))


# |- Mean - Last 5 days ----
tab.mean.5d <- IPCA %>% 
  select(Data, Media, DataReferencia, baseCalculo) %>% 
  dplyr::filter(Data >= Sys.Date() - 120,
                DataReferencia > 2022, 
                baseCalculo == 1) %>% 
  mutate(Media = format(round(Media, 2), nsmall = 2)) %>% 
  select(!baseCalculo) %>% 
  filter(Data >= last.date - 7) %>% 
  pivot_wider(names_from = 'DataReferencia', values_from = 'Media')

tab <- tableGrob(tab.mean.5d, rows = NULL, theme = t2)

tab$widths <- unit(rep(1/ncol(tab)-0.02, ncol(tab)), "npc")

plot.new()
grid.newpage()
grid.draw(tab)
grid.text("Mean IPCA Expectations - Updated last 5 days", x = 0.5, y = 0.85, gp = gpar(fontsize = 16))

