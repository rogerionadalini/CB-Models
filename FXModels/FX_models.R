
dif.fn.1 <- function(x) {100*(log(x) - dplyr::lag(log(x), n = 1L))}

dif.fn.2 <- function(x) {x - dplyr::lag(x, n = 1L)}

estimate_FX_models <- function(data.new, dependent, independent, model.string, model, nobs, nwindows, outofsample.res, outofsample.end) {

  # select regressors
  col.select <- c(which(colnames(data.new)=='Date'), which(colnames(data.new)==dependent), which(colnames(data.new) %in% independent))
  data.filter <- data.new[(nrow(data.new)-nobs-nwindows):nrow(data.new),col.select]
  data.filter <- data.filter %>% mutate(Date = as.Date(Date, origin))
  
  # |- Rolling regression ----
  
  # create matrices to store objecst of interest
  
  coefs.matrix <- matrix(nrow = nrow(data.filter) - nobs, ncol = ncol(data.filter) - 1)
  std.matrix <- coefs.matrix
  ur.matrix <- matrix(ncol = 4, nrow = nrow(data.filter) - nobs)
  adrsq.matrix <- matrix(ncol = 1, nrow = nrow(data.filter) - nobs)
  fcst.matrix <- matrix(ncol = 1, nrow = nrow(data.filter) - nobs)
  models <- list()
  
  #for (i in nobs:nrow(data.filter)) {
  for (i in (nrow(data.filter)-nwindows):nrow(data.filter)) {
      
    start <- i - nobs
    data.aux <- data.filter[start:i,]
    data.aux.fcst <- data.filter[start:i + outofsample.res,]
    model.aux <- lm(model, data.aux)
    
    # test residuals for unit root
    df <- ur.df(model.aux$residuals, type = c("none"), lags = 1)
    ers <- ur.ers(model.aux$residuals, type = c("DF-GLS"), model = c("constant"), lag.max = 1)
    kpss <- ur.kpss(model.aux$residuals, type = c("mu"), use.lag = 1)
    pp <- ur.pp(model.aux$residuals, type = 'Z-tau', model = 'constant', use.lag = 1)
    
    ur.matrix[i - nobs,1] <- df@teststat < df@cval[2] # H0: Has unit root
    ur.matrix[i - nobs,2] <- ers@teststat < ers@cval[2] # H0: Has unit root
    ur.matrix[i - nobs,3] <- kpss@teststat > kpss@cval[2] # H0: Has unit root
    ur.matrix[i - nobs,4] <- pp@teststat < pp@cval[2] # H0: H0: Is stationary
    
    # store objects of interest
    coefs.matrix[i - nobs,] <- model.aux$coefficients
    aux <- summary(model.aux)
    adrsq.matrix[i - nobs,1] <- aux$adj.r.squared
    coefs.std <- coef(aux)
    std.matrix[i - nobs,] <- coefs.std[,2]
    
    # store out-of-sample residuals
    if (i <= nrow(data.filter) - outofsample.res) {
      
      fcst.aux <- predict(model.aux, data.aux.fcst)
      fcst.matrix[i - nobs + outofsample.res, 1] <- tail(fcst.aux, 1)
      
    }
    
  }
  
  # Fixed window estimation and out-of-sample fit
  cut <- which(data.filter$Date == as.Date(outofsample.end))
  data.aux.2 <- data.filter[(cut-nobs):cut,] # SAMPLE FOR ESTIMATION ONLY
  model.aux.2 <- lm(model, data.aux.2)
  modelplot(model.aux.2)
  model.aux.2$fitted.values
  fitted.outofsample <- predict(model.aux.2, data.filter)
  
  #fitted.outofsample <- as.matrix(cbind(1,data.filter[,3:ncol(data.filter)])) %*% as.matrix(coef(model.aux.2))
  #predict(linear_model, newdata = variable_speed, interval = 'confidence')
  
  # |- Plots ----

  
  # |-- Plot recursive out-of-sample esiduals ----
  fcst.df <- data.frame(fcst = fcst.matrix)
  data2plot <- data.frame(Date = tail(data.filter$Date, nwindows + 1), Fair_Value = fcst.df$fcst, Actual = tail(data.filter[,2], nwindows + 1))
  data2plot <- na.omit(data2plot)
  data2export <- data2plot
  
  last.fv <-  data2plot %>% top_n(1, Date) %>% pull(Fair_Value) %>% round(2)
  last.actual <- data2plot %>% top_n(1, Date) %>% pull(Actual) %>% round(2)
  colors <- c("Fair Value" = "black", "Actual" = "red")
  
  p1 <- 
    data2plot %>% 
    ggplot() +
    geom_line(aes(x = as.Date(Date), y = Actual, color = "Actual")) + 
    geom_line(aes(x = as.Date(Date), y = Fair_Value, color = "Fair Value")) +
    scale_x_date(expand = c(0, 5), date_labels = "%b-%Y") +
    theme_bw() + theme(legend.title= element_blank()) +
    labs(x = "", y = "Actual and Fair Value", 
         title = 'Rolling Estimation: Actual vs Fair Value',
         subtitle = paste0("Equation: ", model.string),
         caption = paste0('Out-of-sample distance: ', outofsample.res, '. Estimation window: ', nobs,' business days. Last obs: ', format(last.obs, "%d-%b-%Y")),
         color = "Legend") +
    theme(panel.grid.minor = element_blank(),
          legend.position = c(0.2, 0.15)) +
    scale_color_manual(values = colors)
  
  # Plot residuals (dev from fair value)
  data2plot <- data2plot %>% mutate(value = 100*(log(Actual) - log(Fair_Value)))
  stdev <- sd(data2plot$value)
  data_ends <- data2plot %>%  top_n(1, Date) %>% pull(value) %>% round(1)
  
  p2 <- 
    data2plot %>% 
    ggplot(aes(x = as.Date(Date), y = value)) +
    geom_ribbon(aes(ymin = -stdev*1.5, ymax = stdev*1.5), fill = "blue", alpha = 0.1) +
    geom_hline(aes(yintercept = 0), color = 'black', linewidth = 1) +
    geom_hline(aes(yintercept = -stdev*1.5), color = 'blue', linetype = 2) +
    geom_hline(aes(yintercept = stdev*1.5), color = 'blue', linetype = 2) +
    geom_line() +
    labs(x = "", y = "% Deviation from fair value", 
         title = '% Deviation -/+ 1.5*sd',
         subtitle = paste0("Fair Value: ", last.fv, ". Actual: ", last.actual, ". Deviation in %: ", data_ends)) +
    scale_x_date(expand = c(0, 2), date_labels = "%b-%Y") +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = data_ends)) + 
    theme_bw() +
    theme(panel.grid = element_blank())
  
  plot.roll.res <- p1 / p2
  plot.roll.res.only <- p2
    
  # |-- Plot fitted vs actual ----
  
  # Fixed end date, out-of-sample
  data2plot <- data.frame(Date = data.filter$Date, Fair_Value = fitted.outofsample, Actual = data.filter[,2])
  data2plot <- data2plot[(cut-nobs):nrow(data2plot),] # fiter for estimation sample and beyond
  
  last.fv <-  data2plot %>% top_n(1, Date) %>% pull(Fair_Value) %>% round(2)
  last.actual <- data2plot %>% top_n(1, Date) %>% pull(Actual) %>% round(2)
  colors <- c("Fair Value" = "black", "Actual" = "red")
  rects <- data.frame(xstart = as.Date(outofsample.end), 
                      xend = as.Date(last.obs))
  
  p1 <- 
    data2plot %>% 
    ggplot() +
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), alpha = 0.2) +
    geom_line(aes(x = as.Date(Date), y = Actual, color = "Actual")) + 
    geom_line(aes(x = as.Date(Date), y = Fair_Value, color = "Fair Value")) +
    scale_x_date(expand = c(0, 5), date_labels = "%b-%Y") +
    theme_bw() + theme(legend.title= element_blank()) +
    labs(x = "", y = "Actual and Fair Value", 
         title = 'Fixed sample: Actual vs Fair Value (Out-of-sample on shaded area)',
         subtitle = paste0("Equation: ", model.string),
         caption = paste0('Estimation window: ', nobs,' business days. Last obs: ', format(last.obs, "%d-%b-%Y")),
         color = "Legend") +
    theme(panel.grid.minor = element_blank(),
          legend.position = c(0.20, 0.80)) +
    scale_color_manual(values = colors)
  
  # residuals
  data2plot <- data2plot %>% mutate(value = 100*(log(Actual) - log(Fair_Value)))
  stdev <- sd(data2plot$value)
  data_ends <- data2plot %>%  top_n(1, Date) %>% pull(value) %>% round(1)
  
  # save ect for latter
  z <- data2plot$value/100
  
  p2 <- 
    data2plot %>% 
    ggplot(aes(x = as.Date(Date), y = value)) +
    geom_ribbon(aes(ymin = -stdev*1.5, ymax = stdev*1.5), fill = "blue", alpha = 0.1) +
    geom_vline(aes(xintercept = as.Date(outofsample.end)), color = 'red', linewidth = 0.8) +
    geom_hline(aes(yintercept = 0), color = 'black', linewidth = 1) +
    geom_hline(aes(yintercept = -stdev*1.5), color = 'blue', linetype = 2) +
    geom_hline(aes(yintercept = stdev*1.5), color = 'blue', linetype = 2) +
    geom_line() +
    labs(x = "", y = "% Deviation from fair value", 
         title = '% Deviation -/+ 1.5*sd',
         subtitle = paste0("Fair Value: ", last.fv, ". Actual: ", last.actual, ". Deviation in %: ", data_ends)) +
    scale_x_date(expand = c(0, 2), date_labels = "%b-%Y") +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = data_ends)) + 
    theme_bw() +
    theme(panel.grid = element_blank())
  
  plot.fixed <- p1 / p2
  
  # Fixed sample, last estimation
  data2plot <- data.frame(Date = data$Date[(nrow(data)-nobs):nrow(data)], Actual = model.aux$fitted.values + model.aux$residuals, Fair_Value = model.aux$fitted.values)
  last.fv <-  data2plot %>% top_n(1, Date) %>% pull(Fair_Value) %>% round(2)
  last.actual <- data2plot %>% top_n(1, Date) %>% pull(Actual) %>% round(2)
  colors <- c("Fair Value" = "black", "Actual" = "red")
  
  p1 <- 
  data2plot %>% 
    ggplot() +
    geom_line(aes(x = as.Date(Date), y = Actual, color = "Actual")) + 
    geom_line(aes(x = as.Date(Date), y = Fair_Value, color = "Fair Value")) +
    scale_x_date(expand = c(0, 5), date_labels = "%b-%Y") +
    theme_bw() + theme(legend.title= element_blank()) +
    labs(x = "", y = "Actual and Fair Value", 
         title = 'Fixed sample: Actual vs Fair Value',
         subtitle = paste0("Equation: ", model.string),
         caption = paste0('Estimation window: ', nobs,' business days. Last obs: ', format(last.obs, "%d-%b-%Y")),
         color = "Legend") +
    theme(panel.grid.minor = element_blank(),
          legend.position = c(0.2, 0.15)) +
    scale_color_manual(values = colors)
  
  # Plot residuals (dev from fair value)
  obs <- model.aux$fitted.values + model.aux$residuals
  data2plot <- data.frame(Date = data$Date[(nrow(data)-nobs):nrow(data)], value = 100*(log(obs) - log(model.aux$fitted.values)))
  stdev <- sd(data2plot$value)
  data_ends <- data2plot %>%  top_n(1, Date) %>% pull(value) %>% round(1)
  
  p2 <- 
  data2plot %>% 
    ggplot(aes(x = as.Date(Date), y = value)) +
    geom_ribbon(aes(ymin = -stdev*1.5, ymax = stdev*1.5), fill = "blue", alpha = 0.1) +
    geom_hline(aes(yintercept = 0), color = 'black', linewidth = 1) +
    geom_hline(aes(yintercept = -stdev*1.5), color = 'blue', linetype = 2) +
    geom_hline(aes(yintercept = stdev*1.5), color = 'blue', linetype = 2) +
    geom_line() +
    labs(x = "", y = "% Deviation from fair value", 
         title = '% Deviation -/+ 1.5*sd',
         subtitle = paste0("Fair Value: ", last.fv, ". Actual: ", last.actual, ". Deviation in %: ", data_ends)) +
    scale_x_date(expand = c(0, 2), date_labels = "%b-%Y") +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = data_ends)) + 
    theme_bw() +
    theme(panel.grid = element_blank())
  
  plot.last <- p1 / p2
  
  # |-- PAC and PACF of last model ----
  acf <- ggAcf(model.aux$residuals, lag.max = 100)
  pacf <- ggPacf(model.aux$residuals, lag.max = 100)
  acf+pacf
  
  
  # |-- Model Stability Test ----
  cusum.ols <- efp(model, type = "OLS-CUSUM", data = as.ts(data.aux))
  cusum.rec <- efp(model, type = "Rec-CUSUM", data = as.ts(data.aux))
  mosum.ols <- efp(model, type = "Rec-MOSUM", data = as.ts(data.aux))
  mosum.rec <- efp(model, type = "OLS-MOSUM", data = as.ts(data.aux))
  par(mfrow=c(2,2))
  plot(cusum.ols)
  plot(cusum.rec)
  plot(mosum.ols)
  plot(mosum.rec)
  
  stab.tests <- recordPlot()
  
  # |-- Plot R-squared, coefs and UR Tests ----
  coefs.df <- as.data.frame(coefs.matrix)
  colnames(coefs.df) <- c('Constant', colnames(data.aux[3:ncol(data.aux)]))
  coefs.df$Date <- tail(data$Date, nwindows + 1)
  coefs.long <- coefs.df %>% pivot_longer(values_to = 'Values', !Date, names_to = 'Variables')
  
  std.df <- as.data.frame(std.matrix)
  colnames(std.df) <- c('Constant', colnames(data.aux[3:ncol(data.aux)]))
  std.df$Date <- tail(data$Date, nwindows + 1)
  std.long <- std.df %>% pivot_longer(values_to = 'SD', !Date, names_to = 'Variables')
  SD <- std.long$SD
  
  data2plot <- cbind(coefs.long, SD)
  
  coef.plot <- data2plot %>% 
    ggplot(aes(x = as.Date(Date), y = Values)) +
    geom_line() +
    geom_hline(aes(yintercept = 0), color = 'black', linewidth = 1) +
    geom_ribbon(aes(ymin = Values -2*SD, ymax = Values +SD*2), fill = "blue", alpha = 0.4) +
    labs(title = 'Rolling Coefficients with 2-sd bands',
         x = '', y = '') + 
    facet_wrap(~Variables, scales = 'free_y') +
    theme_bw()
    
  
  # Plot R-squared
  rsq.df <- as.data.frame(adrsq.matrix)
  colnames(rsq.df) <- 'Adjusted.Rsquared'
  rsq.df$Date <- tail(data$Date, nwindows + 1)
  
  rsq.plot <- rsq.df %>% 
    ggplot(aes(x = as.Date(Date), y = Adjusted.Rsquared)) +
    geom_line(linewidth = 1) + 
    theme_bw() +
    labs(title = 'Adjusted R-squared - Rolling Window Estimation',
         x = '', y = '')
  
  # unit root tests
  ur.df <- as.data.frame(ur.matrix)
  colnames(ur.df) <- c('ADF','DF-GLS','KPSS','PP')
  ur.df$Date <- tail(data$Date, nwindows + 1)
  data2plot <- ur.df %>% pivot_longer(values_to = 'Values', !Date, names_to = 'Test')
  
  ur.plot <- data2plot %>% 
    ggplot(aes(x = as.Date(Date), y = Values)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~Test) +
    theme_bw() +
    labs(title = 'Unit Root Tests on Residuals (95% confidence)',
         subtitle = 'FALSE: Has a unit root. TRUE: Stationary',
         x ='', y = '') +
    theme(panel.grid.minor = element_blank())
  
  
  # ***********************************************
  # Fair "variation" ----
  # ***********************************************
  
  data.aux.dif <- data.aux.2 %>% mutate_if(is.numeric, dif.fn.2) # for estimation
  data.aux.dif$ect <- stats::lag(model.aux.2$residuals, k = 1)
  
  data.filter.dif <- data.filter  %>% mutate_if(is.numeric, dif.fn.2) # for out-of-sample
  data.filter.dif <- data.filter.dif[(cut-nobs):nrow(data.filter.dif),]
  data.filter.dif$ect <- stats::lag(z, k = 1)
  
  model.ect <- update(model,    ~ . + ect) # update formula
  model.aux.dif <- lm(model.ect, data.aux.dif)
  modelplot(model.aux.dif)
  summary(model.aux.dif)
  
  fitted.outofsample <- stats::predict(model.aux.dif, data.filter.dif)
  #fitted.outofsample <- stats::predict(model.aux.dif, data.filter.dif[,3:ncol(data.filter.dif)])
  #fitted.outofsample <- as.matrix(cbind(1,data.filter.dif[1,3:ncol(data.filter.dif)])) %*% as.matrix(coef(model.aux.dif))
  
  # plot
  data2plot <- data.frame(Date = data.filter.dif$Date, Fair_Value = fitted.outofsample, Actual = data.filter.dif[,2])
  
  data2plot <- data2plot[-1,]
  
  data2plot <- data2plot %>%
    na.omit() %>% 
    mutate(Fair_Value_cum = data.aux.2[1,2] + cumsum(Fair_Value),
           Actual_cum = data.aux.2[1,2] + cumsum(Actual))
  
  last.fv <-  data2plot %>% top_n(1, Date) %>% pull(Fair_Value_cum) %>% round(2)
  last.actual <- data2plot %>% top_n(1, Date) %>% pull(Actual_cum) %>% round(2)
  colors <- c("Fair Value" = "black", "Actual" = "red")
  rects <- data.frame(xstart = as.Date(outofsample.end), 
                      xend = as.Date(last.obs))
  
  p1 <- 
    data2plot %>% 
    ggplot() +
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), alpha = 0.2) +
    geom_line(aes(x = as.Date(Date), y = Actual_cum, color = "Actual")) + 
    geom_line(aes(x = as.Date(Date), y = Fair_Value_cum, color = "Fair Value")) +
    scale_x_date(expand = c(0, 5), date_labels = "%b-%Y") +
    theme_bw() + theme(legend.title= element_blank()) +
    labs(x = "", y = "Actual and Fair Variation", 
         title = 'Fair variation model: Actual vs Fair (Out-of-sample on shaded area)',
         subtitle = paste0("Equation: ", model.string, " + error correction term"),
         caption = paste0('Estimation window: ', nobs,' business days. Last obs: ', format(last.obs, "%d-%b-%Y")),
         color = "Legend") +
    theme(panel.grid.minor = element_blank(),
          legend.position = c(0.20, 0.80)) +
    scale_color_manual(values = colors)
  
  
  # residuals
  data2plot <- data2plot %>% mutate(value = 100*(log(Actual_cum) - log(Fair_Value_cum)))
  stdev <- sd(data2plot$value)
  data_ends <- data2plot %>%  top_n(1, Date) %>% pull(value) %>% round(1)
  
  p2 <- 
    data2plot %>% 
    ggplot(aes(x = as.Date(Date), y = value)) +
    geom_ribbon(aes(ymin = -stdev*1.5, ymax = stdev*1.5), fill = "blue", alpha = 0.1) +
    geom_vline(aes(xintercept = as.Date(outofsample.end)), color = 'red', linewidth = 0.8) +
    geom_hline(aes(yintercept = 0), color = 'black', linewidth = 1) +
    geom_hline(aes(yintercept = -stdev*1.5), color = 'blue', linetype = 2) +
    geom_hline(aes(yintercept = stdev*1.5), color = 'blue', linetype = 2) +
    geom_line() +
    labs(x = "", y = "% Deviation from fair value", 
         title = '% Deviation -/+ 1.5*sd',
         subtitle = paste0("Fair Value: ", last.fv, ". Actual: ", last.actual, ". Deviation in %: ", data_ends)) +
    scale_x_date(expand = c(0, 2), date_labels = "%b-%Y") +
    scale_y_continuous(sec.axis = sec_axis(~ ., breaks = data_ends)) + 
    theme_bw() +
    theme(panel.grid = element_blank())
  
  plot.fv <- p1 / p2  
  
  
  return(list(plot.roll.res, plot.roll.res.only, plot.last, plot.fixed, plot.fv, coef.plot, stab.tests, rsq.plot, ur.plot, data2export))
  

}

