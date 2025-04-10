
# Setup -------------------------------------------------------------------

rm(list = ls())

# Coleta e tratamento de dados --------------------------------------------

data = get_sidra(x   = 1620,
                 api = paste0("/t/1620/n1/all/v/all/p/all/c11255/",
                              "90707/d/v583%202"))

data_ts = ts(log(data$Valor), start = c(1996, 1), frequency = 4)

data_tsibble = data_ts %>% 
  as_tsibble()

dummies_sazonais = data_ts %>% 
  as_tsibble() %>% 
  mutate(value = value - dplyr::lag(value,1)) %>% 
  mutate(trimestre = factor(
    quarter(index),
    levels = 1:4,
    labels = c("Q1", "Q2", "Q3", "Q4")
    )) %>% 
  bind_cols(model.matrix(~ trimestre - 1, data = .)) %>% 
  drop_na() %>% 
  as.ts()

plot(data_ts)

# Outliers ----------------------------------------------------------------

diff(data_ts) %>% 
  forecast::ggtsdisplay()

model_isat = data_ts %>%
  isat(
    iis = TRUE, sis = TRUE,  tis = TRUE,
    plot = TRUE,
    ar = c(1),
    uis = dummies_sazonais[,-c(1:2)]
    )

model_isat

outliers = data_tsibble %>% 
  mutate(date = index, 
         d2008 = if_else(as.Date(date) %in% c(as.Date("2008-07-01")),1,0),
         d2020 = if_else(as.Date(date) %in% c(as.Date("2020-04-01")),1,0),
         d2021 = if_else(as.Date(date) %in% c(as.Date("2020-07-01")),1,0),
        .keep  = "none") %>% 
  as_tsibble(index = date) %>% 
  as.ts()

# Modelo de Espaço de Estados ---------------------------------------------

model_ss = SSModel(
  data_ts ~ 
    SSMtrend(degree = 2, Q = list(matrix(NA), matrix(NA))) +
    SSMcycle(period = 4*4.416667, Q = matrix(NA)) +
    SSMseasonal(period = 4, sea.type = "trigonometric", Q = matrix(NA)) +
    SSMregression(~outliers, Q = matrix(NA)),
  H = matrix(NA))

custom_fitSSM = function(model, inits) {
  fit = optim(par = inits, fn = function(pars) {
    model$Q[1, 1, 1] <- exp(pars[1])  # Outiliers
    model$Q[2, 2, 1] <- exp(pars[2])  # Outliers
    model$Q[3, 3, 1] <- exp(pars[3])  # Outliers
    model$Q[4, 4, 1] <- exp(pars[4])  # Tendência - nível
    model$Q[5, 5, 1] <- exp(pars[5])  # Tendência - inclinação
    model$Q[6, 6, 1] <- exp(pars[6])  # Sazonalidade
    model$Q[7, 7, 1] <- exp(pars[7])  # Sazonalidade
    model$Q[8, 8, 1] <- exp(pars[8])  # Ciclo econômico
    model$H[1] <- exp(pars[9])  # Erro de observação
    
    # Retorna o negativo da log-verossimilhança para maximização
    - logLik(model)
  }, method = "Nelder-Mead", control = list(maxit = 500))
  
  # Atualiza o modelo com os valores otimizados
  model$Q[1, 1, 1] <- exp(fit$par[1])
  model$Q[2, 2, 1] <- exp(fit$par[2])
  model$Q[3, 3, 1] <- exp(fit$par[3])
  model$Q[4, 4, 1] <- exp(fit$par[4])
  model$Q[5, 5, 1] <- exp(fit$par[5])
  model$Q[6, 6, 1] <- exp(fit$par[6])
  model$Q[7, 7, 1] <- exp(fit$par[7])
  model$Q[8, 8, 1] <- exp(fit$par[8])
  model$H[1] <- exp(fit$par[9])
  
  return(list(model = model, optim.out = fit))
}

# Estimar o modelo usando a função de Perda
fit = custom_fitSSM(model_ss, 
                    inits = c(log(0.001),  # Tendência - nível
                              log(0.001),  # Tendência - inclinação 
                              log(0.01),   # Ciclo econômico
                              log(0.01),   # Sazonalidade
                              log(0.01),   # Sazonalidade 
                              log(0.1),    # Outiliers
                              log(0.01),   # Outiliers
                              log(0.01),   # Outiliers
                              log(0.01)))

# fit = custom_fitSSM(model_ss, inits = rep(log(var(data_ts)), 9))

# fit = fitSSM(model_ss, inits = rep(log(0.01), 10), method = "BFGS")

exp(fit$optim.out$par)

# Filtro de Kalman --------------------------------------------------------

model_kfs = KFS(fit$model, 
                filtering = "state", 
                smoothing = c("state", "mean"))

data = tibble(
  date = yearqtr(time(data_ts)) %>% yearquarter(),
  trend_estimated = model_kfs$alphahat[, "level"],
  cycle_estimated = model_kfs$alphahat[, "cycle"],
  residuals = residuals(model_kfs, type = "response"),
  outliers_estimated_1 = outliers[,1] * coef(fit$model)[,1],
  outliers_estimated_2 = outliers[,2] * coef(fit$model)[,2],
  outliers_estimated_3 = outliers[,3] * coef(fit$model)[,3],
  data_ts - 
    trend_estimated - 
    cycle_estimated -
    residuals -
    outliers_estimated_1 -
    outliers_estimated_2 -
    outliers_estimated_3
  ) %>% 
  set_names(c("date",
              "trend",
              "cycle",
              "residuals",
              "d2008",
              "d2020",
              "d2021",
              "seasonality")) %>% 
  mutate(pib_sa = data_ts - seasonality)

# Resultados --------------------------------------------------------------

# PIB sazonalmente ajustado
pib_sa = data_ts - data$seasonality

# Tendência
p1 = data.frame(Time = time(data_ts), 
           Trend = data$trend) %>% 
  ggplot() +
  aes(x = Time, y = Trend) + 
  geom_line(color = "blue", linewidth = .75) +
  labs(title = "Componente Tendencial", 
       y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))

p1

ggsave("trend.png",
       width  = 4800,
       height = 2160,
       units  = "px",
       dpi    = 576,
       path   = paste0(getwd(), "/saidas/pib/kalman"))

# Ciclo
p2 = data.frame(Time = time(data_ts),
                Cycle = data$cycle) %>% 
  ggplot() +
  aes(x = Time, y = Cycle) +
  geom_line(color = "green", linewidth = .75) +
  labs(title = "Componente Cíclico", 
       y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))

p2

ggsave("cycle.png",
       width  = 4800,
       height = 2160,
       units  = "px",
       dpi    = 576,
       path   = paste0(getwd(), "/saidas/pib/kalman"))

# Sazonalidade
p3 = data.frame(Time = time(data_ts), 
                Seasonality = data$seasonality) %>% 
  ggplot() +
  aes(x = Time, y = Seasonality) +
  geom_line(color = "red", linewidth = .75) +
  labs(title = "Componente Sazonal", 
       y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))

p3

ggsave("seasonality.png",
       width  = 4800,
       height = 2160,
       units  = "px",
       dpi    = 576,
       path   = paste0(getwd(), "/saidas/pib/kalman"))

# Resíduos
p4 = data.frame(Time = time(data_ts), 
                Residuals = data$residuals) %>% 
  ggplot() +
  aes(x = Time, y = Residuals) +
  geom_line(color = "black", linewidth = .75) +
  labs(title = "Componente Residual", 
       y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))

p4

ggsave("residuals.png",
       width  = 4800,
       height = 2160,
       units  = "px",
       dpi    = 576,
       path   = paste0(getwd(), "/saidas/pib/kalman"))

# Outliers
p5 = data.frame(Time  = time(data_ts), 
                 d2008 = data$d2008,
                 d2020 = data$d2020,
                 d2020 = data$d2021) %>% 
  pivot_longer(-1) %>% 
  ggplot() +
  aes(x = Time, y = value, colour = name) +
  geom_line(size = 1) +
  labs(title = "Outliers", 
       y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))

p5

ggsave("outliers.png",
       width  = 4800,
       height = 2160,
       units  = "px",
       dpi    = 576,
       path   = paste0(getwd(), "/saidas/pib/kalman"))

# Série
p6 = ggplot() +
  geom_line(data = data.frame(Time = time(data_ts), 
                              Original = as.numeric(data_ts)), 
            aes(x = Time, y = Original), color = "black", size = 1) +
  geom_line(data = data.frame(Time = time(data_ts),
                              Combined = data$trend +
                                data$cycle +
                                data$d2008 +
                                data$d2020 +
                                data$d2021),
            aes(x = Time, y = Combined), color = "red", size = 1) +
  labs(title = "PIB Original vs. Recomposto (Trend + Cycle + Outliers)", 
       y = "Log do PIB", x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))

p6

ggsave("serie.png",
       width  = 4800,
       height = 2160,
       units  = "px",
       dpi    = 576,
       path   = paste0(getwd(), "/saidas/pib/kalman"))


# Combinado ---------------------------------------------------------------

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)

ggsave("combined.png",
       width  = 4800,
       height = 2160,
       units  = "px",
       dpi    = 576,
       path   = paste0(getwd(), "/saidas/pib/kalman"))

# Hiato -------------------------------------------------------------------

# Plot Hiatus vs IFI
bcb = readxl::read_excel("dados/ri202412anp.xlsx", 
                         sheet = "Graf 2.2.8",
                         skip = 8) %>% 
  drop_na()

bcb$date = as.yearqtr(bcb$`Trimestre`, format = "%Y-%m-%d")

ifi = readxl::read_excel("dados/estimativas-do-hiato-do-produto-ifi.xlsx", 
                         sheet = 2,
                         skip = 1)

ifi$date = as.yearqtr(ifi$`Trim-Ano`, format = "%Y-%m-%d")

gap = data.frame(date = as.yearqtr(time(data_ts)),
                 gap = as.numeric((exp(pib_sa)/exp((data$trend)) - 1))*100)

ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data = gap, 
            aes(x = date, y = gap),
            color = "red",
            size = 1) +
  geom_line(data = ifi, 
            aes(x = date, y = Hiato*100),
            color = "green",
            size = 1) +
  geom_line(data = bcb, 
            aes(x = date, y = `Cenário de referência`),
            color = "blue",
            size = 1) +
  labs(title = "Hiato vs IFI e BCB", 
       y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90))

save(gap, file = "dados/gap.RData")

# Robustez ----------------------------------------------------------------

residuals = data$residuals

residuals %>% summary()

forecast::checkresiduals(residuals,
                         lag = 10)

residuals %>% 
  forecast::ggtsdisplay()

residuals %>%
  isat(
    iis = TRUE, sis = TRUE,  tis = TRUE,
    plot = TRUE
    # uis = dummies_sazonais[,-c(1:2)]
  )

# normalidade
residuals %>% 
  tseries::jarque.bera.test()

residuals %>% 
  shapiro.test()

residuals %>% 
  density() %>% plot()

# heterocedasticidade
residuals %>% 
  feasts::stat_arch_lm()

