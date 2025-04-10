
# Setup -------------------------------------------------------------------

rm(list = ls())

load("dados/ipca.RData")


# Núcleos por exclusão ----------------------------------------------------

## Tratamento de dados ----------------------------------------------------

data = data_raw %>%
  dplyr::select("date",
                "cod",
                "name",
                "var",
                "weight",
                "classificacao",
                "nucleo ex0",
                "nucleo ex1",
                "nucleo ex2",
                "nucleo ex3") %>% 
  pivot_longer(cols = 7:10,
               names_to = "nucleo1",
               values_to = "nucleo",
               values_drop_na = TRUE) %>% 
  dplyr::filter(classificacao == "Subitem") %>% 
  arrange(date) %>%
  group_by(date, nucleo) %>% 
  reframe(date = date,
          var = sum(var*weight)/sum(weight),
          weight = sum(weight)) %>% 
  unique() %>% 
  mutate(index  = cumprod(1 + var/100)*100/(cumprod(1 + first(var)/100)),
         .by     = nucleo,
         .before = weight,
         .keep   = "unused") %>% 
  mutate(var_mom = (index/dplyr::lag(index,1) - 1)*100, 
         var_yoy = (index/dplyr::lag(index,12) - 1)*100,
         var_qoq = (((index/dplyr::lag(index,3))^4) - 1)*100,
         .by = nucleo) %>% 
  mutate(contrib_mom = var_mom*weight/100, 
         contrib_yoy = var_yoy*weight/100,
         contrib_qoq = var_qoq*weight/100) %>% 
  mutate(mm3m = round(zoo::rollmean(var_qoq, 
                                    k = 3,
                                    fill = NA, 
                                    align = "right"), 2),
         mm6m = round(zoo::rollmean(var_qoq, 
                                    k = 6,
                                    fill = NA, 
                                    align = "right"), 2),
         mm12m = round(zoo::rollmean(var_qoq, 
                                     k = 12,
                                     fill = NA, 
                                     align = "right"), 2),
         .by = nucleo) %>% 
  as_tibble() 

## Visualização de dados --------------------------------------------------

colors = c("#2F47AD",
           "#31AFE0")

contrib = list(`Contribuição Mensal` = "contrib_mom",
               `Contribuição Anual` = "contrib_yoy",
               `Contribuição Trimestral` = "contrib_qoq")

var = list("var_mom",
           "var_mom",
           "var_yoy",
           "var_yoy",
           "var_qoq",
           "var_qoq")

### Médias móveis ---------------------------------------------------------

for (i in unique(data$nucleo)) {
  
  g = data %>% 
    dplyr::filter(year(date) >= year(today() - years(5))) %>% 
    pivot_longer(cols = c("mm3m", "mm6m", "mm12m"),
                 names_to = "janelas",
                 values_to = "médias móveis") %>%
    dplyr::filter(year(date) >= year(Sys.Date()) - 10,
                  nucleo == i) %>% 
    drop_na() %>% 
    ggplot() + 
    geom_hline(yintercept = 3, linetype = 2) +
    geom_line(aes(x = date, 
                  y = var_qoq, 
                  color = "q/q SAAR"), 
              linewidth = .75) +
    geom_line(aes(x = date, 
                  y = `médias móveis`, 
                  color = janelas), 
              linewidth = .25) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom", 
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90)) +
    scale_x_date(expand = c(0,0), 
                 date_labels = "%b/%y", 
                 date_breaks = "6 months") + 
    labs(title = "IPCA - Núcleos por Exclusão", 
         y = NULL, x = NULL, 
         subtitle = paste(i), 
         caption = "Elaboração do autor com dados do IBGE")
  
  print(g) 
  print(i) 
  
  ggsave(paste0(i,"_qoq_annualized.png"),
         width  = 4800,
         height = 2160,
         units  = "px",
         dpi    = 576,
         path   = paste0(getwd(), "/saidas/ipca/médias móveis"))
  
}

### Sazonalidade ----------------------------------------------------------

for (i in unique(data$nucleo)) {
  
  g = data %>% 
    dplyr::filter(nucleo == i) %>%
    mutate(year = year(date), 
           month = month(date)) %>%
    drop_na() %>% 
    ggplot(aes(x = month(date), y = var_mom)) +
    stat_summary(fun.min = min, 
                 fun.max = max, 
                 geom = "ribbon", 
                 aes(ymin = ..ymin.., 
                     ymax = ..ymax.., 
                     fill = "Min-Max"), 
                 alpha = 0.2) +
    stat_summary(fun.min = ~ quantile(.x, .90), 
                 fun.max = ~ quantile(.x, .10), 
                 geom = "ribbon",
                 aes(ymin = ..ymin.., 
                     ymax = ..ymax.., 
                     fill = "Q10-Q90"), 
                 alpha = 0.4) +
    stat_summary(fun.min = ~ quantile(.x, .25), 
                 fun.max = ~ quantile(.x, .75), 
                 geom = "ribbon",
                 aes(ymin = ..ymin.., 
                     ymax = ..ymax.., 
                     fill = "Q25-Q75"), 
                 alpha = 0.4) +
    stat_summary(fun = median, 
                 geom = "line", 
                 aes(color = "Median"), 
                 linewidth = .25) +
    geom_line(data = . %>% dplyr::filter(year == year(Sys.Date())), 
              aes(x = month(date),
                  y = var_mom, 
                  color = paste0(year(date))), 
              linewidth = .75) +
    scale_x_continuous(expand = c(0,0), n.breaks = 12) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom", 
          legend.title = element_blank()) +
    labs(title = "Gráfico Sazonal",
         subtitle = paste(i),
         x = "Mês",
         y = NULL,
         fill = "Intervalos",
         color = "Linhas")
  
  print(g)
  print(i)
  
  ggsave(paste0(i,"_sazonal.png"),
         width  = 4800,
         height = 2160,
         units  = "px",
         dpi    = 576,
         path   = paste0(getwd(), "/saidas/ipca/sazonalidade"))
  
}

# Núcleos Estatísticos ----------------------------------------------------

## MS ---------------------------------------------------------------------

data = data_raw %>%
  dplyr::filter(classificacao == "Item") %>% 
  dplyr::select("date",
                "name",
                "var",
                "weight") 

smoothed_items = c("Combustíveis (veículos)",
                   "Combustíveis (domésticos)",
                   "Energia elétrica residencial",
                   "Transporte público",
                   "Fumo",
                   "Cursos regulares",
                   "Cursos diversos",
                   "Comunicação.2")

# Função para aplicar a suavização em uma série (distribuir o reajuste em 12 meses)
smooth_series <- function(x, n_months = 12) {
  # Cria um vetor de saída, inicializado com zeros
  y <- numeric(length(x))
  
  # Percorre cada mês da série
  for (i in seq_along(x)) {
    # Verifica se há um reajuste no mês i
    # (pode-se ajustar a condição para tolerar pequenos valores, se necessário)
    if (x[i] != 0) {
      # Define o último índice para o qual a suavização será aplicada (não ultrapassando o comprimento da série)
      end_idx <- min(i + n_months - 1, length(x))
      
      # Distribui o valor do reajuste igualmente entre o mês atual e os próximos (n_months - 1)
      # Se houver sobreposição (reajustes em meses consecutivos), as contribuições se somarão.
      y[i:end_idx] <- y[i:end_idx] + x[i] / n_months
    }
  }
  return(y)
}


# Aplicar a suavização somente para os itens da lista 'smoothed_items'
data_smoothed <- data %>%
  group_by(name) %>%
  arrange(date) %>%  # Assegure que os dados estão ordenados por data
  mutate(var_smoothed = if_else(name %in% smoothed_items,
                                smooth_series(var, n_months = 12),
                                var)) %>%
  ungroup()

# Visualize as primeiras linhas para conferir o resultado
head(data_smoothed)

calc_ipca_ms <- function(df) {
  # Número total de subitens no mês
  N <- nrow(df)
  
  # Definindo k como 10% de N, arredondando para o inteiro mais próximo
  k <- floor(0.1 * N)
  
  # Se N for insuficiente para remover 2*k itens, retorna NA
  if (N <= 2 * k) return(NA_real_)
  
  # Ordena os subitens pela variação suavizada
  df_sorted <- df %>% arrange(var_smoothed)
  
  # Seleciona os itens que ficam após a remoção dos extremos
  df_trimmed <- df_sorted[(k + 1):(N - k), ]
  
  # Calcula a média ponderada dos subitens remanescentes
  weighted_mean <- sum(df_trimmed$var_smoothed * df_trimmed$weight) / sum(df_trimmed$weight)
  
  return(weighted_mean)
}





# Aplicar a função para cada mês (agrupando por 'date'):
ipca_ms_series <- data_smoothed %>%
  group_by(date) %>%
  summarise(ipca_ms = calc_ipca_ms(cur_data())) %>%
  ungroup()

# Visualiza as primeiras linhas da série do IPCA-MS:
tail(ipca_ms_series)


