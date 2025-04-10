
# Setup -------------------------------------------------------------------

rm(list = ls())

load("dados/ipca.RData")

# Tratamento de dados -----------------------------------------------------

data = data_raw %>%
  dplyr::filter(classificacao == "Subitem") %>% 
  arrange(date) %>%
  group_by(date, tradables) %>% 
  reframe(date = date,
          var = sum(var*weight)/sum(weight),
          weight = sum(weight)) %>% 
  unique() %>% 
  mutate(index  = cumprod(1 + var/100)*100/(cumprod(1 + first(var)/100)),
         .by     = tradables,
         .before = weight,
         .keep   = "unused") %>% 
  mutate(var_mom = (index/dplyr::lag(index,1) - 1)*100, 
         var_yoy = (index/dplyr::lag(index,12) - 1)*100,
         var_qoq = (((index/dplyr::lag(index,3))^4) - 1)*100,
         .by = tradables) %>% 
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
         .by = tradables) %>% 
  as_tibble() 

# Visualização de dados ---------------------------------------------------  

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

## Contribuição -----------------------------------------------------------

m = 1

for (i in contrib) {
  
  g = data %>% 
    dplyr::filter(year(date) >= year(today() - years(5))) %>% 
    ggplot() + 
    geom_bar(aes(date, !!sym(i), fill = tradables), stat = "identity") +
    geom_line(data = data %>% 
                group_by(date) %>% 
                reframe(date = date,
                        value = sum(!!sym(i))) %>% 
                unique() %>% 
                dplyr::filter(year(date) >= year(today() - years(5)),
                       !is.na(value)),
              aes(x = date, y = value, color = "Índice geral"), 
              linewidth = .75) +
    geom_point(data = data %>% 
                 group_by(date) %>% 
                 reframe(date = date,
                         value = sum(!!sym(i))) %>% 
                 unique() %>% 
                 dplyr::filter(year(date) >= year(today() - years(5)),
                        !is.na(value)),
               aes(x = date, y = value)) +
    theme_bw() + 
    theme(panel.grid.minor = element_blank(),
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90)) +
    scale_x_date(expand = c(0,0), 
                 date_labels = "%b/%y", 
                 date_breaks = "6 months") +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = "black") +
    labs(title = names(contrib)[m],
         subtitle = "IPCA - Tradables",
         y = NULL, x = NULL,
         caption = "Elaboração do autor com dados do IBGE")
  
  print(g)
  print(names(contrib)[m])
  
  ggsave(paste0(names(contrib)[m], "_tradables.png"),
         width  = 4800,
         height = 2160,
         units  = "px",
         dpi    = 576,
         path   = paste0(getwd(), "/saidas/ipca/contribuição"))
  
  m = m + 1
  
}

## Médias móveis ----------------------------------------------------------

for (i in unique(data$tradables)) {
  
  g = data %>% 
    dplyr::filter(year(date) >= year(today() - years(5))) %>% 
    pivot_longer(cols = c("mm3m", "mm6m", "mm12m"),
                 names_to = "janelas",
                 values_to = "médias móveis") %>%
    dplyr::filter(year(date) >= year(Sys.Date()) - 10,
           tradables == i) %>% 
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
    labs(title = "IPCA - Tradables", 
         y = NULL, x = NULL, 
         subtitle = paste(i), 
         caption = "Elaboração do autor com dados do IBGE")
  
  print(g) 
  print(i) 
  
  ggsave(paste0(i,"_tradables_qoq_annualized.png"),
         width  = 4800,
         height = 2160,
         units  = "px",
         dpi    = 576,
         path   = paste0(getwd(), "/saidas/ipca/médias móveis"))
  
  }

## Sazonalidade -----------------------------------------------------------

for (i in unique(data$tradables)) {
  
  g = data %>% 
    dplyr::filter(tradables == i) %>%
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
  
  ggsave(paste0(i,"_tradables_sazonal.png"),
         width  = 4800,
         height = 2160,
         units  = "px",
         dpi    = 576,
         path   = paste0(getwd(), "/saidas/ipca/sazonalidade"))  
  
  }
