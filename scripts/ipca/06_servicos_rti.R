
# Setup -------------------------------------------------------------------

rm(list = ls())

load("dados/ipca.RData")

# Tratamento de dados -----------------------------------------------------

data = data_raw %>%
  dplyr::filter(classificacao == "Subitem") %>% 
  arrange(date) %>%
  group_by(date, servicos_rti) %>% 
  reframe(date = date,
          var = sum(var*weight)/sum(weight),
          weight = sum(weight)) %>% 
  unique() %>% 
  drop_na() %>% 
  mutate(index  = cumprod(1 + var/100)*100/(cumprod(1 + first(var)/100)),
         .by     = servicos_rti,
         .before = weight,
         .keep   = "unused") %>% 
  mutate(var_mom = (index/dplyr::lag(index,1) - 1)*100, 
         var_yoy = (index/dplyr::lag(index,12) - 1)*100,
         var_qoq = (((index/dplyr::lag(index,3))^4) - 1)*100,
         .by = servicos_rti) %>% 
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
         .by = servicos_rti) %>% 
  as_tibble() 

# Visualização de dados ---------------------------------------------------  

colors = c("#2F47AD",
           "#31AFE0",
           "#E47632")

var = list("var_mom",
           "var_yoy",
           "var_qoq")

for (i in var) {
  
  g = data %>% 
    select(date, servicos_rti, !!sym(i)) %>% 
    drop_na() %>% 
    ggplot() +
    aes(x = date, y = !!sym(i), colour = servicos_rti) + 
    geom_line(linewidth = .75, na.rm = FALSE) +
    facet_wrap(~servicos_rti, scales = "free") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.position = "none", 
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90)) +
    scale_x_date(expand = c(0,0), 
                 date_labels = "%b/%y", 
                 date_breaks = "6 months") + 
    scale_color_manual(values = colors) +
    labs(title = "IPCA", 
         y = NULL, x = NULL, 
         subtitle = paste0(
           "Serviços - ", case_when(!!sym(i) == "var_mom" ~ "m/m",
                                    !!sym(i) == "var_qoq" ~ "q/q SAAR",
                                    !!sym(i) == "var_yoy" ~ "y/y")
         ), 
         caption = "Elaboração do autor com dados do IBGE")
  
  print(g)
  
  ggsave(paste0(i, "_servicos_rti.png"),
         width  = 4800,
         height = 2160,
         units  = "px",
         dpi    = 576,
         path   = paste0(getwd(), "/saidas/ipca/itens e subitens"))
  
}

## Médias móveis ----------------------------------------------------------

for (i in unique(data$servicos_rti)) {
  
  g = data %>% 
    dplyr::filter(year(date) >= year(today() - years(5))) %>% 
    pivot_longer(cols = c("mm3m", "mm6m", "mm12m"),
                 names_to = "janelas",
                 values_to = "médias móveis") %>%
    dplyr::filter(year(date) >= year(Sys.Date()) - 10,
           servicos_rti == i) %>% 
    drop_na() %>% 
    ggplot() + 
    geom_hline(yintercept = 3, linetype = 2) +
    geom_line(aes(x = date, 
                  y = var_qoq, 
                  color = "t/t SAAR"), 
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
    labs(title = "IPCA", 
         y = NULL, x = NULL, 
         subtitle = paste(i), 
         caption = "Elaboração do autor com dados do IBGE")
  
  print(g) 
  print(i) 
  
  ggsave(paste0(i,"_servicos_rti_qoq_annualized.png"),
         width  = 4800,
         height = 2160,
         units  = "px",
         dpi    = 576,
         path   = paste0(getwd(), "/saidas/ipca/médias móveis"))
  
}

## Sazonalidade -----------------------------------------------------------

for (i in unique(data$servicos_rti)) {
  
  g = data %>% 
    dplyr::filter(servicos_rti == i) %>%
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
  
  ggsave(paste0(i,"_servicos_rti_bcb_sazonal.png"),
         width  = 4800,
         height = 2160,
         units  = "px",
         dpi    = 576,
         path   = paste0(getwd(), "/saidas/ipca/sazonalidade"))
  
}
