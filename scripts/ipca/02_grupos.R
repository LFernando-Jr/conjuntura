
# Setup -------------------------------------------------------------------

rm(list = ls())

load("dados/ipca.RData")

# Tratamento de dados -----------------------------------------------------

data = data_raw %>%
  arrange(date) %>%
  mutate(index  = cumprod(1 + var/100)*100/(cumprod(1 + first(var)/100)),
        .by     = name,
        .before = weight,
        .keep   = "unused") %>% 
  mutate(var_mom = (index/dplyr::lag(index,1) - 1)*100, 
         var_yoy = (index/dplyr::lag(index,12) - 1)*100,
         var_qoq = (((index/dplyr::lag(index,3))^4) - 1)*100,
        .by = name) %>% 
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
        .by = name) %>% 
  as_tibble() 

# Visualização de dados ---------------------------------------------------  

colors = c("#2F47AD",
           "#D62728",
           "#3BA58B",
           "#AEC7E8",
           "#FFBB78",
           "#9467BD",
           "#31AFE0",
           "#E47632",
           "#2CA02C",
           "#FF7F0E",
           "#D4A83F",
           "#E377C2")

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
  
  j = case_when(i == "contrib_mom" ~ "var_mom",
                i == "contrib_yoy" ~ "var_yoy",
                i == "contrib_qoq" ~ "var_qoq")
  
  g = data %>% 
    dplyr::filter(year(date) >= year(today() - years(5)),
           classificacao == "Grupo") %>% 
    ggplot() + 
    geom_bar(aes(date, !!sym(i), fill = grupo), stat = "identity") +
    geom_line(data = dplyr::filter(data,
                            year(date) >= year(today() - years(5)),
                            classificacao == "Geral",
                            !is.na(!!sym(j))),
              aes(x = date, y = !!sym(j), color = "Índice geral"), 
              linewidth = .75) +
    geom_point(data = dplyr::filter(data,
                             year(date) >= year(today() - years(5)),
                             grupo == "Geral",
                             !is.na(!!sym(j))),
               aes(x = date, y = !!sym(j))) +
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
         subtitle = "IPCA - Grupos",
         y = NULL, x = NULL,
         caption = "Elaboração do autor com dados do IBGE")
  
  print(g)
  print(j)
  print(names(contrib)[m])
  
  ggsave(paste0(names(contrib)[m], "_grupos.png"),
         width  = 4800,
         height = 2160,
         units  = "px",
         dpi    = 576,
         path   = paste0(getwd(), "/saidas/ipca/contribuição"))
  
  m = m + 1
  
  }

## Médias móveis ----------------------------------------------------------

for (i in unique(data$name[which(data$classificacao %in% c("Geral",
                                                           "Grupo"))])) {
  g = data %>%
    select(-c(`abertura bcb`,
              alimentos,
              servicos,
              servicos_rti,
              industriais,
              tradables,
              `nucleo ex0`,
              `nucleo ex1`,
              `nucleo ex2`,
              `nucleo ex3`)) %>%
    dplyr::filter(year(date) >= year(today() - years(5)),
           classificacao == c("Geral","Grupo")) %>%
    pivot_longer(cols = c("mm3m", "mm6m", "mm12m"),
                 names_to = "janelas",
                 values_to = "médias móveis") %>%
    dplyr::filter(year(date) >= year(Sys.Date()) - 10,
           name == i) %>%
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
    scale_x_date(expand = c(0, 0),
                 date_labels = "%b/%y",
                 date_breaks = "6 months") +
    labs(title = "IPCA",
         y = NULL, x = NULL,
         subtitle = paste(i),
         caption = "Elaboração do autor com dados do IBGE")

  print(g)
  print(i)

  ggsave(paste0(i, "_grupos_qoq_annualized.png"),
    width  = 4800,
    height = 2160,
    units  = "px",
    dpi    = 576,
    path   = paste0(getwd(), "/saidas/ipca/médias móveis"))
  
  }

## Sazonalidade -----------------------------------------------------------

for (i in unique(data$name[which(data$classificacao %in% c("Geral",
                                                           "Grupo"))])) {
  
  g = data %>% 
    select(-c(`abertura bcb`,
              alimentos,
              servicos,
              servicos_rti,
              industriais,
              tradables,
              `nucleo ex0`,
              `nucleo ex1`,
              `nucleo ex2`,
              `nucleo ex3`)) %>% 
    dplyr::filter(name == i) %>%
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
  
  ggsave(paste0(i,"_grupos_sazonal.png"),
         width  = 4800,
         height = 2160,
         units  = "px",
         dpi    = 576,
         path   = paste0(getwd(), "/saidas/ipca/sazonalidade"))
  
  }

## Itens e subitens -------------------------------------------------------

for (i in unique(data$name[which(data$classificacao == "Grupo")])) {
  
  g = data %>% 
    dplyr::filter(date >= today() - years(1),
           classificacao == ifelse(i == "Despesas pessoais" | 
                                     i == "Comunicação",
                                   "Subitem",
                                   "Item"),
           grupo == i) %>% 
    ggplot() +
    aes(x = date, y = var_mom) +
    geom_line(linewidth = .75) +
    facet_wrap(~name, scales = "free") +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          legend.position = "bottom", 
          legend.title = element_blank()) +
    labs(title = NULL,
         subtitle = paste(i),
         x = "Mês",
         y = NULL,
         fill = "Intervalos",
         color = "Linhas")
  
  print(g)
  print(i)
  
  ggsave(paste0(i,"_itens_subitens.png"),
         width  = 4800,
         height = 2160,
         units  = "px",
         dpi    = 576,
         path   = paste0(getwd(), "/saidas/ipca/itens e subitens"))
  
  }
