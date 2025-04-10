
# Setup -------------------------------------------------------------------

rm(list = ls())

# Coleta de dados ---------------------------------------------------------

inicio = year(Sys.Date()) - 20
final  = year(Sys.Date())
series = list(all_items                                    = "CUSR0000SA0",
              all_items_less_food_and_energy               = "CUSR0000SA0L1E",
              all_items_less_food_shelter_and_energy       = "CUSR0000SA0L12E",
              food                                         = "CUSR0000SAF1",
              food_at_home                                 = "CUSR0000SAF11",
              food_away_from_home                          = "CUSR0000SEFV",
              energy                                       = "CUSR0000SA0E",
              energy_commodities                           = "CUSR0000SACE",
              gasoline                                     = "CUSR0000SETB01",
              fuel_oil                                     = "CUSR0000SEHE01",
              energy_services                              = "CUSR0000SEHF",
              electricity                                  = "CUSR0000SEHF01",
              gas_service                                  = "CUSR0000SEHF02",
              commodities_less_food_and_energy_commodities = "CUSR0000SACL1E",
              new_vehicles                                 = "CUSR0000SETA01",
              used_cars_and_trucks                         = "CUSR0000SETA02",
              apparel                                      = "CUSR0000SAA",
              medical_care_commodities                     = "CUSR0000SAM1",
              services_less_energy_services                = "CUSR0000SASLE",
              shelter                                      = "CUSR0000SAH1",
              rent_of_shelter                              = "CUSR0000SAS2RS",
              rent_of_primary_residence                    = "CUSR0000SEHA",
              owners_equivalent_rent_of_residences         = "CUSR0000SEHC",
              medical_care_services                        = "CUSR0000SAM2",
              transportation_services                      = "CUSR0000SAS4",
              motor_vehicle_maintenance_and_repair         = "CUSR0000SETD",
              motor_vehicle_insurance                      = "CUSR0000SETE",
              airline_fares                                = "CUSR0000SETG01",
              durables                                     = "CUSR0000SAD",
              nondurables                                  = "CUSR0000SAN",
              nondurables_less_food_and_beverages          = "CUSR0000SANL11")

df_bls = get_n_series_table(series_ids = series,
                            api_key    = Sys.getenv("bls_api_key"),
                            start_year = inicio, 
                            end_year   = final) %>% 
  mutate(date   = as.Date(paste(year, substr(period,2,3), 01, sep = "-")),
         .before = "all_items",
         .keep   = "unused")

# Tratamento de dados -----------------------------------------------------

data = df_bls %>%
  pivot_longer(cols = -1) %>%
  mutate(name = gsub("_", " ", name),
         name = str_to_sentence(name, locale = "en"),
         name = tools::toTitleCase(name),
         var_mom = (value/dplyr::lag(value,1) - 1)*100,
         var_yoy = (value/dplyr::lag(value,12) - 1)*100,
         var_qoq = (((value/dplyr::lag(value,3))^4) - 1)*100,
         .by = name) %>%
  mutate(mm3m  = round(zoo::rollmean(var_qoq, k = 3,
                                     fill = NA, align = "right"), 2),
         mm6m  = round(zoo::rollmean(var_qoq, k = 6,
                                     fill = NA, align = "right"), 2),
         mm12m = round(zoo::rollmean(var_qoq, k = 12,
                                     fill = NA, align = "right"), 2),
         .by = name) %>%
  as_tibble()

data %>% 
  dplyr::filter(name %in% c("All Items", 
                     "All Items less Food and Energy", 
                     "All Items less Food Shelter and Energy")) %>% 
  ggplot() + 
  geom_hline(yintercept = 2, linetype = 2) +
  aes(x = date, y = var_yoy, color = name) +
  geom_line(linewidth = .75) +
  theme_bw() + 
  theme(panel.grid.minor   = element_blank(),
        legend.position    = "bottom", 
        legend.title       = element_blank(), 
        axis.text.x        = element_text(angle = 90)) +
  scale_x_date(expand      = c(0,0), 
               date_labels = "%b/%y", 
               date_breaks = "18 months") +
  labs(title = "Aberturas do CPI",
       y = NULL, x = NULL,
       caption = "Elaboração do autor com dados da BLS")

ggsave("core_cpi.png", 
       width = 4800, 
       height = 2160, 
       units = "px",
       dpi = 576, 
       path = paste(getwd(), "/saidas/cpi", sep = ""))

data %>% 
  dplyr::filter(name == "All Items less Food and Energy") %>% 
  ggplot() + 
  geom_hline(yintercept = 2, linetype = 2) +
  geom_line(aes(x = date, 
                y = var_qoq, 
                color = "Core t/t SAAR"), 
            linewidth = .75) + 
  geom_line(aes(x = date, 
                y = mm12m, 
                color = "MM12M"), 
            linewidth = .75) +
  theme_bw() + 
  theme(panel.grid.minor   = element_blank(),
        legend.position    = "bottom", 
        legend.title       = element_blank(), 
        axis.text.x        = element_text(angle = 90)) +
  scale_x_date(expand      = c(0,0), 
               date_labels = "%b/%y",
               date_breaks = "18 months") +
  labs(title = "CPI",
       y = NULL, x = NULL, 
       subtitle = "Less Food and Energy",
       caption = "Capri FO com dados da BLS")

ggsave("core_cpi.png", 
       width = 4800, 
       height = 2160, 
       units = "px",
       dpi = 576, 
       path = paste(getwd(), "/saidas/cpi", sep = ""))

data %>% 
  dplyr::filter(name == "Services less Energy Services") %>% 
  ggplot() + 
  geom_hline(yintercept = 2, linetype = 2) +
  geom_line(aes(x = date, 
                y = var_qoq, 
                color = "t/t SAAR"), 
            linewidth = .75) +
  geom_line(aes(x = date, 
                y = mm12m, 
                color = "MM12M"), 
            linewidth = .75) +
  theme_bw() + 
  theme(panel.grid.minor   = element_blank(),
        legend.position    = "bottom", 
        legend.title       = element_blank(), 
        axis.text.x        = element_text(angle = 90)) +
  scale_x_date(expand      = c(0,0), 
               date_labels = "%b/%y",
               date_breaks = "18 months") +
  labs(title = "CPI",
       y = NULL, x = NULL,
       subtitle = "Services Less Energy Services",
       caption = "Capri FO com dados da BLS")

ggsave("services_cpi.png", 
       width = 4800, 
       height = 2160, 
       units = "px",
       dpi = 576, 
       path = paste(getwd(), "/saidas/cpi", sep = ""))

data %>% 
  dplyr::filter(name == "Shelter") %>% 
  ggplot() + 
  geom_hline(yintercept = 2, linetype = 2) +
  geom_line(aes(x = date, 
                y = var_qoq, 
                color = "t/t SAAR"), 
            linewidth = .75) +
  geom_line(aes(x = date, 
                y = mm12m, 
                color = "MM12M"), 
            linewidth = .75) +
  theme_bw() + 
  theme(panel.grid.minor   = element_blank(),
        legend.position    = "bottom", 
        legend.title       = element_blank(), 
        axis.text.x        = element_text(angle = 90)) +
  scale_x_date(expand      = c(0,0), 
               date_labels = "%b/%y",
               date_breaks = "18 months") +
  labs(title = "CPI",
       y = NULL, x = NULL,
       subtitle = "Shelter",
       caption = "Capri FO com dados da BLS")

ggsave("shelter_cpi.png", 
       width = 4800, 
       height = 2160, 
       units = "px",
       dpi = 576, 
       path = paste(getwd(), "/saidas/cpi", sep = ""))
