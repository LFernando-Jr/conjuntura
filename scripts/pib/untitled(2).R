
class(hiatus_data)

gap = hiatus_data %>% tibble() %>% 
  mutate(date = yearquarter(Time),
         gap = Hiatus,
         .keep = "none")

save(gap, file = "dados/gap.RData")
