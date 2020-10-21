## Creates list of Utilities in the system.

utilities <- readxl::read_xlsx("data/lwsp/water_systems.xlsx")

list <- c(
  "03-68-010", #OWASA
  "03-32-010", #Durham
  "03-92-010", #Raleigh
  "03-92-020", #Cary
  "03-26-010"
)

utility_registry <- utilities %>%
  filter(pwsid %in% list) %>%
  select(pwsid,
         name,
         basin,
         contact,
         title,
         phone,
         email)