library(tidyverse)
library(readxl)

path <- "raw-data/remuneraciones_2024.xlsx"

# Leer y limpiar los datos
salaries <- excel_sheets(path) |>
  set_names() |>
  map_df(~ read_excel(path, sheet = .x)) |>
  janitor::clean_names()

months_list <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                 "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Procesar y pivotear los datos
salaries_wide <- salaries |>
  group_by(nombre_completo, calificacion_profesional_o_formacion, mes) |>
  summarise(salario = sum(honorario_total_bruto), .groups = "drop") |>
  pivot_wider(names_from = mes, values_from = salario) |>
  ungroup()

# Capturar los meses presentes
months_present <- intersect(months_list, colnames(salaries_wide))

# Calcular el total de salarios y reordenar las columnas
salaries_wide <- salaries_wide |>
  mutate(total_salario = rowSums(across(all_of(months_present)), na.rm = TRUE)) |>
  arrange(desc(total_salario)) |>
  select(nombre_completo, calificacion_profesional_o_formacion, all_of(months_present), total_salario) |>
  writexl::write_xlsx("clean-data/salaries_2024.xlsx")
