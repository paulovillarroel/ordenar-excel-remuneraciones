library(shiny)
library(tidyverse)
library(readxl)
library(writexl)
library(DT)

# Define la UI
ui <- fluidPage(
    titlePanel("Procesamiento de Remuneraciones"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Selecciona el archivo Excel:")
        ),
        mainPanel(
            DTOutput("table"),
            downloadButton("download", "Descargar datos procesados")
        )
    )
)

# Define el servidor
server <- function(input, output) {

    # Función para procesar el archivo
    process_file <- reactive({
        req(input$file)

        # Cargar y limpiar los datos
        salaries <- input$file$datapath |>
            excel_sheets() |>
            set_names() |>
            map_df(~ read_excel(input$file$datapath, sheet = .x)) |>
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
            select(nombre_completo, calificacion_profesional_o_formacion, all_of(months_present), total_salario)

        return(salaries_wide)
    })

    # Mostrar la tabla
    output$table <- renderDT({
        datatable(process_file(),
                  options = list(
                      paging = TRUE,
                      pageLength = 10
                  )
        )
    })

    # Descargar datos procesados
    output$download <- downloadHandler(
        filename = function() {
            "salaries_processed.xlsx"
        },
        content = function(file) {
            write_xlsx(process_file(), file)
        }
    )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
