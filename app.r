source("helpers.R")

inst_dependencies()

library(shiny)
library(pdftools)
library(tidyverse)
library(shinythemes)

ui <- navbarPage(theme = shinytheme("sandstone"), "PDF to Table app",
  
  tabPanel("Principal",
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Para prosseguir, selecione as opções de acordo com o tipo de exame e aparelho, 
               número de PDFs a serem convertidos, e então insira seu PDF utilizando a barra de pesquisa."),
      
      br(),
      
      radioButtons("scan_machine", label="Selecione o tipo de aparelho no qual o exame foi feito", 
                   choices=list("Lunar iDXA - GE Healthcare (FMUSP)" = "fmusp",
                             "Horizon DXA - Hologic (IOT)" = "iot"), selected = character(0)),
      
      br(),
      
      conditionalPanel(
        condition = "input.scan_machine == 'fmusp'", 
        # selectInput("scan_region_fmusp", "Selecione a região analisada",
        #             c(" " = "none",
        #               "Fêmur" = "femur",
        #               "Coluna" = "spine",
        #               "Corpo total" = "wb",
        #               "Antebraço" = "forearm"), selected = character(0))
        strong("Não temos suporte para os exames do Lunar iDXA ainda!"),
        br(),
        br()
      ),
      
      conditionalPanel(
        condition = "input.scan_machine == 'iot'", 
        selectInput("scan_region_hologic", "Selecione a região analisada",
                    c(" " = "none",
                      "Fêmur" = "femur",
                      "Coluna" = "spine",
                      "Corpo total" = "wb",
                      "Antebraço" = "forearm",
                      "Composição Corporal (1 Tabela)" = "comp"),
                     # "Composição Corporal (3 Tabelas)" = "comp_2"), removed for the time being due to being tricky
                    selected = character(0))
      ),
      
      conditionalPanel(
        condition = "input.scan_region_hologic != 'none'",
        br(),
        radioButtons(
          "df_shape", "Formato da planilha:",
          choices=c("Original (assim como é no exame)" = "original",
            "'Wide' (uma coluna por variável [Recomendado])" = "wide"),
          selected=character(0)
        )
      ),
      
      conditionalPanel(
        condition = "input.df_shape == 'original' | input.df_shape == 'wide'",
        br(),
        fileInput("file1", label="Selecione seu(s) PDF(s)",
                  multiple = TRUE,
                  accept = "pdf")
      ),
      
      helpText(paste("Gabriel P. Esteves, ", format(Sys.Date(), "%Y"), ".", sep="")),
      helpText("gabriel.perri.esteves@usp.br")
      ),
    
    mainPanel(
    
    conditionalPanel(
      condition = "output.fileUploaded == true",
      h3("Tabela final:"),
      tableOutput("table"),
      strong("Baixe seus resultados para Excel:"),
      br(),
      br(),
      downloadButton("downloadTable"),
      
      conditionalPanel(condition = "input.scan_region_hologic == 'comp_2'",
                       h3("Tabela de Índices"),
                       tableOutput("table_plus"),
                       strong("Baixe os resultados da tabela de índices para Excel:"),
                       br(),
                       br(),
                       downloadButton("downloadTable_plus"))
      
      )
    )
   )
  ),
  QA_tab # See helpers.R file
)

server <- function(input, output) {
  
  final_table <- reactive({
    if(is.null(input$file1)) return(NULL)
    req(input$file1)
    raw <- purrr::map(input$file1$datapath, pdf_text) |> 
      purrr::set_names(input$file1$name)
    final_table_mult <- if (input$scan_region_hologic == "femur") {
      map_df(raw, clean_femur)
    } else if (input$scan_region_hologic == "spine") {
      map_df(raw, clean_spine)
    } else if (input$scan_region_hologic == "wb") {
      map_df(raw, clean_wb)
    } else if (input$scan_region_hologic == "forearm") {
      map_df(raw, clean_forearm)
    } else if (input$scan_region_hologic == "comp") {
      map_df(raw, clean_comp)
    } else if (input$scan_region_hologic == "comp_2") {
      map_df(raw, clean_comp_2_t1)
    }

    if (input$df_shape == "wide") {
      if (input$scan_region_hologic == "comp") {
        final_table_mult <- final_table_mult |> 
          mutate(across(CMO:Porcentagem_gordura, as.numeric)) |>
          pivot_longer(cols=c(CMO:Porcentagem_gordura), names_to = "variable", values_to = "value") |>
          pivot_wider(names_from=c("Região", "variable"), values_from="value")
      } else if (input$scan_region_hologic == "wb") {
        final_table_mult <- final_table_mult |> 
          mutate(across(`Área`:AM, as.numeric)) |>
          pivot_longer(cols=c(`Área`:AM), names_to = "variable", values_to = "value") |>
          pivot_wider(names_from=c("Região", "variable"), values_from="value") |> 
          select(everything(), -ends_with(c("_zscore", "_AM")), "Total_zscore", "Total_AM")
      } else {
        final_table_mult <- final_table_mult |>
          mutate(across(area:AM, as.numeric)) |>
          pivot_longer(cols=c(area:AM), names_to = "variable", values_to = "value") |>
          pivot_wider(names_from=c("regiao", "variable"), values_from="value")
      }
    } else {
      final_table_mult
    }
  })
  
  final_table_plus <- reactive({
    if(is.null(input$file1)) return(NULL)
    req(input$file1)
    raw <- purrr::map(input$file1$datapath, pdf_text) |> 
      purrr::set_names(input$file1$name)
    final_table_mult <- if (input$scan_region_hologic == "comp_2") {
      map_df(raw, clean_comp_2_t2)
    } else return(NULL)
  })

  output$table <- renderTable(final_table())
  
  output$table_plus <- renderTable(final_table_plus())

  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("dxa-table-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      xlsx::write.xlsx(final_table(), file)
    }
  )
  
  output$downloadTable_plus <- downloadHandler(
    filename = function() {
      paste("dxa-table-indexes-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      xlsx::write.xlsx(final_table_plus(), file)
    }
  )
  
  output$fileUploaded <- reactive({
    return(!is.null(input$file1))
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
}

shinyApp(ui, server)