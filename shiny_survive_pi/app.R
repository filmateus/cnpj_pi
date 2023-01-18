library(shiny)
library(flexdashboard)
library(formattable)
library(tidyverse)
library(lubridate)
library(sf)
library(shinythemes)

# tabelas com os códigos e tipos de empresas - 
nat_jud = qsacnpj::tab_natureza_juridica
nat_jud$cod_subclass_natureza_juridica = as.numeric(nat_jud$cod_subclass_natureza_juridica)

# carregando tabela com as empresas filtrando para empresas que fecharam após 2009
empresas = read.csv('empresas_pi.csv')|>
  dplyr::left_join(nat_jud, 
                   by = c("natureza_juridica" = "cod_subclass_natureza_juridica"))|>
  dplyr::filter(nm_natureza_juridica %in% c("Entidades Empresariais"))|>
  dplyr::distinct()

# tabela com as atividades fim das empresas
cnae = qsacnpj::tab_cnae
cnae$cod_cnae = as.integer(cnae$cod_cnae)

# tabela final com as informações principais
df =  read.csv("estabelecimentos_pi.csv")|>
  dplyr::semi_join(empresas, by = "cnpj_basico")|>
  dplyr::filter(situacao_cadastral == 8, 
                identificador_matriz_filial == 1, 
                data_situacao_cadastral > '2009-12-31')|>
  left_join(cnae, by = c("cnae_fiscal_principal" = "cod_cnae" ))|>
  dplyr::mutate(
    censura = ifelse(data_situacao_cadastral == '2018-02-01' | 
                       data_situacao_cadastral == '2015-02-09', 0, 1),
    Periodo = lubridate::interval(data_inicio_atividade, 
                                  data_situacao_cadastral)/years(1),
    Periodo = round(Periodo, 0),
    Região = case_when(id_municipio == 2211001 ~ "Capital",
                       id_municipio != 2211001 ~ "Interior"))|>
  dplyr::select(cnpj_basico, nm_classe, Periodo, censura, Região)


about_page <- tabPanel(
  title = "Sobre",
  titlePanel("Sobre"),
  "Created with R Shiny",
  br(),
  "Janeiro/2023"
)


main_page = tabPanel(
  title = "Empresas",
  titlePanel("Empresas"),
  sidebarLayout(
    sidebarPanel(
     
              
      selectInput('tipo_empresas', 
                  'Tipo de empresa:', 
                  choices = c(sort(unique(df$nm_classe), decreasing = FALSE))
      ),
      selectInput('regiao', 
                  'Região:', 
                  choices = c("Piauí", sort(unique(df$Região),  decreasing = FALSE))
      ),
      checkboxInput(
        'conf_int',
        'Intervalo de Confiaça no Gráfico',
        value = FALSE
      ),
      tags$p(HTML("<b>Informações do banco de Dados:</b>")),
      tags$p(HTML("Os dados utilizados foram coletados no
                  site da Receita Federal")) 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Tabela Kaplan-Meier",
          verbatimTextOutput("tabela_kme")
    ), 
      tabPanel(
        title = "Gráfico de Sobrevivência ",
        plotOutput("grafico_sobrevivência")
       )
      )
    )
  )
)

ui <- navbarPage(

      title = "Análise de Sobrevivência das Empresas no Piauí",
      theme = shinytheme('simplex'),
      main_page,
      about_page
)

server <- function(input, output){
   selectedData <- reactive({
      if(input$regiao == 'Piauí'){
        df[(df$nm_classe == input$tipo_empresas),]
     }else{
        df[(df$nm_classe == input$tipo_empresas) & df$Região == input$regiao,]
     }
   })
  
   km = reactive({
    survival::survfit(survival::Surv(Periodo,censura) ~ nm_classe, data = selectedData())
    })
  
   output$tabela_kme = renderPrint({
    summary(km())
  })
  
   output$grafico_sobrevivência = renderPlot({survminer::ggsurvplot(km(), 
                                data = selectedData(),  conf.int=input$conf_int, ggtheme = theme_bw(),
                                legend.title = sprintf("Gráfico para % s no(a) % s", input$tipo_empresas, input$regiao))})
  
}
# Run the application 
shinyApp(ui = ui, server = server)
