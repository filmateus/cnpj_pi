library(shiny)
library(flexdashboard)
library(formattable)
library(tidyverse)
library(lubridate)
library(sf)
library(shinythemes)
library(plotly)

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
  dplyr::filter(identificador_matriz_filial == 1)|>
  left_join(cnae, by = c("cnae_fiscal_principal" = "cod_cnae" ))|>
  dplyr::mutate(
    Censura = ifelse(data_situacao_cadastral == '2018-02-01'| 
                    data_situacao_cadastral == '2015-02-09'|
                    data_situacao_cadastral == "2008-12-31", 0, 1),
    Censura = ifelse(data_situacao_cadastral != 8 & 
                       data_inicio_atividade == data_situacao_cadastral, 0, 1),
    Periodo = ifelse(data_inicio_atividade != data_situacao_cadastral &
                     data_situacao_cadastral != 8,
        lubridate::interval(data_inicio_atividade, data_situacao_cadastral)/months(1),
        lubridate::interval(data_inicio_atividade, '2022-12-31')/months(1)),
    Periodo = round(Periodo, 0),
    Região = ifelse(id_municipio == 2211001, "Capital", "Interior"),
    Ano = lubridate::year(data_situacao_cadastral))|>
  dplyr::filter(is.na(Periodo) == FALSE,
                is.na(nm_classe) == FALSE,
                Periodo >=1)|>
  dplyr::select(cnpj_basico, nm_classe, Periodo, Censura, Região, Ano)


list_distribution = c("exponential", "weibull", "lognorm", "gompertz")


# survive distribution
exp_surv = function(time, alpha){
  x = exp(-time*alpha)
  return(x)
}

weibull_surv = function(time, alpha_w, beta_w){
  x = exp(-(time/alpha_w)^beta_w)
  return(x)
}

log_normal_surv = function(time, mean_log, sd_log){
  x = pnorm(-(log(time)-mean_log)/sd_log)
  return(x)
}

gompertz_surv = function(time, alpha_g, beta_g){
  x = exp(-(beta_g/alpha_g)*(exp(alpha_g*time)-1))
  return(x)
}



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
      checkboxGroupInput(
        'list_dist',
        'Distribuições:',
        list_distribution,
        selected  = list_distribution),
      
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
                ),
        tabPanel(
         title = "Saida Distribuições",
         verbatimTextOutput("table_survive")
               ),
        tabPanel(
          title = "tabela das distribuições",
          verbatimTextOutput("table_survive2")
                ),
        tabPanel(
          title = "Comparação das distribuições",
          plotly::plotlyOutput("dist_survive"))
          
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

server <- function(input, output, session){
   selectedData <- reactive({
      if(input$regiao == 'Piauí'){
        df[(df$nm_classe == input$tipo_empresas),]
     }else{
        df[(df$nm_classe == input$tipo_empresas) & df$Região == input$regiao,]
     }
   })
  
   km = reactive({
    survival::survfit(survival::Surv(Periodo,Censura) ~ nm_classe, data = selectedData())
   })
   

   output$tabela_kme = renderPrint({list(
                                 "Medidas" = km(),
                                 "Tabela"  =  summary(km())
                                        )
  })
  
   output$grafico_sobrevivência = renderPlot({survminer::ggsurvplot(km(), 
                                data = selectedData(),  conf.int=input$conf_int, ggtheme = theme_bw(),
                                legend.title = sprintf("Gráfico para % s no(a) % s", input$tipo_empresas, input$regiao))})
   
   
   dist_param = reactive({
     estatics = list()
        for(i in list_distribution){
          estatics[[i]] = flexsurv::flexsurvreg(survival::Surv(Periodo,Censura) ~ 1, 
                                               data = selectedData(), dist = i)
           
       
     }
     estatics
})
   
   output$table_survive = renderPrint({
      print(dist_param())
     
    })
   
   data_survive = reactive({
            data.frame(Tempo = c(km()$time),
                  KME =  c(km()$surv),
                  exponential = c(exp_surv(km()$time, dist_param()[[1]][18]$res[1])),
                  weibull = c(weibull_surv(km()$time, dist_param()[[2]][18]$res[2],
                                                      dist_param()[[2]][18]$res[1])),
                  lognorm = c(log_normal_surv(km()$time, dist_param()[[3]][18]$res[1],
                                                        dist_param()[[3]][18]$res[2])),
                  gompertz = c(gompertz_surv(km()$time, dist_param()[[4]][18]$res[1],
                                                        dist_param()[[4]][18]$res[2])))|>
          tidyr::pivot_longer(cols = 3:6, names_to = "Distribuições",
                                          values_to = "Probabilidades")
     })
    
   output$table_survive2 = renderPrint({
     data_survive()
    })
   
   output$dist_survive = renderPlotly({
       plotly::plot_ly(data_survive(), x = ~Probabilidades, y = ~KME, color = ~Distribuições, type = 'scatter', mode = 'dots')|>
       plotly::add_segments(x = 0, xend = 1, y = 0, yend = 1, line = list(color = 'black'),
                            inherit = FALSE, showlegend = FALSE)
     })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)
