library(shiny)
library(flexdashboard)
library(formattable)
library(tidyverse)
library(lubridate)
library(sf)
library(shinythemes)
library(plotly)

# tabela com as atividades fim das empresas
cnae = qsacnpj::tab_cnae
cnae$cod_cnae = as.integer(cnae$cod_cnae)

df = read.csv("file_dash.csv")|>
  dplyr::left_join(cnae, by = c("cnae_fiscal_principal" = "cod_cnae" ))|>
  dplyr::mutate(
    Região = ifelse(id_municipio == 2211001, "Capital", "Interior"),
    Censura = ifelse(situacao_cadastral == 2, 0, 1),
    Periodo = ifelse(data_inicio_atividade != data_situacao_cadastral &
                       data_situacao_cadastral != 2,
                     lubridate::interval(data_inicio_atividade, 
                                         data_situacao_cadastral)/months(1),
                     lubridate::interval(data_inicio_atividade, 
                                         '2022-12-31')/months(1)),
    Periodo = round(Periodo, 0),
    Ano = lubridate::year(data_situacao_cadastral))|>
  dplyr::filter(is.na(Periodo) == FALSE,
                is.na(nm_classe) == FALSE,
                Periodo >=1)


#df$Censura[df$situacao_cadastral == "8" & df$data_situacao_cadastral == "2008-12-31"] = 0
#df$Censura[df$situacao_cadastral == "8" & df$data_situacao_cadastral == "2018-02-01"] = 0
#df$Censura[df$situacao_cadastral == "8" & df$data_situacao_cadastral == "2015-02-09"] = 0

# tabela com as empresas por classe de atividade
df_tipos = df|>
  dplyr::filter(situacao_cadastral == 8)|>
  group_by(nm_classe)|>
  summarise(Empresas = n(), "Média mensal" = round(mean(Periodo),2),
            "Média anual"= c(round(`Média mensal`/12,2)),  Mediana = median(Periodo))|>
  dplyr::rename(Classes = nm_classe)|>
  dplyr::arrange(desc(Empresas))


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
                  choices = c("Piauí", sort(unique(df$Região)))
      ),
      selectInput('ano', 
                  'Ano inicial para análise:', 
                  choices = c(seq(1950, 2020, 10))
      ),
      selectInput('fim', 
                  'Ano final para análise:', 
                  choices = c(2022,seq(1950, 2021, 1))
      ),
      checkboxInput(
        'conf_int',
        'Intervalo de Confiaça no Gráfico',
        value = FALSE
      ),
      #checkboxGroupInput(
       # 'list_dist',
        #'Distribuições:',
        #list_distribution,
        #selected  = list_distribution),
      
      tags$p(HTML("<b>Informações do banco de Dados:</b>")),
      tags$p(HTML("Os dados utilizados foram coletados no
                  site da Receita Federal. Para compreender melhor sobre o assunto, 
                  acesse <a href='https://rpubs.com/filipe26/sobrevivencia_pi'>aqui!</a>")),
      tags$p(HTML("O primeiro painel contém a tabela Empresas que consiste numa relação da 
                  quantidade de empresas por tipo de atividade e 3 medidas descritivas")),
      tags$p(HTML("O segundo painel consiste na distribuição de Kaplan-Meier adaptada para esta pesquisa e 
                  no Gráfico de sobrevivênvia contém a mesma tabela, mas de forma gráfica")),
      tags$p(HTML("Por último, temos a distinção, entre a capital e o interior, de tempo das empresas")),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Empresas",
          DT::dataTableOutput("tabela_tipo")
                ),
        tabPanel(
          title = "Kaplan-Meier",
          DT::dataTableOutput("tabela_kme")
                ),
        tabPanel(
          title = "Gráfico de Sobrevivência ",
          plotOutput("grafico_sobrevivência")
                ),
        tabPanel(
         title = "Comparação Capital e Interior",
         plotOutput("cap_int")
               )
        #,tabPanel(
         # title = "Cidades de fechamento",
          #plotlyOutput("city_closed")
                #)
        #,tabPanel(
         # title = "Comparação das distribuições",
        #  plotly::plotlyOutput("dist_survive"))
          
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
        df[(df$nm_classe == input$tipo_empresas & df$Ano >= input$ano & df$Ano <= input$fim),]
     }else{
        df[(df$nm_classe == input$tipo_empresas & df$Ano >= input$ano & df$Ano <= input$fim) & df$Região == input$regiao,]
     }
   })

   output$tabela_tipo = DT::renderDataTable({
                        DT::datatable(df_tipos, options = list(orderClasses = TRUE))
   })
  
   km = reactive({
    survival::survfit(survival::Surv(Periodo,Censura) ~ 1, data = selectedData())
   })
   

   output$tabela_kme = DT::renderDataTable({
                       DT::datatable(
                                    data.frame(
                                   "Empresas em risco" = c(km()[[3]]),
                                   "Tempo(Mês)" = c(km()[[2]]),
                                   'Tempo(Anos)' = c(round(km()[[2]]/12,2)),
                                   "Empresas fechadas" = c(km()[[4]]),
                                    Sobrevivência = c(round(km()[[6]],4)),
                                    check.names = FALSE), filter = 'top')
  })
   
   output$grafico_sobrevivência = renderPlot({survminer::ggsurvplot(km(),
                                data = selectedData(),  conf.int=input$conf_int,ggtheme = theme_bw(),
                                legend.title = sprintf("Gráfico para % s no(a) % s",
                                                       input$tipo_empresas, 
                                                       input$regiao))})
   
   
   km2 = reactive({
     survival::survfit(survival::Surv(Periodo,Censura) ~ Região, data = selectedData())
   })
   
   output$cap_int = renderPlot({survminer::ggsurvplot(km2(),  data = selectedData(),
                                                      conf.int=input$conf_int,ggtheme = theme_bw(),
                                                      legend.title = sprintf("Gráfico Comparativo Capital e Interior de % s",input$tipo_empresas))})
   
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
    
   #output$table_survive2 = renderPlotly({ })
   
  # output$dist_survive = renderPlotly({
   #    plotly::plot_ly(data_survive(), x = ~Probabilidades, y = ~KME, color = ~Distribuições, type = 'scatter', mode = 'dots')|>
    #   plotly::add_segments(x = 0, xend = 1, y = 0, yend = 1, line = list(color = 'black'),
     #                       inherit = FALSE, showlegend = FALSE)
     #})
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)
