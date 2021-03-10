library(shiny)
library(bs4Dash)
library(readr) 
library(shiny)
library(leaflet)
library(plotly)
library(flexdashboard)

# Carrega a lista de cidades
city_options <- read_lines("data/city_selector.gz")



# Define a interface gráfica para aplicação Shiny
# Documentação para consulta: 
#   bs4Dash - https://rinterface.github.io/bs4Dash/articles/bs4Intro.html  

shinyUI(
    bs4DashPage(
        old_school           = FALSE,
        sidebar_mini         = TRUE,
        sidebar_collapsed    = TRUE,
        controlbar_collapsed = TRUE,
        controlbar_overlay   = FALSE,
        title = "Trabalho Final - COVID 19",
        navbar = bs4DashNavbar(
            selectInput("city",
                        label = "Selecione uma cidade", 
                        choices = city_options,
                        selected = city_options[0]
            )
            #bs4ValueBoxOutput("ultimadata") ,
            #column(3,flexdashboard::gaugeOutput("Gauge1")),
            #column(3,flexdashboard::gaugeOutput("Gauge2")) 
        ),
        sidebar = bs4DashSidebar(
            title = "KSN",
            elevation = 1,
            brandColor = "white",
            src = "topdown.png",
            bs4SidebarMenu(
                bs4SidebarHeader("COVID 19 - Estatísticas"),
                bs4SidebarMenuItem(
                    "Evolução",
                    tabName = "item1",
                    icon = "bar-chart"
                     
                ),        
                 bs4SidebarMenuItem(
                    "Análise Geográfica",
                    tabName = "item2",
                    icon = "map"
                ),

                bs4SidebarMenuItem(
                    "Dados Básicos",
                    tabName = "item3",
                    icon = "list"
                )
            )
        ),
        footer = bs4DashFooter(
            copyrights = "Trabalho Final - Curso de Dashboards com Shiny - Kleber S. Nunes",
            right_text = "2021"
        ),
        body = bs4DashBody(
            bs4TabItems(
            
               bs4TabItem(tabName = "item1",
                           fluidRow(
                               #column(1,gaugeOutput("Gauge1")), 
                               plotlyOutput("grafico_r_efetivo", height = "200px"),
                               plotlyOutput("analise_simples", height = "400px"),
                               
                           )),           
                bs4TabItem(tabName = "item2",
                           fluidRow(
                               bs4InfoBoxOutput("ultimadata"),
                               bs4InfoBoxOutput("numeroCasosAcumulados"),
                               bs4InfoBoxOutput("numeroCasosUltimoDia"), 
                               leafletOutput("mapa", height = "450px"),
                           )),

                bs4TabItem(tabName = "item3",
                           fluidRow(
                               
                               bs4Alert(
                                   title = "Atenção",
                                   status = "warning",
                                   closable = TRUE,
                                   elevation = 4,
                                   "Aguarde a carga da tabela, após finalizada fechar a janela"
                               ),
                               
                               dataTableOutput("tabela"),
                           ))
            )
        )
    )
)

