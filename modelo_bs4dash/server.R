#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard) # Você precisará instalar

library(tidyverse)
library(slider)
library(distcrete)
library(epitrix)
library(incidence)
library(projections)
library(EpiEstim)
#----- faltava
library(rgdal)     # Manipula dados de formatos geográficos
library(dplyr)
#-------------     adicionado ao projeto
library(leaflet)   # Visualizar mapas com Leaflet 
library(plotly)   
#-------------

source("./utils/helpers.R") # Carregar funcoes uteis

# Abrir banco de dados
if (!"covid19" %in% ls()){
    covid19 <- fetch_data_brasil_io(use_cached_data = TRUE)   
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    #---------------------------------------------------------------------
    output$ultimadata <- renderbs4InfoBox({

        codigo_da_cidade <- get_city_code(input$city)   
        
        ultima_data <- 
            covid19 %>% 
            filter(is_last, city_ibge_code == codigo_da_cidade) %>% 
            select(last_available_date )

        
        bs4InfoBox(
            title = "Última Observação",
            value =format.Date(ultima_data$last_available_date,"%d/%m/%y") ,
            status = "primary",
            icon = "calendar-alt",
            gradientColor = "success"
        )
    })
    #--------------------------------------------------------------------- 
    output$numeroCasosAcumulados <- renderbs4InfoBox({
      codigo_da_cidade <- get_city_code(input$city)

      casos_acumulados <- 
        covid19 %>% 
        filter(is_last, city_ibge_code == codigo_da_cidade) %>% 
        select(last_available_confirmed)
      
      bs4InfoBox(title="Casos acumulados", 
                 status = "primary",
                 icon = "line-chart",
                 value=formatC(casos_acumulados$last_available_confirmed,
                      digits = 12,
                      big.mark = ".",
                      decimal.mark = ","),
                 gradientColor = "success"
      )
    })
    #---------------------------------------------------------------------  
    output$numeroCasosUltimoDia <- renderbs4InfoBox({
      codigo_da_cidade <- get_city_code(input$city)
      
      casos_ultimo_dia <- 
        covid19 %>% 
        filter(is_last, city_ibge_code == codigo_da_cidade) %>% 
        select(new_confirmed)
      
      bs4InfoBox(title="Casos nas últimas 24h ", 
                 status = "primary",
                 value =formatC(casos_ultimo_dia$new_confirmed,
                      digits = 12,
                      big.mark = ".",
                      decimal.mark = ","),
                      icon = "clock" ,
                      gradientColor = "success"
      )

    })
    #---------------------------------------------------------------------
    output$numeroReproEfetivo <- renderbs4InfoBox({
      codigo_da_cidade <- get_city_code(input$city)
      
      covid19_efetivo <- read_csv("https://data.brasil.io/dataset/covid19/caso_full.csv.gz") 
      
      data_cidade <- 
        covid19_efetivo %>% 
        filter(city_ibge_code == codigo_da_cidade) %>% 
        select(new_confirmed, date, city_ibge_code) 
      
      r_eff <-get_growth_estimates(data_cidade)
        
      mean_reff <- tail(r_eff$R$`Mean(R)`,1)
      
      bs4InfoBox(
        value = formatC(mean_reff,
                        digits = 3,
                        big.mark = ".",
                        decimal.mark = ","),
        title = "Número de Reprodução Efetivo",
        icon = icon("registered"),
        color = colorize_info(mean_reff)
      )
    })
    #---------------------------
    
    output$Gauge1 <- flexdashboard::renderGauge({
      gauge(60, min = 0, max = 100, symbol = "%") 
    })  
    
    #----------------------------------------------------------------------
    output$mapa <- renderLeaflet({

      # paleta de cores tirada de https://data.library.virginia.edu/setting-up-color-palettes-in-r/
        
      pal <- colorNumeric("Reds", NULL)
      
      
      codigo_da_cidade   <- get_city_code(input$city) 
      codigo_posicao_1_2 <- substr(codigo_da_cidade, start = 1, stop = 2)
      
      geolocalizacao     <- read_csv("data/base_geolocalizacao_br.csv")
      
      geolocalizacao %>% filter(codigo_ibge == codigo_da_cidade)
      
      cidade_selecionada_geo <- 
        geolocalizacao %>% 
        filter(codigo_ibge == codigo_da_cidade)     
      
      # Adicionar divisas das cidades, marcadores e informações no mapa.
      # Ação - Identificar padrões na pasta 'started_template/data/geoson'

      covid19 %>% 
        filter(is_last & 
                 grepl(sprintf("^%s",codigo_posicao_1_2), city_ibge_code))
      
      # Combinar bases de dados da COVID19 e geolocalizacao (latitude e longitude)
      dados_cidade_selecionada <- 
        covid19 %>% 
        filter(is_last & 
                 grepl(sprintf("^%s",codigo_posicao_1_2), city_ibge_code)) %>% 
        left_join(., 
                  geolocalizacao, 
                  by = c("city_ibge_code" = "codigo_ibge"))

      caminho_shapefile <- sprintf("data/geoson/geojs-%s-mun.json",codigo_posicao_1_2)
      
      shapefile <- readOGR(caminho_shapefile)
      glimpse(shapefile)
      
      
      # Join colunas do banco de dados da covid19 com shapefiles
      shapefile@data$id <- as.numeric(shapefile@data$id)
      
      shapefile@data <- 
        left_join(shapefile@data,
                  dados_cidade_selecionada, 
                  by = c("id" = "city_ibge_code"))

      # Adicionar cidade ao centro do mapa
      
      leaflet(shapefile) %>% 
        addProviderTiles(providers$Stamen.TonerLite) %>% 
        setView(lng = cidade_selecionada_geo$longitude,
                lat = cidade_selecionada_geo$latitude, 
                zoom = 11) %>% 
        addPolygons(stroke = FALSE,
                    smoothFactor = .5,
                    fillOpacity = .4,
                    fillColor = ~pal(log(new_confirmed + 1e-2)),
                    label = ~paste0(name, 
                                    ": ",
                                    formatC(new_confirmed,
                                            big.mark = ","))) %>% 
        addAwesomeMarkers(~longitude, 
                          ~latitude, 
                          icon = makeAwesomeIcon(icon = "",
                                                 text = "", 
                                                 markerColor = "cadetblue"),
                          popup = 
                            sprintf("<h2><b>%s</b></h2>
                                   <p><b>Última atualização:</b> %s</p>
                                   <p><b>Última observação:</b> %s</p>
                                   <p><b><h4>Resultado Últimas 24h</h4></b></p>
                                   <p>Casos<b>: %0.f</b></p>
                                   <p>Mortes<b>: %0.f</b></p>
                                   ", 
                                    shapefile@data$nome,
                                    format.Date(shapefile@data$date, "%d/%m/%Y"),
                                    format.Date(shapefile@data$last_available_date, "%d/%m/%Y"),
                                    shapefile@data$new_confirmed,
                                    shapefile@data$new_deaths
                            ),
                          clusterOptions = markerClusterOptions()   
        )
      
    }) 
    
    #---------------------------------------------------------------------
    output$analise_simples <- renderPlotly({
    
    codigo_da_cidade <- get_city_code(input$city) 
      
    dados_analise_mortos <- 
        covid19 %>%
        filter(city_ibge_code == codigo_da_cidade) %>% 
        select(date, new_deaths ) 
    
    names(dados_analise_mortos)[names(dados_analise_mortos) == "new_deaths"] <- "casos"
    
    dados_analise_mortos$tipo <- c("Mortos")
    
    dados_analise_conf <- 
      covid19 %>%
      filter(city_ibge_code == codigo_da_cidade) %>% 
      select(date,new_confirmed ) 
    
    dados_analise_conf$tipo <- c("Confirmados")
    
    names(dados_analise_conf)[names(dados_analise_conf) == "new_confirmed"] <- "casos"
    
    dados_final <- union(dados_analise_mortos,  dados_analise_conf)
    
    p <- ggplot(data=dados_final, aes(x=date, y=casos, group=tipo, colour=tipo)) +
         geom_line() +
         geom_point() +
         labs(title = "Casos Confirmados de infecção e Mortos") +
         scale_y_sqrt() + geom_smooth()
    
    ggplotly(p, dynamicTicks = TRUE)   %>%  rangeslider()   %>% layout(hovermode = "x")

    })
    
    
    #---------------------------------------------------------------------
    output$grafico_r_efetivo <- renderPlotly({
      
      
      # Parâmetros usados para estimar Número de Reprodução
      # [Média e DP do intervalo serial, tempo estimado em dias para o contágio de uma
      # pessoa doente]
      mu <- 7.5
      sigma <- 3.4
      pred_n_days <- 10
      
      # Carregar banco de dados -------------------------------------------------
      # Baixar dados da API Brasil IO
      # OBS: Pode demorar um pouco dependendo de sua conexao
      
      if (!"covid19" %in% ls()){
        covid19_r_efetivo <- fetch_data_brasil_io(use_cached_data = TRUE)   
      }
      
      
      codigo_da_cidade <- get_city_code(input$city) 
      
      # Transformar dados no formato incidência
      data_incidence_function_data <- 
        covid19_r_efetivo %>%
        filter(city_ibge_code == codigo_da_cidade) %>% 
        select(date, new_deaths) %>% 
        uncount(new_deaths)
      
      data_incidence_object <- incidence(data_incidence_function_data$date)
      
      
      # Estimar R Efetivo usando o Pacote EpiEstim
      t_start <- seq(2, nrow(data_incidence_object$counts) - 8)
      t_end <- t_start + 8
      effective_r <- estimate_R(data_incidence_object, method = "parametric_si", 
                                config = make_config(list(mean_si = mu, std_si = sigma,
                                                          t_start = t_start, 
                                                          t_end = t_end)))
      
      # Criar gráfico plotly do R Efetivo
      r_effective_chart <- 
        effective_r$R %>% 
        ggplot(aes(x = data_incidence_object$dates[10:length(data_incidence_object$dates)], 
                   y = `Mean(R)`)) + 
        geom_ribbon(aes(ymin = `Mean(R)` - 2*`Std(R)`,
                        ymax = `Mean(R)` + 2*`Std(R)`), 
                    fill = "grey60") + 
        geom_line(size = 1.2) + 
        scale_y_sqrt() + 
        geom_hline(yintercept = 1, linetype = "dashed", color = "navy") +
        xlab("") + ylab("") +
        labs(title = "Estimativa Reprodução Efetiva") 
      
      ggplotly(r_effective_chart)

    })
    #---------------------------------------------------------------------
    output$tabela <- renderDataTable({
      
      if (!"covid19" %in% ls()){
        covid19 <- fetch_data_brasil_io(use_cached_data = TRUE)   
      }
      
      covid19
      
    })
})
