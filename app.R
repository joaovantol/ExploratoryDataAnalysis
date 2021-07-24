library(shiny)
library(leaflet)
library(spData)
library(tidyverse)
library(maps)
library(sf)
library(dplyr)
library(shinydashboard)
library(rmarkdown)
library(plotly)
library(ineq)

#Leitura e organização dos dados (coletados em https://ourworldindata.org/life-expectancy)

dados <- read.csv("life--expectancy.csv", header = TRUE)
df_median_age <- read.csv("median-age.csv", header = TRUE)
df_child_deaths <- read.csv("child-mortality-around-the-world.csv", header = TRUE)
mortalidade <- read.csv("child-deaths-igme-data.csv", header = TRUE)
filhos <- read.csv("children-born-per-woman.csv", header = TRUE)
pib <- read.csv("maddison-data-gdp-per-capita-in-2011us.csv", header = TRUE)
escola <- read.csv("mean-years-of-schooling-long-run.csv", header = TRUE)
urbano <- read.csv("share-of-population-urban.csv", header = TRUE)
pib <- pib[, -c(5)]

join1 <- full_join(mortalidade, filhos, by = c("Entity" = "Entity", "Year" = "Year", "Code" = "Code"))
join2 <- full_join(join1,dados, by = c("Entity" = "Entity", "Year" = "Year", "Code" = "Code"))
join3 <- full_join(join2,pib, by = c("Entity" = "Entity", "Year" = "Year", "Code" = "Code"))
join4 <- full_join(join3,escola, by = c("Entity" = "Entity", "Year" = "Year", "Code" = "Code"))
join5 <- full_join(join4,df_median_age, by = c("Entity" = "Entity", "Year" = "Year", "Code" = "Code"))
join6 <- full_join(join5,urbano, by = c("Entity" = "Entity", "Year" = "Year", "Code" = "Code"))

colnames(join6) <- c("País","Codigo","Ano","Mortalidade","Filhos", 
                     "Expectativa","PIB", "Escola","Mediana", "Urbana")

data2015 = join6 %>% filter(Ano == 2015)
continentes <- read.csv("all.csv", header = TRUE)
continentes <- continentes[, -c(1,2,4,5,7:11)]
data2015cont <- inner_join(data2015,continentes, by = c("Codigo" = "alpha.3"))

attach(data2015cont)

#Adicionando código Alpha 3 à base de dados world
codes <- read.csv("all.csv", header = TRUE)
codes <- codes[, -c(1,4:11)]
worldcodes <- left_join(world,codes, by = c("iso_a2" = "alpha.2"))
worldcodes[51,12] = "NAM" #Adicionando a Namíbia manualmente

data <- inner_join(worldcodes,dados, by = c("alpha.3" = "Code"))
data_ma <- inner_join(worldcodes,df_median_age, by = c("alpha.3" = "Code"))
data_cd <- inner_join(worldcodes,df_child_deaths, by = c("alpha.3" = "Code"))

data2 <- data[, -c(1,3:10,12,13)]
data_ma2 <- data_ma[, -c(1,3:10,12,13)]
data_cd2 <- data_cd[, -c(1,3:10,12,13)]
data1 = data2 %>% filter(Year >= 1800)

colnames(data_ma2)[4] <- "MedianAge"
colnames(data_cd2)[4] <- "MortalidadeInfantil"

paises <- unique(data1$name_long) %>% sort()

#Organização do dashboard
ui <- dashboardPage(
    
    dashboardHeader(title = "Expectativa de vida"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Mapas", tabName = "mapas", icon = icon("globe-americas")),
            menuItem("Gráficos de linha", tabName = "graficos-linha", icon = icon("chart-line")),
            menuItem("Gráficos de dispersão", tabName = "graficos", icon = icon("chart-bar")),
            menuItem("Curvas de Lorenz", tabName = "lorenz", icon = icon("chart-area")),
            menuItem("Código", tabName = "codigo", icon = icon("file-text-o"),
                     menuSubItem("dados.R", tabName = "dados", icon = icon("angle-right")),
                     menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                     menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))),
            menuItem("Descrição", tabName = "descricao", icon = icon("book-open"))
        )
    ),
    
    dashboardBody(
        tabItems(
            #Mapas
            tabItem(tabName = "mapas",
                    fluidRow(
                        titlePanel(h3("Expectativa de vida, idade média e mortalidade infantil por país ao longo dos anos")),
                        hr(),
                        column(width=6,
                               selectInput("var",
                                           "Var:",
                                           label = ("Selecione a variável:"),
                                           choices = c("Expectativa de vida", 
                                                       "Mediana das idades", 
                                                       "Mortalidade infantil"))),
                        column(width=10,
                               conditionalPanel(
                                   condition = "input.var == 'Expectativa de vida'",
                                   sliderInput("ano1",
                                               "Ano:",
                                               min = min(data1$Year),
                                               max = max(data1$Year),
                                               value = 2019,
                                               sep = "")),
                               conditionalPanel(    
                                   condition = "input.var == 'Mediana das idades'",
                                   sliderInput("ano2",
                                               "Ano:",
                                               min = min(data_ma2$Year),
                                               max = max(data_ma2$Year),
                                               value = 2020,
                                               step = 5,
                                               sep = "")),
                               conditionalPanel(  
                                   condition = "input.var == 'Mortalidade infantil'",
                                   sliderInput("ano3",
                                               "Ano:",
                                               min = min(data_cd2$Year),
                                               max = max(data_cd2$Year),
                                               value = 2017,
                                               step = 1,
                                               sep = ""))),
                        hr(),
                        mainPanel(
                            leafletOutput("mapa")
                        )
                    )
            ),
            #Gráficos de linha
            tabItem(tabName = "graficos-linha",
                    fluidRow(
                        titlePanel(h3("Expectativa de vida por país ao longo dos anos")),
                        column(width=6,
                               selectInput("pais", "Escolha os país(es)", paises, multiple = TRUE)),
                        mainPanel(
                            plotlyOutput("graph")))),
            #Gráficos de dispersão
            tabItem(tabName = "graficos",
                    fluidRow(
                        titlePanel(h3("Relação entre expectativa de vida e outras variáveis para o ano de 2015")),
                        column(width=6,
                               selectInput("var0", "Escolha uma variável dependente:",
                                           choices = c("log(Mortalidade infantil)", "Mediana das idades", "log(PIB per capita)",
                                                       "Escolaridade", "% de população urbana", "Nº de filhos")),
                               column(width=6,
                                      checkboxInput("continente", label = "Agrupar por continente", value = FALSE))
                               
                        ),
                        mainPanel(
                            plotlyOutput("graficodispersao")
                        ))),
            #Curvas de Lorenz
            tabItem(tabName = "lorenz",
                    fluidRow(
                        titlePanel(h3("Curvas de Lorenz: quanto maior a área entre a curva e a reta, maior o grau de desigualdade entre os países do mundo")),
                        column(width=6,
                               selectInput("var8", "Escolha uma variável:",
                                           choices = c("Expectativa de vida","Mortalidade infantil", "Mediana das idades", "PIB per capita",
                                                       "Escolaridade", "% de população urbana", "Nº de filhos"))

                        ),
                        mainPanel(
                            plotOutput("cLorenz")
                        ))),
            #Código
            tabItem(tabName = "codigo",
                    h2("Widgets tab content")),
            #Subitem códigos
            tabItem(tabName = "dados",
                    box(width = NULL, status = "primary", solidHeader = TRUE, title= "dados.R",
                        pre(includeText("dados")))),
            tabItem(tabName = "ui",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title="ui.R",
                         pre(includeText("ui")))),
            tabItem(tabName = "server",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
                         pre(includeText("server")))),
            #Descrição
            tabItem(tabName = "descricao",
                    box( width = NULL, status = "primary", solidHeader = TRUE, title="Descrição",
                         includeMarkdown("descricao.Rmd")))
            
        )))


server <- function(input, output) {
    {
        output$mapa <- renderLeaflet({
            
            #Mapa para expectativa de vida
            if (input$var == "Expectativa de vida"){
                ano <- input$ano1
                bins <- c(15,20,25,35,40,45,50,55,60,65,70,75,80,85)
                data_filtered <- data1 %>% filter(Year %in% ano)
                pal <- colorBin("YlOrRd", domain = data_filtered$Life.expectancy, bins = bins)
                
                labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                  data_filtered$name_long, 
                                  data_filtered$Life.expectancy) %>% lapply(htmltools::HTML)
                
                
                mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                    addPolygons(fillColor = ~pal(Life.expectancy),
                                weight = 2,
                                opacity = 1,
                                color = "black",
                                fillOpacity = 0.7,
                                highlight = highlightOptions(weight = 3,
                                                             color = "black",
                                                             fillOpacity = 0.7,
                                                             bringToFront = TRUE),
                                label = labels,
                                labelOptions = labelOptions(
                                    style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px",
                                    direction = "auto")
                    ) %>%
                    addLegend(pal = pal, 
                              values = ~data_filtered$Life.expectancy, 
                              opacity = 0.7, 
                              title = "Anos",
                              position = "bottomright")
                
                mapa
                
            #Mapa para mediana das idades    
            }else if (input$var == "Mediana das idades"){
                ano <- input$ano2
                bins <- c(10,15,20,25,35,40,45,50,55,60)
                data_filtered <- data_ma2 %>% filter(Year %in% ano)
                pal <- colorBin("YlOrRd", domain = data_filtered$MedianAge, bins = bins)
                
                labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                  data_filtered$name_long,
                                  data_filtered$MedianAge) %>% lapply(htmltools::HTML)
                
                mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                    addPolygons(fillColor = ~pal(MedianAge),
                                weight = 2,
                                opacity = 1,
                                color = "black",
                                fillOpacity = 0.7,
                                highlight = highlightOptions(weight = 3,
                                                             color = "black",
                                                             fillOpacity = 0.7,
                                                             bringToFront = TRUE),
                                label = labels,
                                labelOptions = labelOptions(
                                    style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px",
                                    direction = "auto")
                    )%>%
                    addLegend(pal = pal, values = ~data_filtered$MedianAge, opacity = 0.7, title = "Anos",
                              position = "bottomright")
                
                mapa
                
            #Mapa para mortalidade infantil        
            }else if (input$var == "Mortalidade infantil") {
                ano <- input$ano3
                bins <- c(0,5,10,15,20,25,30,35,40,45,50)
                data_filtered <- data_cd2 %>% filter(Year %in% ano)
                pal <- colorBin("YlOrRd", domain = data_filtered$MortalidadeInfantil, bins = bins)
                
                labels <- sprintf("<strong>%s</strong><br/>%g <sup></sup>",
                                  data_filtered$name_long,
                                  data_filtered$MortalidadeInfantil) %>% lapply(htmltools::HTML)
                
                mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                    addPolygons(fillColor = ~pal(MortalidadeInfantil),
                                weight = 2,
                                opacity = 1,
                                color = "black",
                                fillOpacity = 0.7,
                                highlight = highlightOptions(weight = 3,
                                                             color = "black",
                                                             fillOpacity = 0.7,
                                                             bringToFront = TRUE),
                                label = labels,
                                labelOptions = labelOptions(
                                    style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px",
                                    direction = "auto")
                    )%>%
                    addLegend(pal = pal, values = ~data_filtered$MortalidadeInfantil, opacity = 0.7, title = "% de crianças",
                              position = "bottomright")
                
                mapa
                
            }
        })
    }
    #Gráficos de linhas
    {
        output$graph<- renderPlotly({
            pais <- input$pais
            data_c = data1 %>% filter(name_long %in% pais)
            x <- list(title = "Ano")
            y <- list(title = "Expectativa de vida")
            plot_ly(data_c, x = ~Year, y = ~Life.expectancy, color = ~name_long, type = 'scatter', mode = 'lines') %>% 
                add_markers() %>%
                layout(xaxis = x, yaxis = y, width = 700, height = 500)
        })
    }
  
  #Gráficos de dispersão
  {
    output$graficodispersao <- renderPlotly({
      
      if (input$var0 == "log(Mortalidade infantil)") {var0 <- log(Mortalidade)}
      else if (input$var0 == "Mediana das idades") {var0 <- Mediana}
      else if (input$var0 == "log(PIB per capita)") {var0 <- log(PIB)}
      else if (input$var0 == "Escolaridade") {var0 <- Escola}
      else if (input$var0 == "% de população urbana") {var0 <- Urbana}
      else if (input$var0 == "Nº de filhos") {var0 <- Filhos}
      
      label <- input$var0
      x <- list(title = "Expectativa de vida")
      y <- list(title = label)
      
      if (input$continente == FALSE) {
        plot_ly(data = (data2015cont), x = ~Expectativa, y = ~var0, color = ~País, colors = "Set1",
                type = 'scatter') %>%
          layout(xaxis = x, yaxis = y)
      }
      
      else if (input$continente == TRUE) {
        plot_ly(data = (data2015cont), x = ~Expectativa, y = ~var0, color = ~region, colors = "Set1",
                type = 'scatter') %>%
          layout(xaxis = x, yaxis = y)
      }
    })}
  
    #Curvas de Lorenz
    {
        output$cLorenz <- renderPlot({

            if (input$var8 == "Expectativa de vida") {plot(Lc(Expectativa), main='Curva de Lorenz', col=4)}
            else if (input$var8 == "Mortalidade infantil") {plot(Lc(Mortalidade), main='Curva de Lorenz', col=4)}
            else if (input$var8 == "Mediana das idades") {plot(Lc(Mediana), main='Curva de Lorenz', col=4)}
            else if (input$var8 == "PIB per capita") {plot(Lc(PIB), main='Curva de Lorenz', col=4)}
            else if (input$var8 == "Escolaridade") {plot(Lc(Escola), main='Curva de Lorenz', col=4)}
            else if (input$var8 == "% de população urbana") {plot(Lc(Urbana), main='Curva de Lorenz', col=4)}
            else if (input$var8 == "Nº de filhos") {plot(Lc(Filhos), main='Curva de Lorenz', col=4)}

        },height = 600, width = 600)}
}

shinyApp(ui, server)
#Adicionando o app em https://www.shinyapps.io/
#library(rsconnect)
#rsconnect::deployApp()
#https://carolsb.shinyapps.io/projetoVED/