# Application shiny dashboard sur le coronavirus

### packages

library(shinydashboard)
library(shiny)
library(plotly)
library(scales)
library(reshape)
library(utils)
library(httr)
library(data.table)

### Praparation des données

GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))
data = read.csv(tf)

temp = data[,c(1,7,10,5,6)]
names(temp) = c('date','location','pop','new_cases','new_deaths')
temp$date = as.Date(temp$date,'%d/%m/%Y')

pays = unique(temp$location)
date = sort(unique(temp$date))

temp = temp[order(c(temp$location),temp$date),]


t1 = data.table(group1 = temp$location,sum = temp$new_cases)
t2 = data.frame(t1[,list(cumsum = cumsum(sum)),by = list(group1)])
temp$total_cases = t2$cumsum

t1 = data.table(group1 = temp$location,sum = temp$new_deaths)
t2 = data.frame(t1[,list(cumsum = cumsum(sum)),by = list(group1)])
temp$total_deaths = t2$cumsum

### date de confinement

conf=data.frame(location = c('France','South Africa','Tunisia','United Kingdom','Greece','Belgium',
                             'Italy','Spain','Austria','Israel','United States','India','Colombia','Brazil','Argentina','Venezuela'), 
                date_conf = c('2020-03-17','2020-03-23','2020-03-22','2020-03-23','2020-03-23','2020-03-18',
                              '2020-03-10','2020-03-14','2020-03-20','2020-03-17','2020-03-22','2020-03-24','2020-03-24',
                              '2020-03-24','2020-03-19','2020-03-16'))
conf$date_conf = as.Date(conf$date_conf)
conf$location = as.character(conf$location)

### France

temp1 = temp[temp$location == 'France',]
data1 = temp1[temp1$total_cases != 0 & temp1$date >= as.Date('2020-03-01'),]

j_0 = which.max(data1$date)
j_1 = which.max(data1$date) - 1

aug_deaths = (data1$new_deaths[j_0] - data1$new_deaths[j_1])/data1$new_deaths[j_1]
aug_cas = (data1$new_cases[j_0] - data1$new_cases[j_1])/data1$new_cases[j_1]

nbr_conf = as.numeric(difftime(data1$date[j_0],conf[conf$location == 'France',2]))

### ui

ui <- dashboardPage(
    dashboardHeader(title = "Coronavirus"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("France", tabName = "FrancePage", icon = icon("heartbeat")),
            menuItem("Régions de France", tabName = "RegionPage", icon = icon("home")),
            menuItem("Pays", tabName = "PaysPage", icon = icon("flag")),
            menuItem("Internationale", tabName = "InterPage", icon = icon("globe"))
        )
    ),
    dashboardBody(
        tabItems(
            #france
            tabItem(tabName = "FrancePage",
                    fluidRow(
                        column(width = 6,
                               box(paste("Données"," :  dernière mise à jour le", format(max(data1$date),"%d %B")," après ",
                                         nbr_conf," jours de confinement."), width = 12),
                               fluidRow(
                                   valueBoxOutput("Francemortaug", width = 6),
                                   valueBoxOutput("Francecasaug", width = 6)
                               )
                        ),
                        column(width = 6,
                               box("Les données sont les officielles, récupérées sur https://ourworldindata.org/coronavirus-source-data. Attention le nombre de cas
                n'est pas forcément fiable (changement de comptage et de détection de cas). Les codes et
                les données sont en accès libre : https://github.com/fbouche/Coronavirus. Les pages ''Régions de France'', ''Pays'', et ''Internationale'' sont en cours.", width = 12, height = 165)
                        )
                    ),
                    fluidRow(
                        valueBoxOutput("Francemortjour", width = 3),
                        valueBoxOutput("Francecasjour", width = 3),
                        valueBoxOutput("Francemorttotal", width = 3),
                        valueBoxOutput("Francecastotal", width = 3)
                    ),
                    fluidRow(
                        box(plotlyOutput("Franceplot1"), width = 6, height = 400),
                        box(plotlyOutput("Franceplot2"), width = 6, height = 400)
                    )
            ),
            #region
            tabItem(tabName = "RegionPage",
                    fluidRow(
                        box('En cours...')
                    )
            ),
            #pays
            tabItem(tabName = "PaysPage",
                    fluidRow(
                        box('En cours...')
                    )
            ),
            #international
            tabItem(tabName = "InterPage",
                    fluidRow(
                        box('En cours...')
                    )
            )
        )
    )
)

### server

server <- function(input, output) {
    
    ###graphique
    
    #nombre de nouveaux cas et morts
    output$Franceplot1 <- renderPlotly({
        fig = plot_ly(data1)
        fig = fig %>% add_trace(x = ~date, y = ~new_deaths, type = 'bar', name = 'Morts',
                                marker = list(color = '#OO6699'),
                                hoverinfo = "text",
                                text = ~paste(format(date,"%d %B"),' : ', new_deaths, 'morts'))
        fig = fig %>% add_trace(x = ~date, y = ~new_cases, type = 'scatter', mode = 'lines', name = 'Cas', yaxis = 'y2',
                                line = list(color = '#FF3333'),
                                hoverinfo = "text",
                                text = ~paste(format(date,"%d %B"),' : ', new_cases, ' cas'))
        fig = fig %>% layout(title = 'Nombre de cas et de morts par jour en France',
                             xaxis = list(title = ""), legend = list(x = 0.1, y = 0.9),
                             yaxis2 = list(side = 'left', overlaying = "y", title = 'nombre de cas', showgrid = FALSE, zeroline = FALSE),
                             yaxis = list(side = 'right', title = 'nombre de morts', showgrid = FALSE, zeroline = FALSE))
        
        fig
    })
    
    #nombre total cas et morts
    output$Franceplot2 <- renderPlotly({
        fig = plot_ly(data1)
        fig = fig %>% add_trace(x = ~date, y = ~total_deaths, type = 'bar', name = 'Morts',
                                marker = list(color = '#OO6699'),
                                hoverinfo = "text",
                                text = ~paste(format(date,"%d %B"),' : ', total_deaths, 'morts'))
        fig = fig %>% add_trace(x = ~date, y = ~total_cases, type = 'scatter', mode = 'lines', name = 'Cas', yaxis = 'y2',
                                line = list(color = '#FF3333'),
                                hoverinfo = "text",
                                text = ~paste(format(date,"%d %B"),' : ', total_cases, ' cas'))
        fig = fig %>% layout(title = 'Total de cas et de morts en France',
                             xaxis = list(title = ""), legend = list(x = 0.1, y = 0.9),
                             yaxis2 = list(side = 'left', overlaying = "y", title = 'nombre de cas', showgrid = FALSE, zeroline = FALSE),
                             yaxis = list(side = 'right', title = 'nombre de morts', showgrid = FALSE, zeroline = FALSE))
        
        fig
    })
    
    ###box
    
    output$Francemortaug <- renderValueBox({
        if (aug_deaths>=0) {
            valueBox(
                percent(aug_deaths), paste("Augmentation de morts le", format(max(data1$date),"%d %B")), icon = icon("cross"), color = "blue"
            ) 
        }else{
            valueBox(
                percent(aug_deaths), paste("Diminution de mort le", format(max(data1$date),"%d %B")), icon = icon("cross"), color = "blue"
            ) 
        }
    })  
    
    output$Francecasaug <- renderValueBox({
        
        if (aug_cas>=0) {
            valueBox(
                percent(aug_cas), paste("Augmentation de cas le", format(max(data1$date),"%d %B")), icon = icon("ambulance"), color = "red"
            )
        }else{
            valueBox(
                percent(aug_cas), paste("Diminution de cas le", format(max(data1$date),"%d %B")), icon = icon("ambulance"), color = "red"
            )
        }
    })
    
    output$Francemortjour <- renderValueBox({
        valueBox(
            data1$new_deaths[j_0], paste("Nombre de morts le", format(max(data1$date),"%d %B")), icon = icon("cross"),
            color = "blue"
        )
    })  
    
    output$Francecasjour <- renderValueBox({
        valueBox(
            data1$new_cases[j_0], paste("Nombre de cas le", format(max(data1$date),"%d %B")), icon = icon("ambulance"),
            color = "red"
        )
    })
    
    output$Francemorttotal <- renderValueBox({
        valueBox(
            data1$total_deaths[j_0], 'Total de morts', icon = icon("cross"),
            color = "blue"
        )
    })
    
    output$Francecastotal <- renderValueBox({
        valueBox(
            data1$total_cases[j_0], 'Total de cas', icon = icon("ambulance"),
            color = "red"
        )
    })
}


########### Run the application 

shinyApp(ui = ui, server = server)