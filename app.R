##### Monitoreo de redes - Twitter

#### Paquetes ----

library(shiny)
library(reactable)
library(glue)
library(stringr)
library(httpuv)
library(dplyr)
library(purrr)
library(rtweet)
library(lubridate)

### Autenticación del API de twitter ----

#Contraseñas eliminadas por cuestión de seguridad

##Guardar la contraseña 
a <- rtweet_app()

auth_save(auth = a, name = "")

#Guardar el usuario y contraseña para la app
auth_as("")


### Guardar fecha actual del usuario

fecha_usuario <- Sys.Date() 
fecha_usuario_2 <- floor_date(fecha_usuario, 'weeks')


#### Interfaz del usuario ----

ui <- fluidPage(
    # Titulo de la app
    titlePanel(HTML("<center>Monitoreo en redes sociales: Twitter</center>")),
    
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            numericInput("num_tweets_to_download",
                         "Número de tweets para buscar:",
                         min = 100,
                         max = 18000,
                         value = 200,
                         step = 100),
            textInput("hashtag_to_search",
                      "Palabra o hashtag para buscar:",
                      value = "#periodistas"),
            dateRangeInput("date_picker", label = "Seleccione fechas:", start = fecha_usuario_2, end = fecha_usuario),
            actionButton("get_data", "Buscar", class = "btn-primary"),
            br(),br(),
            downloadButton("download_data", "Descargar tabla")
        ),
        
        # Tabla para Mostrar resultados
        mainPanel(
            reactableOutput("tweet_table")
        )
    )
)

# Función del servidor de la app ----

server <- function(input, output) {
    
    #Función para recolectar los tweets
    tweet_df <- eventReactive(input$get_data, {
        search_tweets(input$hashtag_to_search, n = input$num_tweets_to_download, include_rts = FALSE)
    })
    
    #Funcion para hacer la tabla de tweets
    tweet_table_data <- reactive({
        req(tweet_df())
        tweet_df() %>%
            select(created_at, text, favorite_count, retweet_count) %>%
            filter(between(as.Date(created_at), input$date_picker[1], input$date_picker[2]) )%>%
            select(Fecha = created_at, Tweet = text, Likes = favorite_count, RTs = retweet_count)
    })
    
#Función para presentar la tabla como output    
output$tweet_table <- renderReactable({
        reactable::reactable(tweet_table_data(), 
                             filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                             showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                             columns = list(
                                 Fecha = colDef(defaultSortOrder = "asc"),
                                 Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                                 Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                                 RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE))
                             )
        )
    })

#Función para boton de descarga
output$download_data <- downloadHandler(
    filename = function() {
        paste(input$hashtag_to_search, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
        write.csv(tweet_table_data(), file, row.names = FALSE)
    }
)

    
}

# Run the application 
shinyApp(ui = ui, server = server)
  
