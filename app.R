library(purrr)
library(dplyr)
library(ggplot2)
library(rvest)
library(polite)
library(scales)
library(shiny)
library(glue)
library(stringr)
library(lubridate)
library(ggrepel)
library(ggforce)
library(extrafont)
library(extrafontdb)
library(ggtext)
source("Helpers.R")
source("Helpers2.R")
#dir.create('~/.fonts')
#file.copy("www/Spartan-Bold.ttf", "~/.fonts")
#file.copy("www/Spartan-Light.ttf", "~/.fonts")
#file.copy("www/Spartan-Regular.ttf", "~/.fonts")
#file.copy("www/Spartan-Medium.ttf", "~/.fonts")
#system('fc-cache -f ~/.fonts')

font_import(paths = "www", pattern = "Spartan", prompt = FALSE)
loadfonts()
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Create your own age plot - A Shiny app by @RobinWilhelmus"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("team", "Team", "pec-zwolle"),
            textInput("teamcode", "Teamcode", "1269"),
            textInput("season", "Season (enter 2018 for 18/19)", "2019"),
            actionButton("myButton", "Scrape!"),
            actionButton("myButton2", "Scrape custom season!"),
            textInput("rect","Rectangle 'peak age' color (white will make it disappear)", "red"),
            textInput("line","Time ate club color", "black"),
            textInput("line2","Line contract length color", "black"),
            textInput("dot","Dot color", "black"),
            textInput("name","Player name color", "blue"),
            radioButtons("alpha", "See lines?",
                         c("Both lines" = 3,
                           "Only time at club" = 2,
                           "Only contract length" = 1,
                           "No lines" = 0)),
            actionButton("go", "Plot!")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Instructions", 
                                 h4("Instructions", align = "center"),
                                 h5("Go to transfermarkt.com and search for your favourite team.", align = "left"),
                                 h5("When you're on the page of your team, look at the URL. It looks something like this:", align = "left"),
                                 h5(" "),
                                 h5("https://www.transfermarkt.com/inter-mailand/startseite/verein/46/saison_id/2019", align = "left"),
                                 h5(" "),
                                 h5("There are two thing important: the club (inter-mailand) and the teamcode (46).", align = "left"),
                                 h5("The other number is for the season, but we ignore that. Just copy the two values and paste them in the boxes on the left (including the minus sign (-)", align = "left"),
                                 h5("Click on the 'scrape!' button and wait a little while."),
                                 h5("A (new) table on this page will appear which means the data is scraped and ready to plot!"),
                                 br(),
                                 h4("Go to the 'Age plot' tab"),
                                 br(),
                                 h5("Choose some colours from here:"),
                                 h5("https://cpb-us-e1.wpmucdn.com/sites.ucsc.edu/dist/d/276/files/2015/10/colorbynames.png"),
                                 h5("Or a hex colour code, enter them in the text fields and click on 'plot!'"),
                                 br(),
                                 h5("Choose white as a colour to have no rectangle for peak age"),
                                 tableOutput("myTable")),
                        tabPanel("Age Plot 2019", 
                                 h5("Data scraped for:"),
                                 verbatimTextOutput("text1"),
                                 plotOutput("scatplot")),
                        tabPanel("Age Plot older season",
                                 h5("Data scraped for:"),
                                 verbatimTextOutput("text2"),
                                 verbatimTextOutput("text3"),
                                 plotOutput("scatplot2")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    url=  reactive({
        glue("https://www.transfermarkt.com/{input$team}/leistungsdaten/verein/{input$teamcode}/reldata/%262019/plus/1")
        
    })
    #output$text1 <- renderText(url())
    
    myData <- reactive({
        input$myButton
        data = isolate(TransfermarktShiny(
            team_name = input$team, 
            team_num = input$teamcode))
    })
    myData2 <- reactive({
        input$myButton2
        data = isolate(TransfermarktShinyOlder(
            team_name = input$team, 
            team_num = input$teamcode,
            season = input$season))
    })
    output$text1 <- renderText(myData()$Club[1])
    output$text2 <- renderText(myData2()$Club[1])
    output$text3 <- renderText(myData2()$Seas[1])
    output$myTable <- renderTable(myData())
    
    output$scatplot = renderPlot({
        if (input$go == 0)
            return()
        req(input$go)
        color1 <- isolate(input$rect)
        color2 <- isolate(input$line)
        color3 <- isolate(input$line2)
        color4 <- isolate(input$dot)
        color5 <- isolate(input$name)
        teamname <- isolate(input$team)
        alpha <- isolate(input$alpha)
        if(input$alpha == 3){
            isolate(ScatterShiny(data = myData(),
                                 color1 = color1,
                                 color2 = color2,
                                 color3= color3,
                                 color4= color4,
                                 color5= color5,
                                 teamname = teamname,
                                 alpha = alpha))
        } else 
            if(input$alpha == 2){
                isolate(ScatterShinyTime(data = myData(),
                                         color1 = color1,
                                         color2 = color2,
                                         color3= color3,
                                         color4= color4,
                                         color5= color5,
                                         teamname = teamname,
                                         alpha = alpha))
            } else 
                if(input$alpha == 1){
                    isolate(ScatterShinyContract(data = myData(),
                                                 color1 = color1,
                                                 color2 = color3,
                                                 color3= color2,
                                                 color4= color4,
                                                 color5= color5,
                                                 teamname = teamname,
                                                 alpha = alpha))
                    
                } else
                    if(input$alpha == 0){
                        isolate(ScatterShinyNo(data = myData(),
                                               color1 = color1,
                                               color2 = color3,
                                               color3= color2,
                                               color4=color4,
                                               color5=color5,
                                               teamname = teamname,
                                               alpha = alpha))
                    }
        
    }, height = 400, width = 750 )
    output$scatplot2 = renderPlot({
        if (input$go == 0)
            return()
        req(input$go)
        color1 <- isolate(input$rect)
        color2 <- isolate(input$line)
        color3 <- isolate(input$line2)
        color4 <- isolate(input$dot)
        color5 <- isolate(input$name)
        teamname <- isolate(input$team)
        alpha <- isolate(input$alpha)
        if(input$alpha == 3){
            isolate(ScatterShinyOther(data = myData2(),
                                 color1 = color1,
                                 color2 = color2,
                                 color3= color3,
                                 color4= color4,
                                 color5= color5,
                                 teamname = teamname,
                                 alpha = alpha))
        } else 
            if(input$alpha == 2){
                isolate(ScatterShinyTimeOther(data = myData2(),
                                         color1 = color1,
                                         color2 = color2,
                                         color3= color3,
                                         color4= color4,
                                         color5= color5,
                                         teamname = teamname,
                                         alpha = alpha))
            } else 
                if(input$alpha == 1){
                    isolate(ScatterShinyContractOther(data = myData2(),
                                                 color1 = color1,
                                                 color2 = color3,
                                                 color3= color2,
                                                 color4= color4,
                                                 color5= color5,
                                                 teamname = teamname,
                                                 alpha = alpha))
                    
                } else
                    if(input$alpha == 0){
                        isolate(ScatterShinyNoOther(data = myData2(),
                                               color1 = color1,
                                               color2 = color3,
                                               color3= color2,
                                               color4=color4,
                                               color5=color5,
                                               teamname = teamname,
                                               alpha = alpha))
                    }
        
    }, height = 400, width = 750 )
}

# Run the application 
shinyApp(ui = ui, server = server)
