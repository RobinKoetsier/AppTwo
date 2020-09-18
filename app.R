library(purrr)
library(reactable)
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
library(ggdark)

#library(shinyWidgets)
library(extrafont)
library(extrafontdb)
library(ggtext)
#library(GAlogger)
#library(Cairo)
source("Helpers.R")
source("Helpers2.R")
source("Helpers3.R")
source("Helpers4.R")
library(GAlogger)
ga_set_tracking_id("UA-170459986-1")
ga_set_approval(consent = TRUE)
#options(shiny.usecairo=T)
allComp <- readRDS("my_data.rds")
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Create your own age plot - A Shiny app by @RobinWilhelmus"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            textInput("team", "Team", "pec-zwolle"),
            #pickerInput('picker 1', 'picker1', c(1,2,3,4,5), options=pickerOptions(liveSearch=T)),
            #pickerInput('picker 2', 'picker2', c(1,2,3,4,5), options = list(`live-search`=TRUE))
            #,
            textInput("teamcode", "Teamcode", "1269"),
            textInput("season", "Season (enter 2018 for 18/19)", "2020"),
            textInput("compcode", "Competition code (Enter a dot to get everything combined)", "NL1"),
            actionButton("myButton", "Scrape!"),
            br(),
            actionButton("myButton2", "Scrape older season!"),
            textInput("rect","Rectangle 'peak age' color", "red"),
            textInput("line","Time ate club color", "black"),
            textInput("line2","Line contract length color", "black"),
            textInput("dot","Dot color", "black"),
            textInput("name","Player name color", "black"),
            sliderInput("peak", "Peak age range:",min = 23, max = 35, value = c(25,30)),
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
                                 uiOutput("tab2"),
                                 h5("Go to transfermarkt.com and search for your favourite team.", align = "left"),
                                 h5("When you're on the page of your team, look at the URL. It looks something like this:", align = "left"),
                                 h5(" "),
                                 
                                 h5("https://www.transfermarkt.com/inter-mailand/startseite/verein/46/saison_id/2019", align = "left"),
                                 h5(" "),
                                 h5("There are two thing important: the club (inter-mailand) and the teamcode (46).", align = "left"),
                                 h5("The other number is for the season, but we ignore that. Just copy the two values and paste them in the boxes on the left (including the minus sign (-)", align = "left"),
                                 h5("Click on the 'scrape!' or 'Scrape older season!' button and wait a little while."),
                                 h5("A (new) table on this page will appear which means the data is scraped and ready to plot!"),
                                 br(),
                                 h4("Go to the 'Age plot' or 'Age plot older season' tab"),
                                 h4("Age plot 2019 will only show the players that are still at the club. Use the other scrape function with 2019 as entry to get all the players for that season."),
                                 br(),
                                 h5("Choose some colours from here:"),
                                 uiOutput("tab"),
                                 #h5("https://cpb-us-e1.wpmucdn.com/sites.ucsc.edu/dist/d/276/files/2015/10/colorbynames.png"),
                                 h5("or a hex colour code, enter them in the text fields and click on 'plot!'"),
                                 br(),
                                 h5("Choose white as a colour to have no rectangle for peak age"),
                                 br(),
                                 h4("It will only plot from data that is available on transfermarkt.com, so how far you can come back is different per team!!"),
                                 tableOutput("myTable")),
                        tabPanel("Age Plot 2020", 
                                 h5("Data scraped for:"),
                                 verbatimTextOutput("text1"),
                                 verbatimTextOutput("text5"),
                                 verbatimTextOutput("text11"),
                                 plotOutput("scatplot")),
                        tabPanel("Age Plot 2020 dark theme", 
                                 verbatimTextOutput("text9"),
                                 verbatimTextOutput("text10"),
                                 verbatimTextOutput("text12"),
                                 plotOutput("scatplot4")),
                        tabPanel("Age Plot older season",
                                 h5("Data scraped for:"),
                                 verbatimTextOutput("text2"),
                                 verbatimTextOutput("text3"),
                                 verbatimTextOutput("text4"),
                                 plotOutput("scatplot2")),
                        tabPanel("Age Plot older season dark mode",
                                 h5("Data scraped for:"),
                                 verbatimTextOutput("text6"),
                                 verbatimTextOutput("text7"),
                                 verbatimTextOutput("text8"),
                                 plotOutput("scatplot3")),
                        tabPanel("Competition codes",
                                 reactableOutput("codes", width = "auto", height = "auto",
                                                 inline = FALSE)))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    url=  reactive({
        glue("https://www.transfermarkt.com/{input$team}/leistungsdaten/verein/{input$teamcode}/reldata/%262019/plus/1")
        
    })
    #output$text1 <- renderText(url())
    
    output$tab <- renderUI({
        tags$a(href="https://cpb-us-e1.wpmucdn.com/sites.ucsc.edu/dist/d/276/files/2015/10/colorbynames.png", "Click here!")})
    output$tab2 <- renderUI({
        tags$a(href="https://shinynew.robinkoetsier.nl/ShinyAppAge", "Click here for the app without all the extra options. It's less slow.")})
    myData <- reactive({
        input$myButton
        data = isolate(TransfermarktShiny(
            team_name = input$team, 
            team_num = input$teamcode,
            comp_code = input$compcode))
    })
    myData2 <- reactive({
        input$myButton2
        data = isolate(TransfermarktShinyOlder(
            team_name = input$team, 
            team_num = input$teamcode,
            season = input$season,
            comp_code = input$compcode))
    })
    output$text1 <- renderText(myData()$Club[1])
    output$text2 <- renderText(myData2()$Club[1])
    output$text3 <- renderText(myData2()$Seas[1])
    output$text4 <- renderText(myData2()$Comp[1])
    output$text5 <- renderText(myData()$Comp[1])
    output$text6 <- renderText(myData2()$Club[1])
    output$text7 <- renderText(myData2()$Seas[1])
    output$text8 <- renderText(myData2()$Comp[1])
    output$text9 <- renderText(myData()$Club[1])
    output$text10 <- renderText(myData()$Comp[1])
    output$text11 <- renderText(paste0(myData()$Som[1]," minutes"))
    output$text12 <- renderText(paste0(myData()$Som[1]," minutes"))
    output$myTable <- renderTable(myData())
    output$codes <- renderReactable({
        reactable(
            allComp,
            searchable = TRUE,
            striped = TRUE,
            highlight = TRUE,
            bordered = TRUE,
            theme = reactableTheme(
                borderColor = "#dfe2e5",
                stripedColor = "#f6f8fa",
                highlightColor = "#f0f5f9",
                cellPadding = "8px 12px",
                style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
                searchInputStyle = list(width = "100%")
            )
        )
    })
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
        left <- isolate(input$peak[1])
        right <- isolate(input$peak[2])
        if(input$alpha == 3){
            isolate(ScatterShiny(data = myData(),
                                 color1 = color1,
                                 color2 = color2,
                                 color3= color3,
                                 color4= color4,
                                 color5= color5,
                                 teamname = teamname,
                                 alpha = alpha,
                                 left=left,
                                 right = right))
        } else 
            if(input$alpha == 2){
                isolate(ScatterShinyTime(data = myData(),
                                         color1 = color1,
                                         color2 = color2,
                                         color3= color3,
                                         color4= color4,
                                         color5= color5,
                                         teamname = teamname,
                                         alpha = alpha,
                                         left=left,
                                         right = right))
            } else 
                if(input$alpha == 1){
                    isolate(ScatterShinyContract(data = myData(),
                                                 color1 = color1,
                                                 color2 = color3,
                                                 color3= color2,
                                                 color4= color4,
                                                 color5= color5,
                                                 teamname = teamname,
                                                 alpha = alpha,
                                                 left=left,
                                                 right = right))
                    
                } else
                    if(input$alpha == 0){
                        isolate(ScatterShinyNo(data = myData(),
                                               color1 = color1,
                                               color2 = color3,
                                               color3= color2,
                                               color4=color4,
                                               color5=color5,
                                               teamname = teamname,
                                               alpha = alpha,
                                               left=left,
                                               right = right))
                    }
        
    }, height = 500, width = 940, res = 96)
    output$scatplot4 = renderPlot({
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
        left <- isolate(input$peak[1])
        right <- isolate(input$peak[2])
        if(input$alpha == 3){
            isolate(ScatterShinyDark(data = myData(),
                                 color1 = color1,
                                 color2 = color2,
                                 color3= color3,
                                 color4= color4,
                                 color5= color5,
                                 teamname = teamname,
                                 alpha = alpha,
                                 left=left,
                                 right = right))
        } else 
            if(input$alpha == 2){
                isolate(ScatterShinyTimeDark(data = myData(),
                                         color1 = color1,
                                         color2 = color2,
                                         color3= color3,
                                         color4= color4,
                                         color5= color5,
                                         teamname = teamname,
                                         alpha = alpha,
                                         left=left,
                                         right = right))
            } else 
                if(input$alpha == 1){
                    isolate(ScatterShinyContractDark(data = myData(),
                                                 color1 = color1,
                                                 color2 = color3,
                                                 color3= color2,
                                                 color4= color4,
                                                 color5= color5,
                                                 teamname = teamname,
                                                 alpha = alpha,
                                                 left=left,
                                                 right = right))
                    
                } else
                    if(input$alpha == 0){
                        isolate(ScatterShinyNoDark(data = myData(),
                                               color1 = color1,
                                               color2 = color3,
                                               color3= color2,
                                               color4=color4,
                                               color5=color5,
                                               teamname = teamname,
                                               alpha = alpha,
                                               left=left,
                                               right = right))
                    }
        
    }, height = 500, width = 940, res = 96)
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
        left <- isolate(input$peak[1])
        right <- isolate(input$peak[2])
        if(input$alpha == 3){
            isolate(ScatterShinyOther(data = myData2(),
                                      color1 = color1,
                                      color2 = color2,
                                      color3= color3,
                                      color4= color4,
                                      color5= color5,
                                      teamname = teamname,
                                      alpha = alpha,
                                      left=left,
                                      right = right))
        } else 
            if(input$alpha == 2){
                isolate(ScatterShinyTimeOther(data = myData2(),
                                              color1 = color1,
                                              color2 = color2,
                                              color3= color3,
                                              color4= color4,
                                              color5= color5,
                                              teamname = teamname,
                                              alpha = alpha,
                                              left=left,
                                              right = right))
            } else 
                if(input$alpha == 1){
                    isolate(ScatterShinyContractOther(data = myData2(),
                                                      color1 = color1,
                                                      color2 = color3,
                                                      color3= color2,
                                                      color4= color4,
                                                      color5= color5,
                                                      teamname = teamname,
                                                      alpha = alpha,
                                                      left=left,
                                                      right = right))
                    
                } else
                    if(input$alpha == 0){
                        isolate(ScatterShinyNoOther(data = myData2(),
                                                    color1 = color1,
                                                    color2 = color3,
                                                    color3= color2,
                                                    color4=color4,
                                                    color5=color5,
                                                    teamname = teamname,
                                                    alpha = alpha,
                                                    left=left,
                                                    right = right))
                    }
        
    }, height = 500, width = 940, res = 96) #,res=96 
    output$scatplot3 = renderPlot({
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
        left <- isolate(input$peak[1])
        right <- isolate(input$peak[2])
        if(input$alpha == 3){
            isolate(ScatterShinyOtherDark(data = myData2(),
                                      color1 = color1,
                                      color2 = color2,
                                      color3= color3,
                                      color4= color4,
                                      color5= color5,
                                      teamname = teamname,
                                      alpha = alpha,
                                      left=left,
                                      right = right))
        } else 
            if(input$alpha == 2){
                isolate(ScatterShinyTimeOtherDark(data = myData2(),
                                              color1 = color1,
                                              color2 = color2,
                                              color3= color3,
                                              color4= color4,
                                              color5= color5,
                                              teamname = teamname,
                                              alpha = alpha,
                                              left=left,
                                              right = right))
            } else 
                if(input$alpha == 1){
                    isolate(ScatterShinyContractOtherDark(data = myData2(),
                                                      color1 = color1,
                                                      color2 = color3,
                                                      color3= color2,
                                                      color4= color4,
                                                      color5= color5,
                                                      teamname = teamname,
                                                      alpha = alpha,
                                                      left=left,
                                                      right = right))
                    
                } else
                    if(input$alpha == 0){
                        isolate(ScatterShinyNoOtherDark(data = myData2(),
                                                    color1 = color1,
                                                    color2 = color3,
                                                    color3= color2,
                                                    color4=color4,
                                                    color5=color5,
                                                    teamname = teamname,
                                                    alpha = alpha,
                                                    left=left,
                                                    right = right))
                    }
        
    }, height = 500, width = 940, res = 96) #,res=96 
}

# Run the application 
shinyApp(ui = ui, server = server)

