library(shinythemes)
shinyUI(dashboardPage(
  dashboardHeader(title = "Airbnb_NYC"),
  skin="red",
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("user"),
               menuSubItem("Introduction",tabName = "Introduction"),
               menuSubItem("Usecase",tabName = "Usecase"),
               menuSubItem("Reference",tabName = "Reference")
      ),

      menuItem("Locate the Room", tabName = "locate",icon=icon("map"),
               menuSubItem("Quick look",tabName = "plot1"),
               menuSubItem("Map",tabName="map"),
               menuSubItem("Sentiment analysis", tabName = "sentiment")
      ),
      menuItem("Variable tests",tabName = "tests",icon = icon("chart-bar"),
               menuSubItem("Univariable", tabName = "uni"),
               menuSubItem("Bivariable", tabName = "bi"),
               menuSubItem("trivariable", tabName = "tri")
      ),
      menuItem("Data", tabName = "dt", icon = icon("database"),
               menuSubItem("room select",tabName="room_select"),
               menuSubItem("Variables",tabName="data"),
               menuSubItem("Other information",tabName="other")
      )
      
      
    )
  ),
  dashboardBody(
    # font styles
    tags$head(
      tags$style(HTML("
      .shiny-output-error-validation {
        color: green;
        font-size: 35px;
      }
      .main-sidebar { font-size: 16px; }
    ")),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    
    
    tabItems(
      
      tabItem(tabName = "Introduction",
              h2("Yuncheng shi, Yan Zhang, Yiyao Wang"),  
              br(),
              h3("In recent years, with the development of sharing economy,"),
              br(),
              h3("Bed and Breakfast (B&B)  has won the favor of the majority of travelers for its low price"),
              br(),
              h3("changeable room type, complete facilities and more close to life."),
              br(),
              h3("Gathering information from the Internet and choosing the right house is a daunting task."),
              br(),
              h3("Our App is designed to help customers select the B&B they prefer quickly and convenience, and we can study the
                  relationship between price,room type, block and other factors.")
      ),
      
      tabItem(tabName = "Usecase",
              HTML('<center><img src="user.png" width="800"></center>')
      ),
      
      tabItem(tabName = "Reference",
              h3("[1]Dillon DuBois (2019).Airnb Analytics: Using Data to Boost Vacation Rental Revenue. 
                 Retrieved from:[https://www.airdna.co/blog/airbnb-analytics-drive-rental-revenue]"),
              br(),
              h3("[2]Shirley Chen (2019). How to Analyze Airbnb Performance Data in the Right Way.
                  Retrieved from:[https://medium.com/analytics-vidhya/how-to-analyze-airbnb-performance-data-in-the-right-way-b83f3dad1458]"),
              br(),
              h3("[3]Sarang Gupta (2019). Airbnb Rental Listings Dataset Mining.
              Retrieved from:[https://towardsdatascience.com/airbnb-rental-listings-dataset-mining-f972ed08ddec]"),
              br(),
              h3("[4]Part Time Analyst (2019). Finding Undervalued Air Bnb’s.    
                 Retrieved from:[https://www.r-bloggers.com/2019/09/finding-undervalued-air-bnbs/]")
      ),
      
      
      
      
      
      tabItem(tabName = "map",
              
              fluidRow(column(5,
                              wellPanel(selectizeInput("selected0",
                                                       "Select a neighbourhood_group",
                                                       unique(airbnb$neighbourhood_group)
                              ),
                              
                              selectizeInput("selected1",
                                             "Select a neighbourhood",
                                             unique(airbnb$neighbourhood)
                              ),
                              
                              selectizeInput("selected2",
                                             "Select a room_type",
                                             unique(airbnb$room_type)
                              ),
                              
                              selectizeInput("nightrange",
                                             "Select a mininum night",
                                             unique(airbnb$minimum_nights)
                              ),
                              
                              sliderInput("pricerange",
                                          "Pick a price range", pre = "$",
                                          min=1, max= 400, value = c(1, 400)
                              ),
                              sliderInput("reviewrange",
                                          "Pick a reviews range",
                                          min=1, max= 150, value = c(1, 150)
                              ),
                              sliderInput("reviewscore",
                                          "Pick a score rating range",
                                          min=50, max= 100, value = c(50, 100)
                              )
                              )
                              
              ),
              column(7,
                     leafletOutput("map")
              ),
              column(7,
                     wellPanel(dataTableOutput('roomtable'))
              )
              )
      ),
      
      tabItem(tabName = "sentiment",
              plotOutput("sentimentplot", height=650)
      ),
      
      tabItem(tabName = "plot1",
              fluidRow(column(4,
                              wellPanel(selectInput("room_price", "Choose room or price",
                                                    c(room = "room1", price = "price1")
                              ),
                              conditionalPanel(
                                condition = "input.room_price == 'room1'",
                                sliderInput("selected3",
                                            "Pick a price range", pre = "$",
                                            min=1, max= 400, value = c(1, 400)),
                                selectizeInput("selected4",
                                               "Select a mininum night",
                                               unique(airbnb$minimum_nights)
                                )
                              ),#end conditionPn 1
                              conditionalPanel(
                                condition = "input.room_price == 'price1'",
                                selectizeInput("selected5",
                                               "Select a mininum night",
                                               unique(airbnb$minimum_nights)
                                ),
                                selectizeInput("selected7",
                                               "Select a neighbourhood_group",
                                               unique(airbnb$neighbourhood_group)
                                )
                              )
                              
                              )# end wellpanel  
              ),
              column(8,
                     verbatimTextOutput("summaryname")
              ),
              column(12,
                     plotOutput('plot1',
                                dblclick = "plot1_dblclick",
                                brush = brushOpts(
                                  id = "plot1_brush",
                                  resetOnNew = TRUE
                                )
                     )
              )
              )# end fuildRow
      ),
      
      tabItem(tabName = "uni",
              fluidRow(column(4,
                              wellPanel(selectInput("vartype", "Choose a variable type",
                                                    c(numeric = "numeric", factor = "factor")
                              ),
                              conditionalPanel(
                                condition = "input.vartype == 'numeric'",
                                varSelectInput("var2.1_n","Numeric Variable?"
                                               , data = p2_num),
                                radioButtons("plot.type2.1","Choose a plot type:",
                                             list(  'Histogram', 'Density Plot', 'Frequency Polygon')
                                ),
                                sliderInput("bin2.1", "Number of Bins"
                                            , min = 1, max = 100, value = 40),
                                checkboxInput("ttest2", "t-test?"
                                              , value = FALSE),
                                conditionalPanel(
                                  condition = "input.ttest2==1",
                                  numericInput("num2.1.1", "Null number", value = 0
                                               , min = 0, max = 1000,step = 0.01),
                                  numericInput("num2.1.2", "level of significance", value = 0.05
                                               , min = 0, max = 1,step = 0.01)
                                )#end conditionPanel test
                              ),
                              conditionalPanel(
                                condition = "input.vartype == 'factor'",
                                varSelectInput("var2.1_f","Factor Variable?"
                                               , data = p2_fct),
                                verbatimTextOutput("summary2.1_f")
                              )
                              
                              ),
              ),
              
              column(8,
                     plotOutput("plot2.1",
                                dblclick = "plot2.1_dblclick",
                                brush = brushOpts(
                                  id = "plot2.1_brush",
                                  resetOnNew = TRUE
                                )
                     )
              ),
              column(3,offset = 2,
                     verbatimTextOutput("summary2.1_n")
              ),
              column(5,offset = 1,
                     tableOutput("static")
              )
              )
      ),
      
      tabItem(tabName = "bi",
              
              fluidRow(column(4,
                              absolutePanel(
                                wellPanel(varSelectInput("varx2.2","Variable x?"
                                                         , data = airbnb_p2),
                                          checkboxInput("logx", "Log x transform?"
                                                        , value = FALSE),
                                          varSelectInput("vary2.2","Variable y?"
                                                         , data = airbnb_p2),
                                          checkboxInput("logy", "Log y transform?"
                                                        , value = FALSE),
                                          checkboxInput("ols", "Ols line for numeric variables?"
                                                        , value = FALSE),
                                          radioButtons("test.type2.2","Choose a test type:",
                                                       choiceNames  = c("Chisquare-test","lm(summary)","ANOVA"),
                                                       choiceValues = c("c","s","a")
                                          ),
                                          tableOutput("static2")
                                          
                                ),draggable = T,width=400
                              )
                              
              ),
              column(8,
                     plotOutput("plot2.2", height = "410px",
                                dblclick = "plot2.2_dblclick",
                                brush = brushOpts(
                                  id = "plot2.2_brush",
                                  resetOnNew = TRUE
                                )
                     )
              ),
              
              column(4, offset = 4,
                     verbatimTextOutput("code2.2")
              ),
              column(4,
                     verbatimTextOutput("code2.1")
              ),
              
              )
              
      ),#end bi
      tabItem(tabName = "tri",
              fluidRow(column(4,
                              absolutePanel(
                                wellPanel(varSelectInput("varx2.3","X Variable?"
                                                         , data = airbnb_p2),
                                          checkboxInput("logx2.3", "Log x transform?"
                                                        , value = FALSE),
                                          varSelectInput("vary2.3","Factor y Variable?"
                                                         , data = p2_fct),
                                          varSelectInput("varz2.3","Factor facet Variable?"
                                                         , data = p2_fct),
                                          checkboxInput("change2.3", "Exchange x, y axis?"
                                                        , value = FALSE),
                                          verbatimTextOutput("code3")
                                ),draggable = T,width = 400
                              )
              ),
              
              column(8,
                     plotOutput("plot2.3",height=650)
              )
              )
              
      ),#end tri
      
      tabItem(tabName = "room_select",
              fluidPage(dataTableOutput('selecttable')
              )),
      
      tabItem(tabName = "data",
              fluidPage(dataTableOutput('table')
              )),
      
      tabItem(tabName = "other",
              fluidPage(dataTableOutput('othertable')
              ))
      
      
      
      
    )# end table items
    
  )))