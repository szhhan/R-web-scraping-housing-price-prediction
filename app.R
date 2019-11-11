suppressMessages(library(shiny))
suppressMessages(library(shinythemes))
suppressMessages(library(dplyr))
suppressMessages(library(tidyverse))
suppressMessages(library(leaflet))
suppressMessages(library(RColorBrewer))
suppressMessages(library(scales))
suppressMessages(library(lattice))



load("county_data.RData")
load("county_plot.Rdata")
load("recommendation1.Rdata")
load("recommendation2.Rdata")



county_data = county_data %>% 
    rename('Housing Price' = price,
           'Income Per Capita' = Income_PerCapita,
           'Crime Rate' = crime_rate_per_100000,
           'Unemployment Rate' = pct_unemp,
           'Num Of Colleges' = college_num,
           'Num Of Public Schools' = public_school_count,
           'Num Of Private Schools' = private_school_count,
           'Hospital Rating' = hospital_rating,
           'Num Of Shoppingmalls' = num_shoppingmall,
           'Risk Level' = RiskLevel) %>% 
    left_join(.,recommendation_for_shiny, by=c("County","State")) 

county_plot=county_data %>% 
    left_join(county_plot, by=c("State","County")) %>% 
    na.omit()

state_list = unique(county_data$State)

variable_list = c("State",
                  "County",
                  "Population",
                  "Crime Rate",
                  "Risk Level",                  
                  "Housing Price",
                  "Hospital Rating",                  
                  "Num Of Colleges",
                  "Income PerCapita",  
                  "Unemployment Rate",                    
                  "Num Of Public Schools",
                  "Num Of Private Schools",
                  "Num Of Shoppingmalls")

vars=c("Population",
       "Crime Rate",
       "Risk Level",                  
       "Housing Price",
       "Hospital Rating",                  
       "Num Of Colleges",
       "Income PerCapita",  
       "Unemployment Rate",                    
       "Num Of Public Schools",
       "Num Of Private Schools",
       "Num Of Shoppingmalls")



shinyApp(
    ui = tagList(
        navbarPage(
            theme = shinytheme("cosmo"),      
            "U.S. County Housing Price",
            tabPanel("County Outlook", 
                     sidebarPanel(
                         selectInput("state", 
                                     "Select A State", 
                                     choices = state_list, 
                                     selected = 1),
                         checkboxGroupInput("variable","Select variables to view", variable_list),
                         actionButton("update", "Update"),
                         hr(),
                         h3("Variable Dictionary: "),
                         helpText("- Crime Rate: number of crimes per 100,000"),
                         helpText("- Risk Level: 9 - Highest; 0 - lowest"),
                         helpText("- Housing Price: $1000/square-meters"),
                         helpText("- Hospital Rating: 5 - highest; 0 - lowest"),
                         helpText("- Unemployment Rate: in percentage")
                     ),
                     mainPanel(
                         DT::dataTableOutput("table1")
                     )),
            
            tabPanel("County Housing Price",
                     sidebarPanel(
                         selectInput("state_hist", 
                                     "Select A State", 
                                     choices = state_list, 
                                     selected = 1),
                         sliderInput("slide", 
                                     label = "Choose Your Price Range", 
                                     min = 0.406, max = 4, value = c(0.5, 0.6)),
                         actionButton("bargraph", "Get Housing Price Distribution"),
                         hr(),
                         h3("Variable Dictionary: "),
                         helpText("- Housing Price: $1000/m^2")
                     ),
                     mainPanel(
                         plotOutput("distribution")
                     )),
            
            
            ## create an interactive map
            tabPanel("Interactive map", 
                     div(class="outer",
                         
                         tags$head(
                             # Include our custom CSS
                             includeCSS("styles.css"),
                             includeScript("gomap.js")
                         ),
                         
                         leafletOutput("map", width="100%", height="100%"),
                         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                       draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                       width = 330, height = "auto",
                                       
                                       h2("Variables explorer"),
                                       
                                       selectInput("color", "Color", vars),
                                       selectInput("size", "Size", vars, selected = "Housing Price")
                                       
                         ),
                         
                         tags$div(id="cite",
                                  'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).',"code from https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example"
                         )
                     )
            ),
            
            
            ## recommendation system
            tabPanel("Recommendation Level -- From Modeling", 
                     sidebarPanel(
                         selectInput("state_rd", "Select A State", choices = state_list, selected = 1),
                         actionButton("recommend", "Get Recommendation"),
                         hr(),
                         h4("Recommendation Level Instruction: "),
                         helpText("The larger the level, the more highly the county is recommended."),
                         helpText("- 1: Extremely Not Recommended."),
                         helpText("- 2: Highly Not Recommended."),
                         helpText("- 3: Not Recommended."),
                         helpText("- 4: Fair Offer."),
                         helpText("- 5: Recommened."),
                         helpText("- 6: Highly Recommended."),
                         helpText("- 7: Extremely Recommended.")           
                         
                     ),
                     mainPanel(
                         plotOutput("recommendation_level", height = "600px"),
                         htmlOutput(("Instruction"))
                     )),
            
            
            tabPanel("Recommendation Score -- From Dataset", 
                     sidebarPanel(
                         selectInput("rank_state", 
                                     "Select A State", 
                                     choices = state_list,
                                     selected = 1),
                         
                         checkboxGroupInput("variable_rd",
                                            "Select variables to rank",
                                            c("Cheap","safety","income","transportation","cpi","economic","tax","school","shopping")),
                         actionButton("rank", "Get Recommendation Score"),
                         hr(),
                         h3("Variable Dictionary: "),
                         helpText("- Cheap: the cheaper the housing price, the higher the score"),
                         helpText("- Safety: the safer the county area is, the higher the score"),
                         helpText("- Income: the higher the income level in the county, the higher the score"),
                         helpText("- Transportation: the more convenient the transportation is, the higher the score"),
                         helpText("- CPI: the higher the CPI is, the higher the score"),
                         helpText("- Economic: the more developed the county is, the higher the score"),
                         helpText("- Tax: the lower the tax rate is, the higher the score"),
                         helpText("- School: the larger number of schools in the county, the higher the score"),
                         helpText("- Shopping: the larger number of shopping malls in the county, the higher the score")
                     ),
                     mainPanel(
                         htmlOutput("rank_level_instruction"),
                         plotOutput("rank_plot", height = "500px")
                     ))
        ))
    ,
    
    server = function(input, output, session){
        
        ## panel1 
        tabledata = eventReactive(
            input$update,{
                county_data %>% filter(State == input$state) %>% select(input$variable)
            })
        
        output$table1 = DT::renderDataTable(DT::datatable({
            tabledata()
        }))
        
        
        ## panel 2
        observeEvent(
            input$state_hist, {
                histdata = county_data %>% filter(State == input$state_hist)
                updateSliderInput(session, "slide",
                                  min = min(histdata$`Housing Price`),
                                  max = max(histdata$`Housing Price`))
            })
        
        histdata = eventReactive(
            input$bargraph, {
                county_data %>% filter(State == input$state_hist,
                                       `Housing Price`<= input$slide[2],
                                       `Housing Price` >= input$slide[1])
            }
        )
        
        output$distribution = renderPlot({
            plot = histdata()
            
            ggplot(data = plot, 
                   aes(x = reorder(County, -`Housing Price`), 
                       y = `Housing Price`,
                       fill = `Housing Price`)) +
                geom_bar(stat = "identity",
                         color = "black",
                         position = position_dodge()) +
                geom_text(aes(label = `Housing Price`),
                          position = position_dodge(0.9),
                          vjust = 1.6,
                          size = 3.5,
                          color = "black") +
                xlab("County") +
                ylab("Housing Price ($1000/m^2)") +
                theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1)) +
                scale_fill_gradient(low="lightblue", high="pink")
        })
        
        
        ## panel 3
        # Create the map
        output$map <- renderLeaflet({
            leaflet() %>%
                addTiles(
                    #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                    attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
                ) %>%
                setView(lng = -93.85, lat = 37.45, zoom = 4)
        })
        
        observe({
            colorBy <- input$color
            sizeBy <- input$size
            
            colorData <- county_plot[[colorBy]]
            pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
            radius <- county_plot[[sizeBy]] / max(county_plot[[sizeBy]]) * 30000
            
            
            leafletProxy("map", data = county_plot) %>%
                clearShapes() %>%
                addCircles(~Long, ~Lat, radius=radius, layerId=~Zipcode,
                           stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
                addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                          layerId="colorLegend")
        })
        
        # Show a popup at the given location
        showZipcodePopup <- function(zipcode, lat, lng) {
            selectedZip <- county_plot[county_plot$Zipcode == zipcode,]
            content <- as.character(tagList(
                tags$h4(selectedZip$recommend),
                tags$strong(HTML(sprintf("%s, %s %s",
                                         selectedZip$City, selectedZip$County, selectedZip$State)))
            ))
            leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
        }
        
        # When map is clicked, show a popup with city info
        observe({
            leafletProxy("map") %>% clearPopups()
            event <- input$map_shape_click
            if (is.null(event))
                return()
            
            isolate({
                showZipcodePopup(event$id, event$lat, event$lng)
            })
        })
        
        ### panel 4
        barplotdata = eventReactive(
            input$recommend,{
                county_data %>% 
                    filter(State==input$state_rd) %>% 
                    select(County,recommend) %>% 
                    arrange(recommend)
                
            })
        
        output$Instruction = renderUI({
            
            str1 = "The recommendation level is based on the test result from the predicting model underneath the App. The smaller difference between the real price and the predicted price, the more highly the county is recommended."
            str2 = "Choose a state you want to look at and check the recommendation levels for all the counties within that state."
            
            HTML(paste(str1, str2, sep = '<br/><br/>'))
        })
        
        
        output$recommendation_level = renderPlot({
            barplot=barplotdata()
            
            ggplot(data = barplot, 
                   aes(x = reorder(County, -recommend), 
                       y = recommend,
                       fill = recommend)) + 
                geom_bar(stat="identity",
                         width = 1) + 
                xlab("County") + 
                ylab("recommendation level") +
                coord_flip() +
                theme(axis.text.y = element_text(size = 8))
        })
        
        
        
        ### panel5 
        tabledata2 = eventReactive(
            input$rank,{
                
                state_county = county_data_recommend_for_shiny %>% 
                    filter(State == input$rank_state) %>% 
                    select(County, State)
                
                county_data_recommend_for_shiny %>% 
                    filter(State == input$rank_state) %>% 
                    select(input$variable_rd) %>% 
                    mutate(average = round(rowSums(.)/ncol(.), 2),
                           location = paste(state_county$County,
                                            ",",
                                            state_county$State)) %>% 
                    arrange(desc(average)) %>% 
                    .[1:20, ]
            })
        
        output$rank_level_instruction = renderUI({
            
            str1 = "Based on the values of each variable, we assigned a generalized score for each variable in each county. The higher the score is, the better the indicator is. The overall recommendation score is the average score of all the variables you choose."
            str2 = "Choose a state or multiple states you want to look at and select the features that are more important to you. Then check which county is the best for you!"
            
            HTML(paste(str1, str2, sep = '<br/><br/>'))
        })
        
        
        output$rank_plot = renderPlot({
            plot = tabledata2()
            
            ggplot(data = plot, 
                   aes(x = reorder(location, -average), 
                       y = average,
                       fill = average)) +
                geom_bar(stat = "identity",
                         color = "black",
                         position = position_dodge()) +
                geom_text(aes(label = average),
                          position = position_dodge(0.9),
                          vjust = 1.6,
                          size = 3.5,
                          color = "black") +
                xlab("State & County") +
                ylab("Overall Recommendation Score") +
                theme(axis.text.x = element_text(angle = 40, hjust = 1, vjust = 1)) +
                scale_fill_gradient(low="lightblue", high="pink")
        })
        
    }
)



