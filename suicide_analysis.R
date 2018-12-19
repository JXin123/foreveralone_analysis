library(ggbeeswarm)
library(shiny)
library(shiny)
library(plotly)
library(ggplot2)
library(data.table)
library(dplyr)
library(ggalluvial)
library(rpart)
library(rpart.plot)
library(ggmosaic)
library(DT)
library(tidyverse)
#library(rpart.uti
#library(visNetwork)
#library(rattle)


foreveralone <- read_csv('./foreveralone.csv')
#foreveralone <- fread("./foreveralone.csv")
foreveralone <- foreveralone[gender == 'Transgender female', gender:='Female']
foreveralone <- foreveralone[gender == 'Transgender male', gender:='Male']

foreveralone$race <- 
  sapply(foreveralone$race, function(x){
    ifelse(x == "White non-Hispanic" |
             x == "Asian" | 
             x == "Hispanic (of any race)" |
             x == "Black", yes = x, no = "Other and mixed")
  })

nms <- names(foreveralone)
catvars <- nms[!nms %in% c("time", "age", "friends")]
balloon <- melt(foreveralone)
df <- foreveralone[,c("attempt_suicide", 
                      "gender",
                      "sexuallity" , 
                      "age",
                      "race" ,
                      "bodyweight",
                      "social_fear" , 
                      "depressed",
                      "employment",
                      "friends")]
df <- as.data.frame(unclass(df))


ui<-navbarPage("Suicide Analysis",
               
               tabPanel('Dataset',     sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput("show_vars", "Columns in diamonds to show:",
                                        names(foreveralone), selected =FALSE)
                   ),
                        mainPanel(DT::dataTableOutput("mytable1"))
               )),
                 
               tabPanel("Beeswarm",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("var", 
                                        "Select categorical variable", 
                                        choices = catvars, 
                                        selected = "depressed")
                          ),
                          mainPanel(verbatimTextOutput("beetext"),
                                    h4("Distribution Based on Depressed"),plotOutput('beeswarmPlot'))
                        )),
               
               tabPanel("Mosaic Diagram",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = 'mox', 
                                        label = 'first variable', 
                                        choices = nms, selected = "attempt_suicide"),
                            selectInput(inputId = 'moy', 
                                        label = 'second variable', 
                                        choices = nms, selected = "edu_level")
                            
                          ),
                          
                          mainPanel(verbatimTextOutput("mosaictext"),
                                    h4("Cross-sectional Distribution -- Attempt Suicide VS Education Level"),plotOutput('mosaicPlot', height = "500px"))
                        ) ),
               
               tabPanel("Balloon Diagram",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = 'bally', 
                                        label = 'choose for the variable', 
                                        choices = nms, selected = "edu_level")
                            
                          ),
                          
                          mainPanel(verbatimTextOutput("ballontext"),
                                    h4("Distribution of Categorical Variable based on Age and Friends"),plotOutput('ballPlot', height = "500px"))
                        ) ),
               
               
               
               tabPanel("Alluvial Diagram",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = 'x', 
                                        label = 'X', 
                                        choices = nms, selected = "gender"),
                            selectInput(inputId = 'y', 
                                        label = 'Y', 
                                        choices = nms, selected = "depressed")
                            
                          ),
                          
                          mainPanel(plotOutput('trendPlot'))
                        ) ),
               
               tabPanel("Decision Tree", verbatimTextOutput("dctext"),
                        h1("Decision Tree"),plotOutput('decision'),
                        htmlOutput("text2"))
               
)

server <- function(input, output) {
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(foreveralone[, input$show_vars, drop = FALSE])
  })
  
  
  output$beeswarmPlot <- renderPlot(
    foreveralone %>% 
      ggplot(aes(x = !!sym(input$var), y = age, col = depressed)) + 
      geom_beeswarm() +
      coord_flip() + 
      theme(panel.grid = element_blank(), legend.position = "bottom")
  )
  
  output$trendPlot <- renderPlot({
    
    foreveralone %>% 
      count(!!sym(input$x), !!sym(input$y), attempt_suicide) %>%
      ggplot(aes(y = n,
                 axis1 = !!sym(input$x),
                 axis2 = !!sym(input$y),
                 axis3 = attempt_suicide)) +
      geom_flow(aes(fill = !!sym(input$x)), width = 0.4) +
      geom_stratum(alpha = .6) + 
      geom_text(stat = "stratum", label.strata = TRUE) + 
      scale_x_discrete(limits = c(input$x, input$y, "Attempted suicide")) + 
      theme_minimal() + theme(text = element_text(size = 15), 
                              legend.position = "bottom") + 
      labs(title = glue::glue("Breakdown of {input$x}, {input$y} and attempted suicide"))
    
    
  })
  
  output$ballPlot <- renderPlot({
    ggplot(balloon, aes_string(x= "variable", y = input$bally)) +geom_point(aes(size = value),shape = 21, colour = "black", fill = "skyblue")+theme(panel.background = element_blank(),
                                                                                                                                                    panel.border = element_rect(colour = "blue", fill = NA, size = 1))+scale_size_area(max_size = 20)
  })
  
  output$mosaicPlot <- renderPlot({
    ggplot(data = foreveralone)+geom_mosaic(aes(x = product(bodyweight), fill = attempt_suicide, conds = product(gender)),divider = mosaic("v"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10))
  })
  model <- rpart(attempt_suicide ~., data = df)
  #fit <- ctree(attempt_suicide ~., data = df)
  
  
  ##summary(fit)
  #printcp(model)
  #plotcp(model)
  #plot(model, uniform=TRUE, 
  #main="Regression Tree for Mileage ")
  #text(model, use.n=TRUE, all=TRUE, cex=.8)
  #rpart.rules(model)
  #visTree(model, edgesFontSize = 14, nodesFontSize = 16, width = "100%")
  #asRules(model)
  
  output$decision <- renderPlot(({
    rpart.plot(model)
    
  }))
  
  #output$text1 <- renderText({paste("Decision Tree")})
  
  output$text2 <- renderUI({
    # rpart.rules(model)
    str1<- paste("There are few important variables that used in the Decision Tree:")
    str2<-paste("1. age")
    str3<-paste("2. depressed")
    str4<-paste("3. employment")
    str5<-paste("4. gender")
    str6<-paste("5. race")
    str7<-paste("6. sexuallity")
    HTML(paste(str1, str2,str3,str4,str5,str6,str7,sep = '<br/>'))
  })
  
}


shinyApp(ui=ui,server=server)