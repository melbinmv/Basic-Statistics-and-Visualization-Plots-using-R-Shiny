library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)
library(Hmisc)
library(corrgram)
library(rpivotTable)


#example data


#plotting theme for ggplot2
.theme<- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)


# UI for app
ui<-(pageWithSidebar(
  # title
  headerPanel("Select Options"
            
              ),
  
  
  #input
  sidebarPanel
  (
    
    fileInput("file1", "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    helpText("Default max. file size is 5MB"),
    conditionalPanel(condition = "$('li.active a').first().html()==='About file'",
                     h4(strong("About")),
                     p("This R-Shiny based application is the part of a project done for Msc Business Anlaytics in Manipal University.", align="justify"),
                     h4(strong("Project Team")),
                     h6("Anjana P "),
                     h6("Melbin Martin"),
                     h6("Mohak Agarwal"),
                     h6("Mohamed Bahish"),
                     h6("Muhammed Dilkesh"),
                     h4(strong("Guide")),
                     h6("Mr. Kishore L"),
                     h6("Assistant Professor"),
                     br(),
                     tags$img(src = 'logo .png', height =50, width = 200, )
                     
    ),
    conditionalPanel(condition = "$('li.active a').first().html()==='Graphs'",
                     selectInput("variable","Variable:", choices = NULL),
                     selectInput("group","Group:", choices = NULL),
                     selectInput("plot.type","Plot Type:",
                                 list(Boxplot = "boxplot", Histogram = "histogram", Density = "density", Bar = "bar")
                     ),
                     checkboxInput("show.points", "show points", TRUE),
                     br(),
                     
    ),
    conditionalPanel(condition = "$('li.active a').first().html()==='T-test'",
                    
                     radioButtons("sample",
                                  "Type of t test:",
                                  choices = c("One sample" = "oneSamp", 
                                              "Two sample" = "twoSamp")),
                     selectInput("var1", 
                                 label = "Select a Numerical Variable",
                                 ""
                     ),
                     conditionalPanel(condition = "input.sample == 'twoSamp'",
                                      selectInput("var2", 
                                                  label = "Please select a Numerical Variable",
                                                  ""
                                      ),
                                      radioButtons("varequal",
                                                   "Are the two samples have equal variance:",
                                                   choices = c("Yes" = "y",
                                                               "No" = "n"))
                     ),
                     selectInput("tail",
                                 label = "Select a relationship you want to test:",
                                 choices = c("Equal" = "two.sided", 
                                             "Less" = "less",
                                             "Greater" = "greater")),
                     conditionalPanel(condition = "input.sample == 'oneSamp'",
                                      numericInput("test",
                                                   "Mean value You Want to Test",
                                                   value = 0
                                                   
                                      )
                     ),
                     numericInput("conf",
                                  label = "Confidence level:",
                                  value = 0.95,
                                  min = 0.8,
                                  max = 0.99),
                     helpText("Note: Please assign a number between 0 and 1 in the numeric Input")
                     
    )
    
  ),
  
  # output
  mainPanel(
    uiOutput("tb")
  )
))
