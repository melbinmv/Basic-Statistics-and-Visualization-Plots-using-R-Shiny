
library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)
library(Hmisc)
library(corrgram)
library(rpivotTable)



#plotting theme for ggplot2
.theme<- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)

shinyServer(function(input, output, session){
  
  #update group and
  #variables based on the data
  observe({
    #browser()
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    dataset = read.csv(inFile$datapath, header = TRUE)
    var.opts<-colnames(dataset)
    updateSelectInput(session, "variable", choices = var.opts)
    updateSelectInput(session, "group", choices = var.opts)
  })
  
  data <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    read.table(file=inFile$datapath,sep = ",",  header = TRUE)
    
    
  })
  
  #t-test
  observe({
    updateSelectInput(
      session,
      "var1",
      choices=names(data()))
    
  })
  # Updata value user could select
  observe({
    updateSelectInput(
      session,
      "var2",
      choices=names(data()))
    
  })
  

  
  ttestout <- reactive({
    var1 <- data()[,input$var1]
    conf <- input$conf
    if (is.null(var1)){return(NULL)}
    t1 <- t.test(var1, alternative = input$tail, mu = input$test, conf.level = conf)
    var2 <- data()[,input$var2]
    if (is.null(var2)){return(NULL)}
    ve <- ifelse(input$varequal == 'y', TRUE, FALSE)
    t2 <- t.test(var1, var2, alternative = input$tail, var.equal = ve, conf.level = conf)
    if(input$sample == "oneSamp") {return(t1)}
    if(input$sample == "twoSamp") {return(t2)}
    
  })
  
  
  # Output of one sample t value of t-test
  output$tvalue <- renderTable({
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    tv = vals$statistic
    tv
   
  })
  
  # Output of p value
  output$pvalue <- renderPrint({
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    
    s =format(vals$p.value, scientific = FALSE)
    s
    
  })
  
  # Output of key statistical parametric
  output$parametric <- renderTable({
    var1 <- data()[,input$var1]
    
    if (is.null(var)){return(NULL)}
    var2 <- data()[,input$var2]
    if (is.null(var)){return(NULL)}
    mean1 <- mean(var1)
    mean2 <- mean(var2)
    standard_deviation1 <- sd(var1)
    standard_deviation2 <- sd(var2)
    standard_error1 <- sd(var1)/sqrt(length(var1))
    standard_error2 <- sd(var2)/sqrt(length(var2))
    parametric1 <- data.frame(mean = mean1, 
                              standard_deviation=standard_deviation1, 
                              standard_error=standard_error1)
    rownames(parametric1) <- input$var1
    parametric2 <- data.frame(mean = mean2, 
                              standard_deviation=standard_deviation2, 
                              standard_error=standard_error2)
    rownames(parametric2) <- input$var2
    if(input$sample == "oneSamp") {return(parametric1)}
    if(input$sample == "twoSamp") {return(rbind(parametric1,parametric2))}
  })
  
  
  
  
  
  
  
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file1
  })
  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
  })
  output$tb <- renderUI({
    if(is.null(data()))
    
   h3(strong("Upload a File"))
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),
                  tabPanel("Data View", tableOutput("table")),
                  tabPanel("Descriptive Statistics", verbatimTextOutput("DS")),
                  tabPanel("Correlation",verbatimTextOutput("SUM"),plotOutput("CP")),
                  tabPanel("Graphs", h3(textOutput("caption")),uiOutput("plot"),h3("Dashboard"),rpivotTableOutput("PT")),
                  
                  tabPanel('T-test',
                           
                           fluidRow(column(8, offset = 1,
                                           h3("Output"),
                                           p("Sample statistics:"),
                                           tableOutput('parametric'),
                                        
                                           helpText("The observed t test statistic :"),
                                           tableOutput('tvalue'),
                                           helpText("p-value:"),
                                           textOutput('pvalue'),
                                           helpText("A Low p-value suggests that your sample provides enough evidence that you can reject the null hypothesis for the entire population.")
                                           ))
                  )
                  
                  )
  })
  
  output$caption<-renderText({
    switch(input$plot.type,
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "density" 	=	"Density plot",
           "bar" 		=	"Bar graph")
  })
  
  output$DS <- renderPrint({
    
    infile = input$file1
    if(is.null(infile)) {return()}
    data = read.table(file = infile$datapath,sep = ",",header = TRUE)
    if(is.null(data())) {return()}
    cat(sprintf("\nSummary of the dataset\n\n"))
    print(summary(data))
    
    df = select_if(data,is.numeric)
    cat(sprintf("\n\n Descriptive Statistics about the Data\n\n"))
    print(describe(df))
    
  })
  
  output$SUM<-renderPrint({
    
    
   
    infile = input$file1
    if(is.null(infile)) {return()}
    data = read.table(file = infile$datapath,sep = ",",header = TRUE)
    if(is.null(data())) {return()}
      
  
    
    
    df = select_if(data,is.numeric)
    
    
    
    cat(sprintf("\n Correlation Summary of the dataset\n"))
    
    cor = rcorr(as.matrix(df))$r
    
    print(cor)
  })
  
  output$PT<-renderRpivotTable({
    
   
    
    infile = input$file1
    if(is.null(infile)) {return()}
    data = read.table(file = infile$datapath,sep = ",",header = TRUE)
    if(is.null(data())) {return()}
    
    rpivotTable(data)
    
    
  })
  
  output$CP<-renderPlot({
    
    
    infile = input$file1
    if(is.null(infile)) {return()}
    data = read.table(file = infile$datapath,sep = ",",header = TRUE)
    if(is.null(data())) {return()}
    
    
  
    
    corrgram(data,order = TRUE,lower.panel = panel.shade,upper.panel = panel.pie,main ="Correlation Plot")
  })
  
  
  
  output$plot <- renderUI({
    plotOutput("p")
  })
  
  #get data object
  get_data<-reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    dataset = read.csv(inFile$datapath, header = TRUE)
    check<-function(x){is.null(x) || x==""}
    if(check(dataset)) return()
    
    obj<-list(data=dataset,
              variable=input$variable,
              group=input$group
    )
    
    #require all to be set to proceed
    if(any(sapply(obj,check))) return()
    #make sure choices had a chance to update
    check<-function(obj){
      !all(c(obj$variable,obj$group) %in% colnames(obj$data))
    }
    
    if(check(obj)) return()
    
    
    obj
    
  })
  
  #plotting function using ggplot2
  output$p <- renderPlot({
    
    plot.obj<-get_data()
    
    #conditions for plotting
    if(is.null(plot.obj)) return()
    
    #make sure variable and group have loaded
    if(plot.obj$variable == "" | plot.obj$group =="") return()
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(),
                      "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                      "density" 	=	geom_density(alpha=.75),
                      "bar" 		=	geom_bar(position="dodge")
    )
    
    
    if(input$plot.type=="boxplot")	{		#control for 1D or 2D graphs
      p<-ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$group,
                  y 		= plot.obj$variable,
                  fill 	= plot.obj$group # let type determine plotting
                )
      ) + plot.type
      
      if(input$show.points==TRUE)
      {
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      }
      
    } else {
      
      p<-ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$variable,
                  fill 	= plot.obj$group,
                  group 	= plot.obj$group
                  #color 	= as.factor(plot.obj$group)
                )
      ) + plot.type
    }
    
    p<-p+labs(
      fill 	= input$group,
      x 		= "",
      y 		= input$variable
    )  +
      .theme
    print(p)
  })
  
  # set uploaded file
  upload_data<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #could also store in a reactiveValues
    read.csv(inFile$datapath,
             header = TRUE,
             sep = ',')
  })
  
  observeEvent(input$file1,{
    inFile<<-upload_data()
  })
  
  session$onSessionEnded(function() {
    
    
    stopApp()
  })
  
  
})
