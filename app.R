
library(fields)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(dplyr)
#library(Stat2Data)
library(tidyselect)



data(iris) # ciation for iris
#load file
Kiterfunction <- readRDS("Kiterfunction.rds")




# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "red",
    ### Create the app header ----
    dashboardHeader(
      title = "k-means Clustering", # You may use a shortened form of the title here
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "k_means Clustering")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
        menuItem("Examples", tabName = "example", icon = icon("book")),
        #menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1(em("k"),"-means Clustering"), # This should be the full name.
          h2("M I S S I O N"),
          p("Procced to the example page by clicking the Go Button below. If you have little
            no background with k-means_clustering, proceed to the Prerequisites Page for
            more relevent information."),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goExplore",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "The orginial version of the app was developed and coded by Hongyi Xia",
            br(),
            "I would like to acknowledge and appreciate Professor Hatfield, 
            Professor Pearl, and Professor Jacopo for their guidance and 
            support.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 6/13/2022 by Hongyi Xia.")
          )
        ),
        
        tabItem(
          tabName = "Prerequisites",
          withMathJax(),
          h2("Prerequisites"), 
          p("Review information about the k_means clustering."), 
          br(), 
          box(
            width = 12, 
            collapsible = TRUE,
            collapsed = FALSE,
            title = h3("Clustering"), 
            p("Definition: Clustering is the general unsupervised task of grouping 
              a set of objects in such a way that objects in the same group (called a cluster) 
              are more similar to each other than to those in other groups (clusters)."), 
            p("It can be achieved by various algorithms. Broadly speaking, according 
              to the group partition obtained we can divide clustering methods in two groups:"), 
            p("Hard Clustering: each data point belongs to only one cluster."),
            p("Soft Clustering: for each point we have a probability or likelihood 
              to be assigned in a cluster. In some algorithm it is also possible to 
              identify overlapping clusters"),
            p("We are going to focus mainly on exhaustive (every data point is assigned) 
              hard clustering algorithms, k-means")
          ), 
          box(
            width = 12, 
            collapsible = TRUE,
            collapsed = FALSE,
            title = h3("K-means Algorithm"), 
            p("k -means Clustering is a partitioning algorithm that splits the data in 
              k clusters by iteratively computing centroids and moving data points to 
              the closest centroids until convergence."), 
            p("For determining the distance between data points and centrods in this k_means app,
              Euclidean distance is applied by default, defined 
              by"),
            withMathJax(helpText('$$ \\sum_{i=1}^n \\sqrt{(x_i - y_i)^2} $$'))
            
            
          ), 
          box(
            width = 12, 
            collapsible = TRUE,
            collapsed = FALSE,
            title = h3("Total within cluster sum of squares"), 
            p("The within-cluster sum of squares is a measure of the variability 
              of the observations within each cluster. In general, a cluster that 
              has a small sum of squares is more compact than a cluster that has 
              a _large sum of squares. As the number of observations increases, 
              the sum of squares becomes larger, so it depends on the number of elements."), 
            p("In general we can sum the within-cluster sum of squares to get the 
              total within cluster sum of square that cna be used to assess the 
              goodness of our clustering."),
            p("We also canidentify the optimal number of clusters with the Total within cluster 
              sum of squares by looking for an elbow in the Total within cluster 
              sum of squares versus number of clusters plot (provided to you in
              the example page")
            
          ),
          
          br(), 
          ##### Go Button
          div(
            style = "text-align: center",
            bsButton(
              inputId = "goExplore",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ), 
      
      ### set up example page
      tabItem(
        tabName = "example",
        withMathJax(),
        h2("Illustrative example of K-means with 3 centroids"),
        p("Check out the slider with a play button to see changes in the cluster 
          centroids with iterations of the K-means algorithm. The stacked barplot 
          that accompanies the centroids plot show changes in the within-group 
          distances as iterations progress."),
        fluidRow(
          column(
            width = 4,
            offset = 0,
            wellPanel(
              sliderInput(
                inputId = "Exiter", 
                "Animation:", 
                label = "current iteration",  
                value = 0.5,  
                min = 0.5,
                max = 10,
                step = 0.5,
                animate = animationOptions(interval = 1500)
              )
            ),
            uiOutput(outputId = "dataDescription"),
            p(".")
          ),
          column(
            width = 8,
            offset = 0,
            br(),
            plotOutput(outputId = "Exdotplot"),
            plotOutput(outputId = "Exbarplot")
          )
        )
      ),
        
        #### Set up an Explore Page ----
        ### Explore Page ----
      tabItem(
        tabName = "explore",
        withMathJax(),
        h2("Explore Data Collections"),
        p("This page should include something for the user to do, the more
            active and engaging, the better. The purpose of this page is to help
            the user build a productive understanding of the concept your app
            is dedicated to."),
        fluidRow(
          column(
            width = 4,
            offset = 0,
            wellPanel(
              selectInput(
                inputId = "selectData",
                label = "Choose a data collection",
                choices = c(
                  "iris" = "iris",
                  "Blood" = "Blood1",
                  "Fruit Flies" = "FruitFlies2"
                ),
                selected = "iris"
              ),
              actionButton("ready","Ready!"),
              actionButton("reset","Reset!"),
              tableOutput("checkout")
            )
          ),
          column(
            width = 8,
            offset = 0,
            br(),
            plotOutput(outputId = "plot2",click = "plot_click"),
            plotOutput(outputId = "plotscatter",click = "plot_click")
          )
        )
      ),
        
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p("You'll need to fill in this page with all of the appropriate
            references for your app."),
          p(
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61.1). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2022). boastUtils: BOAST utlities.
            (v 0.1.12.3). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Change, W., and Borges Ribeiro, B. (2021). shinydashboard: Create 
            dashboards with 'shiny'. (v 0.7.2) [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny:
            Web application framework for R. (v 1.7.1). [R package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Hadley, W., RStudio (2021). tidyverse: Easily Install and Load the 
            'Tidyverse'. (v 1.3.1) [R package]. Available from
             https://CRAN.R-project.org/package=tidyverse"
          ),
          p(
            class = "hangingindent",
            "Henry, L., Wickham, H., RStudio (2022). tidyselect: Select from a 
            Set of Strings. (v 1.1.2) [R package]. Available from
             https://cran.r-project.org/web/packages/tidyselect/index.html"
          ),
          p(
            class = "hangingindent",
            "Nychka, D., Furrer, R., Paige, J., Sain, S., Gerber, F., Iverson, M.,
            and University Corporation for Atmospheric Research (2021). 
            fields: Tools for Spatial Data. (v 13.0.0). [R package]. Available from
            https://CRAN.R-project.org/package=fields"
          ),
          p(
            class = "hangingindent",
           "Perrier, V., Meyer, F., and Granjon, D. (2022). shinyWidgets: Custom
            inputs widgets for shiny. (v 0.7.0). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          
  
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)



server <- function(input, output, session) {
  
  ## data description
  observeEvent(
    eventExpr = input$Exiter,
    handlerExpr = {
      description <- switch(
        EXPR = ((input$Exiter)%%1)*2+1,
        "After the data points changes color, the position of centroids change
         as well. The new centroid position will take on the mean x-coordinates and mean
         y-coordinates of data points that the centroid has the same color with. For example, 
         the new position of green centroid will move to the center of cluster of data points
         colored in green. Then, a new round of coloring of data points start over again. These
         iterative step carry on until there is no more change in the coloring of data points and 
         position of the centroids",
        "Notice: The color of the data points change at the background based on the
         distance to the centroid they are cloest to. For example, the green data points
         have the closest distance to the green centroid."
      )
      
      output$dataDescription <- renderUI(description)
    }
  )
  
 
  
  ## Set Buttons
  observeEvent(
    eventExpr = input$goExplore,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "example"
      )
    }
  )
  
  ## Set the Data Collection
  dataCollection <- eventReactive(
    eventExpr = input$mydata,
    valueExpr = {
      switch(
        EXPR = input$mydata,
        iris = iris,
        FruitFlies2 = FruitFlies2
      )
    }
  )
  
  
  
  click_saved <- reactiveValues(singleclick = NULL) #no click
  
  observeEvent(eventExpr = input$plot_click, handlerExpr = { 
    click_saved$singleclick <- rbind(click_saved$singleclick,
                                     c(input$plot_click[1], input$plot_click[2])) 
    
    click_saved$singleclick <- as.data.frame(click_saved$singleclick)
    centroidsplot <- as.data.frame(matrix(unlist(click_saved$singleclick), ncol=2, byrow=F))
    
    output$plot2 <- renderPlot({
      ggplot(data = iris, aes(x=Sepal.Length,y=Sepal.Width), color="black") +
        geom_point(alpha = 0.5) +
        geom_point(data = centroidsplot, aes(x = centroidsplot[,1], y = centroidsplot[,2]), 
                   size=5, shape=17, inherit.aes = FALSE) +
        theme_bw() +
        scale_color_manual(values = boastUtils::psuPalette)
      
      #points(click_saved$singleclick[,1], click_saved$singleclick[,2], col="black", pch = 3, cex=3)
    })
    
    output$check <- renderTable({
      as.data.frame(matrix(unlist(click_saved$singleclick), ncol=2, byrow=F))
    })
    
  })
  
  
  #initialize scatter dataframe
  initScatter <- reactiveValues(init = data.frame(NumClut = c(),
                                        TotWith = c()))
  
  
  # if you click ready 
  observeEvent(eventExpr = input$ready, handlerExpr = { 
    
    if(length(click_saved$singleclick)>0){
      
      centroids <- as.data.frame(matrix(unlist(click_saved$singleclick), ncol=2, byrow=F))
      
      
      ## identify a possible error
      tryCatch({kdata <- kmeans(iris[,1:2], centers = centroids)},
               error = function(e){
                 sendSweetAlert(
                   session = session,
                   type = "error",
                   title = "Choose a proper set of centroid",
                   text = "your current choice of centroid led to 0 assignment of data to some centroids"
                 )
                 
                 click_saved$singleclick <- c(NULL, NULL)
                 
                 output$check <- renderTable({
                   as.data.frame(c(NULL, NULL))
                 })
                 
                 output$plot2 <- renderPlot(
                   expr = {
                     ggplot(data = iris, aes(x=Sepal.Length,y=Sepal.Width), color="black") +
                       geom_point(alpha = 0.5) +
                       theme_bw() +
                       scale_color_manual(values = boastUtils::psuPalette)
                   })
                 
               }
                 )
                
      ## if no error then proceed        
      if(length(click_saved$singleclick)>0){
      
        kdataset <- data.frame(kdata$centers, grouping = as.character(1:nrow(centroids)))
        iris$grouping <- as.character(kdata$cluster)
      
        output$plot2 <- renderPlot(
          expr = {
            ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = grouping)) +
              geom_point(alpha = 0.6) +
              geom_point(data = kdataset, aes(x = Sepal.Length, y = Sepal.Width, color = grouping),
                       size=5, shape=17, inherit.aes = FALSE) +
              theme_bw() +
              scale_color_manual(values = boastUtils::psuPalette)
          })
        
        # create scatter plot
        newobs <- data.frame(NumClut = as.character(length(kdata$withinss)),
                             TotWith = kdata$tot.withinss)
        
        initScatter$init <- rbind(initScatter$init, newobs)
        
        output$plotscatter <- renderPlot(
          expr = {
            ggplot(data = initScatter$init, aes(x = NumClut, y = TotWith)) +
              geom_point() +
              geom_boxplot() +
              theme_bw() +
              scale_color_manual(values = boastUtils::psuPalette)
          })
        
        output$checkout <- renderTable({
          arrange(initScatter$init, NumClut)
        })
        
        }
    } else {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "No centroid found",
        text = "Specify centroid on the diagram"
      )
    }
  })
  
  # if you click RESET
  observeEvent(eventExpr = input$reset, handlerExpr = { 
    
    # back no click
    #click_saved$singleclick <- centroidata[6:10,]  #list() #niente cliccato
    click_saved$singleclick <- c(NULL, NULL)
    
    output$check <- renderTable({
      as.data.frame(c(NULL, NULL))
    })
    
    output$plot2 <- renderPlot(
      expr = {
        ggplot(data = iris, aes(x=Sepal.Length,y=Sepal.Width), color="black") +
          geom_point(alpha = 0.5) +
          theme_bw() +
          scale_color_manual(values = boastUtils::psuPalette)
      })
    
    
  })
  
  # clear table
  
  
  
  
  output$plot2 <- renderPlot(
    expr = {
      ggplot(data = iris, aes(x=Sepal.Length,y=Sepal.Width), color="black") +
        geom_point(alpha = 0.5) +
        theme_bw() +
        scale_color_manual(values = boastUtils::psuPalette)
    })
  
  #plot for the example page
  
  observeEvent(
    eventExpr = input$Exiter,
    handlerExpr = {
      k <- input$Exiter
      scale <- 0
      if (k >= 1.5){
        scale <- 1
      }
      
      basePlot <-
        ggplot(data=filter(Kiterfunction$irisData, iteration == 1), 
               aes(x=Sepal.Length,y=Sepal.Width), color="black") +
        geom_point(alpha=1-scale) +
        theme_bw() +
        scale_color_manual(values = boastUtils::psuPalette) +
        guides(color =  guide_legend(title = "Cluster Grouping")) +
        theme(legend.title = element_text(size = 20)) +
        theme(legend.text = element_text(size = 20)) +
        theme(axis.title = element_text(size = 20)) 
      
      if (k == 0.5){
        outputPlot <- basePlot
      } else {
        outputPlot <- basePlot +
          geom_point(data=filter(Kiterfunction$centroids, iteration == floor(k)), 
                     aes(x=xCoords,y=yCoords,color=grouping), size=5, shape=17)
        
        if (k >= 1.5){
          outputPlot <- outputPlot + 
            geom_point(data=filter(Kiterfunction$irisData, iteration == floor(k-0.5)), 
                       aes(x=Sepal.Length,y=Sepal.Width,color=grouping), alpha=0.5)
        }
        
        if (k >= 2){
          outputPlot <- outputPlot +
            geom_point(data=filter(Kiterfunction$centroids, iteration == floor(k-1)), 
                       aes(x=xCoords,y=yCoords,color=grouping),size=5,shape=2) +
            geom_path(data=filter(Kiterfunction$centroids,
                                  iteration == floor(k) | iteration == floor(k-1)), 
                      aes(x = xCoords, y = yCoords, color = grouping))
        }
      }
      output$Exdotplot <- renderPlot(expr = {outputPlot})
      
      
      
      ## barplot 
      
      totData <- 
        filter(Kiterfunction$centroids, iteration <= k) %>%
        group_by(iteration) %>%
        summarise(total = sum(totWithin))
      
      barOutput <- 
      ggplot(data=filter(Kiterfunction$centroids, iteration <= floor(k)), 
             aes(x=iteration, y=totWithin, fill=grouping)) +
             geom_bar(position="stack", stat="identity") +
             geom_line(data=totData, aes(x=iteration, y=total), inherit.aes = FALSE) +
             scale_fill_manual(values = boastUtils::psuPalette) +
             theme_bw() +
             ylab("Totla Within Sum of Squares") +
             theme(legend.title = element_text(size = 20)) +
             theme(legend.text = element_text(size = 20)) +
             theme(axis.title = element_text(size = 20)) +
             scale_x_continuous(breaks = seq(1,10,1))
             
      
      output$Exbarplot <- renderPlot(expr = {barOutput})
      
    }
  )
  
  
  

  
}


boastApp(ui = ui, server = server)

