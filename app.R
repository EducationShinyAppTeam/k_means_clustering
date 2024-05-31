# Load packages ----
library(fields)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(dplyr)
library(Stat2Data)
library(tidyselect)
library(DT)
library(ggplot2)

library(devtools)
# library(easyGgplot2)
library(Spectrum)
library(data.table)

# Load data ----
data(circles, package = "DRIP")
circle <- as.data.frame(t(circles))

data(iris) # citation for iris
iris <- iris[,1:2]

## k-means Iteration data ----
Kiterfunction <- readRDS("Kiterfunction.rds")

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "green",
    ## Header ----
    dashboardHeader(
      title = "k-means Clustering",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "k_means_clustering")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          id = "home",
          href = 'https://shinyapps.science.psu.edu/',
          icon("home")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Example", tabName = "example", icon = icon("book-open-reader")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1(tags$em("k-"),"means Clustering"),
          p("This app allows students to explore the", tags$em("k"), "-means
            Clustering algorithm."),
          p("This app allows students to explore the", tags$em("k-"), "means
            clustering algorithm. Students can work through an example of how
            the algorithm works and then explore the relationship between the
            number of centers,", tags$em("k,"),"and the total variability within
            each cluster."),
          h2("Instructions"),
          p("In order to use this app more effectively, we recommend working
            through the app in the following order."),
          tags$ol(
            tags$li("Review prerequistes using the Prerequistes tab."),
            tags$li("Work through the example of the algorithm being applied to
                    a real-world data set."),
            tags$li("Explore the relationship between the number of centroids/clusters
                    and the total amount of variation within each cluseter.")
          ),
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
            "This app was originally developed and coded by Hongyi Xia based upon
            an app created by Dr. Jacopo Di Iorio, who oversaw the project.
            Additional updates were implimented by Dr. Hatfield.",
            br(),
            "I would like to acknowledge Drs. Di Iorio, Hatfield, and Pearl for
            their guidance and support.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 5/31/2024 by NJH.")
          )
        ),
        ### Prereqs ----
        tabItem(
          tabName = "Prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("Review information about the ", tags$em("k"), "-means clustering
            algorithm."),
          br(),
          box(
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            title = strong("Clustering"),
            p("Clustering is a machine-assisted learning approach that focuses
              on searching for ways to organize a set of individuals cases (objects
              or living beings) into a set of groups (or clusters). Cases that
              are in the same cluster (group) are similar to each other in terms
              of their values of the attributes we used in forming the clusters.
              By extension, cases that are in", tags$em("different"), "clusters
              should be dissimilar (different)."),
            p("As an unsupervised approach, we do not have an existing grouping
              that we are trying to re-construct. Rather, we are exploring our
              data collection to see in what ways the cases might relate to one
              another."),
            p("There are many different clustering algorithms. We focus on the",
              tags$em("k-"), "means approach in this app. This approach will
              seek to ensure that every case is assigned to one and only one of
              the", tags$em("k"), "clusters.")
          ),
          box(
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            title = tags$strong("The General", tags$em("k-"),"means Algorithm"),
            p("There are several algorithms that fall under the title of",
              tags$em("k-"), "-Means Clustering. Each of these algorithms attempt
              to do the same thing--split the data collection into", tags$em("k"),
              "clusters. The general algorithm follows these steps:"),
            tags$ol(
              tags$li("Set the", tags$em("k"), "initial centroids via"),
              tags$ol(
                type = "a",
                tags$li(tags$strong("The Forgy Method:"), "randomly select",
                        tags$em("k"), "cases from the dataset to act as initial
                        centroids."),
                tags$li(tags$strong("Random Partition:"), "randomly assign each
                        case to one of  the", tags$em("k"), "clusters. The initial
                        centroids arethen calculated for each of these clusters."),
                tags$li(tags$strong("User Specified:"), "the user specifies",
                        tags$em("k"), "points to use as the initial centroids.")
              ),
              tags$li("Calculate the Euclidean distance between each case and
                      each of the centroids."),
              tags$li("Assign each case to the cluster of the centroid they are
                      closest two. If they are equidistant from multiple centroids,
                      randomly assign the case to one of those clusters."),
              tags$li("Re-calculate the centroids for the new cluster assignments."),
              tags$li("Repeat the last three steps until either 1) the centroids
                      do not change or 2) you reach a pre-set number of iterations.")
            )
          ),
          box(
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            title = tags$strong("Centroids"),
            p("The centroid of a data collection (e.g., a cluster) is the point
              whose values correspond to the values of the",
              tags$em("sample arithmetic mean"), "for each attribute we're
              examining based upon the cases in that collection/cluster."),
            p("For example, imagine that we are clustering a collection of people
              based on their height (m), weight (kg), and age (years) into four
              clusters. The centroid for the first group would be
              \\(C_1=\\left(\\overline{\\text{height}},\\overline{\\text{weight}},
              \\overline{\\text{age}}\\right)\\); \\(C_1\\) only draws upon the
              people (cases) who are currently assigned to Cluster 1.")
          ),
          box(
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            title = tags$strong("Euclidean Distance"),
            p("The Euclidean distance between two points can be thought of as
              the length of the (straight) line segment connecting the points and
              is related the Pythagorean theorem. To calculate the Euclidean
              distance between two points, \\(\\vec{p}\\) and \\(\\vec{q}\\), we
              can use the following formula:
              \\[d(\\vec{p},\\vec{q})= \\sqrt{\\sum_{i=1}^n(p_i - q_i)^2}\\]
              where \\(\\vec{p}\\) is the point defined as
              \\(\\left(p_1,p_2,\\ldots,p_n\\right)\\) and \\(\\vec{q}\\) is the
              point defined as \\(\\left(q_1,q_2,\\ldots,q_n\\right)\\)."
            )
          ),
          box(
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            title = tags$strong("Total Within-Cluster Sum of Squares"),
            p("The Within-Cluster Sum of Squares is a measure of the variability
              of the observations within each cluster. In general, a cluster that
              has a small sum of squares is more compact than a cluster that has
              a large sum of squares. That is to say, the cases within a cluster
              are closer together and more similar to each other."),
            p("As the number of cases within a cluster increases, there are more
              more ways these similar cases can still differ from each other. Thus,
              the Within-Cluster Sum of Squares will increase as the cluster size
              increases."),
            p("By looking across all of the clusters, we can get a sense of how
              well this run of the algorithm has clustered the cases. The sum of
              the Within-Cluster Sums of Squares (i.e., Total Within-Cluster Sum
              of Squares), quantifies this aspect. We can use the Total
              Within-Cluster Sum of Squares to find an optimal number of clusters
              by looking for an elbow in the plot of Total Within-Cluster Sum of
              Squares versus the number of clusters.")
          )
        ),
      ### Example page ----
      tabItem(
        tabName = "example",
        withMathJax(),
        h2("Illustrative Example of", tags$em("k-"), "means with 3 Centroids"),
        p("Move the slider (or press the play button) to watch what changes in
          the plot as the", tags$em("k-"), "means clustering algorithm gets
          applied. The slider steps through mutiple iterations of the",
          tags$em("k-"), "means algorithm, displaying how the centroids move and
          data points get re-grouped. The stacked barplot that accompanies the
          centroids plot shows changes in the within-cluster sums of squares as
          iterations progress."),
        fluidRow(
          column(
            width = 4,
            offset = 0,
            wellPanel(
              sliderInput(
                inputId = "exampleIteration",
                label = "Current step",
                value = 0,
                min = 0,
                max = 18,
                step = 1,
                animate = animationOptions(interval = 2000)
              )
            ),
            uiOutput(outputId = "exampleDescription"),
          ),
          column(
            width = 8,
            offset = 0,
            br(),
            plotOutput(outputId = "exampleScatter", height = "500px"),
            plotOutput(outputId = "exampleBarPlot", height = "500px")
          )
        )
      ),
      ### Explore Page ----
      tabItem(
        tabName = "explore",
        withMathJax(),
        h2("Explore Data Collections"),
        p("Two datasets, iris and circles, are provided. Plot centroids on the scatter plot and
          click Run to run the K-means Algorithm. Check out the corresponding Total Within Sum of
          Squre Value in the table and dotplot below."),
        fluidRow(
          column(
            width = 5,
            offset = 0,
            wellPanel(
              selectInput(
                inputId = "selectData",
                label = "Choose a data collection",
                choices = c(
                  "iris" = "iris",
                  "circle" = "circle"
                ),
                selected = "iris"
              ),
              actionButton("ready","Run"),
              actionButton("reset","Clear centroids"),
              actionButton("clear","Clear Table")
            ),
            DTOutput("TotTable") #DTOutput("TotTable")
          ),
          column(
            width = 7,
            offset = 0,
            br(),
            plotOutput(outputId = "plot2",click = "plot_click"),
            #tableOutput("check"),
            plotOutput(outputId = "plotscatter")
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


# Define Server ----
server <- function(input, output, session) {

  ## Define Info Button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = switch(
          EXPR = input$pages,
          example = "Use the slider to step through the algorithm to watch k-means
          cluster get applied to the Iris data collection.",
          explore = "Click in the scatter plot to set initial centroids and then
          run the k-means algorithm. Explore what happens when you change the initial
          centroids and the number of centroids.",
          "Use this app to explore some of the principles behind k-means clustering."
        )
      )
    }
  )

  ## Define Go Button ----
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

  ## Example Page ----

  ### Display Example Iteration Info ----
  observeEvent(
    eventExpr = input$exampleIteration,
    handlerExpr = {
      if (input$exampleIteration == 0) {
        description <- "Plot Data"
      } else if (input$exampleIteration == 1) {
        description <- "Set Initial Centroids"
      } else if (input$exampleIteration == 18) {
        description <- "Convergence"
      } else if (input$exampleIteration %% 2 == 0) {
        description <- "Color Points"
      } else if (input$exampleIteration %% 2 == 1) {
        description <- "Find New Centroids"
      }

      output$exampleDescription <- renderUI(
        expr = {
          p(tags$strong("What's happening:"), description)
        }
      )
    }
  )

  ### Plots on the Example Page ----
  observeEvent(
    eventExpr = input$exampleIteration,
    handlerExpr = {
      # Set the iteration
      k <- (input$exampleIteration + 1)/2
      # scale <- 0
      # if (k >= 1.5) {scale <- 1}

      #### Create the base scatter plot ----
      baseScatter <- ggplot(
        data = filter(.data = Kiterfunction$irisData, iteration == 1),
        mapping = aes(x = Sepal.Length, y = Sepal.Width)
        ) +
        geom_point( #Show/hide unclustered points
          alpha = ifelse(test = input$exampleIteration >= 2, yes = 0, no = 1)
        ) +
        theme_bw() +
        scale_color_manual(values = boastUtils::psuPalette) +
        labs(
          x = "Sepal length (cm)",
          y = "Sepal width (cm)",
          color = "Cluster/Grouping",
          title = "Iris Clustering"
        ) +
        theme(
          text = element_text(size = 20),
          legend.position = "bottom"
        )

      #### Example Scatter Plot ----
      output$exampleScatter <- renderPlot(
        expr = {
          if (input$exampleIteration == 0) {
            baseScatter
          } else {
            outputPlot <- baseScatter +
              geom_point(
                data = filter(
                  .data = Kiterfunction$centroids,
                  iteration == floor(k)
                ),
                mapping = aes(x = xCoords, y = yCoords, color = grouping),
                size = 5,
                shape = 17
              )

            if (k >= 1.5) {
              outputPlot <- outputPlot +
                geom_point(
                  data = filter(
                    .data = Kiterfunction$irisData,
                    iteration == floor(k - 0.5)
                  ),
                  mapping = aes(
                    x = Sepal.Length,
                    y = Sepal.Width,
                    color = grouping
                  ),
                  alpha = 0.5
                )
            }

            if (k >= 2) {
              outputPlot <- outputPlot +
                geom_point(
                  data = filter(
                    .data = Kiterfunction$centroids,
                    iteration == floor(k - 1)
                  ),
                  mapping = aes(x = xCoords, y = yCoords, color = grouping),
                  size = 5,
                  shape = 2
                ) +
                geom_path(
                  data = filter(
                    .data = Kiterfunction$centroids,
                    iteration == floor(k) | iteration == floor(k - 1)
                  ),
                  mapping = aes(x = xCoords, y = yCoords, color = grouping)
                )
            }
            outputPlot
          }
        },
        alt = switch(
          EXPR = input$exampleIteration + 1,
          "Scatter plot of iris sepal width by sepal length",
          "Same scatter plot now with three centroids located at (8,2) in dark blue,
          (8,2.4) in maroon, and (8,2.8) in teal",
          "The same scatter plot as before but now the cases are colored the same
          as which centroid they are closest to",
          "The centroids update; dark blue at (5,2), maroon at (5.6, 2.4),
          teal at (5.8, 3.2).",
          "The same scatter plot as before but now the cases are colored the same
          as which centroid they are closest to",
          "The centroids update; dark blue at (4.7,2.7), maroon at (5.7, 2.6),
          teal at (6, 3.2).",
          "The same scatter plot as before but now the cases are colored the same
          as which centroid they are closest to",
          "The centroids update; dark blue at (4.8,3.1), maroon at (5.8, 2.6),
          teal at (6.4, 3.2).",
          "The same scatter plot as before but now the cases are colored the same
          as which centroid they are closest to",
          "The centroids update; dark blue at (4.7,3.3), maroon at (5.7, 2.6),
          teal at (6.6, 3.2).",
          "The same scatter plot as before but now the cases are colored the same
          as which centroid they are closest to",
          "The centroids update; dark blue at (5,3.4), maroon at (5.8, 2.7),
          teal at (6.8, 3.1).",
          "The same scatter plot as before but now the cases are colored the same
          as which centroid they are closest to",
          "The centroids update; dark blue at (5,3.5), maroon at (5.8, 2.7),
          teal at (6.8, 3.1).",
          "The same scatter plot as before but now the cases are colored the same
          as which centroid they are closest to",
          "The centroids update; dark blue at (5,3.4), maroon at (5.8, 2.7),
          teal at (6.8, 3.1).",
          "The same scatter plot as before but now the cases are colored the same
          as which centroid they are closest to",
          "The centroids update; dark blue at (5,3.4), maroon at (5.8, 2.7),
          teal at (6.8, 3.1).",
          "The scatter plot does not change as we've convereged."
        )
      )

      ### TWCSS Bar Plot ----
      #### Get TWCSS Data ----
      twcssData <- Kiterfunction$centroids %>%
        filter(iteration <= k) %>%
        group_by(iteration) %>%
        summarise(total = sum(totWithin))

      #### Create base bar plot ----
      baseBar <- ggplot(
        data = filter(.data = Kiterfunction$centroids, iteration <= floor(k)),
        mapping = aes(x = iteration, y = totWithin, fill = grouping)
      ) +
        geom_bar(position = "stack", stat = "identity") +
        scale_fill_manual(values = boastUtils::psuPalette) +
        theme_bw() +
        labs(
          x = "Iteration",
          y = "Within-Cluster Sums of Squares",
          fill = "Cluster/Grouping",
          title = "Total Within-Cluster Sum of Squares by Iteration"
        ) +
        scale_x_continuous(breaks = seq(1,10,1)) +
        theme(
          text = element_text(size = 20),
          legend.position = "bottom"
        )
      #### Example TWCSS Bars ----
      output$exampleBarPlot <- renderPlot(
        expr = {
          if (input$exampleIteration < 3) {
            baseBar
          } else {
            baseBar +
              geom_path(
                inherit.aes = FALSE,
                data = twcssData,
                mapping = aes(x = iteration, y = total)
              )
          }
        },
        alt = "Coming soon"
      )
    }
  )

  ## Explore Page ----

  ## Set the Data Collection
  dataCollection <- eventReactive(
    eventExpr = input$selectData,
    valueExpr = {
      switch(
        EXPR = input$selectData,
        iris = iris,
        circle = circle
      )
    }
  )



  click_saved <- reactiveValues(singleclick = NULL) #no click

  observeEvent(eventExpr = input$plot_click,
               handlerExpr = {
                        click_saved$singleclick <- rbind(click_saved$singleclick,
                                                  c(input$plot_click[1], input$plot_click[2]))

    click_saved$singleclick <- as.data.frame(click_saved$singleclick)

    # if there are more than 8 centroids
    if(nrow(click_saved$singleclick)>8){
      sendSweetAlert(
        session = session,
        type = "error",
        title = "More than 8 centroids chosen",
        text = "Limit the number of centroids in the plot to at most 8"
      )
      click_saved$singleclick <- click_saved$singleclick[1:8,]
    }

    centroidsplot <- as.data.frame(matrix(unlist(click_saved$singleclick), ncol=2, byrow=F))

    output$plot2 <- renderPlot( expr = {
      ggplot(data = dataCollection(), aes_string(x=ifelse(input$selectData == "iris",
                                                          "Sepal.Length","x1"),
                                                 y=ifelse(input$selectData == "iris",
                                                          "Sepal.Width","y1")),
             color="black") +
        geom_point(alpha = 0.5) +
        geom_point(data = centroidsplot, aes(x = centroidsplot[,1], y = centroidsplot[,2]),
                   size=5, shape=17, inherit.aes = FALSE) +
        theme_bw() +
        scale_color_manual(values = boastUtils::psuPalette) +
        guides(color =  guide_legend(title = "Cluster Grouping")) +
        xlab(ifelse(input$selectData == "iris", "Sepal Length (cm)", "X")) +
        ylab(ifelse(input$selectData == "iris", "Sepal Width (cm)", "y")) +
        theme(legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              axis.title = element_text(size = 20),
              legend.position="bottom"
              )


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

    # accounted for error when more than 8 centroids
      if(length(click_saved$singleclick)>0){

      centroids <- as.data.frame(matrix(unlist(click_saved$singleclick), ncol=2, byrow=F))

      if (input$selectData == "iris"){
        currentData <- iris[,1:2]
      } else {
        currentData <- circle[,1:2]
      }

      ## identify a possible error
      tryCatch({kdata <- kmeans(currentData, centers = centroids)},
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
                     ggplot(data = dataCollection(), aes_string(x=ifelse(input$selectData == "iris",
                                                                         "Sepal.Length","x1"),
                                                                y=ifelse(input$selectData == "iris",
                                                                         "Sepal.Width","y1")),
                            color="black") +
                       geom_point(alpha = 0.5) +
                       theme_bw() +
                       scale_color_manual(values = boastUtils::psuPalette) +
                       guides(color =  guide_legend(title = "Cluster Grouping")) +
                       xlab(ifelse(input$selectData == "iris", "Sepal Length (cm)", "X")) +
                       ylab(ifelse(input$selectData == "iris", "Sepal Width (cm)", "y")) +
                       theme(legend.title = element_text(size = 20),
                             legend.text = element_text(size = 20),
                             axis.title = element_text(size = 20),
                             legend.position="bottom"
                       )
                   })

               }
                 )

      ## if no error then proceed
      if(length(click_saved$singleclick)>0){

        kdataset <- data.frame(kdata$centers, grouping = as.character(1:nrow(centroids)))
        currentData$grouping <- as.character(kdata$cluster)

        output$plot2 <- renderPlot(
          expr = {
            ggplot(data = currentData, aes_string(x=ifelse(input$selectData == "iris",
                                                                "Sepal.Length","x1"),
                                                  y=ifelse(input$selectData == "iris",
                                                                "Sepal.Width","y1"),
                   color = "grouping")) +
              geom_point(alpha = 0.6) +
              geom_point(data = kdataset, aes_string(x=ifelse(input$selectData == "iris",
                                                              "Sepal.Length","x1"),
                                                     y=ifelse(input$selectData == "iris",
                                                              "Sepal.Width","y1"),
                                                     color = "grouping"),
                       size=5, shape=17, inherit.aes = FALSE) +
              theme_bw() +
              scale_color_manual(values = boastUtils::psuPalette) +
              guides(color =  guide_legend(title = "Cluster Grouping")) +
              xlab(ifelse(input$selectData == "iris", "Sepal Length (cm)", "X")) +
              ylab(ifelse(input$selectData == "iris", "Sepal Width (cm)", "y")) +
              theme(legend.title = element_text(size = 20),
                    legend.text = element_text(size = 20),
                    axis.title = element_text(size = 20),
                    legend.position="bottom"
              )
          })

        # create scatter plot
        newobs <- data.frame(NumClut = as.character(length(kdata$withinss)),
                             TotWith = round(kdata$tot.withinss, digits = 3))

        initScatter$init <- rbind(initScatter$init, newobs)

        output$plotscatter <- renderPlot(
          expr = {
            ggplot(data=initScatter$init, aes(x=NumClut, y=TotWith)) +
              geom_dotplot(binaxis='y', stackdir='center', binwidth = 60, dotsize = 0.1) +
              theme_bw() +
              xlab("Number of Cluster") +
              ylab("Total Within Sum of Square") +
              theme(legend.title = element_text(size = 20),
                    legend.text = element_text(size = 20),
                    axis.title = element_text(size = 20),
                    legend.position="bottom"
              )

          })

        ClusterTable <- arrange(initScatter$init, NumClut)
        names(ClusterTable) <- c("Number of Clusters", "Total Within Sum of Squares")

        output$TotTable <- DT::renderDT(
          expr = ClusterTable,
          caption = "Total Within Sum of Squares for different Number of Clusters", # Add a caption to your table
          style = "bootstrap4", # You must use this style
          rownames = TRUE,
          options = list( # You must use these options
            responsive = TRUE, # allows the data table to be mobile friendly
            scrollX = TRUE, # allows the user to scroll through a wide table
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            columnDefs = list(  # These will set alignment of data values
              # Notice the use of ncol on your data frame; leave the 1 as is.
              list(className = 'dt-center', targets = 1:ncol(ClusterTable))
            )
          )
        )

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

  # clear table button

  observeEvent(eventExpr = c(input$clear, input$selectData), handlerExpr = {

    initScatter$init <- data.frame(NumClut = c(),    # clear table
                                   TotWith = c())
    ClusterTable <- initScatter$init
    #names(ClusterTable) <- c("Number of Clusters", "Total Within Sum of Squares")

    output$TotTable <- DT::renderDT(
      expr = ClusterTable,
      caption = "Total Within Sum of Squares for different Number of Clusters", # Add a caption to your table
      style = "bootstrap4", # You must use this style
      rownames = TRUE, # format round
      options = list( # You must use these options
        responsive = TRUE, # allows the data table to be mobile friendly
        scrollX = TRUE, # allows the user to scroll through a wide table
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        columnDefs = list(  # These will set alignment of data values
          # Notice the use of ncol on your data frame; leave the 1 as is.
          list(className = 'dt-center', targets = 1:ncol(ClusterTable))
        )
      )
    )

    output$plotscatter <- renderPlot(               # respond to scatter plot in ui
      expr = {
        ggplot(data = NULL) +
          geom_point() +
          geom_jitter() +
          theme_bw() +
          scale_color_manual(values = boastUtils::psuPalette) +
          ylab("Total Within Sum of Squares") +
          xlab("Number of Cluster") +
          theme(legend.title = element_text(size = 20),
                legend.text = element_text(size = 20),
                axis.title = element_text(size = 20),
                legend.position="bottom"
          )
      })


  }

               )

  # if you click RESET
  observeEvent(eventExpr = c(input$reset, input$selectData), handlerExpr = {

    # back no click
    #click_saved$singleclick <- centroidata[6:10,]  #list() #niente cliccato
    click_saved$singleclick <- c(NULL, NULL)

    output$check <- renderTable({
      as.data.frame(c(NULL, NULL))
    })

    output$plot2 <- renderPlot(
      expr = {
        ggplot(data = dataCollection(), aes_string(x=ifelse(input$selectData == "iris",
                                                            "Sepal.Length","x1"),
                                                   y=ifelse(input$selectData == "iris",
                                                            "Sepal.Width","y1")),
               color="black") +
          geom_point(alpha = 0.5) +
          theme_bw() +
          scale_color_manual(values = boastUtils::psuPalette) +
          guides(color =  guide_legend(title = "Cluster Grouping")) +
          xlab(ifelse(input$selectData == "iris", "Sepal Length (cm)", "X")) +
          ylab(ifelse(input$selectData == "iris", "Sepal Width (cm)", "y")) +
          theme(legend.title = element_text(size = 20),
                legend.text = element_text(size = 20),
                axis.title = element_text(size = 20),
                legend.position="bottom"
          )
      })


  })






  output$plot2 <- renderPlot(
    expr = {
      ggplot(data = dataCollection(), aes_string(x=ifelse(input$selectData == "iris",
                                                          "Sepal.Length","x1"),
                                                 y=ifelse(input$selectData == "iris",
                                                          "Sepal.Width","y1")),
             color="black") +
        geom_point(alpha = 0.5) +
        theme_bw() +
        scale_color_manual(values = boastUtils::psuPalette) +
        xlab(ifelse(input$selectData == "iris", "Sepal Length (cm)", "X")) +
        ylab(ifelse(input$selectData == "iris", "Sepal Width (cm)", "y")) +
        theme(legend.title = element_text(size = 20),
              legend.text = element_text(size = 20),
              axis.title = element_text(size = 20),
              legend.position="bottom"
        )
    })

}


boastApp(ui = ui, server = server)

