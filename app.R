# Load packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(dplyr)
library(tidyr)
library(DT)
library(ggplot2)
library(Spectrum) # Contains the circles data
library(palmerpenguins)

# Load helper function ----
source("neighborsSearch.R")

# Load data ----
circle <- as.data.frame(t(circles))
data(iris)
data(penguins)

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
              label = "Explore!",
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
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 6/18/2024 by NJH.")
          )
        ),
        ### Prereqs ----
        tabItem(
          tabName = "prerequisites",
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
                        case to one of the", tags$em("k"), "clusters. The initial
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
              br(),
              box(
                width = 12,
                title = "About the Iris Data",
                collapsible = TRUE,
                collapsed = TRUE,
                "The Iris data set contains the measurements of 150 flowers from
              across three species of iris (50 of each). Each flower has the
              length and width (in centimeters) of their petals as well as the
              length and width of their sepals (the small, green, leaf-like outer
              portion of each flower. For this clustering example, we are using
              all four of the quantities. Anderson (1935) originally collected
              the data; Fisher (1936) used the data in a subsequent paper."
              )
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
          p("Using the data collections provided, explore how the different number
          of and position of initial centroids impact the application of the",
            tags$em("k-"), "means clustering approach. After setting initial centroids
          click the Run button apply the algorithm. Be sure to look at the
          corresponding Total Within-Cluster Sum of Squares values in the table
          and plot below."),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              wellPanel(
                selectInput(
                  inputId = "selectData",
                  label = "Choose a data collection",
                  choices = c(
                    "Circles" = "circle",
                    "Iris" = "iris",
                    "Palmer penguins" = "penguins"
                  ),
                  selected = "circle"
                ),
                bsButton(
                  inputId = "runAlgo",
                  label = "Run",
                  size = "large"
                ),
                bsButton(
                  inputId = "clearCenters",
                  label = "Clear centroids",
                  size = "large",
                  icon = icon("eraser"),
                  style = "warning"
                ),
                bsButton(
                  inputId = "clearTWCSS",
                  label = "Clear table",
                  size = "large",
                  icon = icon("eraser"),
                  style = "warning"
                ),
              ),
              box(
                width = 12,
                title = "About the data",
                collapsible = TRUE,
                collapsed = TRUE,
                uiOutput(outputId = "dataInfo")
              )
            ),
            column(
              width = 8,
              offset = 0,
              plotOutput(outputId = "clusterPlot", click = "placeCentroid"),
              uiOutput(outputId = "plotNote")
            )
          ),
          fluidRow(
            column(
              width = 5,
              offset = 0,
              DTOutput(outputId = "twcssTable")
            ),
            column(
              width = 7,
              offset = 0,
              plotOutput(outputId = "twcssDots")
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
            "Anderson, E. (1935). The irises of the Gaspe Peninsula. Bulletin of
            the American Iris Society, 59, 2-5. [Data]."
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v 0.61.1). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Beygelzimer, A., Kakadet, S., Langford, J., Arya, S., Mount, D., and
            Li, S. (2024). FNN: Fast nearest neighbor search algorithms and
            applications. (v 1.1.4). [R package]. Available from
            https://CRAN.R-project.org/package=FNN"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2024). boastUtils: BOAST utlities.
            (v 0.1.12.2). [R package]. Available from
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
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2024). shiny:
            Web application framework for R. (v 1.8.1.1). [R package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Horst, A. M., Hill, A. P., and Gorman, K. B. (2022). palmerpenguins:
            Palmer Archipelago (Antartica) penguin data. (v 0.1.1). [R package,
            data]. Available from https://allisonhorst.github.io/palmerpenguins
            DOI: doi: 10.5281/zenodo.3960218"
          ),
          p(
            class = "hangingindent",
            "John, C. R., and Watson, D. (2020). Spectrum: Fast adapative spectral
            clustering for single and multi-view data. (v 1.1) [R package, data].
            Available from https://CRAN.R-project.org/package=Spectrum"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2024). shinyWidgets: Custom
            inputs widgets for shiny. (v 0.8.6). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Springer-Verlag:New York. (v 3.5.1) [R package]. Available from
            https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K., and Vaughan, D.
            (2023). dplyr: A grammar of data manipulation. (v 1.1.4). [R package].
            Availble from https://CRAN.R-project.org/package=dplyr"
          ),
          p(
            class = "hangingindent",
            "Wickham, H., Vaughan, D., and Girlich, M. (2024). tidyr: Tidy messy
            data. (v 1.3.1) [R package]. Available from
            https://CRAN.R-project.org/package=tidyr"
          ),
          p(
            class = "hangingindent",
            "Xie, Y., Cheng, J., and Tan, X. (2024). DT: A wrapper of the
            JavaScript library 'DataTables'. (v 0.33). [R package]. Available
            from https://CRAN.R-project.org/package=DT"
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
  ### Define reactive values ----
  dataCollection <- reactiveVal(value = NULL, label = "Explore data set")
  plotAxes <- reactiveValues(
    horiz = NULL,
    hNice = NULL,
    vert = NULL,
    vNice = NULL
  )
  userCentroids <- reactiveVal(value = NULL, label = "User centroids")
  clusteredData <- reactiveVal(value = NULL, label = "Data with cluster info")
  twcssData <- reactiveVal(value = NULL, label = "TWCSS data")


  ### Set data collection and axes ----
  observeEvent(
    eventExpr = input$selectData,
    handlerExpr = {
      dataCollection(
        switch(
          EXPR = input$selectData,
          iris = iris,
          circle = circle,
          penguins = penguins
        )
      )

      if (input$selectData == "iris") {
        plotAxes$horiz <- "Sepal.Length"
        plotAxes$hNice <- "Sepal length (cm)"
        plotAxes$vert <- "Sepal.Width"
        plotAxes$vNice <- "Sepal width (cm)"
      } else if (input$selectData == "circle") {
        plotAxes$horiz <- "x1"
        plotAxes$hNice <- "Horizontal position"
        plotAxes$vert <- "y1"
        plotAxes$vNice <- "Vertical position"
      } else if (input$selectData == "penguins") {
        plotAxes$horiz <- "bill_length_mm"
        plotAxes$hNice <- "Bill length (mm)"
        plotAxes$vert <- "bill_depth_mm"
        plotAxes$vNice <- "Bill depth (mm)"
      }

      aboutData <- switch(
        EXPR = input$selectData,
        iris = "The Iris data set contains the measurements of 150 flowers from
              across three species of iris (50 of each). Each flower has the
              length and width (in centimeters) of their petals as well as the
              length and width of their sepals (the small, green, leaf-like outer
              portion of each flower. For this clustering example, we are using
              all four of the quantities. Anderson (1935) originally collected
              the data; Fisher (1936) used the data in a subsequent paper. For
              this part of the app the nearest cases for the points you select
              will get used as centers.",
        circle = "A simulated data set that contains 540 observations along two
                  position attributes. You will be able to specify both attributes
                  for each centroid.",
        penguins = "The Palmer Penguins data set contains the measurements of 344
                    adult penguins found in the Palmer Archipelago of Antartica.
                    In addition to recording the species, sex, and year of the
                    study the penguin was measured, several size attributes appear.
                    These include the bill length and depth (in mm), the flipper
                    length (in mm), and the body mass (in g). For this part of
                    the app, we will only look at the size attributes. For this
                    part of the app the nearest cases for the points you select
                    will get used as centers."
      )
      output$dataInfo <- renderUI(
        expr = {p(aboutData)}
      )

      ### Plot note ----
      graphNote <- switch(
        EXPR = input$selectData,
        iris = "Keep in mind that the plot is only displaying two of the four
                attributes that are getting used for the clustering algorithm.",
        circle = NULL,
        penguins = "Keep in mind that the plot is only displaying two of the four
                    attributes that are getting used for the clustering algorithm."
      )

      output$plotNote <- renderUI(
        expr = {p(graphNote)}
      )
    }
  )

  ### Define the core scatter plot object ----
  coreScatterplot <- eventReactive(
    eventExpr = dataCollection(),
    valueExpr = {
      ggplot(
        data = dataCollection(),
        mapping = aes(x = .data[[plotAxes$horiz]], y = .data[[plotAxes$vert]])
      ) +
        geom_point(alpha = 0.5) +
        theme_bw() +
        scale_color_manual(values = boastUtils::psuPalette) +
        labs(
          x = plotAxes$hNice,
          y = plotAxes$vNice,
          color = "Cluster/Grouping"
        ) +
        theme(
          text = element_text(size = 20),
          legend.position = "bottom"
        )
    }
  )

  ### Listen for mouse clicks ----
  observeEvent(
    eventExpr = input$placeCentroid,
    handlerExpr = {
      if (is.null(userCentroids())) {
        userCentroids(
          data.frame(
            horiz = input$placeCentroid$x,
            vert = input$placeCentroid$y
          )
        )
      } else if (nrow(userCentroids()) < 8) {
        userCentroids(
          rbind(
            userCentroids(),
            c(input$placeCentroid$x, input$placeCentroid$y)
          )
        )
      } else {
        sendSweetAlert(
          session = session,
          type = "warning",
          title = "Maximum Number of Centroids Reached",
          text = "For this exploration, you will not be able to set more than 8
          centroids. Click the Clear centroids button to reset the centroids."
        )
      }
      ### Debugging plot 1 ----
      ### This needs to be called here so that if the user adds a new centroid
      ### to the plot, the plot will revert to their initial set plus the new
      output$clusterPlot <- renderPlot(
        expr = {
          if (is.null(userCentroids())) {
            coreScatterplot()
          } else {
            centroidsScatterplot()
          }
        },
        alt = "testing"
      )
    }
  )

  #### Initial Centroids Plot ----
  centroidsScatterplot <- eventReactive(
    eventExpr = userCentroids(),
    valueExpr = {
      coreScatterplot() +
        geom_point(
          inherit.aes = FALSE,
          data = userCentroids(),
          mapping = aes(x = horiz, y = vert),
          size = 5,
          shape = 17
        )
    }
  )

  ### Debugging plot ----
  ### This generates the initial plot so that the user can add centroids
  output$clusterPlot <- renderPlot(
    expr = {
      if (is.null(userCentroids())) {
        coreScatterplot()
      } else {
        centroidsScatterplot()
      }
    },
    alt = "testing"
  )

  ### Run k-means on data ----
  observeEvent(
    eventExpr = input$runAlgo,
    handlerExpr = {
      if (is.null(userCentroids())) {
        sendSweetAlert(
          session = session,
          title = "Add Centroids",
          type = "error",
          text = "Place at least one initial centroid first."
        )
      } else {
        #### Prepare clustering data ----
        clusteredData(
          dataCollection() %>%
            dplyr::select(where(is.numeric)) %>%
            drop_na()
        )

        if (input$selectData == "penguins") {
          clusteredData(
            clusteredData() %>%
              dplyr::select(!year)
          )
        }

        #### Prepare Centers ----
        #### Randomly select a value from additional attributes
        #### Bad centers get found
        if (input$selectData == "penguins" | input$selectData == "iris") {
          centers <- findNeighbors(
            data = clusteredData(),
            points = userCentroids()
          )

        } else {
          centers <- userCentroids()
        }

        #### Run clustering ----
        clusterResults <- kmeans(
          x = clusteredData(),
          centers = centers
        )

        newCenters <- as.data.frame(clusterResults$centers) %>%
          mutate(
            grouping = LETTERS[row_number()]
          )

        #### Save information ----
        if (is.null(twcssData())) {
          twcssData(
            data.frame(
              kVal = length(unique(clusterResults$cluster)),
              twcss = clusterResults$tot.withinss
            )
          )
        } else {
          twcssData(
            rbind(
              twcssData(),
              c(length(unique(clusterResults$cluster)), clusterResults$tot.withinss)
            )
          )
        }

        #### Update scatter plot ----
        output$clusterPlot <- renderPlot(
          expr = {
            cbind(
              clusteredData(),
              grouping = LETTERS[clusterResults$cluster]
            ) %>%
              ggplot(
                mapping = aes(
                  x = .data[[plotAxes$horiz]],
                  y = .data[[plotAxes$vert]],
                  color = grouping
                )
              ) +
              geom_point(alpha = 0.5) +
              theme_bw() +
              scale_color_manual(values = boastUtils::psuPalette) +
              labs(
                x = plotAxes$hNice,
                y = plotAxes$vNice,
                color = "Cluster/Grouping"
              ) +
              theme(
                text = element_text(size = 20),
                legend.position = "bottom"
              ) +
              geom_point(
                inherit.aes = FALSE,
                data = newCenters,
                mapping = aes(
                  x = .data[[plotAxes$horiz]],
                  y = .data[[plotAxes$vert]],
                  color = grouping
                ),
                size = 5,
                shape = 17
              )
          },
          alt = "Clustered Results"
        )

        #### Update table ----
        output$twcssTable <- renderDT(
          expr = {
            validate(
              need(
                expr = !is.null(twcssData()),
                message = "Pick centroids and click Run."
              )
            )
            round(twcssData(), digits = 3) %>%
              arrange(kVal, desc(twcss)) %>%
              rename(`Number of Clusters` = kVal, TWCSS = twcss)
          },
          caption = "Total Within Sum of Squares for Different Numbers of Clusters",
          style = "bootstrap4",
          rownames = TRUE,
          options = list(
            responsive = TRUE,
            scrollX = TRUE,
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            columnDefs = list(
              list(className = 'dt-center', targets = 1:ncol(twcssData()))
            )
          )
        )

        #### Update dot plot ----
        output$twcssDots <- renderPlot(
          expr = {
            validate(
              need(
                expr = !is.null(twcssData()),
                message = "Pick centroids and click run"
              )
            )
            ggplot(
              data = twcssData(),
              mapping = aes(x = kVal, y = twcss)
            ) +
              geom_point(
                position = position_dodge2(width = 0.1),
                size = 5
              ) +
              scale_x_continuous(
                limits = c(1, 8),
                expand = expansion(add = c(1, 0.5)),
                breaks = 0:8,
                minor_breaks = NULL
              ) +
              labs(
                x = "Number of clusters/groupings",
                y = "Total Within-Cluster Sum of Squares",
                title = "TWCSS by Number of Clusters"
              ) +
              theme_bw() +
              theme(
                text = element_text(size = 20)
              )
          },
          alt = "dot plot"
        )
      }
    }
  )

  ## Clear centroids ----
  observeEvent(
    eventExpr = c(input$clearCenters, input$selectData),
    handlerExpr = {
      userCentroids(NULL)

      output$clusterPlot <- renderPlot(
        expr = {
          coreScatterplot()
        },
        alt = "testing"
      )
    }
  )

  ## Clear TWCSS Table and plot ----
  observeEvent(
    eventExpr = c(input$clearTWCSS, input$selectData),
    handlerExpr = {
      twcssData(NULL)
    }
  )

}


boastApp(ui = ui, server = server)

