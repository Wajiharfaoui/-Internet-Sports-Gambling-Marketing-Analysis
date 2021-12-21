# Importing Libraries

library("shinyWidgets")
library('tidyr')
library('dplyr')
library('assertive')
library('lubridate')
library('shiny')
library("DT")
library('ggplot2')
library('viridis')
library('plotly')
library('hrbrthemes')
library('ggh4x')
library('stringr')
library('maps')
library('ggmap')

# Reading data
basetable <- read.csv("./Basetable.csv")
segmentation <- read.csv("./Segments.csv")
products <- read.csv("./product_count.csv")

############## UI ###############################################
ui <- (fluidPage(pageWithSidebar(
  # Header Panel
  headerPanel("Internet Sports Gambling Activity Marketing Analysis"),
  ########### SideBar Panel ##################
  # Sidebar Panel
  sidebarPanel(
    wellPanel(
      h4("Explore data"),
      uiOutput("product"),
      uiOutput("application"),
      uiOutput("country"),
      uiOutput("gender"),
      uiOutput("language")
    ),
    
    ######### Conditonal Panel ##################
    # Condionatl Panel to enable inputs for various graphs
    conditionalPanel(condition = "input.tabselected==1",
                     
                     wellPanel(
                       h4("Select Demographical variable to plot"),
                       uiOutput("x_demographic")
                     ), ),
    
    
    conditionalPanel(condition = "input.tabselected==2",
                     
                     wellPanel(
                       h4("Select Behavioural variable to plot"),
                       uiOutput("x_behavioral"),
                       uiOutput("y_behavioral")
                     )),
    
    conditionalPanel(condition = "input.tabselected==3",
                     
                     wellPanel(
                       h4("Select variables to plot"),
                       uiOutput("x_overall"),
                       uiOutput("y_overall"),
                       uiOutput("size")
                     )),
    
    conditionalPanel(condition = "input.tabselected==4",
                     
                     wellPanel(
                       h4("Select variables to plot"),
                       uiOutput("x_product"),
                       uiOutput("y_product")
                     )),
    
    wellPanel(
      h4("Download the DataMart"),
      helpText("Select the download format"),
      radioButtons(
        "type",
        "Format type:",
        choices = c("Excel (CSV)", "Text (Space Separated)", "Doc")
      ),
      br(),
      helpText("Click on the download button to download the Lot Dataset"),
      downloadButton("download", "Download Filtered Data"),
      br()
    )
  ),
  ######### End Conditonal Panel ##################
  ########### End SideBar Panel ##################
  
  
  ################ Main Panel ##################
  mainPanel(
    # Application title
    tabsetPanel(
      # Tab : Demographics
      tabPanel(
        "Demographical Insights",
        br(),
        h4(textOutput("demographics")),
        br(),
        value = 1,
        plotOutput(
          outputId = "demographics_plot",
          width = "100%",
          height = "800px"
        )
      ),
      # Tab : Behavioural
      tabPanel(
        title = "Behavioural Insights",
        br(),
        h4(textOutput("behavioural")),
        br(),
        value = 2,
        plotOutput(
          outputId = "behavioural_plot",
          width = "100%",
          height = "750px"
        ),
        h4(textOutput("segmentation")),
        br(),
        h4(textOutput("behavioural_2")),
        verbatimTextOutput("summary_y"),
        
        br(),
        plotlyOutput(
          outputId = "segmentation_plot",
          width = "100%",
          height = "800px"
        ),
        br(),
        tableOutput('table')
      ),
      # Tab : overall
      tabPanel(
        title = "Overall Insights",
        value = 3,
        plotlyOutput(
          outputId = "overall_plot",
          width = "100%",
          height = "750px"
        ),
        h4(textOutput("overall")),
        br(),
        verbatimTextOutput("summary_y_overall"),
        verbatimTextOutput("summary_table")
      ),
      # Tab : overall
      tabPanel(
        title = "Product Insights",
        value = 4,
        plotlyOutput(
          outputId = "product_plot",
          width = "100%",
          height = "800px"
        )
      ),
      # Tab : Data
      tabPanel(
        title = "Data Mart",
        value = 5,
        DT::dataTableOutput("datamart")
      ),
      id = "tabselected"
    )
  )
)))
################ End Main Panel ##################


############ Server ###############################
server <- function(input, output) {
  # Product Selection
  output$product <- renderUI({
    selectInput(
      inputId = "product",
      label = "Select Product Type",
      choices = c(
        "All Products" = "All",
        "Sports book fixed-odd (Prod1)" = "Prod1",
        "Sports book live-action (Prod2)" = "Prod2",
        "Poker BossMedia (Prod3)" = "Prod3",
        "Casino BossMedia  (Prod4)" = "Prod4",
        "Supertoto (Prod5)" = "Prod5",
        "Games VS (Prod6)" = "Prod6",
        "Games bwin (Prod7)" = "Prod7",
        "Casino Chartwell (Prod8)" = "Prod8"
      ),
      selected = "All"
    )
  })
  
  #Reactive Function to return data base on type of product
  filter_data <- reactive({
    # Product type is All will return data with selected columns
    if (input$product == "All") {
      basetable %>%
        select(c(1:29, 163:171))
    }
    # Product type is not All will return data with numeric and segment columns
    else if (input$product != "All") {
      names <- names(basetable)
      classes <- sapply(basetable, class)
      l = vector()
      value = input$product
      # Check type of columns is numeric or integer to subset
      for (name in names[classes == 'numeric' |
                         classes == 'integer']) {
        if (grepl(value, name,  fixed = TRUE)) {
          l <- append(l, name)
        }
      }
      # Check if name includes cluster to be added to selected column
      for (name in names) {
        if (grepl('cluster', name,  fixed = TRUE)) {
          l <- append(l, name)
        }
      }
      l <- append(l, y_omit)
      l <- append(l, c('Gender', 'rfm_segment'))
      basetable_new <- basetable[, l]
    }
  })
  
  # Mapping for key value pair of factors
  dict <-
    list(
      "Gender" = "Gender",
      "ApplicationID" = "ApplicationDescription",
      "Language" = "LanguageDescription",
      "Country" = "CountryName",
      "rfm_segment" = "rfm_segment"
    )
  
  # Default X variables selected
  x <- c("Gender", "ApplicationID", "Language", "Country")
  
  # List to remove from Y variable
  y_omit <-
    c("UserID",
      "LanguageDescription",
      "CountryName",
      "ApplicationDescription")
  
  # Create a list of Y variables dropdown
  create_y <- reactive({
    `%not_in%` <- purrr::negate(`%in%`)
    data <- filter_data()
    l =  vector()
    names <- names(data)
    classes <- sapply(data, class)
    # For selected data select only the numeric and interger colname
    for (name in names[classes == 'numeric' |
                       classes == 'integer']) {
      if ((name %not_in% x) & (name %not_in% y_omit)) {
        l <- append(l, name)
      }
    }
    l
  })
  
  # Application Selection
  output$application <- renderUI({
    pickerInput(
      inputId = "application_input",
      label = "Select Applications",
      choices = unique(filter_data()$ApplicationDescription),
      options = list(`actions-box` = TRUE),
      multiple = TRUE,
      selected = unique(filter_data()$ApplicationDescription)
    )
  })
  
  # Country Selection
  output$country <- renderUI({
    pickerInput(
      inputId = "country_input",
      label = "Select Countries",
      choices = unique(filter_data()$CountryName),
      options = list(`actions-box` = TRUE),
      multiple = TRUE,
      selected = unique(filter_data()$CountryName)
    )
  })
  
  #Gender Selection
  output$gender <- renderUI({
    pickerInput(
      inputId = "gender_input",
      label = "Select Gender",
      choices = unique(filter_data()$Gender),
      options = list(`actions-box` = TRUE),
      multiple = TRUE,
      selected = unique(filter_data()$Gender)
    )
  })
  
  #Language Selection
  output$language <- renderUI({
    pickerInput(
      inputId = "language_input",
      label = "Select Language",
      choices = unique(filter_data()$LanguageDescription),
      options = list(`actions-box` = TRUE),
      multiple = TRUE,
      selected = unique(filter_data()$LanguageDescription)
    )
  })
  
  # Reactive function to filter data based on product type and other filters applied
  # This function is used while viewing Datamart and applies filter accordingly to data
  base <- reactive({
    if (input$product == "All") {
      basetable %>% select(c(1:29)) %>%
        filter(
          ApplicationDescription %in% input$application_input,
          CountryName %in% input$country_input,
          Gender %in% input$gender_input,
          LanguageDescription %in% input$language_input
        )
    }
    else {
      basetable %>% select(c(1:12, contains(input$product))) %>%
        filter(
          ApplicationDescription %in% input$application_input,
          CountryName %in% input$country_input,
          Gender %in% input$gender_input,
          LanguageDescription %in% input$language_input
        )
    }
  })
  
  #DataMart output
  output$datamart = DT::renderDataTable({
    base()
  })
  
  output$x <- renderUI({
    selectInput(
      inputId = "x",
      label = h5("Choose the X- axis variable"),
      choices = as.vector(names(filter_data())),
      selected = "Gender"
    )
  })
  
  output$x_1 <- renderUI({
    selectInput(
      inputId = "x",
      label = h5("Choose the X- axis variable"),
      choices = as.vector(c(
        "Gender", "ApplicationID", "Language", "Country"
      )),
      selected = "Gender"
    )
  })
  
  
  
  output$x_2 <- renderUI({
    selectInput(
      inputId = "x",
      label = h5("Choose the X- axis variable"),
      choices = as.vector(names(filter_data()))
    )
  })
  
  ###Demographics tab ######
  #Output text for Demographics Tab
  output$demographics <- renderText({
    "Please select the X variable to be plotted over its count"
  })
  
  #X input for Demographics Tab
  output$x_demographic <- renderUI({
    selectInput(
      inputId = "x_demographic",
      label = h5("Choose the X- axis variable"),
      choices = as.vector(x),
      selected = "Gender"
    )
  })
  
  #Create Plot for Demographics based on X variable type of plot is created
  create_plot_demographics <- reactive({
    data <- filter_data()
    if (input$product != "All") {
      data <- na.omit(data)
    }
    
    # Plot when x variable is Gender
    if (input$x_demographic == "Gender") {
      gender_summary <-
        data %>% filter(!is.na(Gender)) %>% group_by(Gender) %>% summarise(count = n())
      p <-
        ggplot(gender_summary, aes(x = Gender, y = count)) +  geom_col(fill =
                                                                         c("firebrick2", "#00abff")) + ggtitle(paste("Gender wise Distribution of population")) + xlab("Gender") + ylab("Count")
    }
    # Plot when x variable is ApplicationID
    if (input$x_demographic == "ApplicationID") {
      data_sum <-
        data %>% filter(!is.na(ApplicationDescription)) %>% group_by(ApplicationDescription) %>% summarise(count = n())
      p <-
        ggplot(data_sum,
               aes(x = "", y = count , fill = ApplicationDescription)) +
        geom_bar(stat = "identity") + ggtitle(paste("Application wise Distribution of population")) +
        coord_polar("y")
    }
    # Plot when x variable is Country
    if (input$x_demographic == "Country") {
      summary <-
        data %>% filter(!is.na(CountryName)) %>% group_by(CountryName) %>% summarise(count = n()) %>% rename_at(c('CountryName'), ~ c('region'))
      world_map <- map_data("world")
      summary.map <- left_join(world_map, summary, by = "region")
      p <- ggplot(summary.map, aes(long, lat, group = group)) +
        geom_polygon(aes(fill = count), color = "black") +
        ggtitle(paste("Country wise Distribution of population")) +
        scale_fill_viridis_c(option = "C")
    }
    # Plot when x variable is Language
    if (input$x_demographic == "Language") {
      summary <-
        data %>% filter(!is.na(LanguageDescription)) %>% group_by(LanguageDescription) %>% summarise(count = n()) %>% arrange(desc(count))
      p <-
        ggplot(summary, aes(x = LanguageDescription, y = count)) +
        ggtitle(paste("Language wise Distribution of population")) +  xlab('Count') + ylab('Language') +
        geom_bar(stat = "identity") + coord_flip()
    }
    p
    
  })
  
  # Output Plot for Demographics
  output$demographics_plot <- renderPlot({
    create_plot_demographics()
  })
  ###End Demographics Insights ########
  
  ###Behavioral tab ######
  #Output text for Behavioral Tab
  output$behavioural <- renderText({
    "Please select the X & Y axis variables "
  })
  
  #Output text for Behavioral Tab
  output$behavioural_2 <- renderText({
    "Summary of the Y-axis variable"
  })
  
  #Output x for Behavioral Tab
  output$x_behavioral <- renderUI({
    selectInput(
      inputId = "x_behavioral",
      label = h5("Choose the X- axis variable"),
      choices = as.vector(x),
      selected = "Gender"
    )
  })
  
  #Output y for Behavioral Tab
  output$y_behavioral <- renderUI({
    selectInput(
      inputId = "y_behavioral",
      label = h5("Choose the Y- axis variable"),
      choices = as.vector(as.character(create_y()))
    )
  })
  
  #Create Behavioral Plots based on x and y inputs based on X variable type of plot is created
  create_plot_behavioral <- reactive({
    column = input$y_behavioral
    data <- filter_data()
    # Plot when x variable is Gender
    if (input$x_behavioral == "Gender") {
      gender_summary <-
        data %>% filter(!is.na(Gender) &
                          !is.na(!!as.name(column))) %>% group_by(Gender) %>% summarise(sum_column = sum(!!as.name(column)) / 100000)
      p <-
        ggplot(gender_summary, aes(x = Gender, y = sum_column)) +  geom_col(fill =
                                                                              c("firebrick2", "#00abff")) + ggtitle(paste("Gender wise Distribution of ", column)) + xlab("Gender") + ylab(column)
    }
    # Plot when x variable is ApplicationID
    if (input$x_behavioral == "ApplicationID") {
      data_sum <-
        data %>% filter(!is.na(ApplicationDescription) &
                          !is.na(!!as.name(column))) %>% group_by(ApplicationDescription) %>% summarise(sum_column = sum(!!as.name(column)) / 100000)
      p <-
        ggplot(data_sum,
               aes(x = "", y = sum_column , fill = ApplicationDescription)) +
        geom_bar(stat = "identity") + ggtitle(paste("Application wise Distribution of ", column)) +
        coord_polar("y")
    }
    # Plot when x variable is Country
    if (input$x_behavioral == "Country") {
      summary <-
        data %>% filter(!is.na(CountryName) &
                          !is.na(!!as.name(column))) %>% group_by(CountryName) %>% summarise(sum_column = sum(!!as.name(column)) / 100000) %>% rename_at(c('CountryName'), ~
                                                                                                                                                           c('region'))
      world_map <- map_data("world")
      summary.map <- left_join(world_map, summary, by = "region")
      p <- ggplot(summary.map, aes(long, lat, group = group)) +
        geom_polygon(aes(fill = sum_column), color = "black") +
        ggtitle(paste("Country wise Distribution of ", column)) +
        scale_fill_viridis_c(option = "C")
    }
    # Plot when x variable is Language
    if (input$x_behavioral == "Language") {
      summary <-
        data %>% filter(!is.na(LanguageDescription) &
                          !is.na(!!as.name(column))) %>% group_by(LanguageDescription) %>% summarise(sum_column = sum(!!as.name(column)) / 100000)
      p <-
        ggplot(summary, aes(x = LanguageDescription, y = sum_column)) +
        ggtitle(paste("Language wise Distribution of ", column)) +  xlab(column) + ylab('Language') +
        geom_bar(stat = "identity") + coord_flip()
    }
    p
    
  })
  # Output Plot for Behavioral
  output$behavioural_plot <- renderPlot({
    create_plot_behavioral()
    
  })
  
  # table output 
  output$table <- renderTable(products, align = 'c')
  
  ############# Segmenatation plot ########################
  create_plot_seg <- reactive({
    pp <- products %>% arrange(Number_of_Visits)%>% mutate(name=factor(Product, levels=Product)) %>% 
      ggplot(aes(x = Product, y = Number_of_Visits, fill= Product)) +  geom_col() +
      theme_ipsum() + ggtitle("Barplot for Product Activity") + coord_flip()
    
    pp <- ggplotly(pp, tooltip = "text")
    pp
  })
  
  output$segmentation_plot <- renderPlotly({
    create_plot_seg()
  })
  ############# End Segmenatation plot ########################
  
  ###End Behavioral Insights ########
  
  ###Overall Insights ######
  #Output text for Overall Tab
  output$overall <- renderText({
    "Summary table of the size variable"
  })
  
  #Output x variable Overall Tab
  output$x_overall <- renderUI({
    selectInput(
      inputId = "x_overall",
      label = h5("Choose the X- axis variable"),
      choices = as.vector(append(x, 'rfm_segment')),
      selected = "Gender"
    )
  })
  
  #Output y variable Overall Tab
  output$y_overall <- renderUI({
    selectInput(
      inputId = "y_overall",
      label = h5("Choose the Y- axis variable"),
      choices = as.vector(append(x, 'rfm_segment')),
      selected = "Language"
    )
  })
  
  #Output size variable Overall Tab
  output$size <- renderUI({
    selectInput(
      inputId = "size",
      label = h5("Choose the size variable"),
      choices = as.vector(as.character(append(create_y(
        
      ) , '-'))),
      selected = "-"
    )
  })
  
  #Create Plot based on x, y and size. If size selected is - count would be used for aggregation else sum of size is used for aggregation
  create_plot_overall <- reactive({
    x = as.character(dict[input$x_overall])
    y = as.character(dict[input$y_overall])
    size = input$size
    data <- filter_data()
    data <- na.omit(data)
    # Check is input of siwe is -
    if (size == '-') {
      # If inputs do not have CountryName plot bubble plots based on count
      if ((y != 'CountryName') & (x != 'CountryName')) {
        summary <-
          data %>% group_by(!!as.name(x),!!as.name(y)) %>% summarise(count = n()) %>% arrange(count) %>%  mutate(text = paste(paste(
            paste(paste(x, ": "),!!as.name(x)), paste(paste(y, ": "),!!as.name(y))
          ), paste(paste("\n Total: "), count))) %>% ggplot(aes(
            x = !!as.name(x),
            y = !!as.name(y),
            size = count,
            color = !!as.name(y),
            text = text
          )) +
          geom_point(alpha = 0.7) +
          scale_size(range = c(1.4, 19), name = "Population (M)") +
          scale_color_viridis(discrete = TRUE, guide = "none") +
          theme_ipsum() +
          theme(legend.position = "top") + labs(title = paste(x , " wise distribution of Customers"))
        pp <- ggplotly(summary, tooltip = "text")
      } else {
        # If inputs does have CountryName plot bubble maps based on count
        if ((y != 'CountryName')) {
          color = y
        } else {
          color = x
        }
        summary <-
          data %>% group_by(!!as.name(x),!!as.name(y)) %>% summarise(count = n()) %>% arrange(desc(count)) %>%  mutate(text = paste(paste(
            paste(paste(x, ": "),!!as.name(x)), paste(paste(y, ": "),!!as.name(y))
          ), paste(paste("\n Total: "), count))) %>% rename_at(c('CountryName'), ~
                                                                 c('region'))
        world_map <- map_data("world")
        summary.map <- left_join(world_map, summary, by = "region")
        pp <-
          ggplot(summary.map,
                 aes(
                   x = long,
                   y = lat,
                   group = group,
                   text = text
                 )) +
          geom_polygon(color = "black", fill = "grey") +
          stat_centroid(
            data = subset(summary.map, region %in% summary$region),
            aes(
              size = count,
              group = region,
              color = !!as.name(color)
            ),
            geom = "point",
            alpha = 0.75
          ) + theme(legend.position = "none") + labs(title = "Country wise distribution of customers")
        pp <- ggplotly(pp, tooltip = "text")
      }
    }
    else {
      # If inputs does not have CountryName plot bubble plots based on sum of size
      if ((y != 'CountryName') & (x != 'CountryName')) {
        summary <-
          data %>% group_by(!!as.name(x),!!as.name(y)) %>% summarise(count = sum(!!as.name(size))) %>% arrange(count) %>%  mutate(text = paste(paste(
            paste(paste(x, ": "),!!as.name(x)), paste(paste(y, ": "),!!as.name(y))
          ), paste(paste("\n Total: "), count))) %>% ggplot(aes(
            x = !!as.name(x),
            y = !!as.name(y),
            size = count,
            color = !!as.name(y),
            text = text
          )) +
          geom_point(alpha = 0.7) +
          scale_size(range = c(1.4, 19)) +
          scale_color_viridis(discrete = TRUE, guide = "none") +
          theme_ipsum() +
          theme(legend.position = "top") + labs(title = paste(paste(x, " wise distribution of "),  size))
        pp <- ggplotly(summary, tooltip = "text")
      }  else {
        # If inputs does have CountryName plot bubble maps based on sum of size
        if ((y != 'CountryName')) {
          color = y
        } else {
          color = x
        }
        summary <-
          data %>% group_by(!!as.name(x),!!as.name(y)) %>% summarise(count = sum(!!as.name(size))) %>% arrange(count) %>%  mutate(text = paste(paste(
            paste(paste(x, ": "),!!as.name(x)), paste(paste(y, ": "),!!as.name(y))
          ), paste(paste("\n Total: "), count))) %>% rename_at(c('CountryName'), ~
                                                                 c('region'))
        world_map <- map_data("world")
        summary.map <- left_join(world_map, summary, by = "region")
        pp <-
          ggplot(summary.map,
                 aes(
                   x = long,
                   y = lat,
                   group = group,
                   text = text
                 )) +
          geom_polygon(color = "black", fill = "grey") +
          stat_centroid(
            data = subset(summary.map, region %in% summary$region),
            aes(
              size = count,
              group = region,
              color = !!as.name(color)
            ),
            geom = "point",
            alpha = 0.75
          ) + theme(legend.position = "none") + labs(title = paste("Country wise distribution of ",  size))
        pp <- ggplotly(pp, tooltip = "text")
      }
    }
    pp
    
  })
  
  # Output Plot for Overall
  output$overall_plot <- renderPlotly({
    create_plot_overall()
    
  })
  #######End Overall###############################
  
  #######Product Insights##########################
  li_y_product <- vector()
  # Reactive function that will create a list of all product cluster colnames
  create_y_product <- reactive({
    names <- colnames(basetable)
    for (name in names) {
      if (grepl("cluster", name, fixed = TRUE)) {
        li_y_product <- append(li_y_product,  name)
      }
    }
    li_y_product
  })
  
  # Output x for Product Insights
  output$x_product <- renderUI({
    selectInput(
      inputId = "x_product",
      label = h5("Choose the Y- axis variable"),
      choices = as.vector('rfm_segment'),
    )
  })
  
  # Output y for Product Insights
  output$y_product <- renderUI({
    selectInput(
      inputId = "y_product",
      label = h5("Choose the Y- axis variable"),
      choices = as.vector(append('-', create_y_product())),
      selected = "-"
    )
  })
  
  # Output plot for Product Insights
  create_plot_product <- reactive({
    x = input$x_product
    y = input$y_product
    data =  filter_data()
    # If y = - The distribution for RFM segment is plotted as bar plot
    if (y == '-') {
      pp <-
        ggplot(data) + geom_bar(aes(x = !!as.name(x), fill = !!as.name(x))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(title = "Barplot for Segments of customers")
      
      pp <- ggplotly(pp, tooltip = "text")
    } else {
      # Else The distribution for  selected product cluster is plotted across RFM segment as bar plot
      prod <- str_split(y, "_")
      prod <- prod[[1]][1]
      omit_class <- paste(prod, "_never_played", sep = "")
      pp <-
        data %>% filter(!!as.name(y) != (!!omit_class)) %>% group_by(!!as.name(x),!!as.name(y)) %>%
        summarise(count = n()) %>%
        mutate(perc = count / sum(count)) %>% ggplot(aes(
          x = !!as.name(x),
          y = perc,
          fill = !!as.name(y),
          label = scales::percent(perc)
        )) +
        geom_col(position = 'dodge') +
        geom_text(
          position = position_dodge(width = .9),
          # move to center of bars
          vjust = -0.5,
          # nudge above top of bar
          size = 3
        ) +
        scale_y_continuous(labels = scales::percent) + labs(title = "Barplot for Product Segments among RFM Segments")
      pp <- ggplotly(pp, tooltip = "text")
    }
    pp
  })
  
  # Output plot for the Product
  output$product_plot <- renderPlotly({
    create_plot_product()
    
  })
  
  #######End Product Insights##########################
  
  
  output$summary_y <- renderPrint({
    summary(base()[[input$y_behavioral]])
  })
  
  output$summary_y_overall <- renderPrint({
    summary(base()[[input$size]])
  })
  
  
  #file extension for download
  fileext <- reactive({
    switch(
      input$type,
      "Excel (CSV)" = "csv",
      "Text" = "txt",
      "Doc" = "doc"
    )
  })
  
  
  output$download <- downloadHandler(
    filename = function() {
      paste("DataMart", fileext(), sep = ".")
    },
    
    content = function(file) {
      write.csv(base(), file = file, row.names = FALSE)
    }
  )
}
############ End Server ###############################
# Create a Shiny app object
shinyApp(ui = ui, server = server)
############## End UI ###############################################