
library(shiny)
library(bslib)
library(dplyr)
library(tools)
library(readxl)
library(ggplot2)
library(plotly)
library(DT)
library(SixSigma)

# data for P/T criteria table 
table_pt <- data.frame(
  `range P/T` = c("≤ 10%", "10% < P/T ≤ 20%", "20% < P/T ≤ 30%", "≥ 30%"),
  interpretation = c("Excellent","Good", "Marginal (almost unacceptable)", "Unacceptable and must be corrected"),
  stringsAsFactors = FALSE
)

# data for nc criteria table
table_nc <- data.frame(
  `range nc` = c("nc > 4", "2 ≤ nc ≤ 4", "nc < 2"),
  interpretation = c("Adequate resolution", "Inadequate resolution", "Clearly inadequate resolution"),
  stringsAsFactors = FALSE
)

# Define UI --------------------------------------------------------------------

ui <- page_navbar(
  title = HTML("<span style='color: #2C9AB7; 
               font-size: 26px; font-weight: bold; 
               text-shadow: 2px 2px 4px rgba(0,0,0,0.6); 
               font-family: Orbitron, sans-serif;'> App MetR&R "),
  theme = bs_theme(preset = "flatly"),
  
  ### Pag 1: Descriptive Analysis
  nav_panel(
    "Descriptive Analysis",
    
    page_sidebar(
      #title = "Graphs and descriptive data",
      
      sidebar = sidebar(
        fileInput("file", label = h5("File input")),
        
        sliderInput(
          inputId = "alpha",
          label = "Alpha:",
          min = 0, max = 1,
          value = 0.7
        ),
        
        numericInput(
          inputId = "decimals",
          label = "Decimal numbers:",
          value = 2
        ),
        
        numericInput(
          inputId = "pageLength",
          label = "PageLength (Tables):",
          value = 5
        )
      ),
      
      ### Main content (cards organized 2x2)
      layout_column_wrap(
        width = 1/2,  # 2 columns per row
        height = "auto",
        
        card(
          full_screen = TRUE,
          height = 300,
          card_header(tags$strong("Distribution of measurements by operator")),
          plotOutput("density")
        ),
        
        card(
          full_screen = TRUE,
          height = 300,
          card_header(tags$strong("Boxplot by part and operator")),
          plotOutput("boxplots")
        ),
        
        card(
          full_screen = TRUE,  
          height = "auto",     
          card_header(tags$strong("Summary of data by operator and part")),
          
          h6(tags$strong("Summary by operator")),
          DT::DTOutput("summaryOperator"),
          tags$hr(),
          h6(tags$strong("Summary by part")),
          DT::DTOutput("summaryPart")
        )
        ,
        
        card(
          full_screen = TRUE,
          height = 300,
          card_header(tags$strong("Measurements by part and operator")),
          plotlyOutput("plotly")
        )
      )
    )
  ),
  
  ###Pag 2: Metrology
  nav_panel(
    "Metrology",
    page_sidebar(
      #title = "   ",
      sidebar = sidebar(
        fileInput("file", label = h5("File input")),
        
        numericInput(
          inputId = "decimals",
          label = "Decimal numbers:",
          value = 2
        ),
        
        numericInput(
          inputId = "tolerance",
          label = "Tolerance:",
          value = 1, step = 0.1
        ),
        
        numericInput(
          inputId = "k2",
          label = "Constant k2:",
          value = 4.44
        )
      ),
      
      #"Main content of Metrology"
      #Tab 1
      navset_card_underline(
        title = tags$h4(tags$strong("Results")),
        nav_panel("Graphical and numerical long RR",
                  layout_column_wrap(
                    width = 1/2,
                    height = 300,
                    card(
                      full_screen = TRUE,
                      height = "auto",
                      #card_header(" "),
                      plotOutput("Gage_Analyse", width = "100%", height = "auto"),
                    ),
                    card(full_screen = TRUE,
                          height = "auto", 
                         card_header <- div(
                           tags$h6(tags$strong("ANOVA and Gage R&R")),
                           tags$small("ANOVA: Analysis of Variance | Gage R&R: Repeatability and Reproducibility Study")
                         ),
                         verbatimTextOutput("Anova")))
                  ),
        #Tab 2
        nav_panel("Numerical short R&R",
                  layout_column_wrap(
                    width = 1/2,
                    height = 300,
                    card(full_screen = TRUE,
                         height = "auto",
                         card_header(tags$strong("Valuex box to short R&R")),
                         fluidRow(
                           column(12, uiOutput("averagerangeBox"))
                         ), 
                         fluidRow(
                           column(12, uiOutput("EMBox"))
                         ),
                         fluidRow(
                           column(12, uiOutput("DesvBox"))
                         ),
                         fluidRow(
                           column(12, uiOutput("PtBox"))
                         )
                  ),
  card(full_screen = TRUE,
       height = "auto",
       card_header(tags$strong("Reference values for k2")), 
       DT::DTOutput("k2Table"))
  )
),

#Tab 3
nav_panel("Measurement System Acceptance Criteria",
          fluidPage(
            # Row 1: Two cards side by side (50% and 50%)
            div(
              style = "display: flex; gap: 10px; margin-bottom: 10px;",
              
              # Card 1 (50%)
              card(style = "flex: 5;",  # proportion 5 de 10
                   full_screen = TRUE,
                   height = 300,
                   card_header(tags$strong("P/T criteria")),
                   DTOutput("table_pt")
              ),
              
              # Card 2 (50%)
              card(
                style = "flex: 5;",  # proportion 5 de 10
                full_screen = TRUE,
                height = 300,
                card_header(tags$strong("Nc Criteria")),
                DTOutput("table_nc")
              )
            ),
            
            # Row 2: Card al 100%
            card(
              full_screen = FALSE,
              card_header(tags$strong("Result values - Based on P/T and nc acceptance criteria")),
              card_body(
                # Inputs
                fluidRow(
                  column(6,
                         selectInput("select_PT", "Select P/T value", 
                                     choices = list("Based on Graphical and numerical long RR" = 1, 
                                                    "Based on Numerical short R&R " = 2), selected = 1)
                                     ),
                  
                  column(6,
                         numericInput("nc", "Nc value (Based on Graphical and numerical long RR)", 
                                      value = NA, min = 2, step = 1)
                  ),
                  
                  # Input numérico (solo aparece si se selecciona la opción 1)
                  uiOutput("manualInput"),
                  
                  # Botón para confirmar
                  actionButton("confirm", "Confirm Values"),
                
                ),
                tags$strong(h4("Automatic interpretation")),
                hr(style = "border-top: 2px solid #999; margin-top: 10px; margin-bottom: 20px;"),
                uiOutput("interpretation_card")
              )
            )
          )
        )
       )
    )
  ),
  
  ### Pag 3: Conclusions
nav_panel(
  "Conclusions and References",
  fluidPage(
    tags$style(HTML("
      .custom-card {
        background-color: #f7f7f9; 
        border-left: 6px solid #3EB489; 
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        margin-bottom: 20px;
        padding: 20px;
        border-radius: 6px;
      }
      .custom-card h4 {
        color: #333;
        font-weight: 600;
      }
      .custom-card p {
        font-size: 16px;
        color: #555;
        margin-bottom: 0;
      }
    ")),
    
    # Card of conclusions
    card(
      full_screen = TRUE,
      height = "auto",
      card_header(tags$h4(tags$strong("Conclusions"))),
      div(class = "custom-card",
          tags$h6(tags$strong("Descriptive Analysis")),
          tags$p("Descriptive analysis provides a comprehensive overview of measurement behavior by operator. 
                 Distribution and boxplots allow for the identification of variability, biases, and possible outliers, 
                 while individual measurement visualization facilitates detailed comparisons. Finally, the statistical summary 
                 complements the analysis by consolidating key information.", uiOutput("conclusions"))
      ),
      div(class = "custom-card",
          tags$h6(tags$strong("Graphical and numerical long RR")),
          tags$p("The Gage R&R study allowed for a complete evaluation of the measurement system's accuracy, analyzing 
          the variation originating from the parts, the operators, and their interactions. In addition, it quantified repeatability
          and reproducibility and identified the percentage contribution of each component to the total variation. This analysis also 
          showed the resolution of the system (number of distinct categories), providing a clear view of its ability to distinguish between parts.")
      ),
      div(class = "custom-card",
          tags$h6(tags$strong("Numerical short R&R")),
          tags$p("The short R&R study is useful for seeing how the measurement system performs under controlled conditions, 
          without many sources of variation. When compared to the long R&R, which includes more real-world factors, variability may increase. 
          This difference is normal and expected, and shows that a system may perform well in the short term but have more variation in 
          real-world conditions. Therefore, it is important to consider both analyses to truly understand how the measurement system performs.", uiOutput("conclusions_shortRR"))
      ),
      div(class = "custom-card",
          tags$h6(tags$strong("Measurement System Acceptance Criteria")),
          tags$p("The Measurement System Acceptance Criteria section is key to validating whether the measurement system complies with the
          established accuracy/tolerance (A/T) ranges and number of distinguishable categories (Nc). Automating the results speeds up interpretation 
          and reduces errors, facilitating quick and consistent decisions on system acceptance or improvement.", uiOutput("conclusion_interp"))
          )
    ), 
    
    # Card of references 
    card(
      full_screen = TRUE,
      height = "auto",
      card_header(tags$strong("References")),
      tags$ul(
        tags$li("Gutiérrez Pulido, H., & de la Vara Salazar, R. (n.d.). *Statistical quality control and Six Sigma* (2nd ed.). University of Guadalajara / Center for Research in Mathematics.",
                tags$a(href = "https://www.uv.mx/personal/ermeneses/files/2018/05/6-control-estadistico-de-la-calidad-y-seis-sigma-gutierrez-2da.pdf", 
                       "https://www.uv.mx/personal/ermeneses/files/2018/05/6-control-estadistico-de-la-calidad-y-seis-sigma-gutierrez-2da.pdf")
        ),
        
        tags$li(
          "R Core Team. (2025). *R: A language and environment for statistical computing* [Software]. R Foundation for Statistical Computing. ",
          tags$a(href = "https://www.r-project.org", "https://www.r-project.org")
        ),
        
        tags$li(
          "RStudio Team. (2025). *RStudio: Integrated Development Environment for R* [Software]. Posit Software, PBC. ",
          tags$a(href = "https://www.posit.co", "https://www.posit.co")
        )
      )
    )
  )
)
)
    
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  # Read file uploaded by user
  data_user <- reactive({
    req(input$file) # wait for file to load
    read_excel(input$file$datapath)
  })
  
  ###Descriptive Analysis 
  #Plot 1
  output$density <- renderPlot({
    df <- data_user()  
    req(input$alpha)
    
    df$Operator <- as.factor(df$Operator)
    ggplot(data = df, aes(x = Values, fill = Operator)) +
             geom_density(alpha = input$alpha) +
      scale_fill_manual(values = c(
        "1" = "#5DADE2",  
        "2" = "#FFA500"   
      )) + labs()
  })
  
  # Plot2 : Boxplot 
  output$boxplots <- renderPlot({
    df <- data_user()
    
    df$Operator <- as.factor(df$Operator)
    df$Part<- as.factor(df$Part)
    
    ggplot(df, aes(x = Part, y = Values, fill = Operator)) +
      geom_boxplot() +
      scale_fill_manual(values = c(
        "1" = "#5DADE2",  
        "2" = "#FFA500"   
      )) + labs()
  })
  
  #Plot3: Plotly
  output$plotly <- plotly::renderPlotly({
    df <- data_user()
    
    df$Operator <- as.factor(df$Operator)
    df$Part <- as.factor(df$Part)
    
    p <- ggplot(df, aes(
      x = Operator,
      y = Values,
      color = Part,
      text = paste("Part:", Part, "<br>Tiempo:", round(Values, input$decimals))
    )) +
      geom_point() +
      geom_jitter(width = 0.2, alpha = input$alpha) +
      labs()
    
    plotly::ggplotly(p, tooltip = "text")
  })
  
  # Summary Table by Part
  output$summaryPart <- DT::renderDT({
    df <- data_user()
    req(df$Part, df$Values) # make sure they exist
    
    res_Part<- df %>%
      group_by(Part) %>%
      summarise(
        average = round(mean(Values), input$decimals),
        SD = round(sd(Values), input$decimals),
        range = round(max(Values) - min(Values), input$decimals)
      )
    
    datatable(res_Part,
              #caption = "Statistical summary by part",
              options = list(pageLength = input$pageLength, dom = 't'))
  })
  
  # Summary table by Operator
  output$summaryOperator <- DT::renderDT({
    df <- data_user()
    req(df$Operator, df$Values)  # make sure they exist
    
    res_Operator <- df %>%
      group_by(Operator) %>%
      summarise(
        average = round(mean(Values), input$decimals),
        SD = round(sd(Values), input$decimals),
        range = round(max(Values) - min(Values), input$decimals)
      )
    
    
    datatable(res_Operator,
              #caption = "Statistical summary by operator",
              options = list(pageLength = input$pageLength, dom = 't'))
  })
  
  ###"Graphical result long RR" - Gage
  #Plot Gage
  output$Gage_Analyse <- renderPlot({
    df <- data_user()
    
    # Ensure that the factors are defined
    df$Operator <- as.factor(df$Operator)
    df$Part<- as.factor(df$Part)
    
    my.rr <- ss.rr(var = Values,   
                   part = Part,  
                   appr = Operator, 
                   main = "R&R",
                   data = df, 
                   sub = "10 Parts - 2 Operators",
                   print_plot = T,
                   signifstars = TRUE)
  })
  
  #Numerical result RR largo
  output$Anova <- renderPrint({
      df <- data_user()
      
      # Ensure that the factors are defined
      df$Operator <- as.factor(df$Operator)
      df$Part<- as.factor(df$Part)
      
      my.rr <- ss.rr(
        var = Values,   
        part = Part,  
        appr = Operator, 
        main = "R&R",
        data = df, 
        sub = "10 Parts - 2 Operators",
        print_plot = FALSE,  # Do not show graph
        signifstars = TRUE
      )
  })
  
  #Results short R&R
  
  #value calculations 
  Values_rr <- reactive({
    df <- data_user()
    
    #Ensure that the factors are defined
    df$operador <- as.factor(df$Operator)
    df$Part<- as.factor(df$Part)
    
    # Calcular ranges por operador y Part
    ranges <- tapply(df$Values, list(df$Operator, df$Part), function(x) diff(range(x)))
    
    range_average <- mean(ranges, na.rm = TRUE)
    EM <- input$k2 * range_average
    desv <- EM / 5.15
    tolerance <- input$tolerance  
    Pt <- (EM / tolerance) * 100
    
    list(
      range_average = range_average,
      EM = EM,
      desv = desv,
      Pt = Pt
    )
  })
  
  #Values boxes 
  output$averagerangeBox <- renderUI({
    value_box(
      title = "Range average",
      value = round(Values_rr()$range_average, input$decimals),
      showcase = bsicons::bs_icon("bar-chart"),
      theme_color = NULL,
      style = "background-color: #FBC02D; color: black;"
    )
  })
  
  output$EMBox <- renderUI({
    value_box(
      title = "Measurement error (ME)",
      value = round(Values_rr()$EM, input$decimals),
      showcase = bsicons::bs_icon("exclamation-triangle"),
      theme_color = NULL,
      style = "background-color: #558B2F; color: black;"  
    )
  })
  
  output$DesvBox <- renderUI({
    value_box(
      title = "Estimated deviation",
      value = round(Values_rr()$desv, input$decimals),
      showcase = bsicons::bs_icon("graph-up"),
      theme_color = NULL,
      style = "background-color: #1F618D; color: black;"
    )
  })
  
  output$PtBox <- renderUI({
    value_box(
      title = "Precision / Tolerance (%)",
      value = paste0(round(Values_rr()$Pt, input$decimals), "%"),
      showcase = bsicons::bs_icon("percent"),
      theme_color = NULL,
      style = "background-color: #D32F2F; color: black;"
    )
  })
  
  #k2 table
  output$k2Table <- DT::renderDT({
    k2_table <- data.frame(
      `Number_of_Parts` = 1:10,
      `2_Operators` = c(3.65, 4.02, 4.19, 4.26, 4.33, 4.36, 4.40, 4.40, 4.44, 4.44),
      `3_Operators` = c(2.70, 2.85, 2.91, 2.94, 2.96, 2.98, 2.99, 2.99, 2.99, 2.99),
      `4_Operators` = c(2.30, 2.40, 2.43, 2.44, 2.45, 2.46, 2.47, 2.48, 2.48, 2.48),
      `5_Operators` = c(2.08, 2.15, 2.16, 2.17, 2.18, 2.19, 2.19, 2.19, 2.20, 2.20)
    )
    
    DT::datatable(k2_table, 
                  rownames = FALSE,
                  options = list(
                    pageLength = 10,
                    dom = 't')  # Only the table, without search/pager
                    #dom = 'ftp' # f = filter (buscador), t = table, p = pagination
                  )
  })
  
  ### Acceptance criteria
  #PT
  output$table_pt <- renderDT({
    datatable(table_pt, options = list(dom = 't'), rownames = FALSE) %>%
      formatStyle(
        'interpretation',
        target = 'row',
        backgroundColor = styleEqual(
          c("Excellent", "Good", "Marginal (almost unacceptable)", "Unacceptable and must be corrected"),
          c("lightgreen", "lightblue", "khaki", "salmon")
        )
      )
  })
  
  #Nc
  output$table_nc <- renderDT({
    datatable(table_nc, options = list(dom = 't'), rownames = FALSE) %>%
      formatStyle(
        'interpretation',
        target = 'row',
        backgroundColor = styleEqual(
          c("Adequate resolution", "Inadequate resolution", "Clearly inadequate resolution"),
          c("lightgreen", "khaki", "salmon")
        )
      )
  })
  
  #To the interpretation
  output$manualInput <- renderUI({
    if (input$select_PT == "1") { 
      numericInput("manual_value", "Enter the % P/T value:", value = NULL)
    }
  })
  
  #used to save the final number to be used
  final_value <- reactiveVal(NULL)
  
  #When the confirmation button is clicked
  observeEvent(input$confirm, {
    if (input$select_PT == "1") {
      final_value(input$manual_value)
    } else if (input$select_PT == "2") {
      req(input$tolerance, input$k2, data_user())  #make sure they exist
      final_value(round(Values_rr()$Pt, input$decimals)) 
    }
  })
  
  #Results of interpretacion 
  output$interpretation_card <- renderUI({
    req(!is.na(final_value()), !is.na(input$nc))
    
    PTValue <-  final_value()
    
    # interpretation P/T
    interpretation_pt <- if (PTValue <= 10) {
      "Excellent measurement process"
    } else if (PTValue <= 20) {
      "Good measurement process"
    } else if (PTValue <= 30) {
      "Marginal process (almost unacceptable)"
    } else {
      "Unacceptable process"
    }
    
    sugerence_pt <- if (PTValue <= 20) {
      "No inmediate action required"
    } else if (PTValue <= 30) {
      "Review and follow-up recommended"
    } else {
      "Measurement system needs to be corrected"
    }
    
    # interpretation nc
    interpretation_nc <- if (input$nc > 4) {
      "Appropriate resolution"
    } else if (input$nc >= 2) {
      "Inadequate resolution"
    } else {
      "Clearly inadequate resolution"
    }
    
    sugerence_nc <- if (input$nc > 4) {
      "No improvement required"
    } else if (input$nc >= 2) {
      "Improvement recommended if possible"
    } else {
      "Resolution must be improved urgently"
    }
    
    HTML(paste0(
      "<div style='display: flex; gap: 20px;'>",
      "  <div style='flex: 1;'>",
      "    <b>P/T value = ", PTValue, "% -> </b> ", interpretation_pt, "<br>",
      "    <b>Suggestion:</b> ", sugerence_pt,
      "  </div>",
      "  <div style='flex: 1;'>",
      "    <b>Nc value = ", input$nc, " -> </b> ", interpretation_nc, "<br>",
      "    <b>Suggestion:</b> ", sugerence_nc,
      "  </div>",
      "</div>"
    ))
  })
  
  #To reactive conclusions descriptive analysis 
  summary_data <- reactive({
    df <- data_user()
    req(df$Operator, df$Values)
    
    df %>%
      group_by(Operator) %>%
      summarise(
        average = round(mean(Values), input$decimals),
        SD = round(sd(Values), input$decimals),
        range = round(max(Values) - min(Values), input$decimals)
      )
  })
  
  output$summaryOperator <- DT::renderDT({
    summary_data()
  })
  
  output$conclusions <- renderUI({
    res <- summary_data()
    req(nrow(res) > 0)
    
    # Operario con mayor promedio
    max_avg <- res$Operator[which.max(res$average)]
    max_avg_val <- max(res$average)
    
    # Operario con mayor rango
    max_range <- res$Operator[which.max(res$range)]
    max_range_val <- max(res$range)
    
    tags$p("In this case, ",
             "the operator with the highest average was ", max_avg, " (", tags$strong(max_avg_val), "). ",
             "and the highest range of variation was ", max_range, " (", tags$strong(max_range_val), ")."
           )
  })
  
  #To reactive conclusions numerical short RR
  output$conclusions_shortRR <- renderUI({
    vals <- Values_rr()
    req(vals)
    
    # Redondear para mostrar
    range_avg <- round(vals$range_average, input$decimals)
    EM <- round(vals$EM, input$decimals)
    desv <- round(vals$desv, input$decimals)
    Pt <- round(vals$Pt, input$decimals)
    
    # Generar texto
    tags$p("The average range between operators and parts is  ", tags$strong(range_avg), 
             ", the estimated measurement error (ME) is ", tags$strong(EM), 
             ", with a standard deviation of ", tags$strong(desv), 
             ". This represents a ", tags$strong(Pt), "% of the total specified tolerance.")
  })
  
  ##To reactive conclusions of interpretacion 
    
  output$conclusion_interp <- renderUI({
    req(!is.na(final_value()), !is.na(input$nc))
    
    PTValue <- final_value()
    ncValue <- input$nc
    
    sugerence_pt <- if (PTValue <= 20) {
      "no immediate action required"
    } else if (PTValue <= 30) {
      "review and follow-up recommended"
    } else {
      "measurement system needs to be corrected"
    }
    
    sugerence_nc <- if (ncValue > 4) {
      "no improvement required"
    } else if (ncValue >= 2) {
      "improvement recommended if possible"
    } else {
      "resolution must be improved urgently"
    }
    
    tags$p(
      "The suggestion regarding the measurement process (P/T) was ", tags$strong(sugerence_pt),
             " and to the resolution (Nc) was ", tags$strong(sugerence_nc, ".")
    )
  })
} 
  
# Run the application 
shinyApp(ui = ui, server = server)

