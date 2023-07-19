library(shinydashboard)
library(shinyjs)

dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = div(tags$img(src='hexlogo.png', height = '40', width = '40'), "PROPHETS")
  ),
  
  dashboardSidebar(
    
    useShinyjs(),
    
    sidebarMenu(id = "sidebar",
                tags$head(tags$style(".inactiveLink {
                                     pointer-events: none;
                                     cursor: none;
                                     }")),
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("PFS ratio", tabName = "pfsratio", startExpanded = TRUE,
               menuSubItem("Data upload", tabName = "dataupload", icon = icon("upload")),
               menuSubItem("PFSr analysis", tabName = "pfsra", icon = icon("subscript")),
               menuSubItem("PFSr visualization", tabName = "pfsrv", icon = icon("chart-column"))
               ),
      menuItem("Sample size calculator", tabName = "scalc", icon = icon("calculator"))
    )
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "home"),
      
      tabItem(tabName = "dataupload",
              fluidRow(
                column(4, radioButtons("btn_delim", "Delimiter", choices = c("comma", "semicolon", "space", "tab"))),
                column(4,
                       fluidRow(
                         column(3, 
                                div(style = "display: inline-block; vertical-align: -20px;",
                                    checkboxInput("btn_header", "Header", value = TRUE))
                         )
                         
                       ),
                       fluidRow(
                         column(3, checkboxInput("btn_quotes", "Quotes", value = TRUE))
                       )
                )
              ),
              fileInput("file", label = NULL),
              DT::dataTableOutput("inputdata")
              ),
      
      tabItem(tabName = "pfsra",
              fluidRow(
                column(3, numericInput("delta", "Delta", min = 0.1, max = 3, value = 1, step = 0.1) ),
                column(3, numericInput("prob", "Probability", min = 0.01, max = 1, step = 0.01, value = 0.5) )
              ),
              fluidRow(
                column(3, checkboxInput("modified", "Modified", FALSE)),
                column(6, numericInput("min_pfs2", "Minimal PFS2 in months", min = 0, value = 0.6))
              ),
              fluidRow(column(12,
                              h4("Model summary"),
                              tableOutput("stats_summary"))
                       )
              ),
              

      tabItem(tabName = "pfsrv",
              
              numericInput("delta_pfsrv", "Delta", min = 0.1, max = 3, value = 1, step = 0.1),
              fluidRow(
                column(3, checkboxInput("modified_pfsrv", "Modified", FALSE)),
                column(6, numericInput("min_pfs2_pfsrv", "Minimal PFS2 in months", min = 0, value = 0.6))
              ),
              tabsetPanel(
                tabPanel("Swimmer plot",
                         plotOutput("plot_swimmer")
                ),
                tabPanel("Correlation between PFS1 and PFS2",
                         checkboxInput("logscale", "log scale", value = FALSE),
                         plotOutput("plot_pfs_correlation")
                         ),
                tabPanel("Cumulative hazard ratio",
                         checkboxGroupInput("cumhaz", label = NULL, choices = c("PFS1", "PFS2"), selected = c("PFS1", "PFS2")),
                         plotOutput("plot_cumhaz")
                         ),
                tabPanel("Weibull plot",
                         plotOutput("plot_weibull")
                         ),
              )
              
              ),
      
      tabItem(tabName = "scalc",
              fluidRow(
                column(6, 
                       radioButtons("method", "Method for sample size calculation", 
                                    choiceNames = c(
                                      "Sample size calculation", 
                                      "Sample size calculation using GES", 
                                      "Sample size calculation according to a pre-specified proportion of patients with positive PFSr", 
                                      "Calculation of the statistical power of a study with a given sample size",
                                      "Calculation of the statistical power of a study with a given sample size using GES"),
                                    choiceValues = c("scalc", "scalc_ges", "scalc_p", "pcalc", "pcalc_ges"),
                                    selected = "scalc"
                       )
                ),
                column(6, 
                       numericInput("alpha", "Alpha", 0.05, step = 0.05),
                       conditionalPanel(
                         condition = "input.method=='scalc' || input.method=='scalc_ges'",
                         numericInput("power", "Power", value  = 0.8, min = 0.5, max = 0.99, step = 0.01),
                       ),
                       conditionalPanel(
                         condition = "input.method=='scalc' || input.method=='scalc_p' || input.method=='pcalc'",
                         list(
                           numericInput("rho", "Rho", value = 0.5, min = -1, max = 1, step = 0.1),
                           numericInput("alt_hr", "Alternative PFS ratio", value = 1.33, min = 0.1, max = 3.0, step = 0.1)
                         )
                       ),
                       conditionalPanel(
                         condition = "input.method=='scalc' || input.method=='pcalc'",
                         list(
                           numericInput("null_hr", "Null PFS ratio", value = 1, min = 0.1, max = 3.0, step = 0.1),
                           numericInput("k", "k", value = 1, min = -1, max = 1, step = 0.1)
                         )
                       ),
                       conditionalPanel(
                         condition = "input.method=='scalc' || input.method=='scalc_ges' || input.method=='scalc_p'",
                         numericInput("lost", "Proportion of non-informative pairs (Lost)", value = 1, step = 0.1)
                       ),
                       conditionalPanel(
                         condition = "input.method=='scalc' || input.method=='pcalc'",
                         selectInput("model", "Model", choices = c("GBVE", "Weibull"), selected = "GBVE", multiple = FALSE)
                       ),
                       conditionalPanel(
                         condition = "input.method=='scalc_ges' || input.method=='pcalc_ges'",
                         numericInput("ges", "GES", value = 0.3, step = 0.1)
                       ),
                       conditionalPanel(
                         condition = "input.method=='scalc_p'",
                         list(
                           numericInput("p0", "P0 (Null hypothesis)", min = 0, max = 1, value = 0.15, step = 0.1),
                           numericInput("p1", "P1 (Alterantive hypothesis)", min = 0, max = 1, value = 0.24, step = 0.1),
                           numericInput("beta", "Beta", 0.1, step = 0.05)
                         )
                       ),
                       conditionalPanel(
                         condition = "input.method=='pcalc' || input.method=='pcalc_ges'",
                         numericInput("samplesize", "Sample size", min = 1, value = 150)
                       )
                )
              ), 
              fluidRow(
                column(12, tableOutput("scalc_results"))
              )
              
              
              #verbatimTextOutput("scalc_results")
              )
      
    )
    
  )
  
)