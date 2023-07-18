library(shinydashboard)

dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = div(tags$img(src='hexlogo.png', height = '40', width = '40'), "PROPHETS")
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("PFS ratio", tabName = "pfsratio",
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
              fileInput("file", label = NULL),
              DT::dataTableOutput("inputdata")
              ),
      
      tabItem(tabName = "pfsra"),
      
      tabItem(tabName = "pfsrv"),
      
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