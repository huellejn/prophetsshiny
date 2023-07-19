library(prophets)

function(input, output, session) {
  
  # Data upload
  
  ## Get the path to the file
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  ## Read the data
  inputfile <- reactive({
    delim <- input$btn_delim
    read.delim(userFile()$datapath,
             header = input$btn_header,
             sep = ifelse(delim == "comma", ",",
                          ifelse(delim == "semicolon", ";",
                                 ifelse(delim == "space", "", "\t"))),
             quote = ifelse(input$btn_quotes == TRUE, "\"'", ""),
             stringsAsFactors = FALSE)
  })
  
  ## Print message if file was successfully uploaded
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  

  ## Check that necessary columns are present
  observeEvent(inputfile(), {
    
    columns_required <- c("PFS1", "PFS2")
    columns_actual <- names(inputfile())
    
    duration <- 5
    
    if( all(columns_required %in% columns_actual) ) {
      msg_dataUpload <- "All required columns are present."
      type <- "default"
      removeCssClass(selector = "a[data-value='pfsra']", class = "inactiveLink")
      removeCssClass(selector = "a[data-value='pfsrv']", class = "inactiveLink")
      
    } else {
      msg_dataUpload <-  paste("The following columns are missing:", paste(columns_required[!columns_required %in% columns_actual], collapse = ", " ) )
      type <- "error"
      addCssClass(selector = "a[data-value='pfsra']", class = "inactiveLink")
      addCssClass(selector = "a[data-value='pfsrv']", class = "inactiveLink")
    }
    
    showNotification(
      msg_dataUpload,
      duration = duration,
      closeButton = TRUE,
      type = type
    )
  })
  
  output$inputdata <- DT::renderDataTable({inputfile()}, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  
  # PFSr analysis
  output$stats_summary <- renderTable({
    df <- inputfile()
    df$ratio <- df$PFS2 / df$PFS1
    
    if(input$modified == TRUE) {
      df <- modify_PFS(df, delta = input$delta, min_pfs2 = input$min_pfs2 )
    }
    
    res <- prophets_summary(
      data = df,
      delta = input$delta
    )
  })
  
  # PFSr visualization
  
  ## Swimmer plot
  output$plot_swimmer <- renderPlot({
    df <- inputfile()
    df$ratio <- df$PFS2 / df$PFS1
    if(input$modified == TRUE) {
      df <- modify_PFS(df, delta = input$delta_pfsrv, min_pfs2 = input$min_pfs2_pfsrv )
    }
    res <- swimmerplot_PFSr(
      df, 
      delta = input$delta_pfsrv)
    return(res)
  })
  
  ## Correlation of PFS1 and PFS2
  output$plot_pfs_correlation <- renderPlot({
    df <- inputfile()
    df$ratio <- df$PFS2 / df$PFS1
    
    if(input$modified == TRUE) {
      df <- modify_PFS(df, delta = input$delta, min_pfs2 = input$min_pfs2 )
    }
    res <- plot_correlation_PFS(
      df, 
      delta = input$delta_pfsrv,
      log_scale = input$logscale)
    return(res)
    
  })
  
  ## Cumulative Hazard Ratio
  output$plot_cumhaz <- renderPlot({
    df <- inputfile()
    df$ratio <- df$PFS2 / df$PFS1
    if(input$modified == TRUE) {
      df <- modify_PFS(df, delta = input$delta_pfsrv, min_pfs2 = input$min_pfs2_pfsrv )
    }
    res <- plot_cumHaz(
      df,  
      selected_PFS = input$cumhaz 
    )
    return(res)
  })
  
  ## Weibull plot
  output$plot_weibull <- renderPlot({
    df <- inputfile()
    df$ratio <- df$PFS2 / df$PFS1
    if(input$modified == TRUE) {
      df <- modify_PFS(df, delta = input$delta_pfsrv, min_pfs2 = input$min_pfs2_pfsrv )
    }
    res <- plot_weibull(df)
    return(res)
    
  })
  
  # Sample size calculator
  output$scalc_results <- renderTable({
    
    # sreq(iv$is_valid())
    
    # Get input values
    val_method <- input$method
    
    val_alpha <- input$alpha
    val_power <- input$power
    val_rho <- input$rho
    val_alt_hr <- input$alt_hr
    val_null_hr <- input$null_hr
    val_k <- input$k
    val_model <- input$model
    val_lost <- input$lost
    val_ges <- input$ges
    val_p0 <- input$p0
    val_p1 <- input$p1
    val_beta <- input$beta
    val_sample_size <- input$samplesize
    
    if(val_method == "scalc") {
      res <- PFSr_samplesize(
        alpha = val_alpha,
        power = val_power,
        rho = val_rho,
        alt_HR = val_alt_hr,
        null_HR = val_null_hr,
        k = val_k,
        model = val_model,
        verbose = FALSE)
    } else if(val_method == "scalc_ges") {
      res <- ges_PFSr_samplesize(
          ges = val_ges, 
          alpha = val_alpha, 
          power = val_power, 
          lost = val_lost, 
          verbose = FALSE
        )
    } else if(val_method == "scalc_p") {
      res <- PFSr_samplesize_proportion(
        p0 = val_p0, 
        p1 = val_p1,
        alpha = val_alpha, 
        beta = val_beta,
        pfsratio = val_alt_hr, 
        lost = val_lost,
        verbose = FALSE
      )
    } else if(val_method=="pcalc") {
      res <- PFSr_power_calculate(
        sample_size = val_sample_size, 
        null_HR = val_null_hr,
        alt_HR = val_alt_hr, 
        rho = val_rho, 
        alpha = val_alpha,
        k = val_k, 
        model = val_model,
        verbose = FALSE)
    } else if(val_method == "pcalc_ges") {
      res <- PFSr_power_calculation_ges(
        sample_size = val_sample_size, 
        ges = val_ges, 
        alpha = val_alpha,
        verbose = FALSE
      )
    }
    
    
    names(res)[names(res)=="n"] <- "required sample size"
    
    return(res)
    
  })
  
  
}