library(prophets)

function(input, output, session) {
  
  # Data upload
  
  ## The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  ## The user's data, parsed into a data frame
  inputfile <- reactive({
    read.csv(userFile()$datapath,
             header = TRUE,
             #quote = input$quote,
             stringsAsFactors = FALSE)
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  observeEvent(input$file, {
    columns_required <- c("PFS1", "PFS2")
    columns_actual <- names(inputfile())
    
    duration <- NULL
    
    if( all(columns_required %in% columns_actual) ) {
      msg_dataUpload <- "All required columns are present."
      type <- "default"
      duration <- 5
    } else {
      print(columns_actual)
      msg_dataUpload <-  paste("The following columns are missing:", paste(columns_required[!columns_required %in% columns_actual], collapse = ", " ) )
      type <- "error"
    }
    
    showNotification(
      msg_dataUpload,
      duration = duration,
      closeButton = TRUE,
      type = type
    )
  })
  
  output$inputdata <- DT::renderDataTable({inputfile()}, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  
  
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