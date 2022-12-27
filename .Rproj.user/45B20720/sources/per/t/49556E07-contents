library(shiny)

#server ----
shinyServer(function(input, output, session) {
  global_var <- reactiveValues(distributed_trips = NULL, tlfd_plot = NULL, tlfd_table = NULL, ta_results = NULL)
  
  
  #executing gravity model ----
  observeEvent(input$run_gravity_model,{
    f <- input$input_file
    
    if(input$calc_ff == TRUE && input$deter_func_code != 0){
      attr <- extractingAVector(file_path = f$datapath, sheet_name = input$ap_sheetname, para = "attractions")
      prod <- extractingAVector(file_path = f$datapath, sheet_name = input$ap_sheetname, para = "productions")
      cost_mat <- df_to_mat(file_path = f$datapath, sheet_name = input$ff_sheetname)

      ff_mat <- calculate_friction_fator(b = input$beta,
                               n = input$power,
                               deter_func_code = input$deter_func_code,
                               resist_para_mat = cost_mat)
      
      
      global_var$distributed_trips <- tripDistribution(attractions = attr,
                       productions = prod,
                       friction_factor = ff_mat)
      if(input$num_adjustmets>0){
        global_var$distributed_trips <- adjusting_trips(no_iter = input$num_adjustmets,
                        attractions = attr,
                        productions = prod,
                        trips_matrix = global_var$distributed_trips)
      }
      

    } else if(input$calc_ff == TRUE && input$deter_func_code == 0){
      output$warning <- renderUI(h4("Select a Deterrence Function"))
    } else if(input$calc_ff == FALSE && input$deter_func_code == 0){
      attr <- extractingAVector(file_path = f$datapath, sheet_name = input$ap_sheetname, para = "attractions")
      prod <- extractingAVector(file_path = f$datapath, sheet_name = input$ap_sheetname, para = "productions")
      ff_mat <- df_to_mat(file_path = f$datapath, sheet_name = input$ff_sheetname)
      
      global_var$distributed_trips <- tripDistribution(attractions = attr,
                                            productions = prod,
                                            friction_factor = ff_mat)
      
      if(input$num_adjustmets>0){
        global_var$distributed_trips <- adjusting_trips(no_iter = input$num_adjustmets,
                                          attractions = attr,
                                          productions = prod,
                                          trips_matrix = global_var$distributed_trips)
      }
    }
    
    output$td_msg <- renderUI(
      h3(paste("Trip Distribution is Completed."))
    )
  })
  
  
  #downloading gravity model results ----
  output$download_gravity_results <- downloadHandler(
    filename = function() {
      paste("garvity_model_results", "xlsx", sep = ".")
    },
    
    content = function(file) {
      df <- data.frame(global_var$distributed_trips)
      df <- cbind(od = colnames(df), df)
      write_xlsx(df, file)
    }
  )
  
  #tlfd comparison ui  ----
  output$tlfd_comp_files<- renderUI({
    req(input$tlfd_comp_code)
    
    column(width = 12,
      fluidRow(
          column(width = 4,
                 br(),
                 h4("Demand Matrix")),
          column(width = 4,
                 fileInput(inputId = "demand_mat_file2", label = "New Demand Matrix")),
          column(width = 4,
                 textInput(inputId = "demand_sheet2", label = "Sheet Name")),
      )
    )
  })
  
  #tlfd panels ui and rendering plot and table ----
  observeEvent(input$calc_tlfd,{
    
    output$tlfd_panels <- renderUI({
      column(width = 12,
             tabsetPanel(
               tabPanel(title = "Plot",
                        plotOutput(outputId = "tlfd_plot"),
                        downloadButton(outputId = "download_tlfd_plot", label = "Download")),
               tabPanel(title = "Table",
                        tableOutput(outputId = "tlfd_table"),
                        downloadButton(outputId = "download_tlfd_table", label = "Download"))
             )
      )
    })
    
    f1 <- input$distance_mat_file
    s1 <- input$dist_sheet
    f2 <- input$demand_mat_file1
    s2 <- input$demand_sheet1
    
    distance <- df_to_mat(file_path = f1$datapath, sheet_name = s1)
    demand1 <- df_to_mat(file_path = f2$datapath, sheet_name = s2)
    
    if(input$tlfd_comp_code == FALSE){
      tlfd <- tlfd(distance_mat = distance,
                        demand_mat = demand1,
                        intervals = input$tlfd_inter)
    } else if(input$tlfd_comp_code == TRUE){
      f3 <- input$demand_mat_file2
      s3 <- input$demand_sheet2
      demand2 <- df_to_mat(file_path = f3$datapath, sheet_name = s3)
      tlfd <- compare_tlfds(distance_mat = distance,
                               demand_mat1 = demand1,
                               demand_mat2 = demand2,
                               intervals = input$tlfd_inter)
    }
    
    global_var$tlfd_plot <- tlfd[[2]]
    global_var$tlfd_table <- tlfd[[1]]
    output$tlfd_plot <- renderPlot(global_var$tlfd_plot)
    output$tlfd_table <- renderTable(global_var$tlfd_table)
  })
  
  #downloading tlfd plot ----
  output$download_tlfd_plot <- downloadHandler(
    filename = function() {
      paste("tlfd_plot", ".png", sep = "")
    },
    
    content = function(file) {
      ggsave(file, plot = global_var$tlfd_plot, device = "png", bg = "white",
             width = 12, height = 6.5, dpi = 300)
    }
  )
  
  #downloading tlfd table into excel ----
  output$download_tlfd_table <- downloadHandler(
    filename = function() {
      paste("tlfd_table", "xlsx", sep = ".")
    },
    
    content = function(file) {
      df <- global_var$tlfd_table
      write_xlsx(df, file)
    }
  )
  
  #trip assignment ----
  observeEvent(input$run_trip_assignment,{
    p1 <- input$links_file
    sh1 <- input$links_sheetname
    p2 <- input$flows_file
    sh2 <- input$flows_sheetname
    
    links_df <- read_data(input_file_path = p1$datapath, sheet_name = sh1)
    flows_df <- read_data(input_file_path = p2$datapath, sheet_name = sh2)

    road_network <- creating_graph(df_links = links_df,
                                   a = input$alpha,
                                   b = input$ta_beta)
    road_network <- initialization(g = road_network,
                                   df_flows = flows_df)
    
    if(input$conv_alg_code == 1){
      algorithm_name <- "msa"
    } else if(input$conv_alg_code == 2){
      algorithm_name <- "fw"
    }
    
    global_var$ta_results <- trip_assignment(g = road_network,
                          df_flows = flows_df,
                          iterations = input$iterations,
                          bisection_iterations = input$bisec_iter,
                          gap = input$gap,
                          alg = algorithm_name,
                          AoN_algorithm = AoN)
    
    output$ta_msg <- renderUI(
      h3(paste("Trip Assignment is Completed. Last iteration num: ", global_var$ta_results[[2]][length(global_var$ta_results[[2]])]))
    )
    
  })
  
  #download ta results df ----
  output$ta_reult <- downloadHandler(
    filename = function() {
      paste("trip_assignment_results", "xlsx", sep = ".")
    },
    
    content = function(file) {
      df <- as_data_frame(global_var$ta_results[[1]]) %>% 
        select(-target_flow)
      write_xlsx(df, file)
    }
  )
  
  #downloading aec convergence plot ----
  output$aec_plot <- downloadHandler(
    filename = function() {
      paste("aec_converg_plot", ".png", sep = "")
    },
    
    content = function(file) {
      p <- data.frame(
        "num" = global_var$ta_results[[2]],
        "aec" = global_var$ta_results[[3]]) %>% 
        ggplot(aes(num, aec))+
        geom_line()+
        geom_point()+
        theme_minimal()+
        labs(title = "AEC Convergence Plot",
             x = "Iteration Number",
             y = "Average Excess Cost (AEC)")
      
      
      ggsave(file, plot = p, device = "png", bg = "white",
             width = 12, height = 6.5, dpi = 300)
    }
  )
  
  #sample dfs for display ----
  output$ap_sample_df <- renderTable({
    data.frame(
      "zone_id" = c("a","b"),
      "attractions" = c(1000,1500),
      "productions" = c(1250,800)
    )
  }, digits = 0)
  
  output$mat_sample <- renderTable({
    data.frame(
      "od" = c("a","b"),
      "a" = c(1,3),
      "b" = c(3,5)
    )
  }, digits = 0)
  
  output$links_sample_df <- renderTable({
    data.frame(
      "id" = c(1,2),
      "from" = c(1,3),
      "to" = c(3,5),
      "capacity" = c(1000,1200),
      "free_flow_tt" = c(5,4)
    )
  }, digits = 0)
  
  output$flow_sample_df <- renderTable({
    data.frame(
      "id" = c(1,2),
      "from" = c(1,3),
      "to" = c(3,5),
      "flow" = c(400,650)
    )
  }, digits = 0)

  
})
  
  
  
  
  
  
  