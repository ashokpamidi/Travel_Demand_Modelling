library(shiny)

shinyUI(fluidPage(
  
  tabsetPanel(
    
    #About ----
    tabPanel(title = "About",
             column(width = 3),
             column(width = 6,
                    fluidRow(
                      column(width = 12, align = "center",
                             h2("Web Application for Travel Demand Modelling"))  
                    ),
                    fluidRow(
                      column(width = 12,
                             p(style="text-align: justify;font-size:18px;",
                             "The objective of this project is to provide an easily accessible Web Application for Transport Planners to
                              easily execute and modify the steps in traditional four-step Travel Demand Modelling. At the moment, it has 
                              algorithms for Trip Distribution (Gravity Model) and Trip Assignment (Static User Equilibrium) coded at the backend and 
                              easily accessible interface to execute these two steps. Each panel has a 'How it works' window to provide brief instructions 
                              on how to execute those models. Please read the instructions and arrange the data as mentioned to avoid unneccessary errors.
                              This Application is still in developing stage so tests for all the edge cases were not written yet. So you may get some errors here 
                              and there but it shouldn't be an issue. If you get an error, just go back and read the instructions and make sure all the spellings
                              are proper before trying again."),
                      markdown("For any queries or suggestions, you can reach out me on Linkedin [here](https://www.linkedin.com/in/pamidiashok/) or at ashokpamidi07@gmail.com.
                               You can find the scripts and sample data [here](https://github.com/ashokpamidi/)."))
                    )
                ),
             column(width = 3)
             ),
    
    #Trip Distribution ----
    tabPanel(title = "Trip Distribution",
             
             sidebarLayout(
               sidebarPanel(
                 h4("How it works?"),
                 withMathJax(),
                 helpText("This application uses the following Gravity Model $$T_{ij}=\\frac{P_{i}(A_{j}F_{ij})}{\\sum_{j}A_{j}F_{ij}}$$
                          It takes Attractions (\\(A_{j}\\)), Productions (\\(P_{i}\\)) and Friction Factor (\\(F_{ij}\\)) as input to calculate trips between zone i and zone j.
                          If the Friction Factor matrix is available, it can be directly uploaded or else it can be calculated by checking 
                          'Calculate Friction Factor' button."),
                 helpText("Three Deterrence Functions are available to calculate Friction Factor Matrix. They are - Exponential(\\((f(c_{ij}) = exp(-\\beta{c_{ij}})\\)), 
                 Power(\\(f(c_{ij}) = c_{ij}^{-n}\\)), and Combined (\\(f(c_{ij}) = {c_{ij}^n}exp(-\\beta{c_{ij}}\\)))."),
                 
                 p("Arrange the Attractions and Producitons data as shwon below (please maintain the same headings):"),
                 fluidRow(column(width = 12,tableOutput(outputId = "ap_sample_df"), align = "center")),
                 
                 p("Template for OD is given below (make sure that 'zone_id' in the above table is matching with zone codes in the matrix):"),
                 fluidRow(column(width = 12,tableOutput(outputId = "mat_sample"), align = "center")),
                 
                 markdown("To run the model, upload the excel file that has these two dataframes and provide corresponding sheet names. 
                          Once the data is provided choose the Deterrence func, its parameters and No.of Row & Column adjustments to be made.
                          Click the 'Execute Gravity Model' button to run the model and 'Distributed Matrix' button to get the distributed matrix 
                          in an excel file."),
                 markdown("Fisrt rows will be adjusted followed by columns. If you privde '1', only rows will be adjusted. If you provide '2', first rows 
                          will be and the column adjustemenst will be made to that row-adjusted matrix. Choose the number as per your requirement."),
                 markdown("To generate **TLFD**, upload the Distance and Demand matrices and click 'Generate TLFD' button. To compare two TLFds, 
                          check the 'Compare TLFDs button. It allows you to upload one more demand matrix. Both TLFD plot(png) and TLFD table (excel) can be downloaded."),
                 markdown("You can find the scripts and sample data [here](https://github.com/ashokpamidi/).")
               ),
               
               mainPanel(
                 #gravity model column -----
                 column(width = 6,
                        h2("Trip distribution - Gravity Model"),
                        
                        fluidRow(
                          column(width = 4,
                                 br(),
                                 h4("Upload input File"),
                                 ),
                          column(width = 4,
                                 fileInput(inputId = "input_file", label = "")),
                          column(width = 4,
                                 br(),
                                 checkboxInput(inputId = "calc_ff", label = "Calculate Friction Factor"))
                        ),
                        
                        fluidRow(
                          column(width = 4,
                                 br(),
                                 h4("Sheet Names"),
                                 ),
                          column(width = 4,
                                 textInput(inputId = "ap_sheetname", label = "Attractions & Productions")),
                          column(width = 4,
                                 textInput(inputId = "ff_sheetname", label = "Distance/Cost/FF"))
                          ),
                        
                        fluidRow(
                          column(width = 12, h4("Select Deterrence Function"))
                        ),
                        
                        fluidRow(
                          column(width = 4,
                                 selectInput(inputId = "deter_func_code", label = "", choices = list("Exponential" = 1, "Powered" = 2, "Combined" = 4, "None" = 0), selected = 0, multiple = FALSE)),
                          column(width = 4,
                                 numericInput(inputId = "beta", label = "Beta", value = 0.1)),
                          column(width = 4,
                                 numericInput(inputId = "power", label = "n Value", value = 2))
                        ),
                        
                        fluidRow(
                          column(width = 6,
                                 h4("No.of Row and Column adjustments")),
                          column(width = 6,
                                 numericInput(inputId = "num_adjustmets", label = "", value = 2, width = 1200))
                        ),
                        
                        fluidRow(
                          column(width = 12, uiOutput(outputId = "warning"), align = "center", style="color:red")
                        ),
                        
                        fluidRow(
                          column(width = 6,
                                 actionButton(inputId = "run_gravity_model",label = "Execute Gravity Model", width = "100%"),
                                 align = "right"),
                          column(width = 6,
                                 downloadButton(outputId = "download_gravity_results", label = "Distributed Matrix", style = "width:100%;"),
                                 align = "left")
                        ),
                        
                        fluidRow(
                          column(width = 12,
                                 uiOutput(outputId = "td_msg"), align = "center")
                        )
                          
                        ),
                 
                 
                 #tlfd column -----
                 column(width = 6,
                        h2("Trip Length Frequency distribution (TLFD)"),
                        fluidRow(
                          column(width = 4,
                                 br(),
                                 h4("Distance Matrix")),
                          column(width = 4,
                                 fileInput(inputId = "distance_mat_file", label = "Distance Matrix")),
                          column(width = 4,
                                 textInput(inputId = "dist_sheet", label = "Sheet Name"))
                        ),
                        
                        fluidRow(
                          column(width = 4,
                                 br(),
                                 h4("Demand Matrix")),
                          column(width = 4,
                                 fileInput(inputId = "demand_mat_file1", label = "Demand Matrix")),
                          column(width = 4,
                                 textInput(inputId = "demand_sheet1", label = "Sheet Name"))
                        ),
                        
                        fluidRow(
                          column(width = 6,
                                 br(),
                                 h4("No.of intervals to consider")),
                          column(width = 6,
                                 numericInput(inputId = "tlfd_inter", label = "", value = 5))
                        ),
                        
                        fluidRow(
                          column(width = 12,
                                 checkboxInput(inputId = "tlfd_comp_code", label = "Compare TLFDs"),
                                 align = 'center')
                        ),
                        
                        fluidRow(
                          uiOutput(outputId = "tlfd_comp_files")
                        ),
                        
                        fluidRow(
                          column(width = 12,
                                 actionButton(inputId = "calc_tlfd", label = "Generate TLFD")),
                          align = "center"
                        ),
                        
                        fluidRow(
                          uiOutput(outputId = "tlfd_panels")
                        ),
                 )
              )
            )
          ),
    
    
    
    #Trip assignment ----
    tabPanel(title = "Trip Assignment",
             sidebarLayout(
               sidebarPanel(
                 h4("How it works?"),
                 withMathJax(),
                 p("Equilibrium Assignment is an itertaive process and its objective is to achieve some sort of equilibrium where every traveler 
                 gets to choose the shortest path available to him/her. It starts with an initial All-or-Nothing assignment and then Travel Times for paths will be 
                 updated based on the flow and link parameters. Based on the updated travel times, a portion of trips will be shifted to new shortest paths. This 
                 process will be repeated until permissible convergence is achieved. Most commonly used metric to measure the convergence of equilibrium is 
                 Average Excess Cost (AEC). AEC represents average cost/time difference between his/her choosen path and the shortest path available to him/her."),
                 p("The function used to update the Travel Times is known as Volume Delay Function(VDF)."),
                 p("General form of VDF: $$t=t_0[1+\\alpha(\\frac{V}{C})^\\beta]$$"),
                 p("This application provides two algorithms to calculate the proportion to be shifted in each iteration. They are Method of Successive Averages 
                   (MSA) and Frank-Wolf Algorithm. "),
                 p("Arrange the links and flows data as shown below before uploading (maintain the same column names)."),
                 p("Links between nodes:"),
                 fluidRow(column(width = 12,tableOutput(outputId = "links_sample_df"), align = "center")),
                 p("Flow between nodes:"),
                 fluidRow(column(width = 12,tableOutput(outputId = "flow_sample_df"), align = "center")),
                 markdown("To run..upload the files corresponding to Links and Flows, choose a convergence algorithm, adjust its parameters and 
                          click on 'Trip Assignment' to run Trip Assignment Model. To download the assigned links data, click 'Assignment Result'. Click on
                          to 'AEC Convergence Plot' to how to download the plot between iteration number and AEC."),
                 markdown("You can find the scripts and sample data [here](https://github.com/ashokpamidi/).")
               ),
               
               mainPanel(
                 h2("Static Trip Assignment"),
                 column(width = 6,
                        fluidRow(
                          column(width = 6,
                                 fileInput(inputId = "links_file", label = "File with Links data")),
                          column(width = 6,
                                 textInput(inputId = "links_sheetname", label = "Sheet name"))
                          ),
                        
                        fluidRow(
                          column(width = 6,
                                 fileInput(inputId = "flows_file", label = "File with Traffic Flow data")),
                          column(width = 6,
                                 textInput(inputId = "flows_sheetname", label = "Sheet name"))
                        ),
                        
                        fluidRow(
                          column(width = 12, h4("Volume Delay Function(VDF) Parameters"))
                        ),
                        
                        fluidRow(
                          column(width = 4,
                                 selectInput(inputId = "conv_alg_code",label = "Convergence Algorithm", choices = list("MSA" = 1,"FW" = 2), selected = 1, multiple = FALSE)),
                          column(width = 4,
                                 numericInput(inputId = "alpha", label = "Alpha", value = 0.25)),
                          column(width = 4,
                                 numericInput(inputId = "ta_beta", label = "Beta", value = 4.0))
                        ),
                        
                        fluidRow(
                          column(width = 4,
                                 numericInput(inputId = "iterations", label = "Iterations", value = 100)),
                          column(width = 4,
                                 numericInput(inputId = "bisec_iter", label = "Bisection Iterations", value = 0)),
                          column(width = 4,
                                 numericInput(inputId = "gap", label = "Permissible Gap", value = 0.001))
                        ),
                        
                        fluidRow(
                          column(width = 4,
                                 actionButton(inputId = "run_trip_assignment", label = "Trip Assignment", width = "100%")),
                          column(width = 4,
                                 downloadButton(outputId = "ta_reult", label = "Assignment Result", style = "width:100%;")),
                          column(width = 4,
                                 downloadButton(outputId = "aec_plot", label = "AEC Convergence Plot", style = "width:100%;"))
                        ),
                        
                        fluidRow(
                          column(width = 12,
                                 uiOutput(outputId = "ta_msg"), align = "center")
                        )
               ),
               column(width = 6)
             )
    )
    ),

    #Mode Share ----
    # tabPanel(title = "Mode Choice")
  )
)
)

