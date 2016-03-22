# Load libraries
library(shiny)
library(rCharts)
library(BH) # needed for dplyr - load it here so shiny knows to build it during deployment
library(dplyr)

# Load data that only needs to be loaded when app starts
salary <- readRDS("data/salary_coli.RDS")
job_names <- unique(salary$occ_job_title)

shinyServer(function(input, output, session) {
    
    # Salary data that is reactive to the job title and variables selected
    salary_data <- reactive({
        salary[salary$occ_job_title == input$select_job, input$show_vars, drop = F]
    })
    
    # a large table, reative to input$show_vars
    output$table1 = renderDataTable({
        salary_data()
    }, options = list(orderClasses = TRUE))
    
    # Subset of job titles from the text search - will be used in UI next
    job_names_subset <- reactive({
        job_names[grepl(pattern = input$job_search_string, x = job_names, ignore.case = T)]
    })
    
    output$job_selector <- renderUI({
        selectInput("select_job", label = h3("Select job"), 
                    as.list(job_names_subset()))
    })
    
    # Snippet of html text to show the "best" metro on four different metrics
    output$text_metro_insight <- renderUI({
        text1 <- paste("<b>Highest total employment: </b>", 
                       arrange(salary_data(), desc(total_employment))[, "metro_name"][1])
        text2 <- paste("<b>Highest number of", input$select_job, "employment per 1000 overall jobs: </b>",
                       arrange(salary_data(), desc(job_per_1000))[, "metro_name"][1])
        text3 <- paste("<b>Highest median wage: </b>",
                       arrange(salary_data(), desc(wage_median))[, "metro_name"][1])
        text4 <- paste("<b>Highest median wage (after cost-of-living adj.):</b>",
                       arrange(salary_data(), desc(wage_median_adj))[, "metro_name"][1])
        HTML(paste(text1, text2, text3, text4, sep = '<br/><br/>'))
    })
    
    # For second panel: the selected job title
    output$text_job_title <- renderUI({
        job_title <- paste("<h3>", input$select_job, "</h3>")
        HTML(paste(" ", job_title, " ", sep = "</br>"))
    })
    
    # For second panel: the variable used in bar chart
    output$chart_var_select <- renderUI({    
        selectInput("select_var", label = "Bar chart variable", 
                    c("Total employment" = "total_employment",
                      "Employment per 1000 over all jobs" = "job_per_1000",
                      "Median wage" = "wage_median",
                      "Median wage (after COLI adj.)" = "wage_median_adj")
        )
    })
    
    # Interactive barchart with rCharts
    output$barchart1 <- renderChart2({
        salary_subset = salary_data()[order(-salary_data()[, input$select_var]), ]
        salary_subset = salary_subset[c(1:input$top_n),]
        
        p <- nPlot(x = "metro_name", y = input$select_var,
              data = salary_subset,
              type = 'multiBarChart')
        
        # Format tweaking to accommodate long x-axis labels (metro names)
        p$chart(reduceXTicks = FALSE)
        p$xAxis(staggerLabels = TRUE)
        
        return(p)
    })

    output$downloadData <- downloadHandler(
        filename = function() {paste0("salary_data", 
#                                       gsub("[,]?[-\ /][-]*", "_", input$select_job), 
                                      '.csv')},
        content = function(file) {
            write.csv(salary_data(), file, row.names = F)
    })

})
