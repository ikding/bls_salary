library(shiny)

salary <- readRDS("data/salary_coli.RDS")
job_names <- unique(salary$occ_job_title)

shinyServer(function(input, output) {
    
    salary_data <- reactive({
        salary[salary$occ_job_title == input$select_job, input$show_vars, drop = F]
    })
    
    job_names_subset <- reactive({
        job_names[grepl(pattern = input$job_search_string, x = job_names, ignore.case = T)]
    })
    
    # a large table, reative to input$show_vars
    output$table1 = renderDataTable({
        salary_data()
    }, options = list(orderClasses = TRUE))
    
    output$job_selector <- renderUI({
        selectInput("select_job", label = h3("Select job"), 
                    as.list(job_names_subset()))
    })
    
    output$text_tot_emp <- renderText({
        paste("Highest total employment:", 
              arrange(salary_data(), desc(total_employment))[, "metro_name"][1])
    })
    
    output$text_job_1000 <- renderText({
        paste("Highest number of", input$select_job, "employment per 1000 overall jobs:",
              arrange(salary_data(), desc(job_per_1000))[, "metro_name"][1])
    })

    output$text_wage_median <- renderText({
        paste("Highest median wage:",
              arrange(salary_data(), desc(wage_median))[, "metro_name"][1])
    })
    
    output$text_wage_median_adj <- renderText({
        paste("Highest median wage (after cost-of-living adj.):",
              arrange(salary_data(), desc(wage_median_adj))[, "metro_name"][1])
    })
    
})