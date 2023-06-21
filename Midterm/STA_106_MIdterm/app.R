library(dplyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(shiny)
library(plotly)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Residual Analysis"),
  dashboardSidebar(fileInput("select_data","Select Data"),
                   selectInput("response_variable","Choose Response Variable",choices = c()),
                   selectInput("factor_variable","Choose Factor Variable",choices = c()),
                   selectInput("transformation","Transform Response Variable",choices = c("None","Log","Squared","Inverse"))),
  
  dashboardBody(
    fluidRow(
      box( plotlyOutput("residual_plot")),
      box(plotlyOutput("qq_plot")),
      box(plotOutput("dot_plot"))
    )
  )
)

server <- function(input,output,session){
  data <- reactive({
    data_object <- input$select_data
    print(data_object$datapath )
    if(length(data_object$datapath) != 0){
      data <- read.table(data_object$datapath,header = TRUE)
    }
  })
  
  
  observeEvent(data(),{
    updateSelectInput(session,"response_variable",choices = colnames(data()))   
    updateSelectInput(session,"factor_variable",choices = colnames(data()))   
  })
  
  
  
  
  observeEvent(input$transformation,{
    if(input$transformation == "Log"){
      residuals_transformed <- reactive({
        data() %>% extract2(input$factor_variable) %>% unique %>%  
          map(~ data() %>% 
                filter(!!as.symbol(input$factor_variable) == .x) %>% 
                extract2(input$response_variable) %>% log - data() %>%
                filter(!!as.symbol(input$factor_variable) == .x) %>% extract2(input$response_variable) %>% log %>% mean) %>% 
          unlist
        
      })  
      
      factor_means_transformed <- reactive({data() %>% extract2(input$factor_variable) %>% unique %>%
          map(~data() %>% 
                filter(!!as.symbol(input$factor_variable) == .x) %>%
                extract2(input$response_variable) %>% log %>% mean) %>% 
          unlist 
      })
      fitted_values_transformed<- reactive({
        factor_lengths <- data() %>% extract2(input$factor_variable) %>% as.factor %>% levels %>% map_int(~data()%>% 
                                                                                                            filter(!!as.symbol(input$factor_variable) == .x) %>%
                                                                                                            extract2(input$factor_variable) %>% length)
        fitted_values <- rep(factor_means_transformed(),factor_lengths)
      })
      
      residuals_standard_transformed <- reactive({
        residuals_standard_transformed <- residuals_transformed() -(residuals_transformed() %>% mean)
        residuals_standard_transformed <- residuals_standard_transformed/residuals_transformed() %>% sd
      })
      output$residual_plot <- renderPlotly({
        ggplot(data(),aes(x = fitted_values_transformed(),y = residuals_transformed(),color = !!as.symbol(input$factor_variable))) + geom_point()+
          xlab("Fitted Values")+ylab("Residuals")
        
      })
      
      
      output$dot_plot <- renderPlot({
        if(length(input$factor_variable) != 0 & length(input$response_variable) != 0 & length(data()) != 0){ 
          ggplot(data = data(),aes(x = !!as.symbol(input$factor_variable) %>% as.factor,y = !!as.symbol(input$response_variable) %>% log ,fill = !!as.symbol(input$factor_variable) %>% as.factor))+ 
            geom_dotplot(binaxis = "y",stackdir = "center",binpositions = "all",show.legend = FALSE)+
            xlab(input$factor_variable)+ylab(input$response_variable)+
            stat_summary(fun = mean,geom = "point",color = "red",show.legend = FALSE,size = 4)
        }
      })
      output$qq_plot <- renderPlotly({ ggplot(mapping = aes(sample = residuals_standard_transformed())) + geom_qq()+geom_qq_line(col = 2)+labs(title = "Normal Q-Q Plot")+xlab("Theoretical Quantiles")+ylab("Sample Quantiles")+theme_bw()+theme(panel.grid = element_blank())})
      
    }
    
    
    else if(input$transformation == "Squared"){
      residuals_transformed <- reactive({
        data() %>% extract2(input$factor_variable) %>% unique %>%  
          map(~ data() %>% 
                filter(!!as.symbol(input$factor_variable) == .x) %>% 
                extract2(input$response_variable) %>% multiply_by(data() %>% filter(!!as.symbol(input$factor_variable) == .x) %>% 
                                                                    extract2(input$response_variable)) - data() %>%
                filter(!!as.symbol(input$factor_variable) == .x) %>% extract2(input$response_variable)%>% 
                multiply_by(data() %>% filter(!!as.symbol(input$factor_variable) == .x) %>% extract2(input$response_variable))%>% mean) %>% 
          unlist
        
      })  
      
      factor_means_transformed <- reactive({data() %>% extract2(input$factor_variable) %>% unique %>%
          map(~data() %>% 
                filter(!!as.symbol(input$factor_variable) == .x) %>%
                extract2(input$response_variable) %>%
                multiply_by(data() %>% filter(!!as.symbol(input$factor_variable) == .x) %>% 
                              extract2(input$response_variable))%>% mean) %>% 
          unlist 
      })
      fitted_values_transformed<- reactive({
        factor_lengths <- data() %>% extract2(input$factor_variable) %>% as.factor %>% levels %>% map_int(~data()%>% 
                                                                                                            filter(!!as.symbol(input$factor_variable) == .x) %>%
                                                                                                            extract2(input$factor_variable) %>% length)
        fitted_values <- rep(factor_means_transformed(),factor_lengths)
      })
      
      residuals_standard_transformed <- reactive({
        residuals_standard_transformed <- residuals_transformed() -(residuals_transformed() %>% mean)
        residuals_standard_transformed <- residuals_standard_transformed/residuals_transformed() %>% sd
      })
      output$residual_plot <- renderPlotly({
        ggplot(data(),aes(x = fitted_values_transformed(),y = residuals_transformed(),color = !!as.symbol(input$factor_variable))) + geom_point()+
          xlab("Fitted Values")+ylab("Residuals")
        
      })
      
      
      output$dot_plot <- renderPlot({
        response_variable_squared <- data() %>% extract2(input$response_variable) %>% multiply_by(data() %>% extract2(input$response_variable))
        print(response_variable_squared)
        if(length(input$factor_variable) != 0 & length(input$response_variable) != 0 & length(data()) != 0){ 
          ggplot(data = data(),aes(x = !!as.symbol(input$factor_variable) %>% as.factor,y = response_variable_squared,fill = !!as.symbol(input$factor_variable) %>% as.factor))+ 
            geom_dotplot(binaxis = "y",stackdir = "center",binpositions = "all",show.legend = FALSE)+
            xlab(input$factor_variable)+ylab(input$response_variable)+
            stat_summary(fun = mean,geom = "point",color = "red",show.legend = FALSE,size = 4)
        }
      })
      output$qq_plot <- renderPlotly({ ggplot(mapping = aes(sample = residuals_standard_transformed())) + geom_qq()+geom_qq_line(col = 2)+labs(title = "Normal Q-Q Plot")+xlab("Theoretical Quantiles")+ylab("Sample Quantiles")+theme_bw()+theme(panel.grid = element_blank())})
      
    }   
    
    else if(input$transformation == "Inverse"){
      residuals_transformed <- reactive({
        data() %>% extract2(input$factor_variable) %>% unique %>%  
          map(~ 1 %>% divide_by(data() %>% filter(!!as.symbol(input$factor_variable) == .x) %>%  extract2(input$response_variable)) - 1 %>% 
                divide_by(data() %>% filter(!!as.symbol(input$factor_variable) == .x) %>% extract2(input$response_variable)) %>% mean) %>% 
          unlist
        
      })  
      
      factor_means_transformed <- reactive({data() %>% extract2(input$factor_variable) %>% unique %>%
          map(~ 1 %>% divide_by(data() %>% filter(!!as.symbol(input$factor_variable) == .x) %>%  extract2(input$response_variable))%>% mean) %>% 
          unlist 
      })
      fitted_values_transformed<- reactive({
        factor_lengths <- data() %>% extract2(input$factor_variable) %>% as.factor %>% levels %>% map_int(~data()%>% 
                                                                                                            filter(!!as.symbol(input$factor_variable) == .x) %>%
                                                                                                            extract2(input$factor_variable) %>% length)
        fitted_values <- rep(factor_means_transformed(),factor_lengths)
      })
      
      residuals_standard_transformed <- reactive({
        residuals_standard_transformed <- residuals_transformed() -(residuals_transformed() %>% mean)
        residuals_standard_transformed <- residuals_standard_transformed/residuals_transformed() %>% sd
      })
      output$residual_plot <- renderPlotly({
        ggplot(data(),aes(x = fitted_values_transformed(),y = residuals_transformed(),color = !!as.symbol(input$factor_variable))) + geom_point()+
          xlab("Fitted Values")+ylab("Residuals")
        
      })
      
      
      output$dot_plot <- renderPlot({
        if(length(input$factor_variable) != 0 & length(input$response_variable) != 0 & length(data()) != 0){ 
          ggplot(data = data(),aes(x = !!as.symbol(input$factor_variable) %>% as.factor,y = 1 %>% divide_by(!!as.symbol(input$response_variable)),fill = !!as.symbol(input$factor_variable) %>% as.factor))+ 
            geom_dotplot(binaxis = "y",stackdir = "center",binpositions = "all",show.legend = FALSE)+
            xlab(input$factor_variable)+ylab(input$response_variable)+
            stat_summary(fun = mean,geom = "point",color = "red",show.legend = FALSE,size = 4)
        }
      })
      output$qq_plot <- renderPlotly({ ggplot(mapping = aes(sample = residuals_standard_transformed())) + geom_qq()+geom_qq_line(col = 2)+labs(title = "Normal Q-Q Plot")+xlab("Theoretical Quantiles")+ylab("Sample Quantiles")+theme_bw()+theme(panel.grid = element_blank())})
      
    }   
    else{
      residuals <- reactive({
        data() %>% extract2(input$factor_variable) %>% unique %>%  
          map(~ (data() %>% 
                   filter(!!as.symbol(input$factor_variable) == .x) %>% 
                   extract2(input$response_variable)) - (data() %>%
                                                           filter(!!as.symbol(input$factor_variable) == .x) %>% extract2(input$response_variable) %>% mean)) %>% 
          unlist
        
      }) 
      
      factor_means <- reactive({data() %>% extract2(input$factor_variable) %>% unique %>%
          map(~data() %>% 
                filter(!!as.symbol(input$factor_variable) == .x) %>%
                extract2(input$response_variable) %>% mean) %>% 
          unlist 
      })
      fitted_values <- reactive({
        factor_lengths <- data() %>% extract2(input$factor_variable) %>% as.factor %>% levels %>% map_int(~data()%>% 
                                                                                                            filter(!!as.symbol(input$factor_variable) == .x) %>%
                                                                                                            extract2(input$factor_variable) %>% length)
        fitted_values <- rep(factor_means(),factor_lengths)
      })
      
      
      residuals_standard <- reactive({
        residuals_standard <- residuals() -(residuals() %>% mean)
        residuals_standard <- residuals_standard/residuals() %>% sd
      })
      output$residual_plot <- renderPlotly({
        print(residuals())
        if(length(fitted_values()) != 0 & length(residuals()) != 0 & length(input$factor_variable) != 0){
          ggplot(data(),aes(x = fitted_values(),y = residuals(),color = !!as.symbol(input$factor_variable))) + geom_point()+
            xlab("Fitted Values")+ylab("Residuals")
        }
      })
      
      output$dot_plot <- renderPlot({
        if(length(input$factor_variable) != 0 & length(input$response_variable) != 0 & length(data()) != 0){ 
          ggplot(data = data(),aes(x = !!as.symbol(input$factor_variable) %>% as.factor,y = !!as.symbol(input$response_variable),fill = !!as.symbol(input$factor_variable) %>% as.factor))+ 
            geom_dotplot(binaxis = "y",stackdir = "center",binpositions = "all",show.legend = FALSE)+
            xlab(input$factor_variable)+ylab(input$response_variable)+
            stat_summary(fun = mean,geom = "point",color = "red",show.legend = FALSE,size = 4)
        }
      })
      
      
      output$qq_plot <- renderPlotly({ ggplot(mapping = aes(sample = residuals_standard())) + geom_qq()+geom_qq_line(col = 2)+labs(title = "Normal Q-Q Plot")+xlab("Theoretical Quantiles")+ylab("Sample Quantiles")+theme_bw()+theme(panel.grid = element_blank())})
    } 
    
  })
  
  
  
}
shinyApp(ui = ui, server = server)