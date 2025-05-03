data <- read.csv("Online Sales Data.csv") 
 
# Convert date column to Date type 
data$Date <- as.Date(data$Date, format="%Y-%m-%d") 
 
# Calculate key insights 
total_revenue <- sum(data$Total.Revenue, na.rm = TRUE) 
total_units_sold <- sum(data$Units.Sold, na.rm = TRUE) 
total_transactions <- nrow(data) 
average_order_value <- total_revenue / total_transactions 
 
# Define UI for application 
ui <- fluidPage( 
  theme = shinytheme("cerulean"), 
  titlePanel( 
    title = div( 
      "Online Sales Dashboard" 
    ) 
  ), 
   
  navbarPage( 
    "", 
    tabPanel("Overview",  
             fluidRow( 
               column(width = 3,  
                      selectInput("category_overview", "Select Product Category:",  
                                  choices = c("All", unique(data$Product.Category)), selected = "All")), 
               column(width = 3,  
                      selectInput("region_overview", "Select Region:",  
                                  choices = c("All", unique(data$Region)), selected = "All")) 
             ), 
             fluidRow( 
               column(width = 3,  
                      shinydashboard::valueBox(formatC(total_revenue, format = "f", big.mark = ",", digits = 0), 
"Total Revenue", color = "blue", width = 12)), 
               column(width = 3,  
                      shinydashboard::valueBox(total_units_sold, "Total Units Sold", color = "green", width = 
12)), 
               column(width = 3,  
                      shinydashboard::valueBox(total_transactions, "Total Transactions", color = "purple", width = 
12)), 
               column(width = 3,  
                      shinydashboard::valueBox(formatC(average_order_value, format = "f", big.mark = ",", digits 
= 2), "Average Order Value", color = "orange", width = 12)) 
             ), 
             fluidRow( 
               column(width = 6, plotlyOutput("totalSalesPlot", height = "250px")), 
               column(width = 6, dataTableOutput("salesSummaryTable", height = "150px")) 
             ), 
             div( 
               p("Reference: "), 
               tags$a(href = "https://www.kaggle.com/datasets/shreyanshverma27/online-sales-dataset-popular
marketplace-data",  
                      target = "_blank", "Online Sales Dataset - Popular Marketplace Data") 
             ) 
    ), 
    tabPanel("Sales Analysis",  
             fluidRow( 
               column(width = 3,  
                      selectInput("category_sales", "Select Product Category:",  
                                  choices = c("All", unique(data$Product.Category)), selected = "All")), 
               column(width = 3,  
                      selectInput("region_sales", "Select Region:",  
                                  choices = c("All", unique(data$Region)), selected = "All")) 
             ), 
             plotlyOutput("salesTrendPlot", height = "400px"), 
             p("The Sales Analysis tab provides a detailed view of the sales trends over time. ") 
    ), 
    tabPanel("Revenue Comparison by Region",  
             fluidRow( 
               column(width = 3,  
                      selectInput("category_region", "Select Product Category:",  
                                  choices = c("All", unique(data$Product.Category)), selected = "All")) 
             ), 
             plotlyOutput("revenueComparisonHeatmap", height = "400px"), 
             p("This tab shows a comparison of total revenue over time across different regions using a 
heatmap.") 
    ) 
  ) 
) 
 
# Define server logic 
server <- function(input, output) { 
  # Filtered data 
  filtered_data_overview <- reactive({ 
    data %>% 
      filter((Product.Category == input$category_overview | input$category_overview == "All") & 
               (Region == input$region_overview | input$region_overview == "All")) 
  }) 
   
  filtered_data_sales <- reactive({ 
    data %>% 
      filter((Product.Category == input$category_sales | input$category_sales == "All") & 
               (Region == input$region_sales | input$region_sales == "All")) 
  }) 
   
  filtered_data_region <- reactive({ 
    data %>% 
      filter((Product.Category == input$category_region | input$category_region == "All")) 
  }) 
   
  # Overview plots 
  output$totalSalesPlot <- renderPlotly({ 
    p <- ggplot(filtered_data_overview(), aes(x = Product.Category, y = Total.Revenue, fill = Region)) + 
      geom_bar(stat = "identity") + 
      labs(title = "Total Sales by Product Category and Region",  
           x = "Product Category",  
           y = "Total Revenue") + 
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5)) 
    ggplotly(p) 
  }) 
   
  output$salesSummaryTable <- renderDataTable({ 
    summary <- filtered_data_overview() %>% 
      group_by(Product.Category, Region) %>% 
      summarize(Total.Revenue = sum(Total.Revenue, na.rm = TRUE), 
                Units.Sold = sum(Units.Sold, na.rm = TRUE), 
                .groups = 'drop')  # This line ensures that the grouping is dropped after summarizing 
    datatable(summary) 
  }) 
   
  # Sales Analysis 
  output$salesTrendPlot <- renderPlotly({ 
    p <- ggplot(filtered_data_sales(), aes(x = Date, y = Total.Revenue, color = Product.Category)) + 
      geom_line() + 
      labs(title = "Sales Trend Over Time",  
           x = "Date",  
           y = "Total Revenue") + 
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5)) 
    ggplotly(p) 
  }) 
   
  # Revenue Comparison by Region 
  output$revenueComparisonHeatmap <- renderPlotly({ 
    data_region <- filtered_data_region() 
     
    data_summary <- data_region %>% 
      group_by(Date, Region) %>% 
      summarize(Total.Revenue = sum(Total.Revenue, na.rm = TRUE), 
                .groups = 'drop') 
     
    p <- ggplot(data_summary, aes(x = Date, y = Region, fill = Total.Revenue)) + 
      geom_tile() + 
      scale_fill_gradient(low = "white", high = "blue") + 
      labs(title = "Revenue Comparison by Region Over Time",  
           x = "Date",  
           y = "Region",  
           fill = "Total Revenue") + 
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5)) 
     
    ggplotly(p) 
  }) 
} 
 
# Run the application  
shinyApp(ui = ui, server = server)