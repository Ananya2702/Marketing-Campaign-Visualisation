library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(plotly)
library(dplyr)
library(data.table)
library(tidyr)
library(gridExtra)
library(GGally)
x<- c("Year_Birth","Education","Marital_Status","Income","Log_Income","Total_Children","Recency","visits","NumDealsPurchases","NumWebPurchases","NumCatalogPurchases","NumStorePurchases","NumWebVisitsMonth","Age","Total_Spent","Sqrt_Total_Spent","Standardized_Age")

ed <- c("Undergraduate","PhD","Master")

spending_categories <- c("Wines" = "MntWines", 
                         "Fruits" = "MntFruits", 
                         "Meat Products" = "MntMeatProducts", 
                         "Fish Products" = "MntFishProducts", 
                         "Sweet Products" = "MntSweetProducts", 
                         "Gold Products" = "MntGoldProds")



ui <- dashboardPage(
  dashboardHeader(title = "Marketing Campaign Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Overview", tabName = "data_overview", icon = icon("table")),
      menuItem("EDA", tabName = "eda", icon = icon("bar-chart")),
      menuItem("Demographic Insights", tabName = "demographic_insights", icon = icon("users")),
      menuItem("Campaign Analysis", tabName = "campaign_analysis", icon = icon("area-chart")),
      menuItem("Spending Behavior", tabName = "spending_behavior", icon = icon("credit-card")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("envelope"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home", 
              fluidRow(
                box(
                  title = "Marketing Campaign Analysis!",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 12,
                  HTML('<p style="font-size:16px; text-align:justify;">
                                <strong>In today\'s era of digital transformation,</strong> decoding customer behavior has transcended beyond being a competitive edge – it has become an essential pillar for business survival. The fusion of technology with commerce has opened floodgates of customer data, where each interaction, be it a click, a purchase, or a digital footprint, serves as a window into the intricate world of consumer preferences. Amidst a deluge of data, the real challenge lies in extracting meaningful insights that drive decision-making.
                             </p>
                             <p style="font-size:16px; text-align:justify;">
                                In this cutthroat environment, <strong>a deep understanding of customer behavior</strong> is not just advantageous; it\'s crucial. Our dataset, derived from an intricate marketing campaign, is a rich repository of insights into customer demographics, their buying patterns, and engagement levels. The objective of our analysis is multifaceted:
                             </p>
                             <ul style="font-size:16px; text-align:justify;">
                                <li>Uncover High-Value Demographic Segments</li>
                                <li>Evaluate Marketing Campaign Efficacy</li>
                                <li>Decipher Purchase Patterns Across Segments</li>
                                <li>Predict Future Campaign Responsiveness</li>
                                <li>Optimize Marketing Strategies</li>
                             </ul>
                             <p style="font-size:16px; text-align:justify;">
                                Our journey through this dataset is not just an exercise in data crunching; it\'s a strategic move towards aligning our marketing initiatives with the pulse of our customer base. By understanding the nuances of our audience, we aim to not only resonate more deeply with them but also to propel our company towards greater heights in an increasingly competitive landscape.
                             </p>
                       <footer>Ananya Kaushal   MDS202306</footer>')
                ))) ,
      tabItem(tabName = "data_overview", 
              fluidRow(
                box(
                  title = "Data Description",
                  HTML('<ul>
                <li><strong>People</strong>
                    <ul>
                        <li><strong>ID:</strong> Unique customer identifier.</li>
                        <li><strong>Year_Birth:</strong> Customer\'s birth year.</li>
                        <li><strong>Education:</strong> Education level.</li>
                        <li><strong>Marital_Status:</strong> Marital status.</li>
                        <li><strong>Income:</strong> Yearly household income.</li>
                        <li><strong>Kidhome & Teenhome:</strong> Number of children and teenagers in the household, respectively.</li>
                        <li><strong>Dt_Customer:</strong> Date of enrollment with the company.</li>
                        <li><strong>Recency:</strong> Days since last purchase.</li>
                        <li><strong>Complain:</strong> Indicator (1 or 0) if the customer complained in the last 2 years.</li>
                    </ul>
                </li>
                <li><strong>Products</strong>
                    <ul>
                        <li><strong>Mnt{Product}:</strong> Amount spent on various products (e.g., Wines, Fruits) in the last 2 years.</li>
                    </ul>
                </li>
                <li><strong>Promotion</strong>
                    <ul>
                        <li><strong>NumDealsPurchases:</strong> Purchases made with a discount.</li>
                        <li><strong>AcceptedCmp{i}:</strong> Indicator (1 or 0) if the offer was accepted in the ith campaign.</li>
                        <li><strong>Response:</strong> Indicator (1 or 0) for the last campaign response.</li>
                    </ul>
                </li>
                <li><strong>Place</strong>
                    <ul>
                        <li><strong>Num{Source}Purchases:</strong> Purchases made through various channels (e.g., Web, Catalog, Store).</li>
                        <li><strong>NumWebVisitsMonth:</strong> Website visits in the last month.</li>
                    </ul>
                </li>
            </ul>'),
                  width = 12
                ),
                fluidRow(
                  box(
                    title = "Data Transformations",
                    HTML("<ul>
              <li>Removed duplicate rows.</li>
              <li>Handled outliers in 'Year_Birth' and 'Income'.</li>
              <li>Recategorized 'Education' and 'Marital_Status'.</li>
              <li>Replaced missing values in 'Income' with the median.</li>
              <li>Created new variables: 'visits', 'Income_Level', 'Total_Children', 'Log_Income', 'Sqrt_Total_Spent', 'Standardized_Age'.</li>
              <li>Calculated 'Total_Spent' as the sum of spending on various products.</li>
            </ul>"),
                    width = 12
                  )),
                fluidRow(
                  box(
                    title = "Download Processed Data",
                    downloadButton("downloadData", "Download Data"),
                    width = 12
                  )),
                box(DTOutput("dataTable"), width = 12)
              ),
              HTML('<footer>Ananya Kaushal     MDS202306</footer>')
      ),
      tabItem(tabName = "eda", 
              fluidRow(
                box(selectInput("edaVarSelect", "Select Variable for EDA", 
                                choices = x),
                    width = 6),
                box(plotOutput("edaPlot"), width = 12)
              ),
              box(textOutput("edaConclusionText"), width = 12) ,
              HTML('<footer>Ananya Kaushal     MDS202306</footer>')),
      tabItem(tabName = "demographic_insights",
              tabsetPanel(
                tabPanel("Demographic Distribution",
                         fluidRow(
                           box(title = "Choose Demographic Variable", 
                               selectInput("demographicVarSelect", "Select Demographic Variable",
                                           choices = c("Age", "Education", "Marital_Status", "Income", "Household Composition")),
                               width = 12)
                         ),
                         fluidRow(
                           box(title = "Demographic Plot",
                               plotOutput("demographicPlot"),
                               width = 12)
                         ),
                         fluidRow(
                           box(title = "Demographic Summary",
                               uiOutput("demographicSummary"),
                               width = 12)
                         ),
                         box( textOutput("demographicConclusionText"), width = 12) 
                ),
                tabPanel("Income Distribution by Education",
                         fluidRow(
                           box(title = "Income Distribution by Education Level",
                               plotOutput("incomeEducationPlot"),
                               width = 12)
                         )
                ),
                tabPanel("Average Spending by Marital Status",
                         fluidRow(
                           box(title = "Average Spending by Marital Status",
                               plotOutput("spendingMaritalStatusPlot"),
                               width = 12)
                         )
                ),
                tabPanel("Average Spending by Household Composition",
                         fluidRow(
                           box(title = "Average Spending by Household Composition",
                               plotOutput("spendingHouseholdPlot"),
                               width = 12)
                         )
                ),
                tabPanel("Correlation Analysis",
                         fluidRow(
                           box(title = "Correlation Plot between Age, Income, and Total Spending", 
                               plotOutput("correlationPlot"), 
                               width = 12)
                         )
                )
              ),
              HTML('<footer>Ananya Kaushal     MDS202306</footer>')
      ),
      tabItem(tabName = "campaign_analysis",
              tabsetPanel(
                tabPanel("Response Rate for Each Campaign",
                         fluidRow(
                           box(title = "Response Rate for Each Campaign", 
                               plotOutput("responseRatePlot"),
                               width = 12)
                         )
                ),
                tabPanel("Comparison of Campaign Success",
                         fluidRow(
                           box(title = "Comparison of Campaign Success",
                               plotOutput("campaignSuccessPlot"),
                               width = 12)
                         )
                ),
                tabPanel("Demographic Characteristics of Respondents",
                         fluidRow(
                           box(title = "Filter by Education Level",
                               selectInput("educationLevel", "Select Education Level",
                                           choices = c("All", ed)),
                               width = 6),
                           box(title = "Select Demographic Variable",
                               selectInput("demographicVariable", "Select Demographic Variable",
                                           choices = c("Age", "Income","Log_Income")), 
                               width = 6)
                         ),
                         fluidRow(
                           box(title = "Distribution of Campaign Respondents by Education", 
                               plotOutput("demographicRespondentsPlot"),
                               width = 12)
                         )
                ),
                tabPanel("Engagement by Website Visits",
                         fluidRow(
                           box(title = "Engagement by Website Visits", 
                               plotOutput("websiteVisitsPlot"),
                               width = 12)
                         )
                ),
                tabPanel("Engagement by Purchase Channels",
                         fluidRow(
                           box(title = "Engagement by Purchase Channels",
                               plotOutput("purchaseChannelsPlot"),
                               width = 12)
                         )
                ),
                tabPanel("Overall Campaign Effectiveness",
                         fluidRow(
                           box(title = "Overall Campaign Effectiveness",
                               plotOutput("campaignEffectivenessPlot"),
                               width = 12)
                         )
                ),
                tabPanel("Customer Acquisition Over Time",
                         fluidRow(
                           box(title = "Customer Acquisition Over Time", 
                               plotOutput("customerAcquisitionPlot"),
                               width = 12)
                         )
                )
              ),
              box(title = "Campaign Analysis Conclusion", textOutput("campaignAnalysisConclusionText"), width = 12) 
      ),
      tabItem(tabName = "spending_behavior", 
              tabsetPanel(
                tabPanel("Spending Behavior of Respondents vs Non-Respondents",
                         fluidRow(
                           box(title = "Spending Behavior Comparison",
                               plotOutput("spendingBehaviorPlot"),
                               textOutput("textSpendingBehavior"),
                               width = 12)
                         )
                ),
                tabPanel("Income vs. Spending Categories",
                         fluidRow(
                           box(title = "Income vs. Spending Categories",
                               plotOutput("incomeVsSpendingCategoriesPlot"),
                               textOutput("textIncomeVsSpendingCategories"),
                               width = 12)
                         )
                ),
                tabPanel("Average Spending",
                         fluidRow(
                           box(title = "Average Spending",
                               plotOutput("spendingDistributionPlot"),
                               textOutput("textAvgSpendingProduct"),
                               width = 12)
                         ),
                         fluidRow(
                           box(title = "Income vs Spending : Education",
                               plotOutput("incomeSpendingPlot"), 
                               textOutput("textIncomeVsTotalSpending"),
                               width = 12)
                         )
                ),
                tabPanel("Spending by Demographics",
                         fluidRow(
                           box(title = "Total Spending by Education", 
                               plotOutput("avgSpendingEducationPlot"),
                               textOutput("textAvgSpendingEducation"),
                               width = 12),
                           box(title = "Average Spending by Age", 
                               plotOutput("avgSpendingAgePlot"),
                               textOutput("textAvgSpendingAge"),
                               width = 12),
                           box(title = "Total Spending by Marital Status", 
                               plotOutput("avgSpendingMaritalStatusPlot"),
                               textOutput("textAvgSpendingMaritalStatus"),
                               width = 12)
                         )
                )
              )
      ),
      tabItem(tabName = "conclusion",
              fluidRow(
                box(title = "Overall Conclusion", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, width = 12,
                    uiOutput("conclusionText")  
                )
              )
      )
    )
  )
)
server <- function(input, output) {
  
  data <- read.csv("marketing_campaign.csv", sep = "\t")
  
  data <- data[!duplicated(data), ]
  
  var <- data$Year_Birth
  q1 <- quantile(var, 0.25)
  q3 <- quantile(var, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  outliers <- var[var < lower_bound | var > upper_bound]
  data$Year_Birth[data$Year_Birth %in% outliers] <- median(var, na.rm = TRUE)
  
  
  data$Education[data$Education %in% c("2n Cycle","Basic","Graduation")] <- "Undergraduate"
  data$Marital_Status[data$Marital_Status == "Together"] <- "Married"
  data$Marital_Status[data$Marital_Status == "Alone"] <- "Single"
  data$Marital_Status[data$Marital_Status %in% c("Absurd", "YOLO")] <- "Other"
  data$Income[is.na(data$Income)] <- median(data$Income, na.rm = TRUE)
  data <- data %>%
    mutate(
      visits = ifelse(Recency >= 0 & Recency <= 31, 'Regular', 'Not Regular')
    )
  
  data <- data %>%
    mutate(
      Income_Level = ifelse(Income <= 50000, 'Low',
                            ifelse(Income <= 150000, 'Medium', 'High')),
      Total_Children = Kidhome + Teenhome
    )
  data$Total_Spent <- rowSums(data[, c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds')])
  
  
  data$Dt_Customer <- as.Date(data$Dt_Customer, format="%d-%m-%Y")
  
  #Assuming year to be 2015 given the data
  data$Age <- as.numeric(2015) - data$Year_Birth
  data$Age <- ifelse(data$Age >= 85, 85, data$Age)
  
  var <- data$Income
  q1 <- quantile(var, 0.25)
  q3 <- quantile(var, 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  outliers <- var[var < lower_bound | var > upper_bound]
  
  data$Income[data$Income %in% outliers] <- median(var, na.rm = TRUE)
  campaign_cols <- grep("AcceptedCmp", names(data), value = TRUE)
  
  data <- data %>%
    mutate(
      Log_Income = log1p(Income),  
      Sqrt_Total_Spent = sqrt(Total_Spent),
      Standardized_Age = scale(Age)  
    )
  
  spending_columns <- c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds')
  
  data$Total_Spent <- rowSums(data[, c('MntWines', 'MntFruits', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds')])
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("processed_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  selected_column_summary <- reactive({
    selected_column <- input$columnSelect
    
    if (is.null(selected_column)) {
      return("Please select a column")
    }
    
    column_data <- data[[selected_column]]
    
    if (is.numeric(column_data)) {
      average_value <- mean(column_data, na.rm = TRUE)
      summary_text <- paste("Average:", round(average_value, 2))
    } else {
      mode_value <- names(which.max(table(column_data)))
      summary_text <- paste("Mode:", mode_value)
    }
    
    return(summary_text)
  })
 
  
  
  selected_eda_var <- reactive({
    req(input$edaVarSelect)  
    return(input$edaVarSelect)
  })
  output$edaPlot <- renderPlot({
    var <- selected_eda_var()
    
    if (!(var %in% names(data))) {
      return(NULL)  
    }
    
    if(var == "Income") {
      var_data <- data$Income
      ggplot(data.frame(Income = var_data), aes(x = Income)) + 
        geom_histogram(aes(y = ..density..), color = "black", bins = 30, fill = "skyblue", alpha = 0.7) + 
        geom_density(color="red", size=0.5) +
        labs(x=var, y="Density")+
        theme_minimal() +
        labs(title = paste("Distribution of", var))
    }
    else if(var == "Log_Income")
    {
      var_data <- data$Income
      ggplot(data, aes(x = Log_Income)) +
        geom_histogram(aes(y = ..density..), color = "black", bins = 30, fill = "skyblue", alpha = 0.7) +
        geom_density(color = "red", size = 0.5) +
        labs(x = "Log-Income", y = "Density") +
        theme_minimal() +
        labs(title = "Distribution of Log-Income")
    }
    else if(var=="Total_Spent")
    {
      ggplot(data, aes(x=Total_Spent)) + 
        geom_histogram(aes(y=..density..),color ="black", bins=30, fill="skyblue", alpha=0.7) +
        geom_density(color="red", size=0.5) +
        labs( x="Total Spent", y="Density")
    }
    else if(var=="Sqrt_Total_Spent")
    {
      ggplot(data, aes(x=Sqrt_Total_Spent)) + 
        geom_histogram(aes(y=..density..),color ="black", bins=30, fill="skyblue", alpha=0.7) +
        geom_density(color="red", size=0.5) +
        labs( x="Square Root of Total Spent", y="Density")
    }
    else {
      if(var =="Total_Children")
      {
        ggplot(data, aes_string(x = var)) + 
          geom_bar(fill = "pink", width = 0.7,color = 'black') + 
          theme_minimal() +
          labs(x=var, y="No of Customers")+
          labs(title = paste("Distribution of",var))
      }
      else if (is.numeric(data[[var]])) {
        ggplot(data, aes_string(x = var)) + 
          geom_histogram(aes(y = ..density..), color = "black", fill = "skyblue", alpha = 0.7) +
          geom_density(color="red", size=0.5) +
          theme_minimal() +
          labs(title = paste("Distribution of", var))
      } else {
        if (length(unique(data[[var]])) <= 10) {
          ggplot(data, aes_string(x = var)) + 
            geom_bar(fill = "pink", width = 0.7,color='black') + 
            theme_minimal() +
            labs(x=var, y="No of Customers")+
            labs(title = paste("Distribution of",var))
        } else {
          ggplot(data, aes_string(x = var)) + 
            geom_bar(fill = "pink",color='black') +
            labs(x=var, y="No of Customers")+
            labs(x=var, y="No of Customers")+
            theme_minimal() +
            labs(title = paste("Distribution of",var))
        }
      }
    }
  })
  
  output$demographicPlot <- renderPlot({
    var <- input$demographicVarSelect
    
    req(var)
    
    if(var %in% c("Age", "Income")) {
      ggplot(data, aes_string(x = var)) +
        geom_histogram(fill = "cornflowerblue", bins = 30) +
        labs(title = paste("Distribution of", var), x = var, y = "Count")
    } else if (var == "Household Composition") {
      data %>%
        gather(key = "Type", value = "Count", Kidhome, Teenhome) %>%
        ggplot(aes(x = as.factor(Count), fill = Type)) +
        geom_bar(position = "dodge", color = "black", alpha = 0.7) +
        labs(x = "Number of Children/Teenagers", y = "Number of Customers") +
        scale_fill_manual(values = c("skyblue", "lightcoral")) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      ggplot(data, aes_string(x = var)) +
        geom_bar(fill = "lightgreen") +
        labs( x = var, y = "Count")
    }
  })
  
  output$incomeEducationPlot <- renderPlot({
    ggplot(data, aes(x = Education, y = Income, group = Education)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = "Income Distribution Across Education Levels", x = "Education Level", y = "Income")
  })
  
  
  output$spendingMaritalStatusPlot <- renderPlot({
    ggplot(data, aes(x = Marital_Status, y = Total_Spent, group = Marital_Status)) +
      geom_bar(stat = "summary", fun = "mean", fill = "plum") +
      labs(title = "Average Spending Based on Marital Status", x = "Marital Status", y = "Average Spending")
  })
  
  output$spendingHouseholdPlot <- renderPlot({
    data %>%
      mutate(Household_Composition = ifelse(Total_Children == 0, "No Children", "With Children")) %>%
      ggplot(aes(x = Household_Composition, y = Total_Spent, group = Household_Composition)) +
      geom_bar(stat = "summary", fun = "mean", fill = "aquamarine") +
      labs(title = "Average Spending Based on Household Composition", x = "Household Composition", y = "Average Spending")
  })
  
  output$demographicSummary <- renderUI({
    var <- input$demographicVarSelect
    req(var)
    if (is.numeric(data[[var]])) {
      summary_stats <- summary(data[[var]], na.rm = TRUE)
      stats_text <- paste("Summary for", var, ":",
                          "\nMin:", summary_stats[1],
                          "\n1st Qu.:", summary_stats[2],
                          "\nMedian:", summary_stats[3],
                          "\nMean:", round(mean(data[[var]], na.rm = TRUE), 2),
                          "\n3rd Qu.:", summary_stats[5],
                          "\nMax:", summary_stats[6])
      HTML(paste("<pre>", stats_text, "</pre>"))
    } else {
      freq_table <- sort(table(data[[var]]), decreasing = TRUE)
      freq_text <- paste("Frequency table for", var, ":<br>",
                         paste(names(freq_table), freq_table, sep = ": ", collapse = "<br>"))
      HTML(paste("<p>", freq_text, "</p>"))
    }
  })
  
  output$correlationPlot <- renderPlot({
    correlation_data <- data[, c("Age", "Income", "Total_Spent")]
    
    ggpairs(correlation_data, 
            title = "Correlation Plot between Age, Income, and Total Spending",
            progress = FALSE)
  })
  
  output$responseRatePlot <- renderPlot({
    campaign_cols <- grep("AcceptedCmp", names(data), value = TRUE)
    response_rates <- sapply(data[campaign_cols], function(x) mean(x, na.rm = TRUE))
    
    ggplot(data.frame(Campaign = names(response_rates), ResponseRate = response_rates), aes(x = Campaign, y = ResponseRate)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = "Response Rate for Each Campaign", x = "Campaign", y = "Response Rate")
  })
  campaign_columns <- c('AcceptedCmp1', 'AcceptedCmp2', 'AcceptedCmp3', 'AcceptedCmp4', 'AcceptedCmp5')
  
  output$campaignSuccessPlot <- renderPlot({
    campaign_responses <- colSums(data[, campaign_cols])
    
    ggplot(data.frame(Campaign = names(campaign_responses), Responses = campaign_responses), aes(x = Campaign, y = Responses)) +
      geom_bar(stat = "identity", fill = "coral") +
      labs(title = "Comparison of Campaign Success", x = "Campaign", y = "Number of Responses")
  })
  
  output$demographicRespondentsPlot <- renderPlot({
    respondents <- data[data$Response == 1,]
    
    if (input$educationLevel != "All") {
      respondents <- respondents[respondents$Education == input$educationLevel,]
    }
    
    demographicVar <- input$demographicVariable
    
    ggplot(respondents, aes_string(x = demographicVar, fill = "Education")) +
      geom_histogram(bins = 30, alpha = 0.7, color = 'black') +
      labs(title = paste("Distribution of Campaign Respondents by", demographicVar, "and Education"), 
           x = demographicVar, y = "Count") +
      theme_minimal()
  })
  
  output$spendingBehaviorPlot <- renderPlot({
    data$Respondent <- ifelse(data$Response == 1, "Respondent", "Non-Respondent")
    ggplot(data, aes(x = Respondent, y = Total_Spent, fill = Respondent)) +
      geom_boxplot() +
      labs(title = "Spending Behavior of Respondents vs Non-Respondents", x = "", y = "Total Spent")
    
    
  })
  output$textSpendingBehavior <- renderText({
    return("Respondents to the campaigns appear to have a higher median spending than non-respondents, suggesting that campaign engagement might be an indicator of higher spending.")
  })
  output$websiteVisitsPlot <- renderPlot({
    ggplot(data, aes(x = NumWebVisitsMonth)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
      labs(x = "Number of Visits", y = "Number of Customers") +
      theme_minimal()
  })
  
  output$purchaseChannelsPlot <- renderPlot({
    purchase_channels <- c('NumWebPurchases', 'NumCatalogPurchases', 'NumStorePurchases')
    channel_summary <- colSums(data[purchase_channels])
    
    ggplot(data = data.frame(Channel = names(channel_summary), Purchases = channel_summary), 
           aes(x = reorder(Channel, -Purchases), y = Purchases)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Purchase Channel", y = "Number of Purchases") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$campaignEffectivenessPlot <- renderPlot({
    campaign_responses <- colSums(data[campaign_columns])
    
    ordered_campaigns <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", "AcceptedCmp4", "AcceptedCmp5")
    
    ggplot(data = data.frame(Campaign = factor(names(campaign_responses), levels = ordered_campaigns), Responses = campaign_responses), 
                    aes(x = Campaign, y = Responses)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(x = "Campaigns", y = "Number of Positive Responses") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    
  })
  
  output$customerAcquisitionPlot <- renderPlot({
    monthly_acquisition <- data %>%
      mutate(Month = as.Date(paste0(format(Dt_Customer, "%Y-%m"), "-01"))) %>%
      group_by(Month) %>%
      summarise(Count = n())
    
    ggplot(monthly_acquisition, aes(x = Month, y = Count)) +
      geom_line(color = "skyblue", group = 1) +  
      geom_point(color = "red") +
      labs(x = "Time", y = "Number of Customers Acquired") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  
  output$demographicResponsePlot <- renderPlot({
    demographic_group <- input$demographicGroupSelect
    campaign <- input$campaignSelect
    req(demographic_group, campaign)
    
    ggplot(data, aes_string(x = demographic_group, fill = campaign)) +
      geom_bar(position = "dodge") +
      labs(title = paste("Campaign Response by", demographic_group), x = demographic_group, y = "Count")
  })
  
  
  output$totalSpendingPlot <- renderPlot({
    spending_columns_renamed <- c('Wines', 'Fruits', 'Meat Products', 'Fish Products', 'Sweet Products', 'Gold Products')
    colnames(data)[which(names(data) %in% spending_columns)] <- spending_columns_renamed
    total_spending <- colSums(data[spending_columns_renamed])
    
    ggplot(data = data.frame(Category = names(total_spending), Spending = total_spending), 
           aes(x = reorder(Category, -Spending), y = Spending)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_text(aes(label = scales::dollar(total_spending)), vjust = 1.5, colour = "white") +
      labs(x = "Product Category", y = "Total Amount Spent") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  

  
  
  output$spendingDistributionPlot <- renderPlot({
    spending_summary <- data %>%
      summarise_at(vars(spending_columns), mean, na.rm = TRUE) %>%
      gather(key = "Category", value = "AverageSpending")
    
    ggplot(spending_summary, aes(x = reorder(Category, -AverageSpending), y = AverageSpending)) +
      geom_bar(stat = "identity", fill = "turquoise") +
      geom_text(aes(label = round(AverageSpending, 2)), vjust = -0.3, color = "black") +
      labs(title = "Average Spending on Each Product", x = "Product Category", y = "Average Spending") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
  })
  output$textAvgSpendingProduct <- renderText({
    return("Wines are by far the highest expenditure for customers, indicating a strong preference or higher investment in this category. There's a significant drop in average spending as we move to other categories, with fruits having the least spending.")
  })
  
  
 
  output$avgSpendingEducationPlot <- renderPlot({
    avg_spending_education <- data %>%
      group_by(Education) %>%
      summarise(TotalSpending = sum(Total_Spent, na.rm = TRUE)) %>%
      mutate(Percentage = TotalSpending / sum(TotalSpending) * 100)
    
    ggplot(avg_spending_education, aes(x = "", y = TotalSpending, fill = Education)) +
      geom_col() + 
      coord_polar("y", start = 0) + 
      geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                position = position_stack(vjust = 0.5)) +
      labs(title = "Total Spending by Education", x = "", y =  "Total Spending") +
      theme_void() + 
      scale_fill_brewer(palette = "Set3") 
  })
  
  output$avgSpendingAgePlot <- renderPlot({
    ggplot(data, aes(x = Age, y = Total_Spent)) +
      stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
      labs(title = "Average Spending by Age", x = "Age", y = "Average Spending") +
      theme_minimal()
  })
  
  output$incomeSpendingPlot <- renderPlot({
    
    ggplot(data, aes(x = Income, y = Total_Spent)) +
      geom_point(aes(color = Education), alpha = 0.6) +  
      geom_smooth(method = "lm", color = "blue", se = FALSE) +  
      labs(title = "Income vs Total Spending", x = "Income", y = "Total Spending") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")  
  })
  
  output$textIncomeVsTotalSpending <- renderText({
    return("There's a clear trend that as income increases, total spending also increases. The color differentiation by education shows that this trend holds across different education levels.")
  })
  
  
  output$incomeVsSpendingCategoriesPlot <- renderPlot({
    long_data <- data %>%
      pivot_longer(cols = all_of(spending_columns), names_to = "Category", values_to = "Spending")
    
    ggplot(long_data, aes(x = Spending, y = Income)) +
      geom_point(aes(color = Category), alpha = 0.7) +
      facet_wrap(~ Category, scales = "free_x") +
      theme_minimal() +
      labs(x = "Spending", y = "Income", title = "Income vs. Spending for Each Category") +
      scale_color_brewer(palette = "Set1") 
    
    
  })
  output$textIncomeVsSpendingCategories <- renderText({
    return("There's a positive correlation between income and spending across all categories, with those having higher incomes tending to spend more. The distribution of spending across categories is varied, indicating different preferences or priorities among customers at different income levels.")
  })
 
  output$avgSpendingMaritalStatusPlot <- renderPlot({
    avg_spending_marital_status <- data %>%
      group_by(Marital_Status) %>%
      summarise(TotalSpending = sum(Total_Spent, na.rm = TRUE)) %>%
      mutate(Percentage = TotalSpending / sum(TotalSpending) * 100)
    
    ggplot(avg_spending_marital_status, aes(x = "", y = TotalSpending, fill = Marital_Status)) +
      geom_col() + 
      coord_polar("y", start = 0) + 
      geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                position = position_stack(vjust = 0.5)) +
      labs(title = "Total Spending by Marital Status", x = "", y = "") +
      theme_void() + 
      scale_fill_brewer(palette = "Set3") 
  })
  
  
  output$textAvgSpendingEducation <- renderText({
    return("The pie chart shows across all education qualifications, customers with just an undergraduate degree spend more")
  })
  
  output$textAvgSpendingAge <- renderText({
    return("Average spending fluctuates across age groups, with some peaks that could represent life stages with greater spending needs or disposable income.")
  })
  
  output$textAvgSpendingMaritalStatus <- renderText({
    return("Married individuals have the highest percentage of average spending, potentially due to combined household incomes or family-oriented purchases.")
  })
  output$conclusionText <- renderUI({
    HTML(paste0(
      "<h3>Overall Conclusion:</h3>",
      "<p>Our comprehensive analysis of customer behavior and campaign response highlights several actionable insights. We observed that spending behavior is positively correlated with income, particularly for high-value product categories like wines and meats. Engagement levels, as reflected by the campaign response rates, suggest that targeted marketing efforts can significantly influence customer spending. Notably, married and single segments exhibit higher spending, implying potential for tailored marketing strategies. Additionally, store purchases dominate, signaling the importance of in-store experience, while online and catalog channels offer opportunities for growth. The fluctuation in customer acquisition over time underscores the influence of seasonal marketing and possibly external market factors. Overall, the data underscores the importance of strategic segmentation and personalized marketing to enhance customer engagement and optimize campaign effectiveness.</p>",
      "<h4>Detailed Insights:</h4>",
      "<ul>",
      "<li><strong>Demographics:</strong> The majority of our customers are middle-aged, specifically between 40 to 60 years. This suggests that our marketing campaigns resonate well with this age group. Furthermore, most of our customers earn between $20,000 and $80,000 annually, indicating a broad middle-income client base.</li>",
      "<li><strong>Education and Family:</strong> A notable portion of our clientele have pursued undergraduate studies, followed by those holding PhDs and master's degrees. Interestingly, many of our customers are married without children at home. This demographic insight may be crucial for tailoring future campaigns.</li>",
      "<li><strong>Spending Patterns:</strong> The primary product driving sales is wine, indicating its popularity or perhaps its higher price point. However, it's essential to note that income doesn't directly correlate with spending on other products, suggesting diverse purchase preferences across income groups.</li>",
      "<li><strong>Campaign Responses:</strong> The final campaign saw a substantial response, indicating its effectiveness. The primary channel for these responses was store purchases, suggesting the importance of physical outlets in our marketing strategy.</li>",
      "<li><strong>Engagement Over Time:</strong>Customer acquisition has remained consistent over time, and the frequency of visits peaks at 8 times a month. This consistency offers a stable customer base but also indicates room for growth.</li>",
      "<li><strong>Recency Analysis:</strong> The uniform distribution of customers against days since the last purchase can be instrumental in segmenting our audience for targeted campaigns. By understanding the frequency of customer purchases, we can tailor our outreach more effectively.</li>",
      "</ul>",
      "<footer>Ananya Kaushal MDS202306</footer>"
    ))
  })
  
  output$edaConclusionText <- renderText({
    selected_var <- input$edaVarSelect
    req(selected_var) 
    
    if(selected_var == "Income") {
      return("The Population Income distribution shows a right-skewed pattern, indicating that a larger proportion of customers fall into the lower income brackets, while fewer customers have very high incomes. This suggests that marketing strategies could be tailored to appeal to a predominantly middle to low-income customer base. The chart above shows income to be normally distributed after removing the outliers and imputing them with the median")
    } else if(selected_var == "Age") {
      return("The Age distribution indicates that our customer base is fairly spread out across different age groups, with a slight concentration in the middle-age brackets. This diversity presents an opportunity to cater to a wide range of preferences and needs in our product offerings and marketing approaches.")
    } else if(selected_var == "Total_Spent") {
      return("The Total Spent distribution reveals that a significant number of customers have a moderate to high level of spending, with a tail of customers who spend very large amounts. Targeted promotions could be created to encourage higher spending from customers who are already inclined to spend more.")
    }
    else if (selected_var=="Recency"){
      return("Recency follows a uniform distribution")
      }
    else {
      return(paste("The distribution of", selected_var, "provides valuable insights into the characteristics of our customer base, which can inform targeted marketing strategies and business decisions."))
    }
  })
  
  output$demographicConclusionText <- renderText({
    return(paste0("Our demographic insights indicate a predominant customer base in the 20-50 age range, ",
                  "with a significant representation of undergraduate-level education. ",
                  "The majority are married, suggesting family-oriented purchasing behavior. ",
                  "Furthermore, a consistent pattern emerges in the income distribution across education levels, ",
                  "showing that higher education correlates with increased income, which may translate to greater spending power. ",
                  "These nuanced understandings of our customers' demographic profiles are crucial for ",
                  "targeting marketing efforts and aligning our product offerings with customer needs."))
  })
  output$campaignAnalysisConclusionText <- renderText({
    return(paste0("The analysis of campaign responses indicates a varying success rate across different initiatives. ",
                  "Campaigns 1 and 5 displayed the highest response rates, suggesting their messaging or medium was particularly resonant with our audience. ",
                  "In contrast, Campaign 2's lower performance may indicate a need for adjustment in strategy or targeting. ",
                  "Demographically, higher engagement was observed among customers with undergraduate education levels, ",
                  "which aligns with our largest customer base. These insights should guide the optimization of future campaigns, ",
                  "focusing on high-performing themes and channels while improving or discarding the less effective ones. ",
                  "Moreover, understanding the demographic characteristics of respondents can help in personalizing the campaigns ",
                  "to increase their effectiveness further."))
  })
  
}





shinyApp(ui, server)

