
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(scales)
library(leaflet)
library(reshape2) # For heatmap data reshaping

# --- Data Loading and Cleaning ---

# Load data
# Assuming the file is in the same directory
file_path <- "Student Satisfaction Survey for Agricultural Sciences Program - Form responses.csv"

if (!file.exists(file_path)) {
  stop("CSV file not found. Please ensure the file is in the working directory.")
}

raw_data <- read_csv(file_path, show_col_types = FALSE)

# Rename columns for easier handling
# We keep a mapping of original questions to new names for tooltips/labels
questions_map <- colnames(raw_data)
names(questions_map) <- c(
  "StudentID", "CourseCode", "YearLevel", "DegreeType", "PriorExperience", 
  "ReputationChoice", "RuralResidency", "FarmBackground", "Schooling", 
  "DurationSatisfaction", "ExtendForResidency", "MotivPrivateSector", "MotivResearch", 
  "BridgingUnits", "SNAGSInvolvement", "LecturesFaceToFace", "AssessmentsFair", 
  "FacultyExpertise", "FeedbackConstructive", "GuestLectures", "FacultySupport", 
  "AgTechIntegration", "ContentRelevance", "TradeOffsPrep", "EconomicsUnits", 
  "FirstYearUnits", "TechDepth", "Workload", "OnlineLearning", "Electives", 
  "LibraryResources", "CurriculumFlow", "CareerServices", 
  "SNAGS_Industry", "SNAGS_Cohort", "SNAGS_Social", "SNAGS_Hesitant", 
  "UnitsNeeded", "StudyAbroadConsider", "StudyAbroadCountries", "UnitsTooBroad", 
  "Comments", "ExpectedIncome", "SubmissionID", "SubmissionDate" # Adjusting for potential extra columns if any, based on CSV inspection
)

# The CSV provided in context has "Student ID", "Submission ID" at the end in the header row but data seems to match.
# Let's inspect the provided CSV structure from the prompt again.
# Header: Student ID - Student ID, Student ID - COURSE CODE, Which year level..., ..., What is your expected income as a graduate ($), Student ID, Submission ID
# The last two columns "Student ID" and "Submission ID" might be duplicates or system fields.
# Let's just rename the first N columns that match our interest.

new_names <- c(
  "StudentID", "CourseCode", "YearLevel", "DegreeType", "PriorExperience", 
  "ReputationChoice", "RuralResidency", "FarmBackground", "Schooling", 
  "DurationSatisfaction", "ExtendForResidency", "MotivPrivateSector", "MotivResearch", 
  "BridgingUnits", "SNAGSInvolvement", "LecturesFaceToFace", "AssessmentsFair", 
  "FacultyExpertise", "FeedbackConstructive", "GuestLectures", "FacultySupport", 
  "AgTechIntegration", "ContentRelevance", "TradeOffsPrep", "EconomicsUnits", 
  "FirstYearUnits", "TechDepth", "Workload", "OnlineLearning", "Electives", 
  "LibraryResources", "CurriculumFlow", "CareerServices", 
  "SNAGS_Industry", "SNAGS_Cohort", "SNAGS_Social", "SNAGS_Hesitant", 
  "UnitsNeeded", "StudyAbroadConsider", "StudyAbroadCountries", "UnitsTooBroad", 
  "Comments", "ExpectedIncome"
)

# Assign names to the first length(new_names) columns
colnames(raw_data)[1:length(new_names)] <- new_names

# Data Cleaning
survey_data <- raw_data %>%
  mutate(
    # Clean Income: remove commas, quotes, non-numeric chars (keep numbers)
    ExpectedIncomeNumeric = as.numeric(gsub("[^0-9]", "", ExpectedIncome)),
    
    # Convert Likert scales (1-5) to factors if needed, or keep as numeric for mean calc
    # Most 1-5 columns are numeric in CSV.
    # SNAGS columns are text (Strongly Agree, etc.) -> Convert to factor with order
    SNAGS_Industry = factor(SNAGS_Industry, levels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    SNAGS_Cohort = factor(SNAGS_Cohort, levels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    SNAGS_Social = factor(SNAGS_Social, levels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    SNAGS_Hesitant = factor(SNAGS_Hesitant, levels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))
  )

# Define Likert Questions (Numeric 1-5)
likert_cols <- c(
  "ReputationChoice", "DurationSatisfaction", "ExtendForResidency", "MotivPrivateSector", 
  "MotivResearch", "BridgingUnits", "LecturesFaceToFace", "AssessmentsFair", 
  "FacultyExpertise", "FeedbackConstructive", "GuestLectures", "FacultySupport", 
  "AgTechIntegration", "ContentRelevance", "TradeOffsPrep", "EconomicsUnits", 
  "FirstYearUnits", "TechDepth", "Workload", "OnlineLearning", "Electives", 
  "LibraryResources", "CurriculumFlow", "CareerServices", "UnitsTooBroad"
)

# --- UI Definition ---

ui <- dashboardPage(
  dashboardHeader(title = "Ag Sciences Survey"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Satisfaction", tabName = "satisfaction", icon = icon("smile")),
      menuItem("SNAGS", tabName = "snags", icon = icon("users-cog")),
      menuItem("Curriculum & Future", tabName = "curriculum", icon = icon("graduation-cap")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab: Overview
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("total_responses"),
          valueBoxOutput("avg_income"),
          valueBoxOutput("farm_bg_percent")
        ),
        fluidRow(
          box(title = "Year Level Distribution", status = "primary", solidHeader = TRUE, plotlyOutput("plot_year_level")),
          box(title = "Degree Type", status = "primary", solidHeader = TRUE, plotlyOutput("plot_degree_type"))
        )
      ),
      
      # Tab: Demographics
      tabItem(tabName = "demographics",
        fluidRow(
          box(title = "Prior Experience in Ag", plotlyOutput("plot_prior_exp")),
          box(title = "Rural vs Metro Background", plotlyOutput("plot_rural"))
        ),
        fluidRow(
          box(title = "Schooling Background", plotlyOutput("plot_schooling"), width = 12)
        )
      ),
      
      # Tab: Satisfaction
      tabItem(tabName = "satisfaction",
        fluidRow(
          box(title = "Multivariate Analysis Protocol (PCA)", status = "primary", solidHeader = TRUE, width = 12,
              p("Following the proposed protocol, we performed Principal Component Analysis (PCA) to identify underlying factors of student satisfaction."),
              p("The heatmap below shows how each question contributes to the identified Factors (Components). Groupings indicate shared themes (e.g., Practical Learning, Teaching Quality).")
          )
        ),
        fluidRow(
          box(title = "Factor Loadings (Question Groupings)", plotlyOutput("plot_pca_loadings", height = "500px"), width = 8),
          box(title = "Variance Explained", plotlyOutput("plot_pca_scree", height = "500px"), width = 4)
        ),
        fluidRow(
          box(title = "Cluster Analysis: Factors by Student Background", status = "warning", solidHeader = TRUE, width = 12,
              fluidRow(
                column(4, selectInput("pca_factor_select", "Select Factor:", choices = c("PC1", "PC2", "PC3", "PC4"))),
                column(4, selectInput("group_select", "Compare across Group:", 
                                      choices = c("FarmBackground", "RuralResidency", "DegreeType", "YearLevel", "PriorExperience"), 
                                      selected = "FarmBackground"))
              ),
              plotlyOutput("plot_pca_groups")
          )
        ),
        fluidRow(
          box(title = "PCA Biplot", status = "warning", solidHeader = TRUE, width = 12,
              fluidRow(
                column(4, selectInput("biplot_x", "X Axis:", choices = c("PC1", "PC2", "PC3", "PC4"), selected = "PC1")),
                column(4, selectInput("biplot_y", "Y Axis:", choices = c("PC1", "PC2", "PC3", "PC4"), selected = "PC2")),
                column(4, selectInput("biplot_color", "Color/Symbol by:", 
                                      choices = c("FarmBackground", "RuralResidency", "DegreeType", "YearLevel", "PriorExperience"), 
                                      selected = "DegreeType"))
              ),
              plotlyOutput("plot_pca_biplot", height = "600px")
          )
        ),
        fluidRow(
          box(title = "Hierarchical Cluster Analysis", status = "success", solidHeader = TRUE, width = 12,
              fluidRow(
                column(4, sliderInput("hca_k", "Number of Clusters (k):", min = 2, max = 10, value = 3)),
                column(4, selectInput("hca_method", "Linkage Method:", choices = c("ward.D2", "complete", "average"), selected = "ward.D2"))
              ),
              fluidRow(
                column(6, plotOutput("plot_dendrogram", height = "400px")),
                column(6, plotlyOutput("plot_hca_pca", height = "400px"))
              ),
              h4("Top Questions Distinguishing the Clusters"),
              p("These questions have the largest difference in average score between the identified clusters."),
              tableOutput("table_hca_drivers"),
              p("The Dendrogram (left) shows how students are grouped based on similarity. The Scatter Plot (right) shows these clusters projected onto the first two PCA factors.")
          )
        ),
        fluidRow(
          box(title = "Detailed Question Analysis", status = "info", solidHeader = TRUE, width = 12,
              selectInput("sat_question", "Select Question:", choices = likert_cols, selected = "ContentRelevance", width = "100%"),
              uiOutput("question_text_ui"), # Dynamic UI for full question text
              plotlyOutput("plot_likert_dist")
          )
        )
      ),
      
      # Tab: SNAGS
      tabItem(tabName = "snags",
        fluidRow(
          box(title = "SNAGS Involvement", plotlyOutput("plot_snags_involved")),
          box(title = "Perception of SNAGS", plotlyOutput("plot_snags_perception"))
        )
      ),
      
      # Tab: Curriculum & Future
      tabItem(tabName = "curriculum",
        fluidRow(
          box(title = "Expected Income Distribution", plotlyOutput("plot_income")),
          box(title = "Study Abroad Consideration", plotlyOutput("plot_abroad"))
        ),
        fluidRow(
          box(title = "Study Abroad Destinations", leafletOutput("map_abroad"), width = 12)
        ),
        fluidRow(
          box(title = "Units Needed (Word Cloud / Bar)", plotlyOutput("plot_units_needed"), width = 12)
        )
      ),
      
      # Tab: Data
      tabItem(tabName = "data",
        fluidRow(
          box(width = 12, DTOutput("raw_table"))
        )
      )
    )
  )
)

# --- Server Logic ---

server <- function(input, output) {
  
  # Overview Boxes
  output$total_responses <- renderValueBox({
    valueBox(nrow(survey_data), "Total Responses", icon = icon("list"), color = "blue")
  })
  
  output$avg_income <- renderValueBox({
    avg_inc <- mean(survey_data$ExpectedIncomeNumeric, na.rm = TRUE)
    valueBox(paste0("$", formatC(avg_inc, format="f", digits=0, big.mark=",")), "Avg Expected Income", icon = icon("dollar-sign"), color = "green")
  })
  
  output$farm_bg_percent <- renderValueBox({
    pct <- mean(survey_data$FarmBackground == "Yes", na.rm = TRUE) * 100
    valueBox(paste0(round(pct, 1), "%"), "From Farm Background", icon = icon("tractor"), color = "yellow")
  })
  
  # Overview Plots
  output$plot_year_level <- renderPlotly({
    p <- survey_data %>%
      count(YearLevel) %>%
      ggplot(aes(x = YearLevel, y = n, fill = YearLevel)) +
      geom_col() +
      theme_minimal() +
      labs(y = "Count", x = "")
    ggplotly(p)
  })
  
  output$plot_degree_type <- renderPlotly({
    p <- survey_data %>%
      count(DegreeType) %>%
      ggplot(aes(x = DegreeType, y = n, fill = DegreeType)) +
      geom_col() +
      theme_minimal() +
      labs(y = "Count", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  # Demographics Plots
  output$plot_prior_exp <- renderPlotly({
    p <- ggplot(survey_data, aes(x = PriorExperience, fill = PriorExperience)) + geom_bar() + theme_minimal()
    ggplotly(p)
  })
  
  output$plot_rural <- renderPlotly({
    p <- ggplot(survey_data, aes(x = RuralResidency, fill = RuralResidency)) + geom_bar() + theme_minimal()
    ggplotly(p)
  })
  
  output$plot_schooling <- renderPlotly({
    p <- ggplot(survey_data, aes(x = Schooling, fill = Schooling)) + geom_bar() + theme_minimal() + coord_flip()
    ggplotly(p)
  })
  
  # Satisfaction Logic
  
  # Reactive PCA Calculation
  pca_results <- reactive({
    # Select Likert columns and remove rows with NAs for PCA
    pca_data <- survey_data %>%
      select(all_of(likert_cols)) %>%
      na.omit()
    
    # Run PCA
    # Scale=TRUE is important for Likert if variances differ, though usually similar range.
    prcomp(pca_data, scale. = TRUE)
  })
  
  # PCA Loadings Plot (Heatmap)
  output$plot_pca_loadings <- renderPlotly({
    pca <- pca_results()
    
    # Get loadings for first 4 components (as suggested by protocol example: PQ&S, IR&A, P&EL, PS&R)
    loadings <- pca$rotation[, 1:4]
    
    # Convert to long format for heatmap
    loadings_long <- melt(loadings)
    colnames(loadings_long) <- c("Question", "PC", "Value")
    
    # Add full question text for tooltip
    loadings_long$FullQuestion <- questions_map[match(new_names[match(loadings_long$Question, new_names)], new_names)]
    
    p <- ggplot(loadings_long, aes(x = PC, y = Question, fill = Value, text = FullQuestion)) +
      geom_tile() +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
      theme_minimal() +
      labs(x = "Principal Component (Factor)", y = "", fill = "Loading") +
      theme(axis.text.y = element_text(size = 8))
    
    ggplotly(p, tooltip = c("y", "x", "fill", "text"))
  })
  
  # PCA Scree Plot
  output$plot_pca_scree <- renderPlotly({
    pca <- pca_results()
    var_explained <- pca$sdev^2 / sum(pca$sdev^2)
    
    df <- data.frame(
      PC = paste0("PC", 1:length(var_explained)),
      Variance = var_explained
    ) %>% head(10) # Show top 10
    
    p <- ggplot(df, aes(x = reorder(PC, -Variance), y = Variance)) +
      geom_col(fill = "steelblue") +
      geom_line(aes(group = 1), color = "black") +
      geom_point() +
      theme_minimal() +
      labs(x = "Component", y = "Proportion of Variance", title = "Scree Plot") +
      scale_y_continuous(labels = scales::percent)
    
    ggplotly(p)
  })
  
  # PCA Groups Plot
  output$plot_pca_groups <- renderPlotly({
    req(input$pca_factor_select, input$group_select)
    
    pca <- pca_results()
    
    # Get scores
    scores <- as.data.frame(pca$x)
    
    # We need to merge scores back with the original data (careful with NAs)
    # Re-select data used for PCA to ensure alignment
    data_used <- survey_data %>%
      select(all_of(likert_cols), all_of(input$group_select)) %>%
      na.omit()
    
    # Add scores to data
    data_used <- cbind(data_used, scores)
    
    factor_col <- input$pca_factor_select
    group_col <- input$group_select
    
    p <- ggplot(data_used, aes(x = .data[[group_col]], y = .data[[factor_col]], fill = .data[[group_col]])) +
      geom_boxplot() +
      theme_minimal() +
      labs(x = group_col, y = paste(factor_col, "Score"), title = paste("Distribution of", factor_col, "by", group_col)) +
      theme(legend.position = "none")
    
    ggplotly(p)
  })

  # PCA Biplot
  output$plot_pca_biplot <- renderPlotly({
    req(input$biplot_color, input$biplot_x, input$biplot_y)
    
    pca <- pca_results()
    scores <- as.data.frame(pca$x)
    loadings <- as.data.frame(pca$rotation)
    
    # Merge scores with demographic data
    # Re-select data used for PCA to ensure alignment
    data_used <- survey_data %>%
      select(all_of(likert_cols), all_of(input$biplot_color)) %>%
      na.omit()
    
    plot_data <- cbind(data_used, scores)
    
    x_pc <- input$biplot_x
    y_pc <- input$biplot_y
    
    # Scaling factor for loadings to match plot scale (simple scaling)
    # We want arrows to be visible but not overwhelm the plot
    scale_factor <- min(max(abs(scores[[x_pc]])), max(abs(scores[[y_pc]]))) * 2
    
    loadings$x_scaled <- loadings[[x_pc]] * scale_factor
    loadings$y_scaled <- loadings[[y_pc]] * scale_factor
    loadings$Question <- rownames(loadings)
    
    # Get variance explained for labels
    var_explained <- summary(pca)$importance[2,]
    x_var <- round(var_explained[x_pc] * 100, 1)
    y_var <- round(var_explained[y_pc] * 100, 1)
    
    p <- ggplot(plot_data, aes(x = .data[[x_pc]], y = .data[[y_pc]])) +
      geom_point(aes(color = .data[[input$biplot_color]], shape = .data[[input$biplot_color]]), size = 3, alpha = 0.7) +
      geom_segment(data = loadings, aes(x = 0, y = 0, xend = x_scaled, yend = y_scaled), 
                   arrow = arrow(length = unit(0.2, "cm")), color = "red", alpha = 0.5) +
      geom_text(data = loadings, aes(x = x_scaled, y = y_scaled, label = Question), 
                color = "red", size = 3, vjust = 1.5) +
      theme_minimal() +
      labs(title = paste("PCA Biplot:", x_pc, "vs", y_pc),
           x = paste0(x_pc, " (", x_var, "%)"),
           y = paste0(y_pc, " (", y_var, "%)"))
    
    ggplotly(p)
  })

  # HCA Logic
  hca_model <- reactive({
    # Use same data subset as PCA (Likert columns, no NAs)
    hca_data <- survey_data %>%
      select(all_of(likert_cols)) %>%
      na.omit()
    
    # Scale data
    hca_data_scaled <- scale(hca_data)
    
    # Distance matrix
    dist_mat <- dist(hca_data_scaled, method = "euclidean")
    
    # Hierarchical clustering
    hclust(dist_mat, method = input$hca_method)
  })
  
  output$plot_dendrogram <- renderPlot({
    hc <- hca_model()
    
    # Plot dendrogram
    plot(hc, main = "Cluster Dendrogram", xlab = "Students", sub = "", labels = FALSE, hang = -1)
    rect.hclust(hc, k = input$hca_k, border = "red")
  })
  
  output$plot_hca_pca <- renderPlotly({
    hc <- hca_model()
    k <- input$hca_k
    clusters <- cutree(hc, k = k)
    
    # Get PCA scores to visualize clusters
    pca <- pca_results()
    scores <- as.data.frame(pca$x)
    scores$Cluster <- factor(clusters)
    
    p <- ggplot(scores, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(size = 3, alpha = 0.7) +
      theme_minimal() +
      labs(title = "Clusters on PCA Map (PC1 vs PC2)",
           x = "PC1", y = "PC2")
    
    ggplotly(p)
  })
  
  output$table_hca_drivers <- renderTable({
    hc <- hca_model()
    k <- input$hca_k
    clusters <- cutree(hc, k = k)
    
    # Get data
    data_used <- survey_data %>%
      select(all_of(likert_cols)) %>%
      na.omit()
    
    data_used$Cluster <- factor(clusters)
    
    # Calculate means per cluster
    cluster_means <- data_used %>%
      group_by(Cluster) %>%
      summarise(across(everything(), mean)) %>%
      pivot_longer(-Cluster, names_to = "Question", values_to = "Mean") %>%
      pivot_wider(names_from = Cluster, values_from = Mean, names_prefix = "Cluster ")
    
    # Calculate variance of means to find drivers
    drivers <- cluster_means %>%
      rowwise() %>%
      mutate(
        Variance = var(c_across(starts_with("Cluster")))
      ) %>%
      arrange(desc(Variance)) %>%
      head(5) %>%
      select(-Variance)
    
    # Add full question text
    # questions_map is a named vector: names=NewName, values=FullText
    drivers$FullQuestion <- questions_map[drivers$Question]
    
    # Reorder columns
    drivers %>% select(Question, FullQuestion, everything())
  })
  
  # Question Text UI
  output$question_text_ui <- renderUI({
    req(input$sat_question)
    col_name <- input$sat_question
    full_q <- questions_map[which(new_names == col_name)]
    
    h4(paste("Q:", full_q), style = "color: #2c3e50; font-weight: bold; margin-bottom: 15px;")
  })

  output$plot_likert_dist <- renderPlotly({
    req(input$sat_question)
    col_name <- input$sat_question
    
    # Get full question text
    full_q <- questions_map[which(new_names == col_name)]
    
    # Ensure all levels 1-5 are present
    plot_data <- survey_data %>%
      filter(!is.na(.data[[col_name]])) %>%
      mutate(Score = factor(.data[[col_name]], levels = 1:5))
    
    p <- ggplot(plot_data, aes(x = Score, fill = Score)) +
      geom_bar() +
      scale_fill_brewer(palette = "Blues", drop = FALSE) +
      scale_x_discrete(drop = FALSE) +
      theme_minimal() +
      labs(x = "Score (1 = Strongly Disagree, 5 = Strongly Agree)", y = "Count", title = "") # Title moved to UI
    
    ggplotly(p)
  })
  
  output$table_likert_stats <- renderTable({
    req(input$sat_question)
    col_name <- input$sat_question
    
    survey_data %>%
      summarise(
        Mean = mean(.data[[col_name]], na.rm = TRUE),
        Median = median(.data[[col_name]], na.rm = TRUE),
        SD = sd(.data[[col_name]], na.rm = TRUE),
        N = sum(!is.na(.data[[col_name]]))
      )
  })
  
  # output$plot_corr removed as it is replaced by PCA Loadings

  
  # SNAGS
  output$plot_snags_involved <- renderPlotly({
    p <- ggplot(survey_data, aes(x = SNAGSInvolvement, fill = SNAGSInvolvement)) + geom_bar() + theme_minimal()
    ggplotly(p)
  })
  
  output$plot_snags_perception <- renderPlotly({
    # Gather SNAGS likert data
    snags_long <- survey_data %>%
      select(starts_with("SNAGS_")) %>%
      pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
      filter(!is.na(Response))
    
    p <- ggplot(snags_long, aes(x = Question, fill = Response)) +
      geom_bar(position = "fill") +
      coord_flip() +
      theme_minimal() +
      labs(y = "Proportion", title = "SNAGS Perceptions") +
      scale_fill_brewer(palette = "RdBu")
    
    ggplotly(p)
  })
  
  # Curriculum
  output$plot_income <- renderPlotly({
    p <- ggplot(survey_data, aes(x = ExpectedIncomeNumeric)) +
      geom_histogram(fill = "forestgreen", bins = 10) +
      theme_minimal() +
      scale_x_continuous(labels = dollar_format()) +
      labs(x = "Expected Income ($)", y = "Count")
    ggplotly(p)
  })
  
  output$plot_abroad <- renderPlotly({
    p <- ggplot(survey_data, aes(x = StudyAbroadConsider, fill = StudyAbroadConsider)) + geom_bar() + theme_minimal()
    ggplotly(p)
  })
  
  output$map_abroad <- renderLeaflet({
    # Extract countries
    countries_list <- survey_data %>%
      select(StudyAbroadCountries) %>%
      separate_rows(StudyAbroadCountries, sep = "\n") %>%
      mutate(Country = trimws(StudyAbroadCountries)) %>%
      filter(!is.na(Country) & Country != "") %>%
      # Fix typos
      mutate(Country = case_when(
        Country == "New Zeland" ~ "New Zealand",
        TRUE ~ Country
      )) %>%
      count(Country)
    
    # Manual coordinates for observed countries
    coords <- data.frame(
      Country = c("Netherlands", "USA", "New Zealand", "Canada", "France", 
                  "Ireland", "South America", "Scandinavia", "Mexico", "Israel", "Denmark"),
      Lat = c(52.13, 37.09, -40.90, 56.13, 46.22, 53.14, -8.78, 60.47, 23.63, 31.04, 56.26),
      Lon = c(5.29, -95.71, 174.88, -106.34, 2.21, -7.69, -55.49, 8.46, -102.55, 34.85, 9.50)
    )
    
    map_data <- left_join(countries_list, coords, by = "Country") %>%
      filter(!is.na(Lat)) # Filter out any we missed
    
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Lon, lat = ~Lat,
        radius = ~sqrt(n) * 5, # Scale size by count
        popup = ~paste(Country, ": ", n, " students"),
        color = "blue", fillOpacity = 0.7
      )
  })
  
  output$plot_units_needed <- renderPlotly({
    # Split multiple choice responses
    units <- survey_data %>%
      select(UnitsNeeded) %>%
      separate_rows(UnitsNeeded, sep = "\n") %>%
      filter(!is.na(UnitsNeeded) & UnitsNeeded != "") %>%
      count(UnitsNeeded) %>%
      arrange(desc(n))
    
    p <- ggplot(units, aes(x = reorder(UnitsNeeded, n), y = n)) +
      geom_col(fill = "purple") +
      coord_flip() +
      theme_minimal() +
      labs(x = "", y = "Count", title = "Areas where more units are needed")
    
    ggplotly(p)
  })
  
  # Data Table
  output$raw_table <- renderDT({
    # Exclude ID columns for anonymity
    display_data <- raw_data %>%
      select(-any_of(c("StudentID", "SubmissionID", "SubmissionDate", "Student ID", "Submission ID")))
    
    datatable(display_data, options = list(scrollX = TRUE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
