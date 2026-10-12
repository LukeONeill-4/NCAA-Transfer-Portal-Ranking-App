library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(bslib)

offense <- read.csv("individual_offensive_PAR_2025.csv")
goal <- read.csv("individual_goalkeepers_cond_2025.csv")

# -------------------------
# UI
# -------------------------

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "litera",
    primary = "#E89EB8"
  ),
  
  titlePanel("Women's Soccer Transfer Portal Analysis"),

  tabsetPanel(
    
    # =====================================================
    # TAB 1: OFFENSIVE RANKINGS
    # =====================================================
    
    tabPanel("Offensive Rankings",
             sidebarLayout(
               sidebarPanel(
                 selectInput("position_rank", "Position:",
                             choices = c("All", unique(offense$position))),
                 sliderInput("min_minutes_rank", "Minimum Minutes:",
                             min = 0,
                             max = max(offense$MIN, na.rm = TRUE),
                             value = 200),
                 hr(),
                 h4("Player Value Weights"),
                 sliderInput("w_goals", "Goals", 0, 10, 5, 0.5),
                 sliderInput("w_assists", "Assists", 0, 10, 5, 0.5),
                 sliderInput("w_points", "Points", 0, 10, 5, 0.5),
                 sliderInput("w_sog", "Shot Accuracy", 0, 10, 5, 0.5),
                 sliderInput("w_minutes", "Minutes", 0, 10, 5, 0.5)
               ),
               mainPanel(
                 plotOutput("ranking_plot"),
                 br(),
                 DTOutput("ranking_table")
               )
             )
    ),
    
    # =====================================================
    # TAB 2: GOALKEEPER RANKINGS
    # =====================================================
    
    tabPanel("Goalkeeper Rankings",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("min_minutes_rank_goal", "Minimum Minutes:",
                             min = 0,
                             max = max(goal$MIN, na.rm = TRUE),
                             value = 200),
                 hr(),
                 h4("Goalkeeper Value Weights"),
                 sliderInput("w_ga", "Goals Allowed", 0, 10, 5, 0.5),
                 sliderInput("w_saves", "Saves", 0, 10, 5, 0.5),
                 sliderInput("w_minutes", "Minutes", 0, 10, 5, 0.5),
                 sliderInput("w_SV.Percetage", "Save Percentage", 0,10,5,.5)
               ),
               mainPanel(
                 plotOutput("ranking_plot_goal"),
                 br(),
                 DTOutput("ranking_table_goal")
               )
             )
    ),
    
    # =====================================================
    # TAB 3: PERFORMANCE VIEW
    # =====================================================
    
    tabPanel("Performance View",
             sidebarLayout(
               sidebarPanel(
                 selectInput("position_perf", "Position:",
                             choices = c("All", unique(offense$position))),
                 sliderInput("min_minutes_perf", "Minimum Minutes:",
                             min = 0,
                             max = max(offense$MIN, na.rm = TRUE),
                             value = 200),
                 hr(),
                 h4("Select Metrics"),
                 selectInput("x_metric", "X-Axis Metric:",
                             choices = c(
                               "Goals per 90" = "G_per90",
                               "Assists per 90" = "A_per90",
                               "Points per 90" = "PTS_per90",
                               "Shots" = "SH",
                               "Shot Accuracy" = "SOG.Percentage",
                               "Minutes" = "MIN"
                             ),
                             selected = "G_per90"),
                 selectInput("y_metric", "Y-Axis Metric:",
                             choices = c(
                               "Goals per 90" = "G_per90",
                               "Assists per 90" = "A_per90",
                               "Points per 90" = "PTS_per90",
                               "Shots" = "SH",
                               "Shot Accuracy" = "SOG.Percentage",
                               "Minutes" = "MIN"
                             ),
                             selected = "A_per90"),
                 hr(),
                 h4("Performance Weights"),
                 sliderInput("w_goals_p", "Goals", 0, 10, 5, 0.5),
                 sliderInput("w_assists_p", "Assists", 0, 10, 5, 0.5),
                 sliderInput("w_points_p", "Points", 0, 10, 5, 0.5),
                 sliderInput("w_sog_p", "Shot Accuracy", 0, 10, 5, 0.5),
                 sliderInput("w_minutes_p", "Minutes", 0, 10, 5, 0.5)
               ),
               mainPanel(
                 plotlyOutput("scatter_plot")
               )
             )
    ),

    # =====================================================
    # TAB 4: LEADERBOARDS
    # =====================================================
    
    tabPanel("Leaderboards",
             sidebarLayout(
               sidebarPanel(
                 selectInput("leaderboard_type", "Choose Leaderboard:",
                             choices = c(
                               "Top Goal Scorers" = "scorers",
                               "Best Playmakers" = "playmakers",
                               "Best Shooters" = "shooters",
                               "Clutch Players" = "clutch",
                               "Team Value (Avg Points)" = "team_value"
                             ))
               ),
               mainPanel(
                 DTOutput("leaderboard_table")
               )
             )
    )
  )
)

# -------------------------
# SERVER
# -------------------------

server <- function(input, output) {
  safe_norm <- function(x) {
    (x - min(x, na.rm = TRUE)) /
      (max(x, na.rm = TRUE) - min(x, na.rm = TRUE) + 1e-6)
  }
  
  # =====================================================
  # OFFENSIVE RANKINGS
  # =====================================================
  
  scored_data <- reactive({
    df <- offense %>%
      mutate(
        G_per90 = (G / MIN) * 90,
        A_per90 = (A / MIN) * 90,
        PTS_per90 = (PTS / MIN) * 90
      ) %>%
      filter(MIN > input$min_minutes_rank)
    if (input$position_rank != "All") {
      df <- df %>% filter(position == input$position_rank)
    }
    
    total_w <- max(input$w_goals + input$w_assists + input$w_points + input$w_sog + input$w_minutes, 1e-6)
    
    df %>%
      mutate(
        player_score =
          (input$w_goals / total_w) * safe_norm(G_per90) +
          (input$w_assists / total_w) * safe_norm(A_per90) +
          (input$w_points / total_w) * safe_norm(PTS_per90) +
          (input$w_sog / total_w) * safe_norm(SOG.Percentage) +
          (input$w_minutes / total_w) * safe_norm(MIN)
      ) %>%
      arrange(desc(player_score))
  })
  
  # =====================================================
  # GOALKEEPER RANKINGS
  # =====================================================
  
  goalie_data <- reactive({
    df <- goal %>%
      mutate(
        GA_per90 = (GA / MIN) * 90,
        SV_per90 = (SV / MIN) * 90,
        GA_inv   = max(GA_per90, na.rm = TRUE) - GA_per90
      ) %>%
      filter(MIN > input$min_minutes_rank_goal)
    
    total_w <- max(input$w_ga + input$w_saves + input$w_minutes, 1e-6)
    
    df %>%
      mutate(
        player_score =
          (input$w_ga / total_w) * safe_norm(GA_inv) +
          (input$w_saves / total_w) * safe_norm(SV_per90) +
          (input$w_minutes / total_w) * safe_norm(MIN)
      ) %>%
      arrange(desc(player_score))
  })
  
  # =====================================================
  # PERFORMANCE DATA
  # =====================================================
  
  performance_data <- reactive({
    df <- offense %>%
      mutate(
        G_per90 = (G / MIN) * 90,
        A_per90 = (A / MIN) * 90,
        PTS_per90 = (PTS / MIN) * 90
      ) %>%
      filter(!is.na(MIN), MIN > input$min_minutes_perf)
    if (input$position_perf != "All") {
      df <- df %>% filter(position == input$position_perf)
    }
    
    total_w <- input$w_goals_p + input$w_assists_p + input$w_points_p +
      input$w_sog_p + input$w_minutes_p
    
    df %>%
      mutate(
        player_score =
          (input$w_goals_p / total_w) * safe_norm(G_per90) +
          (input$w_assists_p / total_w) * safe_norm(A_per90) +
          (input$w_points_p / total_w) * safe_norm(PTS_per90) +
          (input$w_sog_p / total_w) * safe_norm(SOG.Percentage) +
          (input$w_minutes_p / total_w) * safe_norm(MIN)
      )
  })
  
  # =====================================================
  # OFFENSIVE RANKING PLOT
  # =====================================================
  
  output$ranking_plot <- renderPlot({
    top10 <- scored_data() %>% slice_head(n = 10)
  
    ggplot(top10, aes(x = reorder(Player, player_score), y = player_score)) +
      geom_col(fill = "#E89EB8") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top Players by Custom Score",
           x = "Player",
           y = "Player Score")
  })
  
  # =====================================================
  # GOALKEEPER RANKING PLOT
  # =====================================================
  
  output$ranking_plot_goal <- renderPlot({
    top10_goal <- goalie_data() %>% slice_head(n = 10)
    
    ggplot(top10_goal, aes(x = reorder(Player, player_score), y = player_score)) +
      geom_col(fill = "#E89EB8") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top Goalkeepers by Custom Score",
           x = "Goalkeeper",
           y = "Goalkeeper Score")
  })
  
  # =====================================================
  # OFFENSIVE TABLE
  # =====================================================
  
  output$ranking_table <- renderDT({
    datatable(
      scored_data() %>%
        select(Player, position, college_out,
               GP, GS, MIN,
               G, A, PTS,
               SH, SOG.Percentage,
               G_per90, A_per90, PTS_per90,
               player_score) %>%
        rename(Position = position, `Outgoing College` = college_out, `SOG Percentage` = SOG.Percentage,
               `G / 90 Min` = G_per90, `A / 90 Min` = A_per90, `PTS / 90 MIN` = PTS_per90, 
               `Player Score` = player_score),
      options = list(pageLength = 10, scrollX = TRUE),
      filter = "top",
      rownames = FALSE
    )
  })
  
  # =====================================================
  # GOALKEEPER TABLE
  # =====================================================
  
  output$ranking_table_goal <- renderDT({
    datatable(
      goalie_data() %>%
        select(Player, Position, college_out,
               GP, GS, MIN,
               GA, GAA, SV,
               SV.Percentage, W, L, T,
               SHO, SF,
               player_score) %>%
        rename(`Outgoing College` = college_out, `SV Percentage` = SV.Percentage,
               `Player Score` = player_score),
      options = list(pageLength = 10, scrollX = TRUE),
      filter = "top",
      rownames = FALSE
    )
  })
  
  # =====================================================
  # SCATTER PLOT
  # =====================================================
  
  output$scatter_plot <- renderPlotly({
    df <- performance_data()

    metric_labels <- c(
      G_per90 = "Goals per 90",
      A_per90 = "Assists per 90",
      PTS_per90 = "Points per 90",
      SH = "Shots",
      SOG.Percentage = "Shot Accuracy",
      MIN = "Minutes"
    )
    
    x_var <- input$x_metric
    y_var <- input$y_metric
    
    x_label <- metric_labels[[x_var]]
    y_label <- metric_labels[[y_var]]
    
    corr <- cor(df[[x_var]], df[[y_var]], use = "complete.obs")
    
    p <- ggplot(df, aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      text = paste0(
        "<b>", Player, "</b><br>",
        "Team: ", college_in, "<br>",
        x_label, ": ", round(.data[[x_var]], 2), "<br>",
        y_label, ": ", round(.data[[y_var]], 2), "<br>",
        "Score: ", round(player_score, 3)
      )
    )) +
      geom_point(aes(color = player_score), size = 3, alpha = 0.7) +
      scale_color_viridis_c(name = "Performance Score") +
      theme_minimal() +
      labs(
        title = paste0("Custom Metric Comparison | Correlation: ", round(corr, 3)),
        x = x_label,
        y = y_label
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # =====================================================
  # LEADERBOARDS
  # =====================================================
  
  output$leaderboard_table <- renderDT({
    df <- offense %>% filter(!is.na(MIN))
    
    table <- switch(input$leaderboard_type,
                    "scorers" = df %>% arrange(desc(G)) %>% select(Player, position, G, PTS, MIN),
                    "playmakers" = df %>% arrange(desc(A)) %>% select(Player, position, A, PTS, MIN),
                    "shooters" = df %>% arrange(desc(SH.Percentage)) %>% select(Player, position, SH, SH.Percentage, SOG.Percentage, MIN),
                    "clutch" = df %>% arrange(desc(GW)) %>% select(Player, position, GW, G, PTS, MIN),
                    "team_value" = df %>% group_by(college_in) %>%
                      summarise(avg_points = mean(PTS, na.rm = TRUE),
                                avg_goals = mean(G, na.rm = TRUE),
                                players = n()) %>%
                      arrange(desc(avg_points))
    )
    
    datatable(table,
              options = list(pageLength = 10),
              filter = "top",
              rownames = FALSE)
  })
}

# -------------------------
# RUN APP
# -------------------------

shinyApp(ui, server)