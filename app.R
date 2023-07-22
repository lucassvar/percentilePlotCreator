# Libraries and Data -------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(magick)
library(worldfootballR)
library(stringr)
library(googledrive)
library(showtext)
library(roxygen2)
library(png)
showtext_auto()
load(file = "rda/URLs_data.rda")
stats_percentiles <- c("Non-Penalty Goals", "Non-Penalty xG", "Shots on target", "Goals/Shot",
                       "xAG", "Passes Completed", "Pass Completion % (Short)", "Pass Completion % (Medium)",
                       "Pass Completion % (Long)", "Shot-Creating Actions", "Tackles", "Blocks", "Interceptions",
                       "Successful Take-Ons", "Progressive Carries", "Progressive Passes Rec", "Progressive Passes",
                       "Aerials Won", "Yellow Cards", "Fouls Committed", "Fouls Drawn")
stats_GK_percentiles <- c("PSxG-GA", "Save Percentage", "Clean Sheet Percentage", "Save% (Penalty Kicks)",
                          "Pass Completion Percentage", "Launch %", "Crosses Stopped %", "Def. Actions Outside Pen. Area",
                          "Avg. Distance of Def. Actions", "PSxG/SoT", "Free Kick Goals Against", "Crosses Faced", "Goals Against",
                          "Avg. Length of Goal Kicks", "Goal Kicks", "Launch% (Goal Kicks)", "Penalty Kicks Attempted")

# Plot Generator Functions ------------------------------------------------------
plot_generator <- function(percentiles_data = NULL){
  # Different stats selection for field players vs goalkeepers
  if (tail(names(sort(table(percentiles_data$Versus))), 1)  !=  "Goalkeepers") {
    #Filter to show the report data of the statistics we want, and remove duplicated stats
    player_percentiles_data <- percentiles_data %>%
      filter(Statistic %in% stats_percentiles) %>%
      drop_na()
    player_percentiles_data <- player_percentiles_data[!duplicated(player_percentiles_data$Statistic), ]
    
    # CHange the stats' names to make the plot more readable
    for (row in seq(1:nrow(player_percentiles_data))) {
      if (player_percentiles_data$Statistic[row] == "Non-Penalty Goals") {
        player_percentiles_data$Statistic[row] <- "np:Gls"
        player_percentiles_data$StatGroup[row] <- "Shooting"
      } else if (player_percentiles_data$Statistic[row] == "Non-Penalty xG") {
        player_percentiles_data$Statistic[row] <- "np:xG"
        player_percentiles_data$StatGroup[row] <- "Shooting"
      } else if (player_percentiles_data$Statistic[row] == "Shots on target") {
        player_percentiles_data$Statistic[row] <- "SoT"
        player_percentiles_data$StatGroup[row] <- "Shooting"
      } else if (player_percentiles_data$Statistic[row] == "Goals/Shot") {
        player_percentiles_data$Statistic[row] <- "Gls/Sh"
        player_percentiles_data$StatGroup[row] <- "Shooting"
      } else if (player_percentiles_data$Statistic[row] == "xAG") {
        player_percentiles_data$Statistic[row] <- "xAG"
        player_percentiles_data$StatGroup[row] <- "Passing"
      } else if (player_percentiles_data$Statistic[row] == "Passes Completed") {
        player_percentiles_data$Statistic[row] <- "Cmp"
        player_percentiles_data$StatGroup[row] <- "Passing"
      } else if (player_percentiles_data$Statistic[row] == "Pass Completion % (Short)") {
        player_percentiles_data$Statistic[row] <- "Short %"
        player_percentiles_data$StatGroup[row] <- "Passing"
      } else if (player_percentiles_data$Statistic[row] == "Pass Completion % (Medium)") {
        player_percentiles_data$Statistic[row] <- "Med. %"
        player_percentiles_data$StatGroup[row] <- "Passing"
      } else if (player_percentiles_data$Statistic[row] == "Pass Completion % (Long)") {
        player_percentiles_data$Statistic[row] <- "Long %"
        player_percentiles_data$StatGroup[row] <- "Passing"
      } else if (player_percentiles_data$Statistic[row] == "Shot-Creating Actions") {
        player_percentiles_data$Statistic[row] <- "SCA"
        player_percentiles_data$StatGroup[row] <- "Possession"
      } else if (player_percentiles_data$Statistic[row] == "Tackles") {
        player_percentiles_data$Statistic[row] <- "Tkl"
        player_percentiles_data$StatGroup[row] <- "Defense"
      } else if (player_percentiles_data$Statistic[row] == "Blocks") {
        player_percentiles_data$Statistic[row] <- "Blocks"
        player_percentiles_data$StatGroup[row] <- "Defense"
      } else if (player_percentiles_data$Statistic[row] == "Interceptions") {
        player_percentiles_data$Statistic[row] <- "Int"
        player_percentiles_data$StatGroup[row] <- "Defense"
      } else if (player_percentiles_data$Statistic[row] == "Successful Take-Ons") {
        player_percentiles_data$Statistic[row] <- "Drib Cmp"
        player_percentiles_data$StatGroup[row] <- "Possession"
      } else if (player_percentiles_data$Statistic[row] == "Progressive Carries") {
        player_percentiles_data$Statistic[row] <- "Prog Carries"
        player_percentiles_data$StatGroup[row] <- "Possession"
      } else if (player_percentiles_data$Statistic[row] == "Progressive Passes") {
        player_percentiles_data$Statistic[row] <- "Prog Passes"
        player_percentiles_data$StatGroup[row] <- "Passing"
      } else if (player_percentiles_data$Statistic[row] == "Progressive Passes Rec") {
        player_percentiles_data$Statistic[row] <- "Prog Rec"
        player_percentiles_data$StatGroup[row] <- "Possession"
      } else if (player_percentiles_data$Statistic[row] == "Aerials Won") {
        player_percentiles_data$Statistic[row] <- "Aerials Won"
        player_percentiles_data$StatGroup[row] <- "Defense"
      } else if (player_percentiles_data$Statistic[row] == "Yellow Cards") {
        player_percentiles_data$Statistic[row] <- "Yellows"
        player_percentiles_data$StatGroup[row] <- "Defense"
      } else if (player_percentiles_data$Statistic[row] == "Fouls Committed") {
        player_percentiles_data$Statistic[row] <- "Fouls"
        player_percentiles_data$StatGroup[row] <- "Defense"
      } else if (player_percentiles_data$Statistic[row] == "Fouls Drawn") {
        player_percentiles_data$Statistic[row] <- "Fouls Created"
        player_percentiles_data$StatGroup[row] <- "Possession"
      }
    }
    
    # Order by stat group
    player_percentiles_data <- player_percentiles_data[order(player_percentiles_data$StatGroup), ]
    
    # Add ID for each stat group
    player_percentiles_data <- player_percentiles_data %>% mutate(id = case_when(
      StatGroup == "Defense" ~ 1,
      StatGroup == "Passing" ~ 2,
      StatGroup == "Possession" ~ 3,
      StatGroup == "Shooting" ~ 4
    ),
    StatGroup = case_when(
      StatGroup == "Defense" ~ "Defensa",
      StatGroup == "Passing" ~ "Pases",
      StatGroup == "Possession" ~ "Posesión",
      StatGroup == "Shooting" ~ "Tiros"
    ))
    
    # Group colors for the plot
    group.colors <- c("Defensa" = "#C86742",
                      "Pases" = "#84B86F",
                      "Posesión" = "#E3AE2E",
                      "Tiros" = "#6F78B3")
    
    
  } else if (tail(names(sort(table(percentiles_data$Versus))), 1)  ==  "Goalkeepers") {
    # Filter to show the report data of the statistics we want, and remove duplicated stats
    player_percentiles_data <- percentiles_data %>%
      filter(Statistic %in% stats_GK_percentiles) %>%
      drop_na()
    player_percentiles_data <- player_percentiles_data[!duplicated(player_percentiles_data$Statistic), ]
    
    # Change statistics names (to make the plot more readable)
    for (row in seq(1:nrow(player_percentiles_data))) {
      if (player_percentiles_data$Statistic[row] == "PSxG-GA") {
        player_percentiles_data$Statistic[row] <- "PSxG-GA"
        player_percentiles_data$StatGroup[row] <- "Goalkeeping"
      } else if (player_percentiles_data$Statistic[row] == "Save Percentage") {
        player_percentiles_data$Statistic[row] <- "Save %"
        player_percentiles_data$StatGroup[row] <- "Goalkeeping"
      } else if (player_percentiles_data$Statistic[row] == "Clean Sheet Percentage") {
        player_percentiles_data$Statistic[row] <- "Clean Sheets %"
        player_percentiles_data$StatGroup[row] <- "Goalkeeping"
      } else if (player_percentiles_data$Statistic[row] == "Save% (Penalty Kicks)") {
        player_percentiles_data$Statistic[row] <- "Penalty %"
        player_percentiles_data$StatGroup[row] <- "Set Piece"
      } else if (player_percentiles_data$Statistic[row] == "Pass Completion Percentage") {
        player_percentiles_data$Statistic[row] <- "Cmp %"
        player_percentiles_data$StatGroup[row] <- "Passing"
      } else if (player_percentiles_data$Statistic[row] == "Launch %") {
        player_percentiles_data$Statistic[row] <- "Launch %"
        player_percentiles_data$StatGroup[row] <- "Passing"
      } else if (player_percentiles_data$Statistic[row] == "Crosses Stopped %") {
        player_percentiles_data$Statistic[row] <- "Crosses Stp %"
        player_percentiles_data$StatGroup[row] <- "Set Piece"
      } else if (player_percentiles_data$Statistic[row] == "Def. Actions Outside Pen. Area") {
        player_percentiles_data$Statistic[row] <- "DOA"
        player_percentiles_data$StatGroup[row] <- "Defense"
      } else if (player_percentiles_data$Statistic[row] == "Avg. Distance of Def. Actions") {
        player_percentiles_data$Statistic[row] <- "Dist DOA"
        player_percentiles_data$StatGroup[row] <- "Defense"
      } else if (player_percentiles_data$Statistic[row] == "PSxG/SoT") {
        player_percentiles_data$Statistic[row] <- "PSxG/SoT"
        player_percentiles_data$StatGroup[row] <- "Goalkeeping"
      } else if (player_percentiles_data$Statistic[row] == "Free Kick Goals Against") {
        player_percentiles_data$Statistic[row] <- "FK Goals"
        player_percentiles_data$StatGroup[row] <- "Set Piece"
      } else if (player_percentiles_data$Statistic[row] == "Crosses Faced") {
        player_percentiles_data$Statistic[row] <- "Crosses Faced"
        player_percentiles_data$StatGroup[row] <- "Set Piece"
      } else if (player_percentiles_data$Statistic[row] == "Goals Against") {
        player_percentiles_data$Statistic[row] <- "Gls Against"
        player_percentiles_data$StatGroup[row] <- "Goalkeeping"
      } else if (player_percentiles_data$Statistic[row] == "Avg. Length of Goal Kicks") {
        player_percentiles_data$Statistic[row] <- "Length Goal Kicks"
        player_percentiles_data$StatGroup[row] <- "Passing"
      } else if (player_percentiles_data$Statistic[row] == "Goal Kicks") {
        player_percentiles_data$Statistic[row] <- "Goal Kicks"
        player_percentiles_data$StatGroup[row] <- "Passing"
      } else if (player_percentiles_data$Statistic[row] == "Launch% (Goal Kicks)") {
        player_percentiles_data$Statistic[row] <- "Goal Kicks %"
        player_percentiles_data$StatGroup[row] <- "Passing"
      } else if (player_percentiles_data$Statistic[row] == "Penalty Kicks Attempted") {
        player_percentiles_data$Statistic[row] <- "PK Against"
        player_percentiles_data$StatGroup[row] <- "Set Piece"
      } 
    }
    
    # Order by stat group
    player_percentiles_data <- player_percentiles_data[order(player_percentiles_data$StatGroup), ]
    
    # Add id for each stat group and current season
    player_percentiles_data <- player_percentiles_data %>% mutate(id = case_when(
      StatGroup == "Defense" ~ 1,
      StatGroup == "Passing" ~ 2,
      StatGroup == "Goalkeeping" ~ 3,
      StatGroup == "Set Piece" ~ 4
    ),
    StatGroup = case_when(
      StatGroup == "Defense" ~ "Defensa",
      StatGroup == "Passing" ~ "Pases",
      StatGroup == "Goalkeeping" ~ "Portero",
      StatGroup == "Set Piece" ~ "Balón Parado"
    ))
    
    # Group colors for the plot
    group.colors <- c("Defensa" = "#C86742",
                      "Pases" = "#84B86F",
                      "Portero" = "#662E9B",
                      "Balón Parado" = "#FFC1CF")
  }
  
  
  # Get player's name and the position compared
  playername <- player_percentiles_data$Player[1]
  playerversus <- tail(names(sort(table(percentiles_data$Versus))), 1)
  
  
  # Change the positions from English to Spanish
  if (playerversus == "Forwards") {
    playerversus <- "Delanteros"
  } else if (playerversus == "Att Mid / Wingers") {
    playerversus <- "Med. Ofensivos / Extremos"
  } else if (playerversus == "Midfielders") {
    playerversus <- "Mediocampistas"
  } else if (playerversus == "Center Backs") {
    playerversus <- "Defensas Centrales"
  } else if (playerversus == "Fullbacks") {
    playerversus <- "Laterales"
  } else if (playerversus == "Goalkeepers") {
    playerversus <- "Porteros"
  }
  
  
  # Create the plot
  plt <- ggplot(player_percentiles_data) +
    # Make custom panel grid
    geom_hline(
      aes(yintercept = y),
      data.frame(y = c(25, 50, 75, 100)),
      color = "lightgrey", linetype = "dotted"
    ) +
    
    # Add bars to represent the percentiles
    geom_col(
      aes(x = reorder(str_wrap(Statistic, 5), id), y = Percentile, fill = StatGroup
      ),
      position = "dodge2", show.legend = TRUE, alpha = 1
    ) +
    
    # Lollipop shaf for guidance
    geom_segment(
      aes(x = reorder(str_wrap(Statistic, 5), id), y = 0,
          xend = reorder(str_wrap(Statistic, 5), id), yend = 100),
      color = "black",
      linetype = "dotted"
    ) +
    
    # Make it circular
    coord_polar() +
    
    # Scale y axis so bars don't start at the center
    scale_y_continuous(
      limits = c(-50, 100)
    ) +
    
    # Theme configuration
    theme(
      # Remove axis ticks and text
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      # Use gray text for the labels
      axis.text.x = element_text(color = "gray12", size = 35, lineheight = 1.5,
                                 hjust = 20),
      # Move the legend to the bottom and make it horizontal
      legend.direction = "horizontal",
      legend.position = "bottom"
    ) +
    
    # Add labels
    labs(
      title = (paste("\n", playername)),
      subtitle = str_wrap(paste("\n", "Estadísticas comparadas con: ", playerversus, sep = ""), 70),
      caption = "\n @LaMediaInglesa\n @blauds - https://github.com/blauerds\nSource: Opta via Fbref"
    ) +
    
    # Add labels with the percentile values
    geom_label(aes(x = reorder(str_wrap(Statistic, 5), id), y = Percentile, color = StatGroup),
               label = player_percentiles_data$Percentile, size = 10,
               position = position_stack(vjust = 0.5),
               show.legend = FALSE) +
    
    # Customize general theme
    theme(
      # Style the text in the title, subtitle, and caption
      text = element_text(color = "grey12", family = "barlow", size = 35),
      plot.title = element_text(face = "bold", size = 50, hjust = 0.5, lineheight = 0.3),
      plot.subtitle = element_text(size = 35, hjust = 0.5, lineheight = 0.4),
      plot.caption = element_text(size = 29, hjust = 0.5, lineheight = 1.5),
      
      # Make the background white and remove extra grid lines
      panel.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major.x = element_blank(),
      
      legend.box.just = "center",
      
      plot.margin = margin(20, 20, 20, 20, "pt")
    ) +
    
    # Spcecify group colors manually
    scale_fill_manual(values = group.colors, name = NULL) +
    scale_colour_manual(values = group.colors, name = NULL)
  
  return(plt)
}

# Define UI --------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Percentile Plot Creator"),
  sidebarLayout(
    sidebarPanel(
      # Select League
      selectInput(inputId = "league_selected",
                  label = "League",
                  choices = c("All", unique(URLs_data$League)),
                  selected = "All"),
      # Select Team
      selectInput(inputId = "team_selected",
                  label = "Team",
                  choices = c("All", unique(URLs_data$Team)),
                  selected = "All"),
      # Select Player
      selectInput(inputId = "player_selected",
                  label = "Player",
                  choices = c("-", unique(URLs_data$Player)),
                  selected = "-")
    ),
    mainPanel(
      h3("Percentile Plot (LMI):"),
      plotOutput("percentilePlot", height = "2100px", width = "2100px")
    )
  )
)

# Define SERVER ----------------------------------------------------------------
server <- function(input, output, session){
  # Update Team choices based on League selection
  observeEvent(input$league_selected, {
    if (input$league_selected != "All") {
      teams_filtered <- URLs_data %>%
        filter(League == input$league_selected) %>%
        pull(Team) %>%
        unique()
    } else {
      teams_filtered <- unique(URLs_data$Team)
    }
    updateSelectInput(session, "team_selected", choices = c("All", teams_filtered), selected = "All")
  })
  
  # Update Player choices based on League selection
  observeEvent(input$team_selected, {
    if (input$team_selected != "All") {
      players_filtered <- URLs_data %>%
        filter(Team == input$team_selected) %>%
        pull(Player) %>%
        unique()
    } else {
      players_filtered <- unique(URLs_data$Player)
    }
    updateSelectInput(session, "player_selected", choices = c("-", players_filtered), selected = "All")
  })
  
  
  # Filter the data based on the selections
  filtered_data <- reactive({
    dataset <- URLs_data
    if (input$league_selected != "All") {
      dataset <- URLs_data %>% filter(League == input$league_selected)
    }
    if (input$team_selected != "All") {
      dataset <- URLs_data %>% filter(Team == input$team_selected)
    }
    if (input$player_selected != "All") {
      dataset <- URLs_data %>% filter(Player == input$player_selected)
    }
    dataset
  })
  
  
  # Extract the percentiles if a player was selected
  percentiles <- reactive({
    filtered_df <- filtered_data()
    if (input$player_selected != "-") {
      dataset <- fb_player_scouting_report(player_url = filtered_df$URL[1], pos_versus = "primary")
    } else{
      dataset <- c()
    }
    dataset
  })
  
  # Display Output
  output$percentilePlot <- renderPlot({
    if (input$player_selected != "-") {
      plydata <- percentiles()
      percPlot <- plot_generator(plydata)
      percPlot
    }
  })
}



shinyApp(ui, server)


