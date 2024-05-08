


library(shiny)
library(shinyjs)
library(DT)
library(rmarkdown)

# Create a data frame with the desired columns
df <- data.frame(
    half = integer(),
    team_attack = character(),
    zone = integer(),
    action = character(),
    outcome = character(),
    shot_pressure = character(),
    timestamp = character(),
    stringsAsFactors = FALSE
)

# setting directory for video folder
addResourcePath(prefix = 'vids', directoryPath = '~/Documents/XT_GAA')

# actions
action_names <- c("Start", "Stop", "Kickout", "Free", "Sideline", "Free 45", 
                  "Kick Pass", "Hand Pass", "Carry", "TO", "Shot Point","Shot Goal",
                  "Penalty","Mark", "Move Ball")
names(action_names) <- 1:15

# outcomes
outcome_names <- c("Point", "Goal", "Wide", "TO","Retain")
names(outcome_names) <- 1:5

# shot pressure
shot_pressure_names <- c("Low", "Medium", "High")
names(shot_pressure_names) <- 1:3

# validation list for each variable
allowed_values <- list(
    half = 1:2,
    team_attack = c("A", "B","No Team In Possession"),  # Replace with your actual team names
    zone = 1:16,
    action = c("Start", "Stop", "Kick Out", "Free", "Sideline", "Free 45", "Kick Pass", 
               "Hand Pass", "Carry", "TO", "Shot Point","Shot Goal", "Penalty","Mark", "Move Ball"),
    outcome = c("Point", "Goal", "Wide", "TO","Retain", NA),  # Allow NA as a valid value for outcome
    shot_pressure = c("Low", "Medium", "High", NA)
)

ui <- fluidPage(
    useShinyjs(), # Enable shinyjs functions
    
    tags$head(tags$script(HTML("
    function highlightButton(btnId, groupClass) {
      // Remove the 'btn-primary' class from all buttons with the same group class
      var buttons = document.getElementsByClassName(groupClass);
      for (var i = 0; i < buttons.length; i++) {
        buttons[i].classList.remove('btn-primary');
      }
  
      // Add the 'btn-primary' class to the clicked button
      document.getElementById(btnId).classList.add('btn-primary');
    }
    
    function toggleButton(btnId, groupClass) {
      var button = document.getElementById(btnId);
      
      if (button.classList.contains('btn-primary')) {
        button.classList.remove('btn-primary');
      } else {
        highlightButton(btnId, groupClass);
      }
    }
    
  document.addEventListener('keydown', function(event) {
    var video = document.getElementById('myVideo');
    
    if (event.key === 'Enter') {
      document.getElementById('submit').click();
    } else if (event.keyCode === 37) { // Left arrow key
      document.getElementById('back_5').click();
    } else if (event.keyCode === 39) { // Right arrow key
      document.getElementById('skip_5').click();
    } else if (event.keyCode === 38) { // Up arrow key
      document.getElementById('skip_10').click();
    } else if (event.keyCode === 40) { // Down arrow key
      document.getElementById('back_10').click();
    } else if (event.keyCode === 80) { // 'P' key
      document.getElementById('play_pause').click();
    }
  });
    
  "))),
    
    titlePanel("Data Collection Tool"),
    
    # Add input fields
    sidebarLayout(
        sidebarPanel(
            div(
                fluidRow(
                    column(6, radioButtons("team_attack", 
                                           "Team Attack:", 
                                           choices = c("Team A", "Team B", "No Team"),
                    )
                    ),
                    column(6, radioButtons("half", 
                                           "Half:", 
                                           choices = (1:2),
                    )
                    )
                ),
                # Zone header and buttons
                h5("Zone:", align = "center"),
                div(
                    id = "zone_14", 
                    actionButton("zone_14_btn", "14", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_14_btn','zone-buttons')", 
                                 style = "width: 50px;"),
                    id = "zone_15", 
                    actionButton("zone_15_btn", "15", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_15_btn','zone-buttons')", 
                                 style = "width: 50px;"),
                    id = "zone_16", 
                    actionButton("zone_16_btn", "16", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_16_btn','zone-buttons')", 
                                 style = "width: 50px;"),
                    style = "text-align: center;"
                ),
                
                div(
                    id = "zone_11", 
                    actionButton("zone_11_btn", "11", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_11_btn','zone-buttons')", 
                                 style = "width: 50px;"),
                    id = "zone_12", 
                    actionButton("zone_12_btn", "12", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_12_btn','zone-buttons')", 
                                 style = "width: 50px;"),
                    id = "zone_13", 
                    actionButton("zone_13_btn", "13", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_13_btn','zone-buttons')", 
                                 style = "width: 50px;"),
                    style = "text-align: center;"
                ),
                div(
                    id = "zone_8", 
                    actionButton("zone_8_btn", "8", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_8_btn','zone-buttons')", 
                                 style = "width: 50px;"),
                    id = "zone_9", 
                    actionButton("zone_9_btn", "9", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_9_btn','zone-buttons')", 
                                 style = "width: 50px;"),
                    id = "zone_10", 
                    actionButton("zone_10_btn", "10", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_10_btn','zone-buttons')", 
                                 style = "width: 50px;"),
                    style = "text-align: center;"
                ),
                div(
                    id = "zone_5", 
                    actionButton("zone_5_btn", "5", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_5_btn','zone-buttons')", 
                                 style = "width: 50px;"),
                    id = "zone_6", 
                    actionButton("zone_6_btn", "6", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_6_btn','zone-buttons')", 
                                 style = "width: 50px;"),
                    id = "zone_7", 
                    actionButton("zone_7_btn", "7", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_7_btn','zone-buttons')", 
                                 style = "width: 50px;"),
                    style = "text-align: center;"
                ),
                div(id = "zone_4", 
                    actionButton("zone_4_btn", "4", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_4_btn','zone-buttons')", 
                                 style = "width: 160px; height: 25px; padding: 2px"), style = "text-align: center;"),
                div(id = "zone_3", 
                    actionButton("zone_3_btn", "3", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_3_btn','zone-buttons')", 
                                 style = "width: 160px;"), 
                    style = "text-align: center;"),
                div(id = "zone_2", 
                    actionButton("zone_2_btn", "2", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_2_btn','zone-buttons')", 
                                 style = "width: 160px; height: 50px"), style = "text-align: center;"),
                div(id = "zone_1", 
                    actionButton("zone_1_btn", "1", 
                                 class = "zone-buttons", 
                                 onclick = "highlightButton('zone_1_btn','zone-buttons')", 
                                 style = "width: 160px;"), 
                    style = "text-align: center;"),
                
                h5("Action:", align = "center"),
                div(
                    id = "start", 
                    actionButton("start_btn", "Start", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('start_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    id = "stop", 
                    actionButton("stop_btn", "Stop", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('stop_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    id = "kickout", 
                    actionButton("kickout_btn", "Kickout", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('kickout_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    style = "text-align: center;"          
                ),
                
                div(
                    id = "free", 
                    actionButton("free_btn", "Free", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('free_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    id = "sideline", 
                    actionButton("sideline_btn", "Sideline", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('sideline_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    id = "free_45", 
                    actionButton("free_45_btn", "Free 45", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('free_45_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    style = "text-align: center;"          
                ),
                
                div(
                    id = "kick_pass", 
                    actionButton("kick_pass_btn", "Kick Pass", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('kick_pass_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    id = "hand_pass", 
                    actionButton("hand_pass_btn", "Hand Pass", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('hand_pass_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    id = "carry", 
                    actionButton("carry_btn", "Carry", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('carry_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    style = "text-align: center;"          
                ),
                
                div(
                    id = "shot_point", 
                    actionButton("shot_point_btn", "Shot Point", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('shot_point_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    id = "to", 
                    actionButton("to_btn", "TO", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('to_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    id = "penalty", 
                    actionButton("penalty_btn", "Penalty", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('penalty_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    style = "text-align: center;"   
                ),
                
                div(
                    id = "shot_goal", 
                    actionButton("shot_goal_btn", "Shot Goal", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('shot_goal_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    id = "mark", 
                    actionButton("mark_btn", "Mark", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('mark_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    id = "move_ball", 
                    actionButton("move_ball_btn", "Move Ball", 
                                 class = "action-buttons", 
                                 onclick = "highlightButton('move_ball_btn','action-buttons')", 
                                 style = "width: 90px;"),
                    style = "text-align: center;"          
                ),
                
                h5("Outcome:", align = "center"),
                div(
                    id = "point", 
                    actionButton("point_btn", "Point", 
                                 class = "outcome-buttons", 
                                 onclick = "highlightButton('point_btn','outcome-buttons')", 
                                 style = "width: 90px;"),
                    id = "goal", 
                    actionButton("goal_btn", "Goal", 
                                 class = "outcome-buttons", 
                                 onclick = "highlightButton('goal_btn','outcome-buttons')", 
                                 style = "width: 90px;"),
                    id = "wide", 
                    actionButton("wide_btn", "Wide", 
                                 class = "outcome-buttons", 
                                 onclick = "highlightButton('wide_btn','outcome-buttons')", 
                                 style = "width: 90px;"),
                    style = "text-align: center;"          
                ),
                
                div(
                    id = "to2", actionButton("to2_btn", "TO", 
                                             class = "outcome-buttons", 
                                             onclick = "highlightButton('to2_btn','outcome-buttons')", 
                                             style = "width: 90px;"),
                    id = "retain", actionButton("retain_btn", "Retain", 
                                                class = "outcome-buttons", 
                                                onclick = "highlightButton('retain_btn','outcome-buttons')", 
                                                style = "width: 90px;"),
                    style = "text-align: center;"          
                ),
                
                h5("Shot Pressure:", align = "center"),
                div(
                    id = "low", 
                    actionButton("low_btn", "Low", 
                                 class = "shotpressure-buttons", 
                                 onclick = "highlightButton('low_btn','shotpressure-buttons')", 
                                 style = "width: 90px;"),
                    id = "medium", 
                    actionButton("medium_btn", "Medium", 
                                 class = "shotpressure-buttons", 
                                 onclick = "highlightButton('medium_btn','shotpressure-buttons')", 
                                 style = "width: 90px;"),
                    id = "high", 
                    actionButton("high_btn", "High", 
                                 class = "shotpressure-buttons", 
                                 onclick = "highlightButton('high_btn','shotpressure-buttons')", 
                                 style = "width: 90px;"),
                    style = "text-align: center;"          
                ),
                
                hidden(textInput("selected_zone", "")),
                hidden(textInput("selected_action", "")),
                hidden(textInput("selected_outcome", "")),
                hidden(textInput("selected_shot_pressure", "")),
                br(),
                
                # add notes area
                h5("Notes:", align = "center"),
                textAreaInput("notes", "Notes", "", rows = 3),
                
                br(),
                div(
                    actionButton("submit", "Submit"),
                    actionButton("delete_last", "Delete Last Entry"),
                    style = "text-align: center;"
                ),
                br(),
                # Add the screenshot button to the UI
                div(
                    actionButton("screenshot", "Take Screenshot"),
                    downloadButton("download_csv", "Download CSV", style = "text-align: center;"),
                    style = "text-align: center;"
                ),
                br(),
                div(
                    img(src = "vids/pitch_zoned.png", width = "300", height = "350"),
                    hidden(textInput("video_timestamp", "")),
                ),
            ),
            class = "sidebar-content" # Add class to div tag
        ),
        
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Video", 
                                 fluidRow(
                                     column(12,
                                            tags$video(id = "myvideo", 
                                                       type = "video/mov", 
                                                       src = "vids/2023_dublin_vs_kerry_final.mov", 
                                                       width = "100%", 
                                                       height = "auto", 
                                                       controls = NA)),
                                     column(12,
                                            div(
                                                actionButton("back_5", "Back -5s", style = "width: 90px;"),
                                                actionButton("back_10", "Back -10s", style = "width: 90px;"),
                                                actionButton("back_30", "Back -30s", style = "width: 90px;"),
                                                actionButton("play_pause", "Play/Pause", style = "width: 90px;"),
                                                actionButton("skip_5", "Skip +5s", style = "width: 90px;"),
                                                actionButton("skip_10", "Skip +10s", style = "width: 90px;"),
                                                actionButton("skip_30", "Skip +30s", style = "width: 90px;"),
                                                style = "text-align: center;"
                                            )
                                     )
                                 ),
                                 tabPanel("Video",
                                          DT::dataTableOutput("table"))
                        )
            )
        )
    )
)



server <- function(input, output, session) {
    selected_zone <- reactiveVal() # Create a reactiveVal to store the selected zone
    
    # Observe zone button clicks
    observeEvent(input$zone_1_btn, { selected_zone(1) }, ignoreInit = TRUE)
    observeEvent(input$zone_2_btn, { selected_zone(2) }, ignoreInit = TRUE)
    observeEvent(input$zone_3_btn, { selected_zone(3) }, ignoreInit = TRUE)
    observeEvent(input$zone_4_btn, { selected_zone(4) }, ignoreInit = TRUE)
    observeEvent(input$zone_5_btn, { selected_zone(5) }, ignoreInit = TRUE)
    observeEvent(input$zone_6_btn, { selected_zone(6) }, ignoreInit = TRUE)
    observeEvent(input$zone_7_btn, { selected_zone(7) }, ignoreInit = TRUE)
    observeEvent(input$zone_8_btn, { selected_zone(8) }, ignoreInit = TRUE)
    observeEvent(input$zone_9_btn, { selected_zone(9) }, ignoreInit = TRUE)
    observeEvent(input$zone_10_btn, { selected_zone(10) }, ignoreInit = TRUE)
    observeEvent(input$zone_11_btn, { selected_zone(11) }, ignoreInit = TRUE)
    observeEvent(input$zone_12_btn, { selected_zone(12) }, ignoreInit = TRUE)
    observeEvent(input$zone_13_btn, { selected_zone(13) }, ignoreInit = TRUE)
    observeEvent(input$zone_14_btn, { selected_zone(14) }, ignoreInit = TRUE)
    observeEvent(input$zone_15_btn, { selected_zone(15) }, ignoreInit = TRUE)
    observeEvent(input$zone_16_btn, { selected_zone(16) }, ignoreInit = TRUE)
    
    selected_action <- reactiveVal() # Create a reactiveVal to store the selected action
    
    # Observe zone button clicks
    observeEvent(input$start_btn, { selected_action(1) }, ignoreInit = TRUE)
    observeEvent(input$stop_btn, { selected_action(2) }, ignoreInit = TRUE)
    observeEvent(input$kickout_btn, { selected_action(3) }, ignoreInit = TRUE)
    observeEvent(input$free_btn, { selected_action(4) }, ignoreInit = TRUE)
    observeEvent(input$sideline_btn, { selected_action(5) }, ignoreInit = TRUE)
    observeEvent(input$free_45_btn, { selected_action(6) }, ignoreInit = TRUE)
    observeEvent(input$kick_pass_btn, { selected_action(7) }, ignoreInit = TRUE)
    observeEvent(input$hand_pass_btn, { selected_action(8) }, ignoreInit = TRUE)
    observeEvent(input$carry_btn, { selected_action(9) }, ignoreInit = TRUE)
    observeEvent(input$to_btn, { selected_action(10) }, ignoreInit = TRUE)
    observeEvent(input$shot_point_btn, { selected_action(11) }, ignoreInit = TRUE)
    observeEvent(input$shot_goal_btn, { selected_action(12) }, ignoreInit = TRUE)
    observeEvent(input$penalty_btn, { selected_action(13) }, ignoreInit = TRUE)
    observeEvent(input$mark_btn, { selected_action(14) }, ignoreInit = TRUE)
    observeEvent(input$move_ball_btn, { selected_action(15) }, ignoreInit = TRUE)
    
    selected_outcome <- reactiveVal() # Create a reactiveVal to store the selected action
    
    # Observe zone button clicks
    observeEvent(input$point_btn, { selected_outcome(1) }, ignoreInit = TRUE)
    observeEvent(input$goal_btn, { selected_outcome(2) }, ignoreInit = TRUE)
    observeEvent(input$wide_btn, { selected_outcome(3) }, ignoreInit = TRUE)
    observeEvent(input$to2_btn, { selected_outcome(4) }, ignoreInit = TRUE)
    observeEvent(input$retain_btn, { selected_outcome(5) }, ignoreInit = TRUE)
    
    selected_shot_pressure <- reactiveVal() # Create a reactiveVal to store the selected action
    
    # Observe zone button clicks
    observeEvent(input$low_btn, { selected_shot_pressure(1) }, ignoreInit = TRUE)
    observeEvent(input$medium_btn, { selected_shot_pressure(2) }, ignoreInit = TRUE)
    observeEvent(input$high_btn, { selected_shot_pressure(3) }, ignoreInit = TRUE)
    
    observeEvent(input$table_cell_edit, {
        info <- input$table_cell_edit
        i <- info$row
        j <- info$col + 1  # Add 1 because the rownames are not displayed
        v <- info$value
        
        column_name <- colnames(df)[j]
        
        if (v %in% allowed_values[[column_name]]) {
            df[i, j] <<- DT::coerceValue(v, df[i, j])
        } else {
            showNotification(paste("Invalid value for", 
                                   column_name, ": only existing values are allowed."), 
                             type = "error", duration = 5)
        }
    })
    
    
    observeEvent(input$submit, {
        req(selected_zone()) # Ensure the selected zone is not NULL
        req(selected_action()) # Ensure the selected action is not NULL
        
        # Get video timestamp from hidden input field
        video_timestamp <- input$video_timestamp
        
        # Get outcome if selected, otherwise set to NA
        outcome <- if (!is.null(selected_outcome())) {
            outcome_names[as.character(selected_outcome())]
        } else {
            NA
        }
        
        # Get shot pressure if selected, otherwise set to NA
        shot_pressure <- if (!is.null(selected_shot_pressure())) {
            shot_pressure_names[as.character(selected_shot_pressure())]
        } else {
            NA
        }
        
        new_row <- data.frame(
            id = nrow(df) + 1, # add an ID column
            half = input$half,
            team_attack = input$team_attack,
            zone = as.integer(selected_zone()),
            action = action_names[as.character(selected_action())],
            outcome = outcome,
            shot_pressure = shot_pressure,
            timestamp = video_timestamp,
            notes = input$notes,
            stringsAsFactors = FALSE
        )
        df <<- rbind(df, new_row)
        output$table <- DT::renderDataTable(df[order(df$id, decreasing = TRUE), ], 
                                            rownames = FALSE, editable = TRUE)
        
        # Reset the notes input field
        updateTextInput(session, "notes", value = "")
        
        # Reset the selected_outcome value to NULL
        selected_outcome(NULL)
        
        # Reset the selected_outcome value to NULL
        selected_shot_pressure(NULL)
        
        # Deselect the outcome button
        runjs("highlightButton('', 'outcome-buttons')")
        
        # Deselect the outcome button
        runjs("highlightButton('', 'shotpressure-buttons')")
    })
    
    
    # Play or pause the video when button is clicked
    observeEvent(input$play_pause, {
        jscode <- "var video = document.getElementById('myvideo'); if (video.paused) { video.play(); } else { video.pause(); }"
        runjs(jscode)
    })
    
    video_can_play <- reactiveVal(FALSE)
    
    observeEvent(video_can_play(), {
        observeEvent(input$skip_5, {
            jscode <- "var video = document.getElementById('myvideo'); video.currentTime += 5;"
            runjs(jscode)
        }, ignoreInit = TRUE)
        
        observeEvent(input$skip_10, {
            jscode <- "var video = document.getElementById('myvideo'); video.currentTime += 10;"
            runjs(jscode)
        }, ignoreInit = TRUE)
        
        observeEvent(input$skip_30, {
            jscode <- "var video = document.getElementById('myvideo'); video.currentTime += 30;"
            runjs(jscode)
        }, ignoreInit = TRUE)
        
        observeEvent(input$back_5, {
            jscode <- "var video = document.getElementById('myvideo'); video.currentTime -= 5;"
            runjs(jscode)
        }, ignoreInit = TRUE)
        
        observeEvent(input$back_10, {
            jscode <- "var video = document.getElementById('myvideo'); video.currentTime -= 10;"
            runjs(jscode)
        }, ignoreInit = TRUE)
        
        observeEvent(input$back_30, {
            jscode <- "var video = document.getElementById('myvideo'); video.currentTime -= 30;"
            runjs(jscode)
        }, ignoreInit = TRUE)
    }
    )
    
    observeEvent(input$delete_last, {
        if (nrow(df) > 0) {
            df <<- df[-nrow(df),]
            output$table <- DT::renderDataTable(df[order(df$id, decreasing = TRUE), ], 
                                                rownames = FALSE, editable = TRUE)
        }
    })
    
    # Download CSV file when button is clicked
    output$download_csv <- downloadHandler(
        filename = function() {
            video_timestamp <- input$video_timestamp
            paste("data_", Sys.Date(), "_", round(video_timestamp,2), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(df, file)
        }
    )
    
    
    # Update the video timestamp when the video is played or paused
    video_update_js <- "
  var video = document.getElementById('myvideo');
  video.addEventListener('canplay', function() {
    Shiny.setInputValue('video_can_play', true);
  });

  setInterval(function() {
    if (!video.paused) {
      Shiny.setInputValue('video_timestamp', video.currentTime);
    }
  }, 100);"
    
    observeEvent(input$screenshot, {
        # JavaScript code for taking the screenshot
        jscode <- paste0("
      var video = document.getElementById('myvideo');
      var canvas = document.createElement('canvas');
      canvas.width = video.videoWidth;
      canvas.height = video.videoHeight;
      var ctx = canvas.getContext('2d');
      ctx.drawImage(video, 0, 0, canvas.width, canvas.height);
      var dataURL = canvas.toDataURL('image/jpeg');
      
      var link = document.createElement('a');
      link.href = dataURL;
      
      var currentDate = new Date();
      var dateString = currentDate.toISOString().split('T')[0];
      var videoTimestamp = Math.round(", input$video_timestamp, ");
      
      link.download = 'screenshot_' + dateString + '_t_' + videoTimestamp + '.jpeg';
      link.click();
    ")
        
        # Run the JavaScript code
        runjs(jscode)
    })
    
    
    runjs(video_update_js)
}



shinyApp(ui, server)
