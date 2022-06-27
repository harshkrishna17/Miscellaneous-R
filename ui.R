PlayerFinishingOverview <- div("Player Finishing Overview", style = "color:#D81B60")

ui <- fluidPage(
  setBackgroundColor("#14171A"),
  titlePanel(PlayerFinishingOverview),
  setSliderColor("#D81B60", 1),
  sidebarLayout(
    sidebarPanel(
      numericInput("player", "Understat Player ID:", value = 3294),
      selectizeInput("situation", "Situation:", choices = c("OpenPlay", "DirectFreekick", "FromCorner", "SetPiece", "Penalty"), multiple = TRUE, selected = c("OpenPlay", "DirectFreekick", "FromCorner", "SetPiece", "Penalty")),
      selectizeInput("shotType", "Shot Type:", choices = c("LeftFoot", "RightFoot", "Head", "OtherBodyPart"), multiple = TRUE, selected = c("LeftFoot", "RightFoot", "Head", "OtherBodyPart")),
      sliderInput("year", "Year:",
                  min = 2014, max = 2021,
                  value = c(2014, 2021),
                  sep = ""),
      numericInput("roll_avg", "Rolling Average of Line Chart:", value = 50),
      selectInput("shots", "Shot Map Type:", choices = c("Point", "Hexbin"), selected = "Point"),
      selectizeInput("plots", "Plots:", choices = c("All", "Line Chart", "Shot Map", "Histogram")),
      radioButtons("theme", "Background Theme:", choices = c("Dark", "Light"), selected = "Dark"),
      downloadButton("download", "Download Plot")
    ),
    mainPanel(h2("Introduction & Plot", align = "center", style = "color:white"),
              h4("This simple Shiny app generates a dashboard of visualizations that can be useful in getting an overview of a soccer player's finishing ability. Play around with the options for customizations and try to gain interesting insights!", style = "color:white"),
              h5("Created by Harsh Krishna (@veryharshtakes)", style = "color:white"),
              plotOutput("plot"))
  )
)
