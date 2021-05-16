library(shiny)


shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Recommendation System"),
  
  sidebarPanel(
   
    numericInput("obs", "Number of books in Favourites list:", 2),
    
    numericInput("clB", "Number of books in Classical book list:", 20),
    
    numericInput("tNo", "Number of LDA model topics:", 4)
    
  ),
  
  mainPanel(
    h2("Summary of the sample dataset"),
    verbatimTextOutput("summary"),
    
    h2("Sample User's Favourites"),
    tableOutput("User"),
    
    h2("Recommended classical books"),
    tableOutput("View")
    
  )
))