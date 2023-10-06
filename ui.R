## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')
genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")
shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Project4-Movies Recommender"),
          
          dashboardSidebar(disable = TRUE),

          dashboardBody(includeCSS("css/books.css"),
                        
          
              fluidRow(
                  titlePanel("System I - Rank the Movies"),
                              # Copy the line below to make a select box 
                  selectInput("select", label = h3("Select Movie Genre"), 
                            choices = genre_list,selected = "Action"),
                  box(width = 12, title = "System I: Choose the Movie Genre highest average rank score will be visible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      div(class = "rateitems",
                          uiOutput('ratings')
                      )
                  )
                ),
              fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "System II: Discover Movie you might like base on First selection",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results")
                  )
               )
          )
    )
) 