# This is a page that lays out the UI for a Shiny Dashboard page.
# 
# General Structure:
#   dashboardPage(
#     dashboardHeader(),
#     dashboardSidebar(
#       sidebarMenu(
#         menuItem()
#       )
#     ),
#     dashboardBody(
#       tags$head(HEAD OF HTML PAGE. PUT LINKS TO EXTERNAL CSS, JS, ETC. HERE),
#       tabItems(
#         tabItem(
#           fluidRow(
#             column(
#               box(),
#               tabBox(
#                 tabPanel()
#               )
#             )
#           )
#         )
#       )
#     )
#   )
#
#

# Attach shinydashboard package - doesn't work when calling on server.R for some reason
library("shinydashboard")

# Variables for HCLC locations, used later
locations <- c("Main Area/Study Rooms" = "Public/StudyRooms (26)", 
               "Bookmark" = "Bookmark (30)", "Classroom" = "Classroom (27)", 
               "Help Hub" = "HelpHub (25)", "Community Room" = "CommunityRoom (28)", 
               "Digital Scholarship Lab" = "DSL (29)")

# Create Dashboard
dashboardPage(
# Use red theme
  skin="red",

# Header
  dashboardHeader(title="Library Dashboard"),

# Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem(
#       Display Title
        "Dashboard",
#       tabName specifies which sub-page will display when this menu item is selected
        tabName = "dashboard",
#       Icons from Font Awesome
        icon = icon("dashboard")
      ),
      menuItem("HCLC", tabName = "hclc", icon = icon("users")),
      menuItem("Tech Platforms", tabName = "libtech", icon = icon("code")),
      menuItem("Circulation", tabName = "circ", icon = icon("book")),
      menuItem("Research Help Desk", tabName = "rhd", icon = icon("graduation-cap")),
      menuItem("EIC", tabName = "eic", icon = icon("desktop")),
      menuItem("Branches", tabName = "branches", icon = icon("university"))
    ),
#   Inserts hr HTML element to separate
    hr(),

#   Date Range picker
    dateRangeInput(
#     Lets us reference on server script as input$dates
      "dates",
#     Label
      label = h3("Filter by Date"),
#     Specify beginning and end dates - start 30 days ago, end today
      start=Sys.Date()-30, 
      end=Sys.Date()
    ),

#   Shows warning for incorrect dates - result of output$dateValidate from server
    textOutput("dateValidate")
  ),

# Main body
  dashboardBody(
    
#   Modify head of HTML document
    tags$head(
#     All kinds of tags can go here. CSS files have to live in www folder
      tags$link(rel="stylesheet", type="text/css", href="styles.css")
    ),

#   Shows selected menuItem from before as a tabItem
    tabItems(
      tabItem(
#       Has to match tabName from menuItem - this is the main page
        tabName = "dashboard",
#       We can put all kinds of HTML elements in here
        h2("Library Dashboard"),
#       Bootstrap row
        fluidRow(
#         Display box
          box(
            title = "HCLC Occupancy",
#           Displays plot from output$mainHCLC on server
            plotOutput("mainHclc")
          ),
          box(
            title="Site Visits",
            plotOutput(
              "plot1",
#             Click is one option for interactivity. Server interprets a click as input$plot1_click
              click="plot1_click"
            ),
#           Displays result of output$plot1_data from server, which runs on input$plot1_click from above
            verbatimTextOutput("plot1_data")
            )
        ),
        fluidRow(
          box(
            title = "Circulation",
            height=250
          ),
          box(
            title="Research Help Desk",
            height = 250
          )
        )
      ),
      tabItem(
        tabName = "hclc",
        h2("HCLC Usage"),
        fluidRow(
#         Bootstrap column - a row has a width of 12
          column(
            width=3,
            box(
              title="Filter by Location",
#             Width of 12 fills entire parent element, column()
              width=12,
#             Check boxes with options from "locations" above. Server interprets as input$checkGroup
              checkboxGroupInput("checkGroup", label = NULL,
                               choices = locations,
                               selected = locations)
            )
          ),
          column(
            width=9,
#           Box with tabs
            tabBox(
              width=12,
#             Each tab is called as tabPanel
              tabPanel(
                title="Total Occupany by Time of Day",
                plotOutput("totalMean"),
                hr(),
                h4("Individuals vs. Groups"),
                plotOutput("actFill")
              ),
              tabPanel(
                title="Occupancy by Time of Day / Location",
                plotOutput("locMean"),
                hr(),
                h4("Individuals vs. Groups"),
#               We can't output actFill twice, we need to use a copy
                plotOutput("actFillCopy")
              ),
              tabPanel(
                title="Weekday Heatmap",
                plotOutput("wdayHeat", click="wd_click"),
                textOutput("wd_data"),
                hr(),
                plotOutput("wdActHeat", click="wd_click2"),
                textOutput("wd_data2")
              )
            )
          )
        )
      ),
#     Empty tabs
      tabItem(tabName = "libtech",
        h2("Library Tech Platforms")
      ),
      tabItem(tabName = "circ",
              h2("Circulation")
      ),
      tabItem(tabName = "rhd",
              h2("Research Help Desk")
      ),
      tabItem(tabName = "eic",
              h2("Electronic Information Center")
      ),
      tabItem(tabName = "branches",
              h2("Branch Libraries")
      )
    )
  )
  
  
)



