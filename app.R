
## showing reactive()


library(shiny)

# library(devtools)


library(tidyverse) # for everything ... almost
library(DT)        # for table output
library(bslib)     #for theming


nurses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-05/nurses.csv')

states <- nurses %>% 
  distinct(State) %>% 
  arrange(State) %>% 
  pull(State)

# Define UI for application 
ui <- fluidPage(
  theme = bs_theme(bg = "#0b3d91", 
                   fg = "white",
                   primary = "#FCC780", 
                   secondary = "#D44420", 
                   base_font = font_google("Space Mono"), 
                   bootswatch = "minty"),
  # Application title
  titlePanel("Analysing nurses earnings in US states overtime"),
  
  # Sidebar with inputs 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "states", # to use in code
                  label = "Choose a State:", # how it looks in UI
                  choices = states, 
                  selected = "Alabama"),
      selectInput(inputId = "variable",
                  label = "Choose a variable:",
                  choices = c("Annual Salary Avg", "Hourly Wage Avg", "Yearly Total Employed (State)_Aggregate")),
    ),
    
    mainPanel(
      plotOutput(outputId = "wageplot"))
  )
)

# Define server logic 
server <- function(input, output) {
                  
       state_choice <- reactive(
                      nurses %>% 
                      filter(State == input$states)) 
                     
      #variable_choice <- reactive({nurses %>% 
                             # switch(input$variable,
                              #"Annual Salary Avg" = `Annual Salary Avg`,
                              #"Hourly Wage Avg" = `Hourly Wage Avg`,
                              #"Yearly Total Employed (State)_Aggregate" = `Yearly Total Employed (State)_Aggregate` )})
    
  # Now use that function, with no arguments.
  output$wageplot <- renderPlot({
    state_choice() %>% 
      ggplot(aes(x = Year, y = .data[[input$variable]])) +
      geom_point() +
      geom_line() +
      labs(title = paste("Total",input$variable, "for nurses in", input$states),
           x = "",
           y = "") +
      theme_minimal()
  })
  
  #output$cum_wt_tbl <- renderDataTable(wage_smry())
  
}

# Run the application 
shinyApp(ui = ui, server = server)
