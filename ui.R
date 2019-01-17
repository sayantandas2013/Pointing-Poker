library(shiny)

options(shiny.host="0.0.0.0")


ui <- shinyUI(fluidPage(
  titlePanel("Pointing poker"),
  hr(),
  sidebarLayout(sidebarPanel(
  textInput("name", "Name"),
  selectInput("points", "Your estimated story point", c("", "1", "2", "3", "5", "8", "13", "20", "40", "100"),selected = ""),
  actionButton("submit", "Submit"),
  actionButton("showdata", "Show voting"),
  actionButton('reset','Reset'),
  tableOutput("submission"),width = 3),
  mainPanel(
  DT::dataTableOutput("responses", width = 700),
  h3(textOutput('infoBox'),align = 'center', color = 'green'),
  tags$head(tags$style("#infoBox{color: red;font-size: 25px;font-style: italic;}"))
  )
  ),
  div(class = "footer",includeHTML("footer.html"))
)
  
)

