library(shiny)


setwd("C:/My Work/Pointingpoker/Pointing poker R")
outputDir <- "responses"

saveData <- function(data) {
  data <- t(data)
  data <- data.frame(data)
  # Write the file to the local system
  fn = file.path(outputDir, "points.csv")
  if (file.exists(fn)){
    x1 = data.frame(read.csv(fn,header=TRUE))
    x1 = rbind(x1,data)
    x1 = x1[ !duplicated(x1[, c("name")], fromLast=T),]
    write.csv(
      x = x1,
      file = fn, 
      row.names = FALSE, quote = TRUE
    )
  } else {
    write.csv(
      x = data,
      file = fn, 
      row.names = FALSE, quote = TRUE
    )
  }
  text = data.frame(paste(data$name,"has submitted response"))
  names(text) = "Response"
  fn = file.path(outputDir, "response.csv")
  if (file.exists(fn)){
    x2 = data.frame(read.csv(fn,header=TRUE))
    names(x2) = "Response"
    x2 = rbind(x2,text)
    x2 = subset(x2, !duplicated(x2,fromLast=T))
    write.csv(
      x = x2,
      file = fn, 
      row.names = FALSE
    )
  } else {
    write.csv(
      x = text,
      file = fn, 
      row.names = FALSE
    )
  }
}

loadData <- function(x) {
  # Read all the files into a list
  files <- list.files(outputDir, pattern=x,full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  if (x == "points.csv"){
    data <- do.call(rbind, data)
  } else {if (x == "response.csv"){
    data <- do.call(rbind, data)}
  } 
  data
}

deletedata <- function() {
  fn = file.path(outputDir, "response.csv")
    Response <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(Response) <- c("Response")
    write.csv(
      Response,
      file = fn, 
      row.names = FALSE
    )
  fn = file.path(outputDir, "points.csv")
  Response <- data.frame(matrix(ncol = 2, nrow = 0))
  x <- c("name", "points")
  colnames(Response) <- x
  write.csv(
    Response,
    file = fn, 
    row.names = FALSE
  )
}

show_voting <- function() {
  fn = file.path(outputDir, "response.csv")
  x1 = data.frame(read.csv(fn,header=TRUE))
  names(x1) = "Response"
  text = data.frame(paste("Response is being displayed"))
  names(text) = "Response"
  x1 <- x1[0,]
  x1 <- rbind(x1,text)
  write.csv(
    x = x1,
    file = fn, 
    row.names = FALSE
  )
}

printconsensus <- function(){
  x1 <- loadData("points.csv")
  cond = sd(x1$points)
  if (!is.na(cond)){
  if (sd(x1$points)==0){
    return("CONSENSUS!!!")
  } else {
    return("")
  }
  } else {
    return("There should be at least two votes for consensus")
  }
}

fields <- c("name", "points")

indicator_var = 0

server <- function(input, output, session) { 
  output$submission <- renderTable({
    invalidateLater(1000)
    loadData("response.csv")
  }) 
  
  if (indicator_var == 1){
    output$responses <- renderTable({
      loadData("points.csv")
    }) 
  }
  
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  observeEvent(input$submit, {
    saveData(formData())
    output$submission <- renderTable({
      invalidateLater(1000)
      loadData("response.csv")
    }) 
    
  })
  
  observeEvent(input$showdata, {
    output$responses <- DT::renderDataTable({
      loadData("points.csv")
    })
    show_voting()
    output$infoBox <- renderPrint({
      cat(printconsensus())
    })
    indicator_var = 1
    output$submission <- renderTable({
      invalidateLater(1000)
      loadData("response.csv")
    }) 
  })
  
  
  observeEvent(input$reset, {
    deletedata()
    output$responses <- DT::renderDataTable({})
    output$submission <- renderTable({
      invalidateLater(1000)
      loadData("response.csv")
    })
    output$infoBox <- renderPrint({
      cat("")
    })
  })
  
  
  
}