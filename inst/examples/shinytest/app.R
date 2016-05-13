

library(shiny)
library(rhandsontable)
library(dplyr)

msdata = read.csv("events.csv", stringsAsFactors = F)

ui <- shinyUI(navbarPage(
  "MS Monitoring Program",

  tabPanel("Events",
           sidebarLayout(
             sidebarPanel(
               dateInput("startDate", "Start Date"),
               actionButton("addpt", "Add Patient")
             ),
             mainPanel(timelinevisOutput("timeline"))
           )),
  tabPanel("Add Patient"),
  tabPanel("Edit Patient",
           sidebarLayout(
             sidebarPanel(
               textInput("editSearchNHI", "Search NHI:")
             ),
             mainPanel(
               timelinevisOutput("editTimeline"),
               rHandsontableOutput("editHOT"))
           ))
))

server <- shinyServer(function(input, output, session) {
  values = reactiveValues(msdata = msdata)

  observeEvent(input$addpt, {
    newrow = data.frame(
      EventId = max(values$msdata$EventId) + 1,
      NHI = "ACJ2321",
      Type = "MRI",
      Number = 1,
      DueDate = "1/23/2016",
      Completed = F
    )
    values$msdata = rbind(values$msdata, newrow)
  })

  observe({
    x = input$tlSelectEvent
    cat(str(x))
    updateDateInput(session, "startDate", value = x$start)
  })

  output$timeline <- renderTimelinevis({
    items = data.frame(
      content = values$msdata$Type,
      start = values$msdata$DueDate,
      group = values$msdata$NHI,
      id = values$msdata$EventId
    )
    groups = data.frame(id = unique(values$msdata$NHI),
                        content = unique(values$msdata$NHI))
    timelinevis(items, groups)
  })

  output$editTimeline <- renderTimelinevis({
    items = values$msdata
    items %>%
      filter(NHI == input$editSearchNHI) %>%
      mutate(content = Type, start = DueDate, group = NHI, id = EventId) %>%
      timelinevis()
  })

  output$editHOT <- renderRHandsontable({
    items = values$msdata %>%
      filter(NHI == input$editSearchNHI)
    rhandsontable(items)
  })

})

# Run the application
shinyApp(ui = ui, server = server)
