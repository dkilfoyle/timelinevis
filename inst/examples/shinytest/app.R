

library(shiny)
library(rhandsontable)
library(dplyr)

msdata = read.csv("events.csv", stringsAsFactors = F)
msdata$DueDate = dmy(msdata$DueDate)

ui <- shinyUI(navbarPage("MS Monitoring Program",

  tabPanel("Events",
           sidebarLayout(
             sidebarPanel(
               inputPanel(h3("Filter Events"),
                 textInput("evtsSearchNHI", "NHI", placeholder="Leave blank to search all"),
                 radioButtons("evtsTimeframe", "Timeframe", choices = c("All Pending", "This week", "Next 6 weeks", "Next 3 months", "Overdue", "Completed"))),

               inputPanel(h3("Selected Event"),
                 dateInput("startDate", "Start Date"),
                 textInput("type", "Type"),
                 checkboxInput("completed", "Completed"))
             ),
             mainPanel(timelinevisOutput("timeline"))
           )),
  tabPanel("Add Patient",
           sidebarLayout(
             sidebarPanel(
               actionButton("addpt", "Add Patient")
             ),
             mainPanel(timelinevisOutput("addptTimeline"))
           )),
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
      DueDate = dmy("1/23/2016"),
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
    items = values$msdata
    item = items %>%
      filter(NHI == toupper(input$evtsSearchNHI))

    cat(input$evtsTimeFrame)

    endDate = switch(input$evtsTimeframe,
           "All Pending" = ymd("2100/01/01"),
           "This week" = today() + weeks(1),
           "Next 6 weeks" = today() + weeks(6),
           "Next 3 months" = today() + months(3),
           ymd("2100/01/01"))
    endDate = as_date(endDate)

    cat(endDate,"\n")

    items = items %>%
      filter(DueDate > today(), DueDate < endDate)

    items = items %>%
      mutate(content = Type, start = DueDate, group = NHI, id = EventId)

    groups = data.frame(id = unique(items$NHI), content = unique(items$NHI))
    timelinevis(items, groups)
  })

  output$editTimeline <- renderTimelinevis({
    items = values$msdata
    items %>%
      filter(NHI == toupper(input$editSearchNHI)) %>%
      mutate(content = Type, start = DueDate, group = NHI, id = EventId) %>%
      timelinevis()
  })

  output$editHOT <- renderRHandsontable({
    items = values$msdata %>%
      filter(NHI == toupper(input$editSearchNHI))
    rhandsontable(items)
  })

})

# Run the application
shinyApp(ui = ui, server = server)
