

library(shiny)
library(rhandsontable)
library(dplyr)
library(lubridate)
library(stringr)
library(shinyBS)

msdata = read.csv("events.csv", stringsAsFactors = F)
msdata$DueDate = dmy(msdata$DueDate)

textareaInput <- function(id, label, value="", rows=5, cols=20, class="form-control"){
  tags$div(
    class="form-group shiny-input-container",
    tags$label('for'=id,label),
    tags$textarea(id=id,class=class,rows=rows,cols=cols,value))
}

ui <- shinyUI(

  navbarPage("MS Monitoring Program",

  # includeCSS("www/msdata.css"), #TODO FIX

  tabPanel("Events",
           sidebarLayout(
             sidebarPanel(
               bsCollapse(
                 bsCollapsePanel("Filter Events",
                   textInput("evtsSearchNHI", "NHI", placeholder="Leave blank to search all"),
                   radioButtons("evtsTimeframe", "Timeframe", choices = c("All Pending", "This week", "Next 6 weeks", "Next 3 months", "Overdue", "Completed"))
                  ),
                 bsCollapsePanel("Selected Event",
                   textInput("evtsStartDate", "Due Date"),
                   textInput("evtsType", "Type"),
                   textareaInput("evtsComment", "Comment"),
                   textInput("evtsCompleted", "Completed"),
                   textareaInput("evtsResult", "Result")
                 ),
                 id="evtsCollapse", multiple=T, open="Filter Events"
               )
             ),
             mainPanel(
               timelinevisOutput("evtsTimeline")
             )
           )),
  tabPanel("Add Patient",
           sidebarLayout(
             sidebarPanel(
               actionButton("addpt", "Add Patient")
             ),
             mainPanel(timelinevisOutput("addptTimeline"))
           )),
  tabPanel("Edit Events",
           sidebarLayout(
             sidebarPanel(
               inputPanel(
                 textInput("editSearchNHI", "Search NHI:")),
               inputPanel(
                 dateInput("editDueDate", "Due Date", ""),
                 textInput("editType", "Type"),
                 textInput("editComment", "Comment"),
                 actionButton("editCompletedButton", "Mark as Completed"),
                 dateInput("editCompleted", "Completed", ""),
                 textInput("editResult", "Result"),
                 actionButton("editGenerateNew", "Generate new event")
               ),
               actionButton("editSaveChanges","Save Changes")),
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
    if (is.null(x)) return()
    cat(str(x))
    if (x$id == "evtsTimeline") {
      updateCollapse(session, "evtsCollapse", open="Selected Event", close="Filter Events")
      updateTextInput(session, "evtsStartDate", value = x$items$start)
      updateTextInput(session, "evtsType", value=x$items$Type)
      updateTextInput(session, "evtsResult", value=x$items$Result)
      updateTextInput(session, "evtsComment", value=x$itmes$Comment)
      updateTextInput(session, "evtsCompleted", value=x$items$Completed)
    }
  })



  output$evtsTimeline <- renderTimelinevis({
    items = values$msdata

    nhi = toupper(input$evtsSearchNHI)
    if (str_length(nhi)==7) {
      items = items %>%
        filter(NHI == nhi)
    }

    endDate = switch(input$evtsTimeframe,
           "All Pending" = ymd("2100/01/01"),
           "This week" = today() + weeks(1),
           "Next 6 weeks" = today() + weeks(6),
           "Next 3 months" = today() + months(3),
           ymd("2100/01/01"))
    endDate = as_date(endDate)

    if (input$evtsTimeframe == "Overdue") {
      items = items %>%
        filter(DueDate < today(), is.na(Completed))
    } else if (input$evtsTimeframe == "Completed") {
      items = items %>%
        filter(!is.na(Completed))
    } else
    {
      items = items %>%
        filter(DueDate > today(), DueDate < endDate, is.na(Completed))
    }

    items = items %>%
      mutate(content = Type, start = DueDate, group = NHI, id = EventId)

    groups = data.frame(id = unique(items$NHI), content = unique(items$NHI))
    timelinevis(items, groups, id="evtsTimeline")
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
