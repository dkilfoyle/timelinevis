library(shiny)
library(rhandsontable)
library(dplyr)
library(lubridate)
library(stringr)
library(shinyBS)

msevents = read.csv("events.csv", stringsAsFactors = F)
msevents$DueDate = ymd(msevents$DueDate)
msevents$Completed = ymd(msevents$Completed)

mspts = read.csv("patients.csv", stringsAsFactors = F)
mspts$DateStarted = ymd(mspts$DateStarted)

textareaInput <-
  function(id,
    label,
    value = "",
    rows = 5,
    cols = 20,
    class = "form-control") {
    tags$div(
      class = "form-group shiny-input-container",
      tags$label('for' = id, label),
      tags$textarea(
        id = id,
        class = class,
        rows = rows,
        cols = cols,
        value
      )
    )
  }

hiddenTextInput = function (inputId, label, value = "", width = NULL, placeholder = NULL)
{
  tags$input(id = inputId,
    type = "text", class = "form-control", value = value, placeholder = placeholder, style="display:none;")
}

ui <- shinyUI(navbarPage(
  "MS Monitoring Program",

  tabPanel("Events",
    sidebarLayout(
      sidebarPanel(
        includeCSS("www/msmonitor.css"),
        bsCollapse(
          bsCollapsePanel(
            "Filter Events",
            textInput("evtsSearchNHI", "NHI", placeholder = "Leave blank to search all"),
            radioButtons(
              "evtsTimeframe",
              "Timeframe",
              choices = c(
                "All Pending",
                "This week",
                "Next 6 weeks",
                "Next 3 months",
                "Overdue",
                "Completed"
              )
            )
          ),
          bsCollapsePanel(
            "Selected Event",
            hiddenTextInput("evtsId", "Id"),
            dateInput("evtsStartDate", "Due Date", ""),
            textInput("evtsType", "Type"),
            textareaInput("evtsComment", "Comment"),
            dateInput("evtsCompleted", "Date Completed", ""),
            textareaInput("evtsResult", "Result"),

            div(
              class = "btn-group-vertical",
              role = "group",
              style = "width:100%",
              actionButton("evtsCompletedButton", "Mark as Completed"),
              actionButton("evtsGenerateNew", "Generate new event"),
              actionButton("evtsSaveChanges", "Save Changes")
            )
          ),
          id = "evtsCollapse",
          multiple = T,
          open = "Filter Events"
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Timeline", timelinevisOutput("evtsTimeline")),
          tabPanel("Table", rHandsontableOutput("evtsTable"))
        )
      )
    )),
  tabPanel("Patients",
    DT::dataTableOutput("ptsTable"),
    tags$hr(),

    fluidRow(
      column(width=6,
        textInput("ptsNHI", "NHI", ""),
        textInput("ptsSurname", "Surname", ""),
        textInput("ptsFirstName", "First Name", "")
      ),
      column(width=6,
        selectInput("ptsDrug", "Drug", choices=c("Tecfidera","Natalizumab","Fingolimod","Interferon"),selected=""),
        dateInput("ptsDateStarted","Date Started","")
      )
    ),

    #action buttons
    actionButton("ptsSave", "Save"),
    actionButton("ptsNew", "New"),
    actionButton("ptsDelete", "Delete")
  ),
  tabPanel("Setup")
))

server <- shinyServer(function(input, output, session) {

  values = reactiveValues(msevents = msevents, mspts = mspts)

  # detect event selection
  observe({
    x = input$tlSelectEvent
    if (is.null(x))
      return()
    cat(str(x))
    if (x$id == "evtsTimeline") {
      updateCollapse(session,
        "evtsCollapse",
        open = "Selected Event",
        close = "Filter Events")
      updateTextInput(session, "evtsId", value = x$items$id)
      updateTextInput(session, "evtsStartDate", value = x$items$start)
      updateTextInput(session, "evtsType", value = x$items$Type)
      updateTextInput(session, "evtsResult", value = x$items$Result)
      updateTextInput(session, "evtsComment", value = x$itmes$Comment)
      updateTextInput(session, "evtsCompleted", value = x$items$Completed)
    }
  })

  observeEvent(input$evtsCompletedButton, {
    updateDateInput(session, "evtsCompleted", value=today())
  })

  observeEvent(input$evtsSaveChanges, {
    saveRow = which(values$msevents$EventId == input$evtsId)
    values$msevents[saveRow, "DueDate"] = input$evtsStartDate
    values$msevents[saveRow, "Type"] = input$evtsType
    values$msevents[saveRow, "Result"] = input$evtsResult
    values$msevents[saveRow, "Completed"] = input$evtsCompleted
    values$msevents[saveRow, "Comment"] = input$evtsComment
  })

  getFilteredEvents = reactive({
    items = values$msevents

    nhi = toupper(input$evtsSearchNHI)
    if (str_length(nhi) == 7) {
      items = items %>%
        filter(NHI == nhi)
    }

    endDate = switch(
      input$evtsTimeframe,
      "All Pending" = ymd("2100/01/01"),
      "This week" = today() + weeks(1),
      "Next 6 weeks" = today() + weeks(6),
      "Next 3 months" = today() + months(3),
      ymd("2100/01/01")
    )
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
      mutate(
        content = Type,
        start = DueDate,
        group = NHI,
        id = EventId
      )

    items
  })


  output$evtsTimeline <- renderTimelinevis({
    items = getFilteredEvents()
    groups = data.frame(id = unique(items$NHI), content = unique(items$NHI))
    timelinevis(items, groups, id = "evtsTimeline")
  })

  output$evtsTable <- renderRHandsontable({
    items = getFilteredEvents()
    rhandsontable(items)
  })

  observeEvent(input$addpt, {
    newrow = data.frame(
      EventId = max(values$msevents$EventId) + 1,
      NHI = "ACJ2321",
      Type = "MRI",
      Number = 1,
      DueDate = dmy("1/23/2016"),
      Completed = F
    )
    values$msevents = rbind(values$msevents, newrow)
  })

  output$ptsTable = DT::renderDataTable(values$mspts, options = list(
    lengthChange=F,
    order=list(list(1,"asc")),
    paging=F,
    info=F), selection="single", class = 'cell-border stripe')

  observe({
    if (length(input$ptsTable_rows_selected) > 0) {
      selrow = values$mspts[input$ptsTable_rows_selected,]
      updateTextInput(session, "ptsNHI", value = selrow$NHI)
      updateTextInput(session, "ptsFirstName", value = selrow$FirstName)
      updateTextInput(session, "ptsSurname", value = selrow$Surname)
      updateSelectInput(session, "ptsDrug", selected = selrow$Drug)
      updateDateInput(session, "ptsDateStarted", value = selrow$DateStarted)
    }
  })

  observeEvent(input$ptsNew, {
    updateTextInput(session, "ptsNHI", value = "Enter details")
    updateTextInput(session, "ptsFirstName", value = "then")
    updateTextInput(session, "ptsSurname", value = "click save")
    updateSelectInput(session, "ptsDrug", selected = "")
    updateDateInput(session, "ptsDateStarted", value = "")
  })

  observeEvent(input$ptsSave, {
    saveRow = which(values$mspts$NHI == input$ptsNHI)
    newRow = list(NHI=input$ptsNHI, Surname=input$ptsSurname, FirstName=input$ptsFirstName, Drug=input$ptsDrug, DateStarted=input$ptsDateStarted)

    if (length(saveRow)==0) # new entry
      values$mspts = rbind(values$mspts, newRow)
    else
      values$mspts[saveRow,] = newRow
  })

})

# Run the application
shinyApp(ui = ui, server = server)
