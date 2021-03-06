shinyUI(fluidPage(
    tags$head(
        tags$style(type='text/css', '.wellcol {min-height: 300px}')
    ),
    fluidRow(
        column(width=3,
            #creates a panel with a slightly inset border and grey background
            #equvalent to Bootstrap's well CSS class
            wellPanel(class='wellcol',
                h4('Tide-introduced Flood Risk Analysis V0.2'),
                p('This tool produces a ECDF plot and table of the flow level at a monitoring site.'),
                p('It requires 2 inputs: a flow level time-series data CSV, and the pipe invert elevation, ft (in NAVD88 datum). The level CSV should contain the datetime in the first column, and at lease a column named level or corrected.level.'),
                p('The User can adjust the date frame, the percentile range, and add annotations to the plot.'),
                textInput('site',label=strong("ManholeID")),
                fileInput("file",label=strong("Level time-series data CSV"),
                    multiple=FALSE, accept="csv"),
                numericInput('inv',
                             label=strong("Pipe invert Elevation(ft, NAVD88)"),
                             value=NA, min=0),
                actionButton('submit',label = 'submit')
            )
         ),
        #the condition uses javascript syntax, e.g., input.site (Js) == input$site (R)
        #or input[foo.bar] (JS) == input$foo.bar (R)
       conditionalPanel(condition="input.submit>0",
           column(3,uiOutput('ecdf_ui')),
           column(6,plotOutput('ecdf_plot', height="700px"))
       )
    ),
    fluidRow(dataTableOutput('ecdf_table'))
))