shinyServer(function(input, output, session){
    #reads the uploaded .csv file and assigns the data to a dataframe
    data <- reactive({
        inFile <- input$file
        if(is.null(inFile))
            return(NULL)
        d <- read.csv(inFile$datapath, as.is=TRUE)
        names(d) <- replace(names(d), 1, 'DDATE')
        #determines if the uploaded date/time column contains seconds
        has.seconds <- nchar(gsub('[^:]', '', d$DDATE[1])) == 2
        #reads the date/time column and determines the time formatting
        if (grepl('AM|PM', d$DDATE[1])) {
            if (has.seconds) {
                time.fmt <- '%I:%M:%S %p'
            } else {
                time.fmt <- '%I:%M %p'
            }
        } else {
            if (has.seconds) {
                time.fmt <- '%H:%M:%S'
            } else {
                time.fmt <- '%H:%M'
            }
        }
        #determines the date format for the date/time data
        date.fmt <- if (grepl('-', d$DDATE[1])) '%Y-%m-%d' else '%m/%d/%Y'
        #converts the date/time column to the calandar time format
        d <- within(d, DDATE<-as.POSIXct(tolower(DDATE), format=paste(date.fmt, time.fmt)))
        d[order(d$DDATE),]
    })

    #establishes reactive values
    reactive_values <- reactiveValues(ecdf_anns=numeric(0),ecdf_anns_text=character(0),ecdf_anns_quantile=numeric(0))

    #renders the data manipulation objects for the ecdf plot
    #ecdf_source - selector for the data to use
    #ecdf_timeframe_input - date range selector for establishing the data timeframe to use
    #ecdf_add - adds an additional line to display a different timeframe
    #ecdf_yaxis_ann - allows the user to input a value to render a horizontal annotation line on the plot
    #ecdf_xaxis - allows the user to determine the min/max percentages to display

    dum <- eventReactive(input$submit,{
        d <- data()
        #columns other than DDATE
        cols <- setdiff(names(d), 'DDATE')
        #columns include level; (?i) makes the regex case insensitive
        level.cols <- grepl('(?i)level', cols)
        level.col <- if (any(level.cols)) cols[which.max(level.cols)] else cols[1]
        range.d <- range(d$DDATE)
        #ecdf.title <- if (is.null(ecdf.title)) '' else ecdf.title
        wellPanel(class='wellcol',
                  h4('ECDF controls'),
                  selectInput('ecdf_source', strong('level column'),
                              choices=cols, selected = level.col),
                  dateRangeInput('ecdf_timeframe_input',
                                 strong('Change time frame'),
                                 start=range.d[1],
                                 end=range.d[2],
                                 min=range.d[1],
                                 max=range.d[2]
                                 ),
                  div(style="display:inline-block",
                      textInput('ecdf_yaxis_ann_text',strong('Add annotation'))
                  ),
                  div(style="display:inline-block",
                      numericInput('ecdf_yaxis_ann',
                                   strong('Add critical elevations, ft (NAVD88 datum)'),value=NA, min = 0)
                  ),
                  actionButton('ecdf_ann', 'Add'),
                  sliderInput('ecdf_xaxis',
                              strong('Set x-axis range (%)'),
                              value=c(0, 100), min=0, max=100, step=5
                              ),
                  hr(),
                  h4('Download'),
                  textInput('ecdf_title', strong('ECDF main title')),
                  div(style="display:inline-block",
                      downloadButton('dl_ecdf', 'ECDF plot')),
                  div(style="display:inline-block",
                      downloadButton('dl_table', 'ECDF table'))
        )
    })

    output$ecdf_ui <- renderUI(dum())

    #function to make the ecdf plot
    mk_ecdf_plot <- reactive({
        d <- data()
        ecdf.source <- input$ecdf_source
        time.range <- input$ecdf_timeframe_input
        #this statement is needed, otherwise a warning shows up because you'r comparing POSIXt with date
        time.range <-as.POSIXct(paste(time.range, '00:00:00'))
        anns <- reactive_values$ecdf_anns
        anns.txt<- reactive_values$ecdf_anns_text
        anns.ecdf <- reactive_values$ecdf_anns_quantile
        xaxis.range <- input$ecdf_xaxis
        invft <- input$inv
        if (! is.null(ecdf.source)) {
            d <- d[c('DDATE', ecdf.source)]
            d <- d[complete.cases(d), ]
            #add a column that saves adjusted value
            d$elev <- d[,ecdf.source]+invft
            next.d <- subset(d, DDATE >= time.range[1] & DDATE <= time.range[2])
            trange.label <- paste(format(time.range, format='%Y-%m-%d'), collapse=' -- ')
            ranks <- rank(next.d[[ecdf.source]], ties.method='max')
            #the rank won't change when a fixed number is add to the num seq
            #ranks2 <- rank(next.d$elev, ties.method='max')
            quantiles <- (ranks / max(ranks, na.rm=TRUE)) * 100
            next.d <- transform(next.d, source=trange.label, quantile=quantiles)
            data.subsets <- unique(next.d)
            # change the column to plot to the adjusted column
            p <- plot_ecdf(data.subsets, ecdf.source, 'elev' , xaxis.range, anns,anns.txt,anns.ecdf)
            return(p)
        }
    })

    mk_summary_table <-reactive({
        d <- data()
        ecdf.source <- input$ecdf_source
        time.range <- input$ecdf_timeframe_input
        invft <- input$inv
        if (! is.null(ecdf.source)) {
            #this statement is needed, otherwise a warning shows up because you'r comparing POSIXt with date
            time.range <- as.POSIXct(paste(time.range, '00:00:00'))
            d <- d[c('DDATE', ecdf.source)]
            d <- d[complete.cases(d), ]
            #add a column that saves adjusted value
            d$elev <- d[,ecdf.source]+invft
            df <- subset(d, DDATE >= time.range[1] & DDATE <= time.range[2])
            ranks <- rank(df[[ecdf.source]], ties.method='max')
            quantiles <- (ranks / max(ranks, na.rm=TRUE)) * 100
            ecdf <- transform(df, quantile=quantiles)
            unique(ecdf)
        }
    })

    #observes the ecdf_ann button
    #when the button is pressed, the value in the date range input is added to the ecdf_anns reactive values
    observeEvent(input$ecdf_ann,{
        ann <- input$ecdf_yaxis_ann
        ann_txt<- input$ecdf_yaxis_ann_text
        if (! is.null(ann)) {
            reactive_values$ecdf_anns_text <- c(isolate(reactive_values$ecdf_anns_text), ann_txt)
            reactive_values$ecdf_anns <- c(isolate(reactive_values$ecdf_anns), ann)
            d <- mk_summary_table()
            ann_ecdf <- ecdf(d$elev)(ann)
            reactive_values$ecdf_anns_quantile <- c(isolate(reactive_values$ecdf_anns_quantile), ann_ecdf)
        }
    })

    #outputs the ecdf plot
    output$ecdf_plot <- renderPlot({
        p <- mk_ecdf_plot()
        print(p)
    })
    #outputs the ecdf table
    output$ecdf_table <- renderDataTable({
        mk_summary_table()
    })
    #download the ecdf plot
    output$dl_ecdf <- downloadHandler(
        filename=function() {
            d <- data()
            time.frame <- gsub("-","",as.Date(input$ecdf_timeframe_input))
            paste0(input$site, '_ecdf_', paste(time.frame, collapse='_'), '.png')
        },
        content=function(filename) {
            d <- data()
            time.frame <- gsub("-","",as.Date(input$ecdf_timeframe_input))
            tmp.name <- paste0(input$site, '_ECDF')
            maintitle <- if (input$ecdf_title == '') tmp.name else input$ecdf_title
            subtitle <- paste(time.frame, collapse=' -- ')
            title <- bquote(atop(bold(.(maintitle)), .(subtitle)))
            CairoPNG(filename=filename, height=800, width=1000)
            p <- mk_ecdf_plot()
            p <- p + ggtitle(title)
            print(p)
            dev.off()
        }
    )
    #download the ecdf table
    output$dl_table <- downloadHandler(
        filename=function() {
            time.frame <- gsub("-","",as.Date(input$ecdf_timeframe_input))
            paste0(input$site, '_ecdf_', paste(time.frame, collapse='_'), '.csv')
        },
        content=function(filename) {
            time.frame <- gsub("-","",as.Date(input$ecdf_timeframe_input))
            sum.table <- mk_summary_table()
            #sub.table <- subset(sum.table,DDATE>=input$ecdf_timeframe_input[1] & DDATE<=input$ecdf_timeframe_input[2])
            write.csv(sum.table, filename, row.names=FALSE, quote=FALSE)
        }
    )
})

