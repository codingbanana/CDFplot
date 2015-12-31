mk_summary_table <-{
    d <- read.csv("f1.csv")
    ecdf.source <- "level"
    time.ranges <- full.range
    invft <- 30
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
    d <- d[c('DDATE', ecdf.source)]
    d <- d[complete.cases(d), ]
    #add a column that saves adjusted value
    d$elev <- d[,ecdf.source]+invft
    df <- subset(d, DDATE >= time.ranges[1] & DDATE <= time.ranges[2])
    ranks <- rank(df[[ecdf.source]], ties.method='max')
    quantiles <- (ranks / max(ranks, na.rm=TRUE)) * 100
    ecdf <- transform(df, quantile=quantiles)
    unique(ecdf)
}