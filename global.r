options(error=recover)
options(show.error.locations=TRUE)
library(shiny)
library(ggplot2)
library(reshape2)
# library(scales)
# library(grid)
# library(gridExtra)
# library(plyr)
library(Cairo)

#upload size for file set to 40mb
options(shiny.maxRequestSize = 40*1024^2)
Sys.setenv(TZ='EST')
DEFAULT.INTEREVENT <- 12
DEFAULT.MIN.TOTAL <- 0.1

#function to render the ecdf plot
plot_ecdf <- function(d, column.label, column, xaxis.range, anno.num, anno.txt,anno.ecdf) {
    #sets up the y axis to have breaks at intervals of 50
    flow.y.axis <- local({
        interval <- 5
        axis.start <- 0
        axis.end <- ((max(d[[column]], na.rm=TRUE) + interval) %/% interval) * interval
        scale_y_continuous(breaks=seq(axis.start, axis.end, interval))
    })
    #sets the x axis to the range of the values
    x.axis.size <- diff(xaxis.range)
    #sets the break size for the x axis based on the range of values
    x.axis.break.size <- if (x.axis.size > 50) 10 else if (x.axis.size > 25) 5 else  1
    #subsets the data for the plot to only include the percentage of data within the user-determined min and max ranges
    d <- d[d$quantile >= xaxis.range[1] & d$quantile <= xaxis.range[2], ]
    max.observation <- by(d, INDICES=list(d$source), FUN=function(g) {
        g[which.max(g$quantile), ]
    })

    max.observation <- do.call(rbind, max.observation)

    ylab.text <- paste(column.label, '(ft, NAVD88)')

    #renders the ecdf plot
    p <- ggplot(data=d, aes_string(x='quantile', y=column, shape='source', colour='source')) +
        scale_x_continuous(breaks=seq(xaxis.range[1], xaxis.range[2], by=x.axis.break.size), limits=xaxis.range) +
        flow.y.axis +
        geom_step(size=1) +
        geom_point() +
        geom_point(data=max.observation, size=4) +
        annotate('text', x=max.observation$quantile, y=max.observation[[column]],
                 label=round(max.observation[[column]], 2), size=4, vjust=-.7, fontface='bold') +  # vjust .7
        theme_bw() +
        theme(legend.position='bottom',
              legend.title=element_blank(),
              axis.title.y=element_text(vjust=-.1),
              axis.title.x=element_text(vjust=-.1),
              plot.margin=unit(c(0,1,1,1), 'lines')
        ) +
        guides(col=guide_legend(ncol=1)) +
        ylab(ylab.text) +
        xlab('Percentage of time less than equal corresponding flow')
    #add annotation lines
    if (length(anno.num) > 0) {
        annotations <- data.frame(yintercept=anno.num, x=rep(min(d$quantile), length(anno.num)), labels=paste(anno.txt, anno.num, 'ft  (',paste(round(100*anno.ecdf, 2), "%", sep=""),')'))
        p <- p +geom_hline(data=annotations, aes(yintercept=yintercept)) +
            annotate('text', x=annotations$x, y=annotations$yintercept, label=annotations$labels, size=4, vjust=-.4, hjust=.2)
    }
    return(p)
}
