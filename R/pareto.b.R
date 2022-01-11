
paretoClass <- R6::R6Class(
    "paretoClass",
    inherit = paretoBase,
    private = list(
        .init = function() {
            image <- self$results$pareto
            size <- private$.plotSize()
            image$setSize(size$width, size$height)
        },
        .run = function() {
            x <- self$options$x
            counts <- self$options$counts
            
            data <- self$data
            
            if ( ! is.null(x)) {
                valuesCol <- factor(data[[x]])
                
                if (! is.null(counts)) {
                    countsCol <- jmvcore::toNumeric(data[[counts]])
                    df <- data.frame(x=valuesCol, counts=countsCol)
                    df <- as.data.frame(xtabs(counts ~ x, data=df))
                    
                    labels <- list(x=x, y=counts)
                } else {
                    df <- as.data.frame(table(valuesCol))
                    
                    labels <- list(x=x, y=.('Counts'))
                }
                
                names(df) <- c('x', 'counts')
                
                df <- df[order(df$counts, decreasing=TRUE), ]
                df$x <- factor(df$x, levels=df$x)
                df$cum <- cumsum(df$counts)
                
                image <- self$results$pareto
                image$setState(list(df=df, labels=labels))
            }
        },
        .pareto = function(image, ggtheme, theme, ...) {
            if (is.null(image$state))
                return(FALSE)
            
            df <- image$state$df
            labels <- image$state$labels
            
            formula <- paste0("~./", sum(df$counts), "*100")
            formula <- as.formula(formula)
            
            p <- ggplot2::ggplot(df, ggplot2::aes(x=x)) +
                ggplot2::geom_bar(
                    ggplot2::aes(y=counts), 
                    width=0.6, 
                    fill=theme$fill[2], 
                    color=theme$color[2], 
                    stat="identity"
                ) +
                ggplot2::geom_point(
                    ggplot2::aes(y=cum), size=3, color=theme$color[1]
                ) +
                ggplot2::geom_path(
                    ggplot2::aes(y=cum, group=1), 
                    size=1.1, 
                    lty="dashed", 
                    color=theme$color[1]
                ) +
                ggplot2::scale_y_continuous(
                    sec.axis = ggplot2::sec_axis(
                        formula, name = .("Cumalitive Percentage")
                    )
                ) +
                ggplot2::labs(x=labels$x, y=labels$y) + 
                ggtheme
            
            if (self$options$angle > 0) {
                p <- p + ggplot2::theme(
                    axis.text.x = ggplot2::element_text(
                        angle = self$options$angle, hjust = 1
                    )
                )
            }

            return(p)
        },
        .plotSize = function() {
            x <- self$options$x
            
            if (is.null(x))
                return(list(width=450, height=350))
            
            levels <- levels(self$data[[self$options$x]])
            nLevels <- length(levels)
            nCharLevels <- max(nchar(levels))
            
            xLabels <- nCharLevels * .1 * self$options$angle

            width <- max(450, nLevels * 25)
            height <- 350 + xLabels
            
            return(list(width=width, height=height))
        })
)
