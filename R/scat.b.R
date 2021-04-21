
scatClass <- R6::R6Class(
    "scatClass",
    inherit = scatBase,
    #### Active bindings ----
    active = list(
        dataProcessed = function() {
            if (is.null(private$.dataProcessed)) {
                x <- self$options$x
                y <- self$options$y
                g <- self$options$group
                
                if (! is.null(x) && ! is.null(y)) {
                    xCol <- jmvcore::toNumeric(self$data[[x]])
                    yCol <- jmvcore::toNumeric(self$data[[y]])
                    if (is.null(g)) {
                        gCol <- rep("var", length(xCol))
                    } else {
                        gCol <- factor(self$data[[g]])
                    }
                    
                    data <- data.frame(x=xCol, y=yCol, g=gCol)
                    data <- jmvcore::naOmit(data)
                    private$.dataProcessed <- data
                }
            }
            return(private$.dataProcessed)
        }
    ),
    private = list(
        #### Member variables ----
        .dataProcessed = NULL,

        #### Init function ----
        .init = function() {
            image <- self$results$scat
            size <- private$.plotSize()
            image$setSize(size$width, size$height)
        },
        
        #### Plot function ----
        .scat = function(image, ggtheme, theme, ...) {
            if (is.null(self$dataProcessed))
                return(FALSE)
            
            data <- self$dataProcessed
            marg <- self$options$marg
            line <- self$options$line
            method <- ifelse(line == "linear", "lm", "auto")
                    
            p <- 
                ggplot2::ggplot(data, ggplot2::aes(x=x, y=y, color=g, fill=g)) + 
                ggplot2::geom_point(alpha=.8, size=2.5) + 
                ggtheme +
                ggplot2::labs(
                    x=self$options$x, 
                    y=self$options$y, 
                    fill=self$options$group, 
                    color=self$options$group
                )
            
            if (line != "none") {
                p <- p + 
                    ggplot2::geom_smooth(method = method, se = self$options$se)
            }
            
            colors <- NULL
            if (is.null(self$options$group)) {
                colors <- list(
                    ggplot2::scale_color_manual(values=theme$color[1]),
                    ggplot2::scale_fill_manual(values=theme$fill[2]), 
                    ggplot2::scale_shape_manual(values=21)
                )
                p <- p + 
                    ggplot2::theme(legend.position = "none") + colors
            }
            
            xBreaks <- ifElseNull(
                !self$options$xBreaksAuto, self$options$xNBreaks
            )
            yBreaks <- ifElseNull(
                !self$options$yBreaksAuto, self$options$yNBreaks
            )
            
            xLimit <- ifElseNull(
                ! self$options$xRangeAuto,
                c(self$options$xmin, self$options$xmax)
            )
            yLimit <- ifElseNull(
                ! self$options$yRangeAuto,
                c(self$options$ymin, self$options$ymax)
            )
            
            p <- p + 
                ggplot2::scale_x_continuous(n.breaks=xBreaks, limits=xLimit) +
                ggplot2::scale_y_continuous(n.breaks=yBreaks, limits=yLimit)
            
            if (marg == "dens") {
                xdens <- 
                    ggplot2::ggplot(data=data, ggplot2::aes(x=x, fill=g)) +
                    ggplot2::geom_density(alpha=0.5, size=.2) + 
                    ggtheme + 
                    ggplot2::theme_void() +
                    ggplot2::theme(legend.position = "none") +
                    colors 

                if (! self$options$xRangeAuto) {
                    xdens <- xdens + 
                        ggplot2::xlim(self$options$xmin, self$options$xmax)
                }
                
                ydens <- 
                    ggplot2::ggplot(data=data, ggplot2::aes(x=y, fill=g)) +
                    ggplot2::geom_density(alpha=0.5, size=.2) + 
                    ggplot2::coord_flip() +
                    ggtheme + 
                    ggplot2::theme_void() +
                    ggplot2::theme(legend.position = "none") +
                    colors
                
                if (! self$options$xRangeAuto) {
                    ydens <- ydens + 
                        ggplot2::xlim(self$options$ymin, self$options$ymax)
                }
                
                p <-
                    xdens + patchwork::plot_spacer() + 
                    p + ydens + 
                    patchwork::plot_layout(
                        ncol=2, 
                        nrow=2, 
                        widths = c(4, 1), 
                        heights = c(1, 4),
                        guides = "collect"
                    ) & 
                    ggplot2::theme(
                        plot.background = ggplot2::element_rect(
                            fill = "transparent", colour = NA
                        )
                    )
            } else if (marg == "box") {
                themeBox <- ggplot2::theme_void() +
                    ggplot2::theme(
                        panel.grid.major = ggplot2::element_blank(),
                        panel.grid.minor = ggplot2::element_blank(),
                        strip.background = ggplot2::element_rect(
                            fill="transparent", color=NA
                        ),
                        panel.background=ggplot2::element_rect(
                            fill="transparent", color=NA
                        ),
                        legend.position = "none"
                    )
                
                xdens <- ggplot2::ggplot() +
                    ggplot2::geom_boxplot(
                        data=data, 
                        ggplot2::aes(x=g, y=x, fill=g, color=g), 
                        position=ggplot2::position_dodge(0.8), 
                        width=0.5, 
                        alpha=0.9, 
                        notch=TRUE
                    ) + 
                    ggplot2::coord_flip() +
                    ggtheme + 
                    themeBox + 
                    colors
                
                if (! self$options$xRangeAuto) {
                    xdens <- xdens + 
                        ggplot2::ylim(self$options$xmin, self$options$xmax)
                }
                
                ydens <- ggplot2::ggplot() +
                    ggplot2::geom_boxplot(
                        data=data, 
                        ggplot2::aes(x=g, y=y, fill=g, color=g), 
                        position=ggplot2::position_dodge(0.8),
                        width=0.5, 
                        alpha=0.9, 
                        notch=TRUE
                    ) + 
                    ggtheme + 
                    themeBox + 
                    colors
                
                if (! self$options$yRangeAuto) {
                    ydens <- ydens + 
                        ggplot2::ylim(self$options$ymin, self$options$ymax)
                }
                
                nLevels <- length(levels(data$g))
                widths <- c(7, 1 + nLevels * 0.2)
                heights <- c(1 + nLevels * 0.2, 7)
                
                p <-
                    xdens + patchwork::plot_spacer() + 
                    p + ydens + 
                    patchwork::plot_layout(
                        ncol = 2, 
                        nrow = 2, 
                        widths = widths, 
                        heights = heights,
                        guides = "collect"
                    ) & 
                    ggplot2::theme(
                        plot.background = ggplot2::element_rect(
                            fill = "transparent", colour = NA
                        )
                    )
            }
            
            return(p)
        },
        
        #### Helper functions ----
        .plotSize = function() {
            g <- self$options$group
            
            marg <- 0
            nLevels <- 1
            legend <- 0
            
            x <- 826.5 / 700
            
            title <- 47 / x
            ticks <- 35 / x
            xaxis <- 516 / x
            yaxis <- 450 / x
            
            if ( ! is.null(g)) {
                levels <- levels(self$data[[g]])
                nLevels <- length(levels)
                nCharLevels <- max(nchar(levels))
                nCharName <- as.numeric(nchar(g))
                
                preLegend <- 18 / x
                legendTitle <- (10 * nCharName) / x
                preIcon <- 7 / x
                icon <- 10 / x
                postIcon <- 9 / x
                legendLevels <- (7.5 * nCharLevels) / x
                
                legend <- max(
                    preLegend + preIcon + icon + postIcon + legendLevels, 
                    preLegend + legendTitle
                )
            }
            
            marg <- 0
            if (self$options$marg == "box") {
                box <- 26 / x
                marg <- nLevels * box
            } else if (self$options$marg == "dens") {
                marg <- 26 / x * 4
            }

            post <- 28.5 / x
            
            width <- title + ticks + xaxis + marg + legend + post
            height <- title + ticks + yaxis + marg + post
            
            return(list(width=width, height=height))
        })
)
