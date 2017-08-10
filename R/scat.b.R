
scatClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "scatClass",
    inherit = scatBase,
    private = list(
        .run = function() {
            
            x <- self$options$x
            y <- self$options$y
            g <- self$options$group
            
            if ( ! is.null(x) && ! is.null(y)) {
                
                xCol <- jmvcore::toNumeric(self$data[[x]])
                yCol <- jmvcore::toNumeric(self$data[[y]])
                gCol <- if (is.null(g)) rep("var", length(xCol)) else factor(self$data[[g]])
                
                data <- data.frame(x=xCol, y=yCol, g=gCol)
                
                data <- jmvcore::naOmit(data)
                
                image <- self$results$scat
                image$setState(data)
                
            }
        },
        .scat = function(image, ggtheme, theme, ...) {
            
            if (is.null(image$state))
                return(FALSE)
            
            library('ggstance')
            
            data <- image$state
            marg <- self$options$marg
            
            p <- ggplot2::ggplot(data, ggplot2::aes(x=x, y=y, color=g, shape=g)) + 
                ggplot2::geom_point(alpha=.8, size=3) + ggtheme +
                ggplot2::labs(x=self$options$x, y=self$options$y, color=self$options$group, shape=self$options$group)
            
            if (self$options$line)
                p <- p + ggplot2::geom_smooth(method = "lm", se = self$options$se)
            
            if (is.null(self$options$group))
                p <- p + ggplot2::theme(legend.position = 'none')
            
            if (marg == 'dens') {
                xdens <- cowplot::axis_canvas(p, axis='x') +
                    ggjoy::geom_ridgeline(data=data, ggplot2::aes(x, y=0, height=..density.., fill=g),
                                          stat='xdensity', alpha=0.7, size=.2, trim=FALSE)
                
                ydens <- cowplot::axis_canvas(p, axis='y') +
                    ggjoy::geom_vridgeline(data=data, ggplot2::aes(x=0, y=y, width=..density.., fill=g),
                                           stat='ydensity', alpha=0.7, size=.2, trim=FALSE)
                
                p <- cowplot::insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position="top")
                p <- cowplot::insert_yaxis_grob(p, ydens, grid::unit(.2, "null"), position="right")
                
                p <- cowplot::ggdraw(p)    
            
            } else if (marg == 'box') {
                
                themeBox <- ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                           panel.grid.minor = ggplot2::element_blank(),
                                           strip.background = ggplot2::element_rect(fill='transparent', color=NA),
                                           panel.background=ggplot2::element_rect(fill='transparent', color=NA))
                
                xdens <- ggplot2::ggplot() +
                    ggplot2::geom_boxplot(data=data, ggplot2::aes(x=g, y=x, fill=g, color=g), position=ggplot2::position_dodge(0.8),
                                          width=0.5, alpha=0.9, notch=TRUE) + themeBox +
                    ggplot2::coord_flip()
                
                ydens <- ggplot2::ggplot() +
                    ggplot2::geom_boxplot(data=data, ggplot2::aes(x=g, y=y, fill=g, color=g), position=ggplot2::position_dodge(0.8),
                                          width=0.5, alpha=0.9, notch=TRUE) + themeBox
                
                nLevels <- length(levels(data$g))
                
                p <- cowplot::insert_xaxis_grob(p, xdens, grid::unit(.05 * nLevels, "null"), position="top")
                p <- cowplot::insert_yaxis_grob(p, ydens, grid::unit(.05 * nLevels, "null"), position="right")
                
                p <- cowplot::ggdraw(p)    
            }
            
            print(p)
            
            TRUE
            
        }),
    public=list(
        asSource=function() {
            
            paste0("This module does not yet support syntax mode.")
            
        })
)
