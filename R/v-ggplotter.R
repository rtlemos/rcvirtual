#' rcvirtual.ggplotter
#'
#' Reference Class for plotting data with ggplot
#'
#' This reference class contains fields (aka "attributes")
#' and methods (aka "procedures") specific to generating
#' ggplots of data.
#'
#' @import ggplot2
#' @importFrom methods new
#' @exportClass rcvirtual.ggplotter
#'
setRefClass(
  Class = "rcvirtual.ggplotter",
  contains = c("rcvirtual.plotter", "VIRTUAL"),
  methods = list(

    construct.gglayer = function(mylayer){

      if (class(mylayer)[1] == "NULL") return(NULL)
      df <- mylayer$df
      res <- switch(
        mylayer$type,
        "vector" = {
          uu <- 0.3 * sqrt(mean(df$vx ^ 2 + df$vy ^ 2))
          out <- ggplot2::geom_segment(
            data = df,
            mapping = ggplot2::aes(
              x = x, y = x,  xend = x + vx, yend = y + vy),
            arrow = ggplot2::arrow(length = unit(uu, "cm")),
            na.rm = TRUE)
          return(out)
        },
        "points" = {
          if (any(names(df) == "z")) {
            out <- ggplot2::geom_point(
              data = df,
              mapping = ggplot2::aes(x = x, y = y, color = z),
              pch = mylayer$pch,
              cex = mylayer$cex,
              na.rm = TRUE)
          } else {
            out <- ggplot2::geom_point(
              data = df,
              mapping = ggplot2::aes(x = x, y = y),
              pch = mylayer$pch,
              cex = mylayer$cex,
              na.rm = TRUE)
          }
          return(out)
        },
        "usa" = ,
        "state" = ,
        "county" = {
          out <- .self$get.map.layer(map.type = ttype)
          #xlim = c(min(main$df$x), max(main$df$x)),
          #ylim = c(min(main$df$y), max(main$df$y)),
          #tol = max(abs(main$df$x[2] - main$df$x[1]),
          #          abs(main$df$y[2] - main$df$y[1])) / 2.0)
          return(out)
        },
        "contour" = {
          out <- ggplot2::stat_contour(
            data = df,
            ggplot2::aes(x = x, y = y, z = z))
          return(out)
        },
        "raster" = {
          out <- ggplot2::geom_raster(
            data = df,
            ggplot2::aes(x = x, y = y, fill = z))
          return(out)
        },
        "text" = {
          out <- ggplot2::geom_text(
            data = df, ggplot2::aes(x = x, y = y, label = z))
          return(out)
        },
        "path" = {
          out <- ggplot2::geom_path(
            data = df,
            ggplot2::aes(x = x, y = y, group = z))
          return(out)
        },
        "line" = {
          out <- ggplot2::geom_line(
            data = df,
            ggplot2::aes(x = x, y = y, group = z))
          return(out)
        },
        "rect" = {
          out <- ggplot2::geom_rect(
            data = df,
            ggplot2::aes(xmin = x[1], xmax = x[2],
                         ymin = y[1], ymax = y[2]),
            alpha = 0,
            color = "black")
          return(out)
        },
        default = NULL
      )
      return(res)
    },

    qqplot = function(specs, xpos = 1, ypos = 1){
      "Quantile-quantile plot"

      observed.quantiles <- specs$x
      theoretical.quantiles <- qqnorm(observed.quantiles,
                                      plot.it = FALSE)$x
      bnd <- c(min(observed.quantiles,
                   theoretical.quantiles),
               max(observed.quantiles,
                   theoretical.quantiles))
      df <- data.frame(
        theoretical.quantiles = theoretical.quantiles,
        observed.quantiles = observed.quantiles)
      myqqline <- if (specs$best.qqline) {
        ggplot2::geom_smooth(
          method = lm, se = FALSE, color = "black")
      } else NULL
      mybisectrix <- if (specs$bisectrix) {
        ggplot2::geom_abline(
          intercept = 0, slope = 1,
          linetype = "dashed",
          colour = "dark grey")
      } else NULL
      myplot <- ggplot2::ggplot(
        df, ggplot2::aes(
          x = theoretical.quantiles,
          y = observed.quantiles)) +
        ggplot2::geom_point(pch = 3) +
        mybisectrix +
        myqqline +
        ggplot2::scale_x_continuous(limits = bnd) +
        ggplot2::scale_y_continuous(limits = bnd)
      .self$set.in.buffer(myplot, xpos, ypos)
    },

    lineplot = function(specs, xpos = 1, ypos = 1){
      "Multiple line plot"

      xl <- if (is.null(specs$xlab)) "x" else specs$xlab
      yl <- if (is.null(specs$ylab)) "y" else specs$ylab
      if (is.null(specs$zlab)) {
        zl <- ""
        dolegend <- FALSE
      } else {
        zl <- specs$zlab
        dolegend <- TRUE
      }
      gp <- if (!is.null(specs$dopoints)) {
        if (specs$dopoints) ggplot2::geom_point()
      }
      tt <- if (!is.null(specs$title)) {
        ggplot2::ggtitle(specs$title)
      }
      df <- data.frame(x = specs$x, y = specs$y, z = specs$z)
      myplot <- ggplot2::ggplot(
        df, ggplot2::aes(
          x = x, y = y, colour = z, group = z)) +
        ggplot2::geom_line() +
        gp +
        ggplot2::xlab(xl) +
        ggplot2::ylab(yl) +
        tt +
        ggplot2::scale_colour_discrete(
          name = zl, guide = dolegend)
      .self$set.in.buffer(myplot, xpos, ypos)
    },

    scatterplot = function(out, mytitle, xpos = 1,
                           ypos = 1){
      "Plots a scatterplot of two model parameters"

      nl <- nlevels(out$model)
      fc <- .self$palettes$fill[1:nl]
      myplot <- ggplot2::ggplot(
        data = out, ggplot2::aes(
          x = x, y = y, colour = model)) +
        ggplot2::geom_point(point = 2, alpha = 0.2) +
        ggplot2::xlab(mytitle[1]) +
        ggplot2::ylab(mytitle[2]) +
        ggplot2::scale_colour_manual(
          values = .self$palettes$fill, guide = FALSE)
      .self$set.in.buffer(myplot, xpos, ypos)
    },

    tsplot = function(out, mytitle, mylabs, xpos = 1,
                      ypos = 1){
      "Plots a time series plot with observed and fit
      (median plus 95 CI)"

      myplot <- ggplot2::ggplot(
        data = out, ggplot2::aes(x = x, y = obs, z = z)) +
        ggplot2::geom_ribbon(
          data = out,
          ggplot2::aes(x = x, ymin = low95, ymax = high95),
          fill = grey(.self$palettes$CI[1]),
          alpha = .self$palettes$CI[2]) +
        ggplot2::geom_line(
          data = out,
          ggplot2::aes(x = x, y = median, color = z)) +
        ggplot2::geom_point() +
        ggplot2::scale_color_manual(
          values = .self$palettes$line) +
        ggplot2::xlab(mylabs[1]) +
        ggplot2::ylab(mylabs[2]) +
        ggplot2::labs(title = mytitle) +
        ggplot2::labs(color = mylabs[3])
      .self$set.in.buffer(myplot, xpos, ypos)
    },

    one.density.plot = function(out, mytitle, xpos=1,
                                ypos=1){
      "Plots the posterior density of one hyperparameter"

      df <- data.frame(x = out)
      myplot <- ggplot2::ggplot(
        data = df, ggplot2::aes(x = x)) +
        ggplot2::geom_density() +
        ggplot2::xlab(mytitle) +
        ggplot2::ylab("density") +
        ggplot2::scale_colour_manual(
          values = .self$palettes$line, guide = FALSE)
      .self$set.in.buffer(myplot,xpos,ypos)
    },

    density.plot = function(out, mytitle,
                            dolegend = TRUE,
                            xpos = 1, ypos = 1){
      "Plots the posterior density of >= 1 model parameters"

      nl <- nlevels(out$model)
      fc <- .self$palettes$fill[1:nl]
      auxplot <- ggplot2::ggplot(
        data = out,
        ggplot2::aes(x = x, fill = model)) +
        ggplot2::geom_density(alpha = 0.2) +
        ggplot2::xlab(mytitle) +
        ggplot2::ylab("density")
      if (dolegend) {
        myplot <- auxplot +
          ggplot2::scale_fill_manual(
            values = .self$palettes$fill) +
          ggplot2::theme(legend.position = "top")
      } else {
        myplot <- auxplot +
          ggplot2::scale_fill_manual(
            values = .self$palettes$fill, guide = FALSE)
      }
      .self$set.in.buffer(myplot,xpos,ypos)
    },

    probability.1dplot = function(specs, mypoints = NULL,
                                  xpos = 1, ypos = 1){
      "Plot of the posterior/forecast distribution at a
      single site, over time"

      nt <- length(specs$x)
      df <- data.frame(x = rep(specs$x, 3),
                       y = as.numeric(specs$y),
                       z = as.factor(c(rep("low", nt),
                                       rep("mean", nt),
                                       rep("high", nt))))
      if (is.null(mypoints)) {
        points <-  NULL
        if (is.null(specs$ylab)) {
          yl <- "Estimates and CIs"
        } else {
          yl <- specs$ylab
        }
      } else {
        ptdf <- data.frame(x = mypoints$x, y = mypoints$y)
        points <- ggplot2::geom_point(
          data = ptdf, mapping = ggplot2::aes(x = x, y = y),
          pch = 19, na.rm = TRUE)
        if (is.null(specs$ylab)) {
          yl <- "Estimates, CIs and observations"
        } else {
          yl <- specs$ylab
        }
      }
      myplot <- ggplot2::ggplot() +
        points +
        ggplot2::geom_line(data = df,
                           ggplot2::aes(
                             x = x, y = y, linetype = z)) +
        ggplot2::scale_linetype_manual(
          values = c(low = "dotted",
                     mean = "solid",
                     high = "dotted"),
          guide = FALSE) +
        ggplot2::xlab("Time") +
        ggplot2::ylab(yl)
      .self$set.in.buffer(myplot, xpos, ypos)

    },

    surfaceplot = function(specs, xpos = 1,
                           ypos = 1, mypoints = NULL){
      "Surface plotter for the vectors specs$x, specs$y and
      specs$z. The argument specs$labels is a character vector
      (size 4) with title and axes labels."

      empty.specs <- all(is.na(specs$x))
      crit <- !is.null(mypoints$z)
      if (empty.specs) {
        x <- mypoints$x[crit]
        y <- mypoints$y[crit]
        z <- mypoints$z[crit]
        hresol <- 0
      } else {
        x <- specs$x
        y <- specs$y
        z <- specs$z
        hresol <- max(abs(x[2] - x[1]),
                      abs(y[2] - y[1])) / 2.0
      }
      if (specs$g.factor == 0) {
        gx <- gy <- NA
      } else {
        gx <- specs$g[, 1]
        gy <- specs$g[, 2]
      }
      vx  <- specs$vx
      vy  <- specs$vy
      labels <- specs$labels
      if (labels[2] == "Longitude") {
        #convert from 0-360 to -180 - 180
        xx <- (x + 180) %% 360 - 180
        gx <- (gx + 180) %% 360 - 180
        wrld.lim <- list(x = c(min(xx) - hresol,
                               max(xx) + hresol),
                         y = c(min(y) - hresol,
                               max(y) + hresol))
        wrld <- c(ggplot2::geom_polygon(
          ggplot2::aes(long, lat, group = group),
          size = 0.1,
          colour = "black",
          alpha = 0.2,
          data = ggplot2::map_data(specs$map.type),
          xlim = wrld.lim$x,
          ylim = wrld.lim$y))
      } else {
        xx <- x
        wrld <- NULL
      }
      df <- data.frame(X = xx, Y = y, Z = z,
                       Vx = vx, Vy = vy)
      dg <- data.frame(Gx = gx, Gy = gy)
      if (is.null(mypoints)) {
        zm <- z
      } else {
        zm <- c(z, mypoints$z[crit])
      }
      if (is.null(specs$zlim)) {
        cutp <- c(min(zm, na.rm = TRUE),
                  mean(zm, na.rm = TRUE),
                  max(zm, na.rm = TRUE))
      } else {
        cutp <- c(specs$zlim[1],
                  0.5 * (specs$zlim[1] + specs$zlim[2]),
                  specs$zlim[2])
      }
      uu <- 0.3 * sqrt(mean(vx ^ 2 + vy ^ 2))
      vectors <- if (is.na(vx[1])) NULL else {
        ggplot2::geom_segment(
          data = df,
          mapping = ggplot2::aes(
            x = X, y = Y, xend = X + Vx, yend = Y + Vy),
          arrow = arrow(length = unit(uu, "cm")),
          na.rm = TRUE)
      }
      dpcgrid <- if (specs$g.factor == 0) NULL else {
        ggplot2::geom_point(
          data = dg, mapping = ggplot2::aes(x = Gx, y = Gy),
          pch = 4, cex = specs$g.factor,
          na.rm = TRUE)
      }
      if (is.null(mypoints)) {
        myscatter <- myscatter.rim <- NULL
      } else {
        if (length(mypoints$x) == 0) {
          myscatter <- myscatter.rim <- NULL
        } else {
          sc <- data.frame(sx = mypoints$x[crit],
                           sy = mypoints$y[crit],
                           sz = mypoints$z[crit])
          myscatter <- ggplot2::geom_point(
            data = sc,
            mapping = ggplot2::aes(
              x = sx, y = sy, color = sz),
            cex = mypoints$magnification,
            pch = mypoints$point.type,
            na.rm = TRUE)
          if (mypoints$black.rim) {
            myscatter.rim <- ggplot2::geom_point(
              data = sc,
              mapping = ggplot2::aes(x = sx, y = sy),
              cex = mypoints$magnification + 0.5,
              pch = mypoints$point.type,
              na.rm = TRUE)
          } else {
            myscatter.rim <- NULL
          }
        }
      }
      if (empty.specs) {
        mybase <- ggplot2::ggplot(
          data = sc, ggplot2::aes(x = sx, y = sy)) +
          myscatter.rim +
          myscatter
      } else {
        mybase <- ggplot2::ggplot(
          data = df, ggplot2::aes(y = Y, x = X)) +
          ggplot2::geom_raster(
            data = df, ggplot2::aes(fill = Z)) +
          myscatter.rim +
          myscatter +
          ggplot2::theme_bw() +
          ggplot2::theme(
            axis.title.x = ggplot2::element_text(size = 16),
            axis.title.y = ggplot2::element_text(size = 16,
                                                 angle = 90),
            axis.text.x = ggplot2::element_text(size = 14),
            axis.text.y = ggplot2::element_text(size = 14),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            legend.position = "right",
            legend.key = ggplot2::element_blank())
      }
      myplot <- mybase +
        ggplot2::labs(title = labels[1]) +
        ggplot2::xlab(labels[2]) +
        ggplot2::ylab(labels[3]) +
        ggplot2::coord_equal() +
        ggplot2::xlim(wrld.lim$x) +
        ggplot2::ylim(wrld.lim$y) +
        ggplot2::scale_fill_gradient2(
          labels[4], limits = c(cutp[1], cutp[3]),
          low = "light blue", mid = "orange",
          high = "firebrick ",
          midpoint = cutp[2],
          space = "rgb", na.value = "grey50",
          guide = "colourbar") +
        ggplot2::scale_color_gradient2(
          labels[4], limits = c(cutp[1],cutp[3]),
          low = "light blue", mid = "orange",
          high = "firebrick ",
          midpoint = cutp[2],
          space = "rgb", na.value = "grey50",
          guide = "colourbar") +
        wrld +
        dpcgrid +
        vectors
      .self$set.in.buffer(myplot,xpos,ypos)
    },

    surfaceplot2 = function(base, under, main, top,
                            xpos = 1, ypos = 1){
      "Surface plotter, version 2"

      ###########################
      # Base = axes and legends #
      ###########################
      labels <- base$labels
      leg.pos <- if (is.na(labels[4])) "none" else "right"
      zcut <- main$df$z
      if (all(is.null(base$xlim))) {
        xl <- c(min(main$df$x), max(main$df$x))
      } else {
        xl <- base$xlim
      }
      if (all(is.null(base$ylim))) {
        yl <- c(min(main$df$y), max(main$df$y))
      } else {
        yl <- base$ylim
      }
      if (all(is.null(base$zlim))) {
        if (all(is.null(zcut)) |
            class(zcut)[1] != "numeric") {
          cutp <- rep(NA, 3)
        } else {
          cutp <- c(min(zcut, na.rm = TRUE),
                    mean(zcut, na.rm = TRUE),
                    max(zcut, na.rm = TRUE))
        }
      } else {
        if (!is.na(base$zlim[1])) {
          lb <- base$zlim[1]
        } else {
          lb <- min(zcut, na.rm = TRUE)
        }
        if (!is.na(base$zlim[2])) {
          ub <- base$zlim[2]
        } else {
          ub <- max(zcut, na.rm = TRUE)
        }
        cutp <- c(lb, (lb + ub) / 2, ub)
      }
      base.ggplot <- ggplot2::ggplot(
        data = main$df, ggplot2::aes(y = y, x = x)) +
        ggplot2::labs(title = labels[1]) +
        ggplot2::xlab(labels[2]) +
        ggplot2::ylab(labels[3]) +
        ggplot2::xlim(xl) +
        ggplot2::ylim(yl) +
        ggplot2::coord_equal() +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(size = 16),
          axis.title.y = ggplot2::element_text(size = 16,
                                               angle = 90),
          axis.text.x = ggplot2::element_text(size = 14),
          axis.text.y = ggplot2::element_text(size = 14),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = leg.pos,
          legend.key = ggplot2::element_blank()) +
        ggplot2::scale_fill_gradient2(
          labels[4], limits = c(cutp[1], cutp[3]),
          low = "light blue", mid = "orange",
          high = "firebrick ", midpoint = cutp[2],
          space = "rgb", na.value = "grey50",
          guide = "colourbar") +
        ggplot2::scale_color_gradient2(
          labels[4],
          limits = c(cutp[1], cutp[3]),
          low = "light blue", mid = "orange",
          high = "firebrick ", midpoint = cutp[2],
          space = "rgb", na.value = "grey50",
          guide = "colourbar")

      ###############
      #Other layers #
      ###############
      under.ggplot <- .self$construct.gglayer(
        mylayer = under)
      main.ggplot <- .self$construct.gglayer(mylayer = main)
      top.ggplot <- lapply(top,
                           FUN = .self$construct.gglayer)

      #####################
      # Adding everything #
      #####################
      myplot <- base.ggplot + under.ggplot + main.ggplot
      if (length(top.ggplot) > 0) {
        for (i in 1:length(top.ggplot)) {
          myplot <- myplot + top.ggplot[[i]]
        }
      }
      .self$set.in.buffer(myplot, xpos, ypos)
    },

    sectionplot = function(specs, xpos = 1, ypos = 1,
                           mypoints = NULL){
      "Section plotter for the vectors specs$x or specs$y
      (one of the 2 is a scalar) and specs$z.
      The argument specs$labels is a character vector
      (size 4) with title and axes labels."

      is.along.x <- (all(specs$y == specs$y[1]) )
      if (is.along.x) {
        x <- specs$x
        myxlab <- specs$labels[2]
        mytitle <- paste0(specs$labels[1], " @ ",
                          specs$labels[3], " ", specs$y[1])
        gx  <- if (specs$g.factor == 0) NA else specs$g[, 1]
        vx  <- specs$vx
      } else {
        x <- specs$y
        myxlab <- specs$labels[3]
        mytitle <- paste0(specs$labels[1], " @ ",
                          specs$labels[2], " ", specs$x[1])
        gx  <- if (specs$g.factor == 0) NA else specs$g[, 2]
        vx  <- specs$vy
      }
      z <- specs$z
      labels <- specs$labels
      if (labels[2] == "Longitude") {
        xx <- (x + 180) %% 360 - 180
        gx <- (gx + 180) %% 360 - 180
      } else {
        xx <- x
      }
      df <- data.frame(X = xx, Z = z)
      dg <- data.frame(Gx = gx, Gy = rep(min(z),
                                         length(gx)))
      cutp <- c(min(z), mean(z), max(z))
      dpcgrid <- if (specs$g.factor == 0) NULL else {
        ggplot2::geom_point(
          data = dg, mapping = ggplot2::aes(x = Gx, y = Gy),
          pch = 4, cex = specs$g.factor,
          na.rm = TRUE)
      }

      if (is.null(mypoints)) {
        myscatter <- myscatter.rim <- NULL
      } else {
        cr <- (!is.nan(mypoints$z) & !is.na(mypoints$z))
        if (sum(cr) == 0) {
          myscatter <- myscatter.rim <- NULL
        } else {
          if (is.along.x) {
            sx <- mypoints$x[cr]
          } else {
            sx <- mypoints$y[cr]
          }
          sc <- data.frame(sx = sx, sy = mypoints$z[cr],
                           sz = mypoints$z[cr])
          myscatter <- geom_point(
            data = sc,
            mapping = ggplot2::aes(
              x = sx, y = sy, color = sz),
            cex = mypoints$magnification, na.rm = TRUE)
          if (mypoints$black.rim) {
            myscatter.rim <- geom_point(
              data = sc,
              mapping = ggplot2::aes(x = sx, y = sy),
              cex = mypoints$magnification + 0.5,
              na.rm = TRUE)
          } else {
            myscatter.rim <- NULL
          }
        }
      }
      myplot <- ggplot2::ggplot(
        data = df, ggplot2::aes(y = Z, x = X)) +
        ggplot2::labs(title = mytitle) +
        ggplot2::xlab(myxlab) +
        ggplot2::ylab(labels[4]) +
        ggplot2::geom_segment(
          data = df,
          mapping = ggplot2::aes(
            x = X, y = Z, xend = X + 0.75, yend = Z,
            color = Z)) +
        ggplot2::theme_bw() +
        ggplot2::coord_equal() +
        ggplot2::scale_fill_gradient2(
          labels[4], limits = c(cutp[1],cutp[3]),
          low = "light blue", mid = "orange",
          high = "firebrick ",
          midpoint = cutp[2],
          space = "rgb", na.value = "grey50",
          guide = "colourbar") +
        ggplot2::scale_color_gradient2(
          labels[4], limits = c(cutp[1], cutp[3]),
          low = "light blue", mid = "orange",
          high = "firebrick ",
          midpoint = cutp[2],
          space = "rgb", na.value = "grey50",
          guide = "colourbar") +
        myscatter.rim +
        myscatter +
        dpcgrid +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(size = 16),
          axis.title.y = ggplot2::element_text(size = 16,
                                               angle = 90),
          axis.text.x = ggplot2::element_text(size = 14),
          axis.text.y = ggplot2::element_text(size = 14),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "right",
          legend.key = ggplot2::element_blank()
        )
      .self$set.in.buffer(myplot, xpos, ypos)
    },

    graphplot = function(graph, highlight.node.name = NULL,
                         highlight.edges = 'to', xpos = 1, ypos = 1) {
      'Plot a graph and highlight a node and edges pointing to/from it'

      nargs <- length(graph$names)
      x <- sin(2 * pi * (0:(nargs - 1)) / nargs)
      y <- cos(2 * pi * (0:(nargs - 1)) / nargs)
      from.pos <- as.numeric(unlist(graph$from.pos))
      to.pos <- as.numeric(unlist(graph$to.pos))
      df <- data.frame(x = x, y = y, z = graph$names, stringsAsFactors = FALSE)
      df.arrows <- data.frame(xarrow.start = 0.95 * x[from.pos],
                              yarrow.start = 0.95 * y[from.pos],
                              xarrow.end = 0.95 * x[to.pos],
                              yarrow.end = 0.95 * y[to.pos])
      if (is.null(highlight.node.name)) {
        my.node <- my.edges <- NULL
      } else {
        stopifnot(highlight.node.name %in% df$z)
        id <- which(df$z == highlight.node.name)
        my.node <- geom_text(data = df[df$z == highlight.node.name, ],
                             aes(x = x, y = y, label = z, colour = 'red'))
        my.df <- switch(
          highlight.edges,
          'from' = df.arrows[df.arrows$xarrow.start == 0.95 * x[id] &
                               df.arrows$yarrow.start == 0.95 * y[id], ],
          'to' = df.arrows[df.arrows$xarrow.end == 0.95 * x[id] &
                             df.arrows$yarrow.end == 0.95 * y[id], ]
        )
        my.edges <- geom_segment(
          data = my.df,
          mapping = aes(x = xarrow.start, y = yarrow.start,
                        xend = xarrow.end, yend = yarrow.end),
          arrow = arrow(type = 'closed', length = unit(0.02, "npc")),
          colour = 'red')
      }
      p <- ggplot(df, aes(x = x, y = y)) +
        theme_void() + theme(legend.position = "none") +
        geom_text(data = df[df$types == 'fixed', ], aes(label = z),
                  colour = 'gray') +
        geom_text(data = df[df$types != 'fixed', ], aes(label = z)) +
        my.node +
        geom_segment(data = df.arrows,
                     mapping = aes(x = xarrow.start, y = yarrow.start,
                                   xend = xarrow.end, yend = yarrow.end),
                     arrow = arrow(type = 'closed', length = unit(0.02, "npc"))) +
        my.edges
      .self$set.in.buffer(p, xpos, ypos)
    }
  )
  )
