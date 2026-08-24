#------ WRAPPER FUNCTION

hHelix <-
  function(pts.per.hour = 1,
           t.radius = sphere.radius * 1.25,
           t.alpha = 1,
           start = min(skin.df$datetime, na.rm = TRUE),
           end = max(skin.df$datetime, na.rm = TRUE),
           periods,
           radii,
           sphere.radius = radii[length(radii)] * 0.25,
           mark.start = TRUE,
           skin.df = NULL,
           
           
           marker.df = NULL,
           marker.radius = t.radius,
           marker.color = "red",
           marker.protrusion = 0,
           marker.alpha = 1,
           
           
           time.line = NULL,
           time.line.color = "white",
           
           
           text.marker.df = NULL,
           text.marker.radius = t.radius,
           text.marker.color = "cyan",
           text.marker.protrusion = 0,
           text.protrusion = 0,
           text.cex = 1,
           
           
           return = FALSE,
           ...) {
    dots <- list(...)
    
    checkmate::assert_named(dots)
    
    
    
    if (is.null(start) &
        !is.null(c(unlist(skin.df$datetime), unlist(marker.df$datetime)))) {
      start <- min(skin.df$datetime, na.rm = TRUE)
    }
    
    
    if (is.null(end) &
        !is.null(c(unlist(skin.df$datetime), unlist(marker.df$datetime)))) {
      end <- max(skin.df$datetime, na.rm = TRUE)
    }
    
    
    
    #----PROCESS HELIX ORDER
    
    
    
    order <- 1:length(periods)
    
    
    if (length(order) > 1) {
      radii <- radii[order]
      ratios <- c(periods[1] / periods[2])
    }
    
    if (length(order) == 1) {
      radii <- radii[1:length(order)]
      ratios <- periods[order]
    }
    
    
    if (length(order) == 3)
      ratios <-      c(periods[1] / periods[2] / periods[3], periods[2] / periods[3])
    
    
    
    
    if (!is.null(marker.df)) {
      t <- marker.df$datetime
      if (!is.null(marker.df$col)) {
        marker.color <- marker.df$col
      }
    } else{
      t <- NULL
    }
    
    
    
    
    size <-  as.numeric(difftime(end, start, units = "hours")) * pts.per.hour
    
    HelixInput = do.call("CreateHelixInput", append(
      list(
        t = t,
        size = size,
        main.loop.duration = periods[1],
        start = start,
        end = end
      ),
      match_from_dots(
        dots,
        CreateHelixInput,
        skip = c("t", "main.loop.duration", "start", "end", "size")
      )
    ))
    
    
    
    ###################################################################
    #-----CREATE RENDERED AND (POTENTIALLY) COLOR-CODED HELIX
    ###################################################################
    
    print("Creating helix")
    
    helix = do.call("CalcHelix", append(
      list(
        t = HelixInput$t.structural$t.radians,
        ratios = ratios ,
        radii = radii
      ),
      match_from_dots(dots, CalcHelix, skip = c("t", "ratios", "radii"))
    ))
    
    
    
    
    if (is.null(skin.df)) {
      cols <- c("purple", "#FFD700", "turquoise")
      my.cols <- cols[length(periods)]
    } else{
      skin.df <-
        skin.df[skin.df$datetime >= start &
                  skin.df$datetime <= end, ]  #--reduce to focal window
      
      #----FIND VALUE BRACKET FOR HELIX POINTS
      
      hlx.value <- data.frame(t.posix = HelixInput$t.structural$t.posix)
      hlx.value$index <- findInterval(HelixInput$t.structural$t.posix, sort(skin.df$datetime))
      hlx.value$value.posix <- hlx.value$t.posix#--just to make sure it's posix format
      hlx.value$value.posix[hlx.value$index > 0] <- sort(skin.df$datetime)[hlx.value$index]
      hlx.value$value.posix[hlx.value$index <= 0] <- NA
      
      
      hlx.value$value.posix.num <- as.numeric(hlx.value$value.posix)
      skin.df$datetime.num <- as.numeric(skin.df$datetime)
      hlx.value <- merge(
        hlx.value,
        skin.df,
        by.x = "value.posix.num",
        by.y = "datetime.num",
        all.x = TRUE,
        sort = FALSE
      )
      
      
      my.cols <- hlx.value$col
      
      
    }
    
    if (length(order) > 1)
      i.last <-
      which(abs(HelixInput$t.structural$t.radians - 2 * pi) == min(abs(
        HelixInput$t.structural$t.radians - 2 * pi
      )))[1]
    if (length(order) == 1)
      i.last <-
      which(abs(HelixInput$t.structural$t.radians - 4 * pi) == min(abs(
        HelixInput$t.structural$t.radians - 4 * pi
      )))[1]
    
    i <- 1:i.last
    plot3d(
      x = helix[i, 1],
      y = helix[i, 2],
      z = helix[i, 3],
      type = 'n',
      xlab = "",
      ylab = "",
      zlab = "",
      box = TRUE,
      axes = FALSE,
      ann = FALSE
    )
    plot3d(
      x = helix[, 1],
      y = helix[, 2],
      z = helix[, 3],
      type = 's',
      radius = sphere.radius,
      #radii[length(radii)] * 0.25,
      lwd = 20,
      col = my.cols,
      add = TRUE
    )
    
    
    
    
    ###################################################################
    #-----ADD TIMED MARKERS (OBSERVATIONS) TO HELIX
    ###################################################################
    
    if (!is.null(HelixInput$t.empirical$t.radians)) {
      print("Adding timed markers to plot")
      helix.dat = do.call("CalcHelix", append(
        list(
          t = HelixInput$t.empirical$t.radians,
          radii = radii + c(
            rep(0, length(radii) - 1),
            radii[length(radii)] * marker.protrusion + sphere.radius *  #0 instead of marker.protrusion
              1.1
          ),
         
          ratios = ratios
        ),
        match_from_dots(dots, CalcHelix, skip = c("t", "ratios", "radii"))
      ))
      
      
      rgl.spheres(
        x = helix.dat[, 1],
        y = helix.dat[, 2],
        z = helix.dat[, 3],
        type = 'p',
        r = marker.radius,
        col = marker.color,
        alpha = marker.alpha
      )
      
      
      
      
    }
    
    
    
    
    ###################################################################
    #-----ADD TEXT MARKERS TO HELIX
    ###################################################################
    
    if (!is.null(text.marker.df)) {
      TextHelixInput = do.call("CreateHelixInput", append(
        list(
          t = text.marker.df$datetime,
          size = size,
          main.loop.duration = periods[order[1]],
          start = start,
          end = end
        ),
        match_from_dots(
          dots,
          CreateHelixInput,
          skip = c("t", "main.loop.duration", "start", "end", "size")
        )
      ))
      
      
      TextHelixMarker = do.call("CalcHelix", append(
        list(
          t = TextHelixInput$t.empirical$t.radians,
          radii = radii + c(
            rep(0, length(radii) - 1),
            radii[length(radii)] * text.marker.protrusion + sphere.radius *  # achieves protrusion of the line
              1.1
          ),
          ratios = ratios
        ),
        match_from_dots(dots, CalcHelix, skip = c("t", "radii", "ratios"))
      ))
      
      
      rgl.spheres(
        x = TextHelixMarker[, 1],
        y = TextHelixMarker[, 2],
        z = TextHelixMarker[, 3],
        type = 'p',
        r = text.marker.radius,
          col = text.marker.color,
        alpha = t.alpha
      )
      
      
      TextHelix = do.call("CalcHelix", append(
        list(
          t = TextHelixInput$t.empirical$t.radians,
          
          radii = radii + c(
            rep(0, length(radii) - 1),
            radii[length(radii)] * text.protrusion + sphere.radius * 2
          ),
         
          
          ratios = ratios
        ),
        match_from_dots(dots, CalcHelix, skip = c("t", "radii", "ratios"))
      ))
      
      
      text3d(
        x = TextHelix[, 1],
        y = TextHelix[, 2],
        z = TextHelix[, 3],
        texts =  text.marker.df$text,
       
        adj = c(0, 0),
        color = text.marker.color,
        cex = text.cex
        ,
        add = TRUE
      )
      
      segments3d(
        x = rbind(TextHelixMarker[, 1], TextHelix[, 1]),
        y = rbind(TextHelixMarker[, 2], TextHelix[, 2]),
        z = rbind(TextHelixMarker[, 3], TextHelix[, 3]),
        color = text.marker.color,
        alpha = t.alpha,
        lwd = 1
      )
    }
    
    ###################################################################
    #-----SHOW SOME SPECIFIC TIME, LIKE MIDNIGHT, NOON, ETC.
    ###################################################################
    
    if (3 %in% order & !is.null(time.line)) {
      print("Adding time line to plot")
      
      lapply(1:length(time.line), function(i) {
        time.line.posix <-
          as.POSIXct(paste(sort(unique(
            as.POSIXct(format(
              HelixInput$t.structural$t.posix, "%Y-%m-%d"
            ))
          )), time.line[i]))
        
        HelixInput = do.call("CreateHelixInput", append(
          list(
            t = time.line.posix,
            size = size,
            main.loop.duration = periods[order[1]],
            start = start,
            end = end
          ),
          match_from_dots(
            dots,
            CreateHelixInput,
            skip = c("t", "main.loop.duration", "start", "end", "size")
          )
        ))
        
        
        HelixMarker = do.call("CalcHelix", append(
          list(
            t = HelixInput$t.empirical$t.radians,
            radii = radii + c(
              rep(0, length(radii) - 1),
              radii[length(radii)] * 0 + sphere.radius *  # achieves protrusion of the line
                1.1
            ),
            ratios = ratios
          ),
          match_from_dots(dots, CalcHelix, skip = c("t", "radii", "ratios"))
        ))
        
        
        
        plot3d(
          x = HelixMarker[, 1],
          y = HelixMarker[, 2],
          z = HelixMarker[, 3],
          type = 'l',
          lwd = 2,
          col = time.line.color[i],
          add = TRUE
        )
        
      })
    }
    
    ###################################################################
    #-----MARK STARTING AND END POINT OF HELIX
    ###################################################################
    
    if (mark.start) {
      print("Adding start marker to plot")
      
      helix.dat = do.call("CalcHelix", append(
        list(
          t =  HelixInput$t.structural$t.radians[c(1, length(HelixInput$t.structural$t.radians))],
          radii = radii,
          ratios = ratios
        ),
        match_from_dots(dots, CalcHelix, skip = c("t", "ratios", "radii"))
      ))
      
      
      rgl.spheres(
        x = helix.dat[, 1],
        y = helix.dat[, 2],
        z = helix.dat[, 3],
        type = 'p',
        r = sphere.radius * 1.1,
        col = c("darkgreen", "brown"),
        alpha = t.alpha
      )
      
      
      if (!is.null(text.marker.df)) {
        StartTextHelix = do.call("CalcHelix", append(
          list(
            t = HelixInput$t.structural$t.radians[c(1, length(HelixInput$t.structural$t.radians))],
            
            radii = radii + c(
              rep(0, length(radii) - 1),
              radii[length(radii)] * text.protrusion * 2 + sphere.radius * 1.1
            ),
           
            ratios = ratios
          ),
          match_from_dots(dots, CalcHelix, skip = c("t", "radii", "ratios"))
        ))
        
     
        
        start.text <- paste(
          c("Start: ", "End: "),
          
          format(HelixInput$t.structural$t.posix[c(1, length(HelixInput$t.structural$t.posix))], "%Y-%m-%d")
          
          
        )
        
        text3d(
          x = StartTextHelix[, 1],
          y = StartTextHelix[, 2],
          z = StartTextHelix[, 3],
          texts =  start.text,
         
          adj = c(0, 0),
          color = c("darkgreen", "brown"),
          cex = text.cex
          ,
          add = TRUE
        )
        
        segments3d(
          x = rbind(helix.dat[1, 1], StartTextHelix[1, 1]),
          y = rbind(helix.dat[1, 2], StartTextHelix[1, 2]),
          z = rbind(helix.dat[1, 3], StartTextHelix[1, 3]),
          color = c("darkgreen"),
          alpha = t.alpha,
          lwd = 1
        )
        
        
        segments3d(
          x = rbind(helix.dat[2, 1], StartTextHelix[2, 1]),
          y = rbind(helix.dat[2, 2], StartTextHelix[2, 2]),
          z = rbind(helix.dat[2, 3], StartTextHelix[2, 3]),
          color = c("brown"),
          alpha = t.alpha,
          lwd = 1
        )
      }
      
    }
    
    if (return)
      return(helix)
    
  }
