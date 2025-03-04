
hHelix <-
  function(#order = 1:3,
    pts.per.hour = 1,
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
    #cov.col = NULL,
    time.line = NULL,
    time.line.color = "white",
    #"00:00:00"
    show.lunar = TRUE,
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
    
    

    
    dat <- tibble(datetime = seq(start, end, by = "hour"))
    
    
    if (!is.null(marker.df)) {
      t <- marker.df$datetime
      t.color <- marker.df$col
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
     
     cols<-c("purple","#FFD700","turquoise")
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
      
      
      head(skin.df)
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
      head(hlx.value)
      
      
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
    #-----ADD TIMED MARKERS TO HELIX
    ###################################################################
    
    if (!is.null(HelixInput$t.empirical$t.radians)) {
      print("Adding timed markers to plot")
      helix.dat = do.call("CalcHelix", append(
        list(
          t = HelixInput$t.empirical$t.radians,
          radii = radii,
          #+ c(rep(0, length(radii) - 1), radii[length(radii)] * 0.25 * 1),
          ratios = ratios
        ),
        match_from_dots(dots, CalcHelix, skip = c("t", "ratios", "radii"))
      ))
      
      
      
      
      
      rgl.spheres(
        x = helix.dat[, 1],
        y = helix.dat[, 2],
        z = helix.dat[, 3],
        type = 'p',
        r = t.radius,
        #radii[length(radii)] * 0.25 / 2,
        col = t.color,
        alpha = t.alpha
      )
      
      
    }
    
    
    ###################################################################
    #-----SHOW SOME SPECIFIC TIME, LIKE MIDNIGHT (could also show full moon and new year)
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
            radii = radii + c(rep(0, length(radii) - 1), radii[length(radii)] * 0 + sphere.radius *
                                1.1),
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
    #-----MARK STARTING POINT OF HELIX
    ###################################################################
    
    if (mark.start) {
      print("Adding Start Marker to Plot")
      
      helix.dat = do.call("CalcHelix", append(
        list(
          t =  HelixInput$t.structural$t.radians[1],
          radii = radii,
          #+ c(rep(0, length(radii) - 1), radii[length(radii)] * 0.25 * 1),
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
        #radii[length(radii)] * 0.25 / 2,
        col = "magenta",
        alpha = t.alpha
      )
      
      
      
      
    }
    
    if (return)
      return(helix)
    
  }
