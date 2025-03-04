

#########################################################
#     LUNAR AND SOLAR ILLUMINATION
#########################################################

#----

GetSolarLunar <- function(t , lonlat){
  
  out<-data.frame(datetime=t)
  #crepuscule(t(as.matrix(c(10.7771632517409,59.66546183649254))),as.POSIXct(u.posix), , solarDep=18,direction="dawn", POSIXct.out=TRUE)$time
  out$sunrise<-sunriset(t(as.matrix(lonlat)),as.POSIXct(t), direction="sunrise", POSIXct.out=TRUE)$time
  out$sunset<-sunriset(t(as.matrix(lonlat)),as.POSIXct(t), direction="sunset", POSIXct.out=TRUE)$time
  out$daylight <- t>out$sunrise & t<out$sunset
  #--because of polar winter and summer
  out$sunaltitude<-getSunlightPosition(date = as.POSIXct(out$datetime), lat = lonlat[2], lon = lonlat[1],
                                       data = NULL, keep = c("altitude"))$altitude
  out$daylight<-ifelse(!is.na(out$daylight),out$daylight,out$sunaltitude>=0)
  out$moonlight<-getMoonIllumination(date = as.POSIXct(t), keep = c("fraction"))$fraction
  return(out)
}


# match_from_dots <- function(dots, fn){ #SOURCE: https://community.rstudio.com/t/dots-vs-arg-lists-for-function-forwarding/4995
#   arg <- match(names(formals(fn)), names(dots))
#   dots[arg[!is.na(arg)]]
# }
#########################################################
#     ...
#########################################################

match_from_dots <- function(dots, fn,skip=NULL){
  arg <- match(names(formals(fn)), names(dots))
  #print(arg)
  dots<-dots[arg[!is.na(arg)]]
  dots<-dots [!names(dots)%in%skip]
  #print(dots)
  dots
}
#########################################################
#     FUNCTION TO PREPATE INPUT DATA
#########################################################


CreateHelixInput<-function(t = NULL,size,start=NULL,end=NULL,main.loop.duration){
  
  
  if(is.null(start)) start <- as.POSIXct(paste(min(t)))#start.time - as.difftime(5, unit="days")
  if(is.null(end)) end<- as.POSIXct(paste(max(t)))#start.time - as.difftime(5, unit="days")
  
  t<-t[t>=start & t<=end]
  
  
  duration<-as.numeric(difftime(end,start,units = "days"))
  #print(duration)
  
  n.main.loops<-duration/main.loop.duration
  #print(n.main.loops)
  t.posix <- seq(start,end,length.out=size)
  t.posix.diff<-as.numeric(difftime(t.posix,start,units="days"))
  t.radians <-  t.posix.diff * 2*pi*n.main.loops/duration
  
  if(!is.null(t)){
    empirical.t.posix.diff <- as.numeric(difftime(t, start,unit="days"))
    empirical.t.radians <- empirical.t.posix.diff * 2*pi*n.main.loops/duration
  }
  
  #plot(I(t.radians)~t.radians)
  #if(!is.null(t))points(I(empirical.t.radians)~empirical.t.radians,col="red",pch=19,cex=2)
  
  if(!is.null(t))   out<-list(t.structural=data.frame(t.posix,t.posix.diff,t.radians),
                              t.empirical=data.frame(t.posix=t,t.posix.diff=empirical.t.posix.diff,t.radians=empirical.t.radians)
  )
  
  if(is.null(t))  out<-list(t.structural=data.frame(t.posix,t.posix.diff,t.radians),
                            t.empirical=NULL)
  
  
  return(out)
  
}


#########################################################
#     FUNCTIONS TO CALCULATE POSITIONS ALONG AND BUILD HELICAL STRUCTURE
#########################################################

# n.points: number of points to use to construct the smallest loop. More points: smoother and slower

CalcHelix_NEW <- function(t=NULL, n.loops=c(2,12,30), n.points=10, radius=c(2,0.5,0.1), h=NULL,  plot=FALSE, tangent=TRUE,returnList=FALSE){
  
  if(is.null(h)){
    h<-1/(2*pi)
    if(length(radius)>1)   h<-radius[1]/pi#--TO GET PROPER ASPECT
    
  }
  
  if(is.null(t)) t <- seq(0,2*pi*n.loops[1],length.out=n.points*prod(n.loops)) 
  
  u<-n.loops[2]*t
  try(w<-n.loops[3]*u,silent=TRUE)
  R<-radius[1]
  try(a<-radius[2],silent=TRUE)
  try(b<-radius[3],silent=TRUE)
  
  
  y.2nd<-NULL
  y.3rd<-NULL
  
  
  
  
  #---------------------------------------
  #---------------------------------------
  #---------------1st ORDER
  #---------------------------------------
  #---------------------------------------
  y.expr<-list(quote(h*t),quote(R*cos(t)),quote(R*sin(t)))
  
  #--evaluate for t
  y.1st <- do.call(cbind,lapply(y.expr,function(x)eval(x)))
  
  
  if(plot){
    par(mar=c(0,0,0,0))
    
    i.last<-which(abs(t-2*pi)==min(abs(t-2*pi)))[1]
    i<-1:i.last
    
    plot3d( #---FOR PROPER ASPECT
      x=y.1st[i,1], y=y.1st[i,2], z=y.1st[i,3], 
      type = 'n'
    )
    
    plot3d( 
      x=y.1st[,1], y=y.1st[,2], z=y.1st[,3], 
      type = 'l',
      col = "purple",lwd=3,add=TRUE
    )
  }
  
  out <- y.1st
  
  if(length(radius)>1){
    #---------------------------------------
    #---------------------------------------
    #---------------SHORTER-FASTER SECOND ORDER (TO SPEED THINGS UP LATER) --->https://math.stackexchange.com/questions/470538/helix-in-a-helix
    #---------------------------------------
    #---------------------------------------
    
    if(!tangent){
      
      y.expr<-list(quote(h*t),quote(R*cos(t)+a*cos(u)),quote(R*sin(t)+a*sin(u)))
      y.2nd <- do.call(cbind,lapply(y.expr,function(x)eval(x)))
      
    }else{
      
      y.expr<-list(quote(h*t),quote(R*cos(t)),quote(R*sin(t)))
      y.tangent.expr <- list(quote(h),quote(-R*sin(t)),quote(R*cos(t)))
      y.normal.unit.expr <- list(quote(0),quote(-cos(t)),quote(-sin(t))) 
      y.binormal.unit.expr <- list(quote(R/(sqrt(R^2+h^2))), quote(h*sin(t)/(sqrt(R^2+h^2))),quote(-h*cos(t)/(sqrt(R^2+h^2))))
      y.final.path.expr<-lapply(1:3,function(i)substitute(A+a*B*cosu+a*C*sinu,list(A=y.expr[[i]],B=y.normal.unit.expr[[i]],C=y.binormal.unit.expr[[i]],a=quote(a),cosu=quote(cos(u)),sinu=quote(sin(u)))))
      
      
      #--evaluate for t
      y.2nd <- do.call(cbind,lapply(y.final.path.expr,function(x)eval(x)))
    }
    
    if(plot){
      par(mar=c(0,0,0,0))
      plot3d( 
        x=y.2nd[,1], y=y.2nd[,2], z=y.2nd[,3], 
        type = 'l',
        col = "#FFD700",add=TRUE,lwd=2
      )
    }
    out <- y.2nd
  }
  
  
  
  #---------------------------------------
  #---------------------------------------
  #---------------THIRD ORDER
  #---------------------------------------
  #---------------------------------------
  if(length(radius)>2){#---BLOCK ALL
    
    
    if(!tangent){
      
      y.expr<-list(quote(h*t),quote(R*cos(t)+a*cos(u)+b*cos(w)),quote(R*sin(t)+a*sin(u)+b*sin(w)))
      y.3rd <- do.call(cbind,lapply(y.expr,function(x)eval(x)))
      
      
    }else{
      
      #--FORMULATING THE EXPRESSION (USING R's DERIVATIVE CALCULATOR)
      
      y.expr <- y.final.path.expr#list(expression(h*t),expression(R*cos(t)),expression(R*sin(t)))
      y.tangent.expr <- list(D(substitute(A,list(A=y.expr[[1]])),"u"),D(substitute(A,list(A=y.expr[[2]])),"u"),D(substitute(A,list(A=y.expr[[3]])),"u"))
      magnitude.tangent.expr <- substitute(sqrt(A^2 + B^2 + C^2), list(A=y.tangent.expr[[1]], B=y.tangent.expr[[2]], C=y.tangent.expr[[3]]))
      y.tangent.unit.expr <- lapply(y.tangent.expr,function(x)substitute((A)/(B),list(A=x,B=magnitude.tangent.expr)))
      
      y.normal.expr <-  list( (D(y.tangent.unit.expr[[1]],"u")),  (D(y.tangent.unit.expr[[2]],"u")) , (D(y.tangent.unit.expr[[3]],"u")))
      magnitude.normal.expr <-substitute(sqrt(A^2 + B^2 + C^2), list(A=y.normal.expr[[1]], B=y.normal.expr[[2]], C=y.normal.expr[[3]]))
      y.normal.unit.expr <- lapply(y.normal.expr ,function(x)substitute((A)/(B),list(A=x,B=magnitude.normal.expr)))
      
      #--indices shifted: the z,x,y below are x,y,z in my version
      Cx <- substitute((ay) * (bz) - (az) * (by),list(ay=y.tangent.unit.expr[[2]],bz=y.normal.unit.expr[[3]], az=y.tangent.unit.expr[[3]], by=y.normal.unit.expr[[2]]))
      Cy <- substitute((az) * (bx) - (ax) * (bz),list(az=y.tangent.unit.expr[[3]],bx=y.normal.unit.expr[[1]], ax=y.tangent.unit.expr[[1]], bz=y.normal.unit.expr[[3]]))
      Cz <- substitute((ax) * (by) - (ay) * (bx),list(ax=y.tangent.unit.expr[[1]],by=y.normal.unit.expr[[2]], ay=y.tangent.unit.expr[[2]], bx=y.normal.unit.expr[[1]]))
      
      y.binormal.expr <- list(Cx, Cy,Cz)
      magnitude.binormal.expr <-substitute(sqrt(A^2 + B^2 + C^2), list(A=y.binormal.expr[[1]], B=y.binormal.expr[[2]], C=y.binormal.expr[[3]]))
      y.binormal.unit.expr <- lapply(y.binormal.expr ,function(x)substitute((A)/(B),list(A=x,B=magnitude.binormal.expr)))
      
      
      # ###################################
      # #--EVALUATING THE EXPRESSIONS (FOR t) --- takes a long time for 3rd order helix
      # 
      # y2 <- do.call(cbind,lapply(y.expr,function(x)eval(x)))
      # y.tangent <- do.call(cbind,lapply(y.tangent.expr,function(x)eval(x)))
      # y.tangent.unit <- do.call(cbind,lapply(y.tangent.unit.expr,function(x)eval(x)))
      # y.normal.unit <- do.call(cbind,lapply(y.normal.unit.expr,function(x)eval(x)))
      # y.binormal.unit <- do.call(cbind,lapply(y.binormal.unit.expr,function(x)eval(x)))
      
      
      y.final.path.expr<-lapply(1:3,function(i)substitute(A+a*B*cosu+a*C*sinu,list(A=y.expr[[i]],B=y.normal.unit.expr[[i]],C=y.binormal.unit.expr[[i]],a=quote(b),cosu=quote(cos(w)),sinu=quote(sin(w)))))
      #eval(temp[[1]])
      
      ###################################
      #--EVALUATING THE EXPRESSIONS (FOR t)
      
      y <- do.call(cbind,lapply(y.expr,function(x)eval(x)))
      y.tangent <- do.call(cbind,lapply(y.tangent.expr,function(x)eval(x)))
      y.tangent.unit <- do.call(cbind,lapply(y.tangent.unit.expr,function(x)eval(x)))
      y.normal.unit <- do.call(cbind,lapply(y.normal.unit.expr,function(x)eval(x)))
      y.binormal.unit <- do.call(cbind,lapply(y.binormal.unit.expr,function(x)eval(x)))
      
      #--evaluate for t
      y.3rd <- do.call(cbind,lapply(y.final.path.expr,function(x)eval(x)))
      
      # 
      # 
      # 
      # # ###################################
      # # #--CHECKING THAT VECTORS ARE NORMALIZED
      # # table(apply(y.tangent.unit,1,function(x)sqrt(sum(x^2)))) 
      # # table(apply(y.normal.unit,1,function(x)sqrt(sum(x^2))))
      # # table(apply(y.binormal.unit,1,function(x)sqrt(sum(x^2))))
      # 
      # #---THE FINAL CONSTRUCT (PATH)
      # 
      # y.3rd <- y2 + b*y.normal.unit*cos(w) + b*y.binormal.unit*sin(w)
    }
    
    
    if(plot){
      par(mar=c(0,0,0,0))
      plot3d( 
        x=y.3rd[,1], y=y.3rd[,2], z=y.3rd[,3], 
        type = 'l',
        col = "turquoise",lwd=1,add=TRUE
      )
    }
    out <- y.3rd
  }#
  
  
  
  
  if(!returnList){
    return(out)
  }else{
    return(list(FirstOrder=y.1st,SecondOrder=y.2nd,ThirdOrder=y.3rd))
  }
  
  
  
  
}

#########################################################
#     WRAPPER FUNCTION TO CREATE HELIX
#########################################################

WrapHelix_NEW <-
  function(order = 1:3,
           t.color="red",
           t.radius = sphere.radius * 1.25,
           t.alpha = 1,
           periods,
           radii,
           sphere.radius = radii[length(radii)]*0.25,
           
           cov.df = NULL,
           cov.col = NULL,
           time.line = NULL,
           time.line.color = "white",#"00:00:00"
           show.lunar = TRUE,
           return =FALSE,
           ...) {
    dots <- list(...)
    
    checkmate::assert_named(dots)
    
    
    #----PROCESS HELIX ORDER
    
    print(radii)
    
    if (length(order) > 1)
      radii <- radii[order]
    if (length(order) == 1)
      radii <- radii[1:length(order)]
    
    
    ratios <-
      c(periods[1] / periods[2] / periods[3], periods[2] / periods[3])
    
    try(if (identical(order, c(1, 3)))
      ratios <- c(periods[1] / periods[3]), silent = TRUE)
    try(if (identical(order, c(2, 3)))
      ratios <- c(periods[2] / periods[3]), silent = TRUE)
    try(if (identical(order, c(1, 2)))
      ratios <- c(periods[1] / periods[2]), silent = TRUE)
    
    try(if (length(order) == 1)
      ratios <- periods[order])
    
    
    print(radii)
    
    HelixInput = do.call("CreateHelixInput", append(
      list(main.loop.duration = periods[order[1]]),
      match_from_dots(dots, CreateHelixInput, skip = c("main.loop.duration"))
    ))
    
    print("Done CreateHelixInput")
    
    
    
    helix = do.call("CalcHelix", append(
      list(
        t = HelixInput$t.structural$t.radians,
        ratios = ratios ,
        radii = radii
      ),
      match_from_dots(dots, CalcHelix, skip = c("t", "ratios", "radii"))
    ))
    

    print("Done CalcHelix")

    if (is.null(cov.df)) {
      print("Using sun and moon lighting for color")
      HelixInput$SolarLunar = do.call("GetSolarLunar", append(
        list(t = HelixInput$t.structural$t.posix),
        match_from_dots(dots, GetSolarLunar, skip = "t")
      ))
      print("Done GetSolarLunar")
      
      sunglow <- brocolors("crayons")["Sunglow"]
      midnightblue <- brocolors("crayons")["Midnight Blue"]
      my.cols <-
        ifelse(
          HelixInput$SolarLunar$daylight,
          sunglow,
          grey(HelixInput$SolarLunar$moonlight)
        )
      if (!show.lunar)
        my.cols <-
        ifelse(HelixInput$SolarLunar$daylight, sunglow, grey(0.1))
    } else{
      print("Using covariate for color")
      
      
      
      cov.df<-cov.df[order(cov.df$datetime),]
      
      cov.df <-
        cov.df[cov.df$datetime >= start &
                 cov.df$datetime <= end, ]  #--reduce to focal window
      
      #----FIND VALUE BRACKET FOR HELIX POINTS
      
      hlx.value<-data.frame(t.posix=HelixInput$t.structural$t.posix)
      hlx.value$index<-findInterval(HelixInput$t.structural$t.posix,sort(cov.df$datetime))
      hlx.value$value.posix<-hlx.value$t.posix#--just to make sure it's posix format
      hlx.value$value.posix[hlx.value$index>0]<-sort(cov.df$datetime)[hlx.value$index]
      hlx.value$value.posix[hlx.value$index<=0]<-NA
      
      
      head(cov.df)
      hlx.value$value.posix.num<-as.numeric(hlx.value$value.posix)
      cov.df$datetime.num<-as.numeric(cov.df$datetime)
      hlx.value<-merge(hlx.value,cov.df,by.x="value.posix.num",by.y="datetime.num",all.x=TRUE,sort=FALSE)
      head(hlx.value)
      
      
      my.cols<-hlx.value$col
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
      radius = sphere.radius,#radii[length(radii)] * 0.25,
      lwd = 20,
      col = my.cols,
      add = TRUE
    )
    
    try({
      helix.dat = do.call("CalcHelix", append(
        list(
          t = HelixInput$t.empirical$t.radians,
          radii = radii, #+ c(rep(0, length(radii) - 1), radii[length(radii)] * 0.25 * 1),
          ratios = ratios
        ),
        match_from_dots(dots, CalcHelix, skip = c("t", "ratios", "radii"))
      ))
     
      
      print("Done CalcHelix Data")
      
      rgl.spheres(
        x = helix.dat[, 1],
        y = helix.dat[, 2],
        z = helix.dat[, 3],
        type = 'p',
        r = t.radius,#radii[length(radii)] * 0.25 / 2,
        col = t.color,
        alpha=t.alpha
      ) 
      
    },
    silent = TRUE)
    print("Done Adding Extra Markers to Plot")
    
    #-----SHOW SOME SPECIFIC TIME, LIKE MIDNIGHT (could also show full moon and new year)
    
    if (3 %in% order & !is.null(time.line)) {
      time.line.posix <-
        as.POSIXct(paste(sort(unique(
          as.POSIXct(format(
            HelixInput$t.structural$t.posix, "%Y-%m-%d"
          ))
        )), time.line))
      
      HelixInput = do.call("CreateHelixInput", append(
        list(t = time.line.posix, main.loop.duration = periods[order[1]]),
        match_from_dots(dots, CreateHelixInput, skip = c("t", "main.loop.duration"))
      ))
      print("Done CreateHelixInput Marker")
      
      HelixMarker = do.call("CalcHelix", append(
        list(
          t = HelixInput$t.empirical$t.radians,
          radii = radii + c(rep(0, length(radii) - 1), radii[length(radii)] * 0 + sphere.radius*1.1),
          ratios = ratios
        ),
        match_from_dots(dots, CalcHelix, skip = c("t", "radii", "ratios"))
      ))

      print("Done CalcHelix Markers")
      
      
      
      
      plot3d(
        x = HelixMarker[, 1],
        y = HelixMarker[, 2],
        z = HelixMarker[, 3],
        type = 'l',
        lwd = 2,
        col = time.line.color,
        add = TRUE
      )
    }
    
    if(return)return(helix)
  }
