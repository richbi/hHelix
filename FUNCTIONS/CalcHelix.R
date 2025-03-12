#----HIERARCHICAL HELIX CONSTRUCTION

#---USEFUL PAGES:
#https://math.stackexchange.com/questions/470538/helix-in-a-helix
#https://mathematica.stackexchange.com/questions/18598/finding-unit-tangent-normal-and-binormal-vectors-for-a-given-rt
#https://math.stackexchange.com/questions/974178/how-to-calculate-the-angle-between-2-vectors-in-3d-space-given-a-preset-function
#https://math.stackexchange.com/questions/461547/whats-the-equation-of-helix-surface/461637#461637
#https://math.stackexchange.com/questions/470538/helix-in-a-helix
#https://math.stackexchange.com/questions/3481477/centering-a-helix-along-an-arbitrary-vector-in-3d
#https://math.stackexchange.com/questions/3451205/find-normal-vector-of-a-3d-vector
#https://mathinsight.org/parametrized_curve_tangent_line_examples
#https://www.mathsisfun.com/algebra/vectors-cross-product.html


CalcHelix <- function(t,
                      ratios,
                      radii,
                      h = NULL,
                      plot = FALSE,
                      tangent = TRUE,
                      returnList = FALSE) {
  if (is.null(h)) {
    h <- 1 / (2 * pi)
    if (length(radii) > 1)
      h <- radii[1] / pi#--TO GET PROPER ASPECT
    
  }
  
  u <- ratios[1] * t
  try(w <- ratios[2] * u, silent = TRUE)
  R <- radii[1]
  try(a <- radii[2], silent = TRUE)
  try(b <- radii[3], silent = TRUE)
  
  
  y.2nd <- NULL
  y.3rd <- NULL
  
  
  
  
  #---------------------------------------
  #---------------------------------------
  #---------------1st ORDER
  #---------------------------------------
  #---------------------------------------
  y.expr <- list(quote(h * t), quote(R * cos(t)), quote(R * sin(t)))
  
  #--evaluate for t
  y.1st <- do.call(cbind, lapply(y.expr, function(x)
    eval(x)))
  
  
  if (plot) {
    par(mar = c(0, 0, 0, 0))
    
    i.last <- which(abs(t - 2 * pi) == min(abs(t - 2 * pi)))[1]
    i <- 1:i.last
    
    plot3d(
      #---FOR PROPER ASPECT
      x = y.1st[i, 1],
      y = y.1st[i, 2],
      z = y.1st[i, 3],
      type = 'n'
    )
    
    plot3d(
      x = y.1st[, 1],
      y = y.1st[, 2],
      z = y.1st[, 3],
      type = 'l',
      col = "purple",
      lwd = 3,
      add = TRUE
    )
  }
  
  out <- y.1st
  
  if (length(radii) > 1) {
    #---------------------------------------
    #---------------------------------------
    #---------------SECOND ORDER
    #---------------------------------------
    #---------------------------------------
    
    if (!tangent) {
      y.expr <- list(quote(h * t), quote(R * cos(t) + a * cos(u)), quote(R * sin(t) +
                                                                           a * sin(u)))
      y.2nd <- do.call(cbind, lapply(y.expr, function(x)
        eval(x)))
      
    } else{
      y.expr <- list(quote(h * t), quote(R * cos(t)), quote(R * sin(t)))
      y.tangent.expr <- list(quote(h), quote(-R * sin(t)), quote(R * cos(t)))
      y.normal.unit.expr <- list(quote(0), quote(-cos(t)), quote(-sin(t)))
      y.binormal.unit.expr <- list(quote(R / (sqrt(R^2 + h^2))), quote(h *
                                                                         sin(t) / (sqrt(R^2 + h^2))), quote(-h * cos(t) / (sqrt(R^2 + h^2))))
      y.final.path.expr <- lapply(1:3, function(i)
        substitute(
          A + a * B * cosu + a * C * sinu,
          list(
            A = y.expr[[i]],
            B = y.normal.unit.expr[[i]],
            C = y.binormal.unit.expr[[i]],
            a = quote(a),
            cosu = quote(cos(u)),
            sinu = quote(sin(u))
          )
        ))
      
      
      #--evaluate for t
      y.2nd <- do.call(cbind, lapply(y.final.path.expr, function(x)
        eval(x)))
    }
    
    if (plot) {
      par(mar = c(0, 0, 0, 0))
      plot3d(
        x = y.2nd[, 1],
        y = y.2nd[, 2],
        z = y.2nd[, 3],
        type = 'l',
        col = "brown",
        add = TRUE,
        lwd = 2
      )
    }
    out <- y.2nd
  }
  
  
  
  #---------------------------------------
  #---------------------------------------
  #---------------THIRD ORDER
  #---------------------------------------
  #---------------------------------------
  if (length(radii) > 2) {
    #---BLOCK ALL
    
    
    if (!tangent) {
      y.expr <- list(quote(h * t),
                     quote(R * cos(t) + a * cos(u) + b * cos(w)),
                     quote(R * sin(t) + a * sin(u) + b * sin(w)))
      y.3rd <- do.call(cbind, lapply(y.expr, function(x)
        eval(x)))
      
      
    } else{
      #--FORMULATING THE EXPRESSION (USING R's DERIVATIVE CALCULATOR)
      
      y.expr <- y.final.path.expr#list(expression(h*t),expression(R*cos(t)),expression(R*sin(t)))
      y.tangent.expr <- list(D(substitute(A, list(A = y.expr[[1]])), "u"),
                             D(substitute(A, list(A = y.expr[[2]])), "u"),
                             D(substitute(A, list(A = y.expr[[3]])), "u"))
      magnitude.tangent.expr <- substitute(
        sqrt(A^2 + B^2 + C^2),
        list(A = y.tangent.expr[[1]], B = y.tangent.expr[[2]], C = y.tangent.expr[[3]])
      )
      y.tangent.unit.expr <- lapply(y.tangent.expr, function(x)
        substitute((A) / (B), list(A = x, B = magnitude.tangent.expr)))
      
      y.normal.expr <-  list((D(y.tangent.unit.expr[[1]], "u")), (D(y.tangent.unit.expr[[2]], "u")) , (D(y.tangent.unit.expr[[3]], "u")))
      magnitude.normal.expr <- substitute(
        sqrt(A^2 + B^2 + C^2),
        list(A = y.normal.expr[[1]], B = y.normal.expr[[2]], C = y.normal.expr[[3]])
      )
      y.normal.unit.expr <- lapply(y.normal.expr , function(x)
        substitute((A) / (B), list(A = x, B = magnitude.normal.expr)))
      
      #--indices shifted: the z,x,y below are x,y,z in my version
      Cx <- substitute((ay) * (bz) - (az) * (by),
                       list(
                         ay = y.tangent.unit.expr[[2]],
                         bz = y.normal.unit.expr[[3]],
                         az = y.tangent.unit.expr[[3]],
                         by = y.normal.unit.expr[[2]]
                       )
      )
      Cy <- substitute((az) * (bx) - (ax) * (bz),
                       list(
                         az = y.tangent.unit.expr[[3]],
                         bx = y.normal.unit.expr[[1]],
                         ax = y.tangent.unit.expr[[1]],
                         bz = y.normal.unit.expr[[3]]
                       )
      )
      Cz <- substitute((ax) * (by) - (ay) * (bx),
                       list(
                         ax = y.tangent.unit.expr[[1]],
                         by = y.normal.unit.expr[[2]],
                         ay = y.tangent.unit.expr[[2]],
                         bx = y.normal.unit.expr[[1]]
                       )
      )
      
      y.binormal.expr <- list(Cx, Cy, Cz)
      magnitude.binormal.expr <- substitute(
        sqrt(A^2 + B^2 + C^2),
        list(A = y.binormal.expr[[1]], B = y.binormal.expr[[2]], C = y.binormal.expr[[3]])
      )
      y.binormal.unit.expr <- lapply(y.binormal.expr , function(x)
        substitute((A) / (B), list(A = x, B = magnitude.binormal.expr)))
      
      
      
      y.final.path.expr <- lapply(1:3, function(i)
        substitute(
          A + a * B * cosu + a * C * sinu,
          list(
            A = y.expr[[i]],
            B = y.normal.unit.expr[[i]],
            C = y.binormal.unit.expr[[i]],
            a = quote(b),
            cosu = quote(cos(w)),
            sinu = quote(sin(w))
          )
        ))
      
      
      ###################################
      #--EVALUATING THE EXPRESSIONS (FOR t)
      
      y <- do.call(cbind, lapply(y.expr, function(x)
        eval(x)))
      y.tangent <- do.call(cbind, lapply(y.tangent.expr, function(x)
        eval(x)))
      y.tangent.unit <- do.call(cbind, lapply(y.tangent.unit.expr, function(x)
        eval(x)))
      y.normal.unit <- do.call(cbind, lapply(y.normal.unit.expr, function(x)
        eval(x)))
      y.binormal.unit <- do.call(cbind, lapply(y.binormal.unit.expr, function(x)
        eval(x)))
      
      #--evaluate for t
      y.3rd <- do.call(cbind, lapply(y.final.path.expr, function(x)
        eval(x)))
      
      
      
      # ###################################
      # #--CHECKING THAT VECTORS ARE NORMALIZED
      # table(apply(y.tangent.unit,1,function(x)sqrt(sum(x^2))))
      # table(apply(y.normal.unit,1,function(x)sqrt(sum(x^2))))
      # table(apply(y.binormal.unit,1,function(x)sqrt(sum(x^2))))
      
    }
    
    
    if (plot) {
      par(mar = c(0, 0, 0, 0))
      plot3d(
        x = y.3rd[, 1],
        y = y.3rd[, 2],
        z = y.3rd[, 3],
        type = 'l',
        col = "lightblue",
        lwd = 1,
        add = TRUE
      )
    }
    out <- y.3rd
  }#
  
  
  
  
  if (!returnList) {
    return(out)
  } else{
    return(list(
      FirstOrder = y.1st,
      SecondOrder = y.2nd,
      ThirdOrder = y.3rd
    ))
  }
  
  
  
  
}
