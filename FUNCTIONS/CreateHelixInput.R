CreateHelixInput <- function(t = NULL,
                             size,
                             start = NULL,
                             end = NULL,
                             main.loop.duration) {
  if (is.null(start))
    start <- as.POSIXct(paste(min(t)))
  if (is.null(end))
    end <- as.POSIXct(paste(max(t)))
  
  t <- t[t >= start & t <= end]
  
  
  duration <- as.numeric(difftime(end, start, units = "days"))
  
  
  n.main.loops <- duration / main.loop.duration
  t.posix <- seq(start, end, length.out = size)
  t.posix.diff <- as.numeric(difftime(t.posix, start, units = "days"))
  t.radians <-  t.posix.diff * 2 * pi * n.main.loops / duration
  
  if (!is.null(t)) {
    empirical.t.posix.diff <- as.numeric(difftime(t, start, unit = "days"))
    empirical.t.radians <- empirical.t.posix.diff * 2 * pi * n.main.loops /
      duration
  }
  
  
  if (!is.null(t))
    out <- list(
      t.structural = data.frame(t.posix, t.posix.diff, t.radians),
      t.empirical = data.frame(
        t.posix = t,
        t.posix.diff = empirical.t.posix.diff,
        t.radians = empirical.t.radians
      )
    )
  
  if (is.null(t))
    out <- list(
      t.structural = data.frame(t.posix, t.posix.diff, t.radians),
      t.empirical = NULL
    )
  
  
  return(out)
  
}
