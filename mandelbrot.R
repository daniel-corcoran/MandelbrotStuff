options(bitmapType='cairo')
library("reshape2")
library("ggplot2")

mandelbrot <- function(cr, cx, depth){
  # Returns mandelbrot function for a complex coordinate
  x <- 0
  y <- 0
  iteration <- 0
  while(x*x + y*y <= 4 & iteration < depth )
    # Apply algorithm
  {
    xtemp <- x*x - y*y + cr
    y <- 2*x*y + cx
    x <- xtemp
    iteration <- iteration + 1
  }
  return(iteration)
}
coordinates <- function(x_pnt, y_pnt, xlim.min, xlim.max, ylim.min, ylim.max){
  mb <- data.frame(x<-seq(x_pnt), y<-seq(y_pnt))
  names(mb) <- c("x", "y")
  mb$n <- 0
  mb.wide <- dcast(mb, x ~ y, value.var="n", sum, fill=0)
  # FIXME: Where does this 500 correlate to? X or Y?
  mb.melt <- melt(mb.wide, id.vars= "x", measure.vars=2:y_pnt, variable.name = "y")
  mb.melt$x <- lapply(mb.melt$x, function(x) ((xlim.max - xlim.min) * x / x_pnt) + xlim.min)
  mb.melt$y <- lapply(as.numeric(mb.melt$y), function(y) ((ylim.max - ylim.min) * y / y_pnt) + ylim.min)
  return(mb.melt)
}

x_pnt <- 200
y_pnt <- 200
depth <- 15

xlim.min <- -2.5
xlim.max <- 1
ylim.min <- -1
ylim.max <- 1

while(T){
  gradient <- colorRampPalette(c("blue", "yellow"))
  gradient <- gradient(depth)
  c <- coordinates(x_pnt, y_pnt, xlim.min, xlim.max, ylim.min, ylim.max)
  mb_helper <- function(x){
    return(mandelbrot(x$x, x$y, depth))
  }
  c$value <- apply(c, MARGIN = 1, FUN=mb_helper)
  c$col<-lapply(c$value, function(v) ifelse(v == depth, "black", gradient[v]))
  plot(1, type="n", xlab="", ylab="", 
       xlim=c(xlim.min, xlim.max),
       ylim=c(ylim.min, ylim.max))
  cat("\nPlotting point vector")
  plot(x=c$x, y=c$y, col=as.character(c$col), xlab = "", ylab="")
  #ggplot(c, aes(as.numeric(x), as.numeric(y), color=c$col)) + geom_point()
  cat("\nRender complete. Locating...")
  g <- locator(1)
  print(g)
  # On click (zoom)
  # Retain the same (center) but cut the X and Y range in half.
  x_ctr <- g$x
  y_ctr <- g$y
  x_dst <- (xlim.max - xlim.min) / 4
  y_dst <- (ylim.max - ylim.min) / 4
  xlim.min <- x_ctr - x_dst
  xlim.max <- x_ctr + x_dst
  ylim.min <- y_ctr - y_dst
  ylim.max <- y_ctr + y_dst
  text(x_ctr, ylim.max, pos=1, labels="loading", col="white")
  
  depth <- depth + 8
  print("New coordinates")
  cat(sprintf("X limits: %f, %f \n", xlim.min, xlim.max))
  cat(sprintf("Y limits: %f, %f ", ylim.min, ylim.max))
}
