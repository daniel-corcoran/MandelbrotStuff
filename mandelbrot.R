options(bitmapType='cairo')
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
plot(1, type="n", xlab="", ylab="", xlim=c(-2.5, 1), ylim=c(-1, 1))
x_pnt <- 500
y_pnt <- 500
depth <- 50
gradient <- colorRampPalette(c("blue", "red", "green"))
gradient <- gradient(depth)
for(x in seq(x_pnt)){
  print(100 * x / x_pnt)
  for(y in seq(y_pnt)){
    x_scale <- (3.5 * x / x_pnt) - 2.5
    y_scale <- (2 * y / y_pnt) - 1
    out <- mandelbrot(x_scale, y_scale, depth)
    if(out == depth){
      points(x_scale, y_scale, pch='.', col="black")
    }
    else{
      points(x_scale, y_scale, pch='.', col=gradient[out])
    }
  }
}
