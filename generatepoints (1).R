generatePoints <- function(xy,sq.size,loc.xy="bottomright",shrink.fac=NULL) {
	if(is.null(shrink.fac)){
		pointsX <- c(xy[1],xy[1],xy[1]-sq.size,xy[1]-sq.size)
		pointsY <- c(xy[2],xy[2]-sq.size,xy[2]-sq.size,xy[2])
		coords <- as.matrix(cbind(pointsX, pointsY))
	} else{
		adj <- 0.2
		pointsX <- c(xy[1]-adj,xy[1]-adj,(xy[1]-sq.size)+adj,(xy[1]-sq.size)+adj)
		pointsY <- c(xy[2]-adj,(xy[2]-sq.size)+adj,(xy[2]-sq.size)+adj,xy[2]-adj)
		coords <- as.matrix(cbind(pointsX, pointsY))		
	}
}