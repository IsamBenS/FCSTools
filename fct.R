draw.cumulated.filled.plots <- function(points.list, max.height=2, x.values.range=c(0.15,1), x.lab="par", y.lab="val")
{
    plot(x.values.range,c(0,max.height),xlab = x.lab, ylab = y.lab)
    y.vals <- unlist(points.list[[1]])
    x.pts <- c(seq(0.1,1,0.1),rev(seq(0.1,1,0.1)))
    lapply(1:(length(points.list)-1), function(i)
    {
        y.pts <- c(y.vals,rev(y.vals+unlist(points.list[[i+1]])))
        polygon(x.pts, y.pts,col=colors()[i*8])
        
        y.vals <<- unlist(y.vals) + unlist(points.list[[i+1]])
    })
    print(y.vals)
}

draw.F.score.barplot <- function(F.score.matrix, populations.names, populations.sizes)
{
    scores.list.col <- sapply(1:nrow(F.score.matrix), function(r)
    {
        return(which(F.score.matrix[r,]==max(F.score.matrix[r,]))[[1]])
    })
    pop.order <- order(populations.sizes)
    
    scores.list <- rep(NA,length(scores.list.col))
    lapply(1:length(scores.list), function(i)
    {
        col.id <- as.integer(unlist(scores.list.col)[[i]])
        scores.list[[col.id]] <- F.score.matrix[i,col.id]
    })
    
    names(scores.list) <- as.character(unlist(populations.names))
    scores.list <- scores.list[pop.order]
    
    
    
    barplot(scores.list, main="F scores by population (ordered by %events)", horiz = T, 
            names.arg = names(scores.list), cex.names = 0.8, xlim = c(0,1.05))
}