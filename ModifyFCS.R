library(flowCore)
library(Biobase)

add.keyword.to.fcs <- function(fcs, added.keyword, added.keyword.name) 
{
    fcs.out <- fcs
    fcs.out@description <- c(fcs.out@description,added.keyword)
    names(fcs.out@description) <- c(names(fcs@description),added.keyword.name)
    
    return(fcs.out)
}

write.FCS.CIPHE <- function(fcs, fcs.path)
{
    descR <- description(fcs)
    lapply(c(1:ncol(fcs@exprs)),function(x)
    {
        descR[[paste0("$P",x,"R")]] <<- 262144
    })
    fcs.out <- flowFrame(fcs@exprs, description = descR)
    fcs.out@description <- descR
    
    write.FCS(fcs.out, fcs.path, delimiter = '#')
}


keyword.exists.FCS <- function(fcs, key.part)
{
    key.exists <- F
    i <- 1
    while(!key.exists && i <= length(fcs@description))
    {
        if(grepl(key.part, names(fcs@description)[i], fixed = T))
        {
           key.exists <- T
        }
        i <- i+1
    }
    
    return(key.exists)
}

get.keywords.with.keypart.FCS <- function(fcs, key.part)
{
    keywords.list <- c()
    i <- 1
    while(i <= length(fcs@description))
    {
        if(grepl(key.part, names(fcs@description)[i], fixed = T))
        {
            keywords.list <- c(keywords.list,fcs@description[[i]])
            names(keywords.list)[length(keywords.list)] <- names(fcs@description)[i]
        }
        i <- i+1
    }
    
    return(keywords.list)
}

generate.FCS.files.from.clusters <- function(fcs, cluster.col)
{
    clusters.ids <- unique(fcs@exprs[,cluster.col])
    fcs.list <- list(1:cluster.ids)
    descR <- description(fcs)
    
    for (i in clusters.ids) 
    {
        events.ids <- which(fcs@exprs[,cluster.col] == i)
        if(length(events.ids) > 0)
        {
            tmp.matrix <- fcs@exprs[events.ids,]
            fcs.list <- c(fcs.list, flowFrame(tmp.matrix, description = descR))
        }
    }
    return(fcs.list)
}