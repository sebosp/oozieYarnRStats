#!/usr/bin/R

library("dplyr")
library("lubridate")
library("ggplot2")
# Get keys from the graphics device
X11(type="Xlib")
keyPressed <- 'x'
readkeygraph <- function(prompt){
	getGraphicsEvent(prompt = prompt,
			 onMouseDown = NULL, onMouseMove = NULL,
			 onMouseUp = NULL, onKeybd = onKeybd,
			)
	Sys.sleep(0.01)
	return(keyPressed)
}

onKeybd <- function(key){
	keyPressed <<- key
}
aggDTFull <- tbl_df(read.csv("aggregation2.csv"));
aggDTFull <- aggDTFull %>% 
		mutate(
		       Start.Time=origin+(Start.Time/1000),
		       Finish.Time=origin+(Finish.Time/1000),
		       Total.Time=as.numeric(Finish.Time-Start.Time)
		)
options(width=220)
exclude <- c() # Fields to exclude
include <- c() # Fields to include
colIdx <- 1
for(curCol in colnames(aggDTFull)){
	if( (class(aggDTFull[[1,curCol]]) == "integer"
	     || class(aggDTFull[[1,curCol]]) == "numeric"
	     || class(aggDTFull[[1,curCol]]) == "dbl")
	  && max(aggDTFull[,curCol],na.rm=TRUE) == min(aggDTFull[,curCol],na.rm=TRUE)
	){
		print(paste("Variable: '",curCol,"' found with same min/max.",sep=""));
		exclude<-c(exclude,colIdx)
	}else if((class(aggDTFull[[1,curCol]]) != "integer" 
		  && class(aggDTFull[[1,curCol]]) != "numeric")
		){
		print(paste("Variable: '",curCol,"' not an integer/numeric.",sep=""))
		exclude<-c(exclude,colIdx)
	}
	colIdx <- colIdx + 1
}
aggDT0 <- aggDTFull %>%
	select(-c(exclude[exclude != which(colnames(aggDTFull) == "AM.Host")]))
# all columns compared against all columns
colIdxA <- length(colnames(aggDT0))
colIdxB <- 31
while(keyPressed != 'q'){
	skip <- 0
	if(   class(aggDT0[[1,colIdxA]]) != "integer" 
	   && class(aggDT0[[1,colIdxA]]) != "numeric"
	   && class(aggDT0[[1,colIdxA]]) != "dbl"
	){
		skip <- 1
	}
	if(class(aggDT0[[1,colIdxB]]) != "integer" 
	   && class(aggDT0[[1,colIdxB]]) != "numeric"
	   && class(aggDT0[[1,colIdxB]]) != "dbl"
	){
		skip <- 1
	}
	if(!skip){
		curColA <- colnames(aggDT0)[colIdxA]
		curColB <- colnames(aggDT0)[colIdxB]
		print(ggplot(aggDT0,aes_string(curColA,curColB))
		      + geom_point(size=2,aes(color=AM.Host))
		      + facet_grid(. ~ AM.Host)
		      + geom_smooth(method="lm",size=1/3))
	}
	keyPressed <- readkeygraph("n,p,u,d")
	if(keyPressed == 'n'){
		colIdxB <- (colIdxB + 1) %% ncol(aggDT0)
	}
	if(keyPressed == 'p'){
		colIdxB <- (colIdxB - 1) %% ncol(aggDT0)
	}
	if(keyPressed == 'u'){
		colIdxA <- (colIdxA + 1) %% ncol(aggDT0)
	}
	if(keyPressed == 'd'){
		colIdxA <- (colIdxA - 1) %% ncol(aggDT0)
	}
	if(keyPressed == 's'){
		ggsave(paste(curColA,'-vs-',curColB,'.png',sep=""))
	}
	if(colIdxA == colIdxB){
		if(keyPressed == 'n'){
			colIdxB <- colIdxB + 1
		}else{
			colIdxB <- colIdxB - 1
		}
	}
	if(colIdxA == 0){
		colIdxA <- ncol(aggDT0)
	}
	if(colIdxB == 0){
		colIdxB <- ncol(aggDT0)
	}
	if(colIdxA > ncol(aggDT0)){
		colIdxA <- 1
	}
	if(colIdxB > ncol(aggDT0)){
		colIdxB <- 1
	}
	print(paste("Key:",keyPressed,"A",colIdxA,"B",colIdxB,"ColA:",curColA," ColB:",curColB,sep=" "))
}
