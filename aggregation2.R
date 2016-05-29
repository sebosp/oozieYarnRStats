#!/usr/bin/R

library("dplyr")
library("lubridate")
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
aggDTFull <- tbl_df(read.csv("aggregation2.csv",stringsAsFactors=FALSE));
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
colIdxA <- 1
colIdxB <- 2
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
		naHostA <- (aggDT0 %>% filter(!is.na(colIdxA) && !is.na(colIdxB) && AM.Host == "") %>% select(colIdxA))[[1]]
		naHostB <- (aggDT0 %>% filter(!is.na(colIdxA) && !is.na(colIdxB) && AM.Host == "") %>% select(colIdxB))[[1]]
		allHostsA <- (aggDT0 %>% filter(!is.na(colIdxA) && !is.na(colIdxB)) %>% select(colIdxA))[[1]]
		allHostsB <- (aggDT0 %>% filter(!is.na(colIdxA) && !is.na(colIdxB)) %>% select(colIdxB))[[1]]
		par(mfrow=c(3,2))
		curColA <- colnames(aggDT0)[colIdxA]
		curColB <- colnames(aggDT0)[colIdxB]
		plot(naHostA,naHostB,xlab=curColA,ylab=curColB,main=paste("Host not specified:",curColA,"vs",curColB,sep=" "))
		abline(lm(naHostB~naHostA),col="blue")
		plot(allHostsA,allHostsB,xlab=curColA,ylab=curColB,main=paste("All hosts:",curColA,"vs",curColB,sep=" "))
		abline(lm(allHostsB~allHostsA),col="blue")
		for(host in unique((aggDT0 %>% filter(AM.Host != "") %>% select(AM.Host))[[1]])){
			print(paste(host,curColA,curColB,colIdxA,colIdxB))
			cHostA <- (aggDT0 %>% filter(!is.na(colIdxA) & !is.na(colIdxB) & AM.Host == host) %>% select(colIdxA))[[1]]
			cHostB <- (aggDT0 %>% filter(!is.na(colIdxA) & !is.na(colIdxB) & AM.Host == host) %>% select(colIdxB))[[1]]
			plot(cHostA,cHostB,xlab=curColA,ylab=curColB,main=paste(host,curColA,"vs",curColB,sep=" "))
			abline(lm(cHostB~cHostA),col="blue")
		}
	}
	keyPressed <- readkeygraph("n,p,u,d")
	if(keyPressed == 'n'){
		colIdxB <- (colIdxB + 1) %% (ncol(aggDT0) + 1)
	}
	if(keyPressed == 'p'){
		colIdxB <- (colIdxB - 1) %% (ncol(aggDT0) + 1)
	}
	if(keyPressed == 'u'){
		colIdxA <- (colIdxA + 1) %% (ncol(aggDT0) + 1)
	}
	if(keyPressed == 'd'){
		colIdxA <- (colIdxA - 1) %% (ncol(aggDT0) + 1)
	}
	if(colIdxA == colIdxB){
		colIdxB <- colIdxB + (keyPressed == 'n'?1:-1)
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
