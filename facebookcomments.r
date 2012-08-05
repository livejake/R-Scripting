 library(RCurl)
 library("rjson")
 library("plyr")
 library("stringr")
 
 
 # Returns json from url of Json 
 
 get.json.from.url <- function(url){
 raw.data <- getURL(url,cainfo = "/Users/jakebialer/Desktop/cacert.pem")	
 res <- fromJSON(raw.data)
 return(res)	
 }
 
 # Returns Facebook comments id from URL of blog post
 
 get.fb.comments.id <- function(url){
 uri <- paste("https://graph.facebook.com/comments/?ids=",url,sep="")
 res <- get.json.from.url(uri)		
 idstring <- res[[url]]$comments$data[[1]]$id
 id <- str_split(idstring,"_")[[1]][1]
 return(id)
 }


 # From the the URL of a post with Facebook commments, recursively grabs FB comments until there are no more comments. It returns list a that contains two data.frames: one with parent comments, the other contains replies (FB comments only have two levels)
 
 get.comments <- function(url = NULL, allcomments = NULL, first.time=TRUE) {
	if(first.time==TRUE){
	 commentsid <- get.fb.comments.id(url)
	 url <- paste("https://graph.facebook.com/",commentsid,"/comments",sep="")
 	}
	 res <- get.json.from.url(url)
	 
  	  parentcomments<- ldply(res$data,function(x) data.frame(id=x$id,fromname=x$from$name,fromid=x$from$id,message=x$message,created_time=x$created_time, likes =ifelse(is.null(x$likes),0,x$likes), count =ifelse(is.null(x$comments$count),0,x$comments$count)))
  
  replies <- ldply(res$data,function(x)(
  data.frame(ldply(x$comments$data, function(y) y$id),ldply(x$comments$data, function(y) y$from$name),ldply(x$comments$data, function(y) y$from$id),ldply(x$comments$data, function(y) y$message),ldply(x$comments$data, function(y) y$created_time)
  )))
  	
  coltitles <- c("reply.id", "reply.fromname", "reply.fromid", "reply.message","reply.created_time")
  
  if(length(replies)== length(coltitles)) {
	  names(replies) <- coltitles
  }
  
 if (first.time==TRUE){ 
 allcomments <- list(parentcomments,replies)
 } else{ 
  somecomments <- list(parentcomments,replies)
  allcomments[[1]] <- rbind(allcomments[[1]],somecomments[[1]])
  allcomments[[2]]<-  rbind(allcomments[[2]],somecomments[[2]])
 } 
  print(dim(allcomments[[1]]))
  print(dim(allcomments[[2]]))
  
  	 if (is.null(res$paging$'next')==FALSE){
   		get.comments(res$paging$'next', allcomments, FALSE)
		  } else{
 	 		 	return(allcomments)	
  	}
  }

  # Example URL  
url <- "http://blog.netflix.com/2011/09/explanation-and-some-reflections.html"

  ## Example usage of comments function
comments <- get.comments(url)
 
