#### Modesto Escobar
# Wed Apr 01 12:08:56 2020 ------------------------------



nametoWikiURL <- function (name, language="en") {
  paste0("https://", language, ".wikipedia.org/wiki/", gsub(" ","_",name))
}

nametoWikiText <- function(name, language="en"){
  paste0("<a href=\'https://", language, ".wikipedia.org/wiki/", gsub(" ","_",name), "', target=\'_blank\'>", name, "</a>")
}


nametoWiki <- function(name, language="en") {
  paste0('<iframe src="https://',language,'.m.wikipedia.org/wiki/',gsub(" ","_",name),'" width="100%" height="100%" frameborder="0" marginwidth="0", margingheight="0"></iframe>')
}


urltoText <- function(url, text=NULL) {
  if (is.null(text)) text <- url
  paste0("<a href=\'",url, "\', target= \'_blank\'>", text, "</a>")
}


urltoInfo <- function(url){
  paste0('<iframe src="',url, '" width="100%" height="100%" frameborder="0" marginwidth="0", margingheight="0"></iframe>')
}



preName <- function(X) sub("(^.*),\\s*(.*$)","\\2 \\1", X)


errorWiki <- function(X, language=c("es", "en", "fr"), directory="./", maxtime=0) {
  errores <- NULL
  for (I in X){
    person <- gsub(" ", "_", I)
    url <-paste("https://",language,".wikipedia.org/wiki/",person,sep="")
    file <- paste0(directory, person,".html")
    oldw <- getOption("warn")
    options(warn = -1)
    E <- tryCatch(download.file(url,destfile=file, quiet=TRUE),error = function(e) person)
    if (E!=0) errores <- c(errores, E)
    options(warn = oldw)
    Sys.sleep(runif(1, min=0, max=maxtime))
  }
  return(errores)
}



searchWiki <- function(name, language=c("en", "es", "fr", "it", "de", "pt", "ca"), all=FALSE, maxtime=0) {
  errores <- data.frame(es=logical(), en=logical(), fr=logical(), it=logical(), 
                        de=logical(), pt=logical(), ca=logical())[,language, drop=FALSE]
  for (I in name){
    errores[I,language] <- rep(FALSE, length(language))
    for (L in language){
      person <- gsub(" ", "_", I)
      url <-URLencode(iconv(paste("https://",L,".wikipedia.org/wiki/",person,sep=""),to="UTF-8"))
      if (valid_url(url)) {
        errores[I,L] <- TRUE
        if (!all) break
      }
      Sys.sleep(runif(1, min=0, max=maxtime))
    }
  }
  return(errores)
}

valid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}


readfile <- function(file, encoding="UTF-8") {
  note <- readChar(file, 1e+9)
  iconv(note, from=encoding)
}

cc <- function(wordlist) {
  wordlist <- gsub("[ ]*,[ ]*",",",wordlist)
  strsplit(wordlist,',')[[1]]
}




getWikidata <- function(namesVector) {
preCode <- 'SELECT ?label ?sexLabel ?birthdate ?birthplaceLabel ?deathdate ?deathplaceLabel ?citizenshipLabel
(GROUP_CONCAT(DISTINCT ?pic;separator="|")     as ?pics)
(GROUP_CONCAT(DISTINCT ?ocLabel;separator="|") as ?occupation)
(GROUP_CONCAT(DISTINCT ?moLabel;separator="|") as ?movement)
(GROUP_CONCAT(DISTINCT ?geLabel;separator="|") as ?genres)
(GROUP_CONCAT(DISTINCT ?inLabel;separator="|") as ?influencedby)
(GROUP_CONCAT(DISTINCT ?in;separator="|")      as ?influencedbyQ)  # AS Qxxxx
(GROUP_CONCAT(DISTINCT ?noLabel;separator="|") as ?notablework)
(GROUP_CONCAT(DISTINCT ?no;separator="|")      as ?notableworkQ)   # As Qxxxx
WHERE {
  BIND(wd:'
  
  postCode <- ' AS ?entity)
    ?entity rdfs:label ?label
    SERVICE wikibase:label {bd:serviceParam wikibase:language "en"}
    FILTER(LANG(?label) = "en")
    OPTIONAL {?entity wdt:P21  ?sex}
    OPTIONAL {?entity wdt:P569 ?birthdate}
    OPTIONAL {?entity wdt:P19  ?birthplace}
    OPTIONAL {?entity wdt:P570 ?deathdate}
    OPTIONAL {?entity wdt:P20  ?deathplace}
    OPTIONAL {?entity wdt:P27  ?citizenship}
    OPTIONAl {?entity wdt:P18  ?pic.} 
    OPTIONAL {?entity wdt:P106 ?oc.
              ?oc rdfs:label ?ocLabel.
              FILTER((LANG(?ocLabel)) = "en")}
    OPTIONAL {?entity wdt:P135 ?mo.
              ?mo rdfs:label ?moLabel.
              FILTER((LANG(?moLabel)) = "en")}
    OPTIONAL {?entity wdt:P136 ?ge.
              ?ge rdfs:label ?geLabel.
              FILTER((LANG(?moLabel)) = "en")}
    OPTIONAL {?entity wdt:P737 ?in.
              ?in rdfs:label ?inLabel.
              FILTER((LANG(?inLabel)) = "en")}
    OPTIONAL {?entity wdt:P800 ?no.
              ?no rdfs:label ?noLabel.
              FILTER((LANG(?noLabel)) = "en")}
}
GROUP BY ?label ?sexLabel ?birthdate ?birthplaceLabel ?deathdate ?deathplaceLabel ?citizenshipLabel
'

getWiki <-function(nombre, pre=preCode, post=postCode){
  i <- find_item(nombre)
  if(length(i)>0) {
    i <- i[[1]]$id
    x <- paste0(pre, i, post)
    X <- suppressMessages(query_wikidata(x)[1,]) 
  }
  else X <- data.frame(label=nombre, sexLabel=NA, birthdate=NA, birthplaceLabel=NA, 
                         deathdate=NA,deathplaceLabel=NA,citizenshipLabel=NA,
                         pics=NA, occupation=NA,movement=NA,genres=NA, 
                         influenceddby=NA, influencebyQ=NA,notablework=NA, notableworkQ=NA,
                       stringsAsFactors = FALSE)
  return(X)
}

transM <- function(X) {
  dimensions <- dim(X)
  x <- unlist(X)
  m <- as.data.frame(matrix(x, nrow=dimensions[2], ncol=dimensions[1], byrow=TRUE), stringsAsFactors=FALSE)
  colnames(m) <- rownames(X)
  return(m)
}

X <- sapply(namesVector,getWiki)
return(transM(X)) 
}



url <- women$pics[2]
name <- women$label[2]

getFiles <- function(lista, path="./", ext=NULL) {
  errores <- NULL
  path <- ifelse(substr(path,nchar(path),nchar(path))!="/",paste0(path,"/"),path)
  lista <- as.data.frame(lista)
  for (case in 1:nrow(lista)) {
    name <- lista[case,1]; url <- lista[case,2]
    if(is.null(ext)) ext <- filext(url) 
    file=paste0(path,sub("/","-",name),".",ext)
    if(!is.na(url) & !file.exists(file)) {
       oldw <- getOption("warn")
       options(warn = -1)
       E <- tryCatch(download.file(url, destfile=file, quiet=TRUE, mode="wb"),error = function(e) name)
       if (E!=0) errores <- c(errores, E)
       options(warn = oldw)
    } 
  }
    return(errores)
}


filext <- function (fn) {
  splitted    <- strsplit(x=fn, split='/')[[1]]   
  fn          <- splitted [length(splitted)]
  ext         <- ''
  splitted    <- strsplit(x=fn, split='\\.')[[1]]
  l           <-length (splitted)
  if (l > 1 && sum(splitted[1:(l-1)] != ''))  ext <-splitted [l] 
  ext
}

linkTwitter <- function(entity) {
  link <- ""
  link <- ifelse(substr(entity,1,1)=="@", paste0('<a href="https://twitter.com/', sub("^@","", entity), '" target="_blank">', 
                                              sub("^@","",entity),'</a>'), link)
  link <- ifelse(substr(entity,1,1)=="#", paste0('<a href="https://twitter.com/hashtag/', sub("^#","", entity), '" target="_blank">', 
                                              sub("^#","",entity),'</a>'), link)
  return(link)
}

hashtag_extract<- function(texto,characters="#@", excludeRT=TRUE, oldText=NULL, newText=NULL){
  charsearch <- paste0("[",characters,"][A-Za-z_0-9]{2,}")
  texto <- gsub("([.,;:])(\\S)","\\1 \\2", gsub("<.*>","",enc2native(texto)))
  texto <- tolower(iconv(texto, to="ASCII//TRANSLIT"))
  if (excludeRT) texto <- sub("^RT .*?: (\\?)*[ ]*","",texto)
  hh <-function(texto,charsearch){
    paste(unlist(regmatches(texto, gregexpr(charsearch, texto, perl=TRUE))),collapse="|")
  }
  R <- sapply(texto,hh,charsearch)
  names(R) <- NULL
  R <- gsub("[[:punct:]]+$","",R)
  R <- gsub("[[:punct:]]+\\|","|",R)
  if (!is.null(oldText) & !is.null(newText)) R <- gsub(oldText,newText,R)
  return(R)
}


mention <- function(data, author="author", text="text", ...) {
  q <- sapply(hashtag_extract(data[[text]],"@"),strsplit,"\\|")
  n.obs <- sapply(q, length)
  seq.max <- seq_len(max(n.obs))
  mat <- as.data.frame(t(sapply(q, "[", i = seq.max)))
  if(dim(mat)[1]==1 & dim(mat)[2]!=1) mat <- t(mat)
  rownames(mat) <- c()
  data[[author]] <- tolower(iconv(data[[author]],to="ASCII//TRANSLIT"))
  data$author <- ifelse(substr(data$author,1,1)=="@", data$author, paste0("@", data$author))
  authors <- as.data.frame(table(data$author)); names(authors) <- c("name", "tweets")
  newData <- cbind(data[,author], mat)
  links <- edgeList(newData, procedures = "shape")
  links$X <- 1
  Links <- aggregate( X~Source+Target, data=links, FUN=length)
  arguments <- list(links=Links, lwidth="X", ...)
  if(!exists("showArrows", arguments)) arguments$showArrows <- TRUE
  if(!exists("size", arguments)) arguments$size <- "tweets"
  if(!exists("labelSize", arguments)) arguments$labelSize <- "degree"
  net <- do.call(netCoin, arguments)
  net$nodes <- merge(net$nodes, authors, all.x=TRUE, all.y=TRUE)
  net$nodes$link <- linkTwitter(net$nodes$name)
  net$nodes$tweets <- ifelse(is.na(net$nodes$tweets), 0, net$nodes$tweets)
  if(exists("language",arguments) && arguments$language=="es") type = "tipo" else type = "type"
  net$nodes[[type]] <-ifelse(net$nodes$tweets==0, "only mentioned", "author")
  net <- netCoin(net, color=type, shape=type)
  return(net)
}


cotweet <- function(data, text="text", searchers="@#", original= TRUE, excludeRT=TRUE, min=1, others=NULL, oldText=NULL, newText=NULL, ...) {
  arguments <- list(...)
  if(!exists("minimum", arguments)) minimum=1
  if(!exists("minL", arguments)) minL=1
  if(!exists("procedures", arguments)) procedures="frequencies"
  if(!exists("criteria", arguments)) criteria="frequencies"
  X <- data[[text]]
  if (!original) X <- X[grepl("^RT @.*:", X)]
  D <- data.frame(Text=hashtag_extract(X, searchers, excludeRT, oldText, newText))
  incidences <- dichotomize(D,"Text", sep="|", add=F, min=min, nas="None")
  graph <- allNet(incidences, procedures=procedures, criteria=criteria, minimum=minimum, minL=minL)
  if(grepl("(@#)|(#@)", searchers)) {
    graph$nodes$type <- ifelse(grepl("^@",graph$nodes$name), "author", ifelse(grepl("^#", graph$nodes$name), "hashtag", "other"))
    graph$nodes$link <- linkTwitter(graph$nodes$name)
  }
  if (!exists("community", arguments)) arguments$community <- "Walktrap"
  if (!exists("color", arguments)) arguments$color  <- "community"
  if (!exists("shape", arguments) & grepl("(@#)|(#@)", searchers)) arguments$shape="type"
  arguments$nodes  <- graph
  do.call(netCoin, arguments)
}

cotext <- function(data, text="text", sep=" ", min=1, ...) {
  arguments <- list(...)
  if(!exists("minimum", arguments)) minimum=1 else minimum <- arguments$minimum; arguments$minimum <- NULL
  if(!exists("maxL", arguments)) maxL <-  Inf else maxL <- arguments$maxL; arguments$maxL <- NULL
  if(!exists("minL", arguments)) minL <- -Inf else minL <- arguments$minL; arguments$minL <- NULL
  if(!exists("support", arguments)) support <- -Inf else support <- arguments$support; arguments$support <- NULL
  if(!exists("procedures", arguments)) procedures="frequencies" else procedures <- arguments$procedures; arguments$procedures <- NULL
  if(!exists("criteria", arguments)) criteria="frequencies" else criteria <- arguments$criteria; arguments$criteria <- NULL
  incidences <- dichotomize(data, text, sep=sep, add=F, min=min, nas="None")
  graph <- allNet(incidences, procedures=procedures, criteria=criteria, minimum=minimum, minL=minL, maxL=maxL, support=support)
  if (!exists("community", arguments)) arguments$community <- "Walktrap"
  if (!exists("color", arguments)) arguments$color  <- "community"
  arguments$nodes  <- graph
  do.call(netCoin, arguments)
}


retweet <- function(data, sender="author", text="text", language="en", nodes=NULL, ...){
  X <- data[grepl("^RT ",data[[text]]), c(sender, text)]
  if(inherits(X[[sender]], "haven_labelled")) X[[sender]] <- as_factor(X[[sender]])
  if(!inherits(X[[sender]], "character")) X[[sender]] <- as.character(paste(X[[sender]]))
  L <- data.frame(Source=X[[sender]], Target=sub("^RT (@*.*?): .*","\\1", X[[text]]), 
                  stringsAsFactors = FALSE)
  L$N <- 0
  Links <- aggregate(N~Source+Target,data= L,FUN=length)
  Nodes <- data.frame(name=unique(c(Links$Source,Links$Target)), stringsAsFactors = FALSE)
  names(Nodes) <- ifelse(language=="es","nombre", ifelse(language=="ca", "nome", "name"))
  if (!is.null(nodes)) Nodes <- merge(Nodes, nodes, all.x=T, by=names(Nodes), sort=FALSE)
  arguments <- list(nodes=Nodes, links=Links, language=language, ...)
  netCoin <- do.call(netCoin, arguments)
  return(netCoin)
}



calCentr <-function(graph, measures=c("degree","closeness","betweenness","eigen"), order="") {
  if (any(measures=="all",  measures=="ALL")) measures <- c("degree","closeness","betweenness","eigen")
  if (inherits(graph,"netCoin")) graph <- toIgraph(graph)
  m <- tolower(substring(measures,1,1))
  if(is.null(attr(igraph::V(graph),"name"))) igraph::V(graph)$name <- igraph::V(graph)
  G <- data.frame(nodes=igraph::V(graph)$name)
  H <- data.frame(nodes="Total")
  if("d" %in% m) {
    R <- centr_degree(graph)
    G$degree <- R$res
    H$degree <- R$centralization
  }
  if("c" %in% m) {
    R <- suppressWarnings(centr_clo(graph))
    G$closeness <- R$res
    H$closeness <- R$centralization
  }
  if("b" %in% m) {
    R <- centr_betw(graph)
    G$betweenness <- R$res
    H$betweenness <- R$centralization
  }
  if("e" %in% m) {
    R <- centr_eigen(graph)
    G$eigen <- R$vector
    H$eigen <- R$centralizacion
  }
  if (order %in% measures) G <- G[order(-G[[order]]),, drop=FALSE]
  list(nodes=G,graph=H)
}

type <- function(Profiles, followers="followers", following="following", 
                 retweets="Retweets", tweets="Tweets", retweeted="Retweeted", X="X", Y="Y") {
  Profiles[["sent"]] <- Profiles[[tweets]]+Profiles[[retweets]]
  Profiles[[X]] <- ifelse(Profiles[[following]]==0,0,log10(Profiles[[followers]]/Profiles[[following]]))
  Profiles[[Y]] <- ifelse(Profiles[["sent"]]==0,0,log10((Profiles[[retweeted]]+1)/Profiles[["sent"]]))
  Profiles[["type"]] <- ifelse(Profiles[[X]] >= 0 & Profiles[[Y]] > 0, "influential",
                               ifelse(Profiles[[X]] >= 0 & Profiles[[Y]] <= 0, "broadcaster", 
                                      ifelse(Profiles[[X]] < 0 & Profiles[[Y]] <= 0, "common user",
                                             ifelse(Profiles[[X]] < 0 & Profiles[[Y]] > 0, "hidden influential", "Not class."))))
  return(Profiles)
}

d_mention <-function(Tweets, author="author", text="text", date="date", imagedir=NULL, ext="png",
                     fields=c("followers", "following", "statuses", "location"),
                     beginDate = NULL, endDate= NULL, interval= 3600, tzone = "Europe/Paris", limits=NULL,
                     nFrames = Inf, seed=1, n = min(5000,nrow(Tweets)), minlabel=5, ...) {
  
  arguments <- list(...)
  
  if(!exists("label",      arguments)) arguments$label <- "name"
  if(!exists("size",       arguments)) arguments$size <- "Retweeted"
  if(!exists("labelSize",  arguments)) arguments$labelSize <- "size"
  if(!exists("color",      arguments)) arguments$color <- "Walktrap"
  if(!exists("repulsion",  arguments)) arguments$repulsion <- 3
  if(!exists("distance",   arguments)) arguments$distance <- 3
  if(!exists("zoom",       arguments)) arguments$zoom <-1
  if(!exists("main",       arguments)) arguments$main <- "Title"
  if(!exists("note",       arguments)) arguments$note <- "Elaborated with netCoin"
  if(!exists("showArrows", arguments)) arguments$showArrows <- TRUE
  if( exists("dir",        arguments)) {
    directory <- arguments$dir 
    arguments$dir <- NULL
  }
  if("location" %in% fields) Tweets$location <- substr(Tweets$location,1,50)
  
  title <- arguments$main
  
  # T1 <- Sys.time() #A ----
  
  Tweets$target<-ifelse(grepl("^RT @",Tweets[[text]]),sub(".*(@.*?):[ ].*","\\1",Tweets[[text]]),"")
  Tweets$author <- tolower(iconv(Tweets[[author]],to="ASCII//TRANSLIT"))
  
  attributes(Tweets[[date]])$tzone <-tzone
  
  if (all(grepl("^$",Tweets$target))) {
    TT <- aggregate(target~author, FUN = length, data=Tweets[Tweets$target=="",])
    names(TT) <- c("name","Tweets")
  }
  
  Messages <-
    Tweets [Tweets$target=="" & Tweets$author!="",]
  Messages <- Messages[order(Messages$date),]
  if("location" %in% fields) Messages$location <- substr(Messages$location,1,50)
  Messages <- Messages[!is.na(date), c("date", "author", "text")]
  
  
  netMentions <- mention(Messages)
  T.out <- as.data.frame(xtabs(X~Source,data=netMentions$links)); names(T.out) <- c("name","Mentions")
  T.in <-  as.data.frame(xtabs(X~Target,data=netMentions$links)); names(T.in) <- c("name","Mentioned")
  Graph <- toIgraph(netMentions)
  
  
  
  #T4 <- Sys.time() 
  
  Community <- membership(cluster_walktrap(Graph))
  Authors <- data.frame(name=attr(Community,"name"), Walktrap=as.vector(Community), stringsAsFactors = FALSE)
  Authors$Walktrap <- paste0("G-",sprintf("%03d",Authors$Walktrap))
  highGroups <- names(sort(table(Authors$Walktrap),decreasing = TRUE))[1:16] # 16 number of high groups
  Authors$Walktrap =ifelse(Authors$Walktrap %in% highGroups,Authors$Walktrap,"Rest")
  
  
  tweets <- Messages[,c("author", fields), drop=FALSE]
  AuthorsT <- aggregate(list(tweets[,fields]), by = list(tweets$author), max)
  names(AuthorsT)[1]<-"name"

  AuthorsT <- merge(AuthorsT, Authors, by="name", all.x=TRUE)
  AuthorsT <- AuthorsT[, c("name", fields), drop=FALSE]
  

  Authors$Degree   <- strength(Graph)
  Authors$label    <- ifelse(Authors$Degree>minlabel, as.character(Authors$name), "")
  Authors$size     <- ifelse(Authors$Degree>2, Authors$Degree, 0)
  Authors          <- merge(Authors, AuthorsT, by="name", all.x=TRUE)
  if(exists("TT")) {
    Authors<- merge(Authors, TT, by="name", all.x=TRUE)
    Authors$Tweets    <- ifelse(is.na(Authors$Tweets), 0, Authors$Tweets)
  }
  else Authors$Tweets <- 0
  Authors <- merge(Authors, T.out, by="name", all.x=TRUE)
  Authors <- merge(Authors, T.in ,  by="name", all.x=TRUE)
  Authors$link <- linkTwitter(Authors$name)
  arguments$image <- NULL
  if(!is.null(imagedir)) {
    Authors$image <- paste0(imagedir,"/",sub("@","",Authors$name),".",ext)
    arguments$image <- "image"
  }
  Authors <- Authors[, c("name", "link", "Tweets", "Mentions", "Mentioned", "Degree", 
                         fields, arguments$image, "Walktrap", "label", "size")]
  
  #T5 <- Sys.time() #E ----
  
  if (is.null(beginDate) || beginDate > max(Messages$date)) beginDate <- min(Messages$date)
  if (is.null(endDate)   || endDate   < min(Messages$date))   endDate <- max(Messages$date)
  
  if(!is.null(limits)) serie <- c(limits[limits > beginDate & limits <= endDate], as.POSIXct(endDate))
  else serie <- c(seq(as.POSIXct(beginDate)+ interval,as.POSIXct(endDate), interval), as.POSIXct(endDate))
  
  ncoin <- zoom <- main <- list()
  nFrames <- ifelse(nFrames==Inf,length(serie),nFrames)
  
  for (i in 1:nFrames) {
    serie <- serie[1:nFrames]
    names(serie)[i]<-paste0("S",i)
  }
  
  count <- 0
  for(i in names(serie)) {
    
    count=count+1; cat('\r', sprintf("%5.1f",(count)/nFrames*100), 
                       '% |', rep('=', count/nFrames*100), 
                       ifelse(count ==nFrames, '|\n',  '>'), sep = '')
    
    arguments$main <-  paste0(title, " ", as.character(serie[i], format="%d/%m/%Y, %H:%M")) 
    arguments$zoom  <- max(.50,arguments$zoom*(.985)^(count-1)) # .10 y .965
    rets  <- Messages[Messages$date <= serie[i],]
    
    
    tt <- # Original number of tweets by authorn
      as.data.frame(table(Messages[Messages[[date]]<= serie[i],author]))
    if(nrow(tt)==0) {
      tt <- as.data.frame(table(Messages[Messages[[date]]<=serie[i],author]))
      tt$Freq=0
      
    }
    names(tt) <- c("name","tweets")
    
    ncoin[[i]] <- mention(Messages[Messages[[date]]<=serie[i],c(author, text)])
    t.out <- as.data.frame(xtabs(X~Source, ncoin[[i]]$links)); names(t.out) <-c("name", "mentions")
    t.in  <- as.data.frame(xtabs(X~Target,data=ncoin[[i]]$links)); names(t.in)  <-c("name", "mentioned")
    
    graph <- toIgraph(ncoin[[i]])
    
    community <- membership(cluster_walktrap(graph))
    nodes <- data.frame(name=attr(community,"name"), walktrap=as.vector(community), stringsAsFactors = FALSE)
    nodes$walktrap  <- paste0("G-",sprintf("%03d", nodes$walktrap))
    nodes <- merge(nodes, t.out, by="name", sort=FALSE, all.x=TRUE)
    nodes <- merge(nodes, t.in, by="name", sort=FALSE, all.x=TRUE)
    nodes <- merge(nodes, Authors, by="name", sort=FALSE, all.x=TRUE)
    nodes <- merge(nodes, tt, by="name", all.x=TRUE)
    nodes$tweets    <- ifelse(is.na(nodes$tweets), 0, nodes$tweets)
    nodes$mentions    <- ifelse(is.na(nodes$mentions), 0, nodes$mentions)
    nodes$mentioned    <- ifelse(is.na(nodes$mentioned), 0, nodes$mentioned)
    nodes <- nodes[,c("name","link", "tweets", "mentions", "mentioned",
                      "Tweets", "Mentions","Mentioned", "Degree", 
                      fields, arguments$image,
                      "Walktrap", "walktrap", "size", "label")]
    
    ncoin[[i]]$nodes <- nodes
    arguments$nodes <- ncoin[[i]]
    ncoin[[i]] <- do.call(netCoin, arguments)
  }
  
  ncoin[["mode"]] <- "frame"
  if(exists("directory")) ncoin[["dir"]] <- directory
  
  #T7 <- Sys.time() #G ----
  
  # lista <- list("Set-up"= T2-T1, "Sample"= T3-T2, "Red"= T4-T3, 
  #              "Statistics"= T5-T4, "Loop"= T6-T5, "multigraph"= T7-T6, "Total"= T7-T1) # To check Sys.time() Add list to all <-
  Frames <- structure(ncoin, class="multigraph")
  all <- structure(list(authors= Authors, messages= Messages, nets = Frames), class="dyntweets")
  return(all)
}

d_cotweet <-function(Tweets, author="author", text="text", date="date",
                    fields=c("followers", "following", "statuses", "location"), searchers="@#",
                    beginDate = NULL, endDate= NULL, interval= 3600, tzone = "Europe/Paris", limits=NULL,
                    nFrames = Inf, seed=1, n = min(5000,nrow(Tweets)), minlabel=5, ...) {
  
  arguments <- list(...)
  
  if(!exists("label",      arguments)) arguments$label <- "name"
  if(!exists("size",       arguments)) arguments$size <- "Retweeted"
  if(!exists("labelSize",  arguments)) arguments$labelSize <- "size"
  if(!exists("color",      arguments)) arguments$color <- "Walktrap"
  if(!exists("repulsion",  arguments)) arguments$repulsion <- 3
  if(!exists("distance",   arguments)) arguments$distance <- 3
  if(!exists("zoom",       arguments)) arguments$zoom <-1
  if(!exists("main",       arguments)) arguments$main <- "Title"
  if(!exists("note",       arguments)) arguments$note <- "Elaborated with netCoin"
  if(!exists("showArrows", arguments)) arguments$showArrows <- FALSE
  if( exists("dir",        arguments)) {
    directory <- arguments$dir 
    arguments$dir <- NULL
  }
  if("location" %in% fields) Tweets$location <- substr(Tweets$location,1,50)
  
  title <- arguments$main
  
  # T1 <- Sys.time() #A ----
  
  Tweets$target<-ifelse(grepl("^RT @",Tweets[[text]]),sub(".*(@.*?):[ ].*","\\1",Tweets[[text]]),"")
  Tweets$author <- tolower(iconv(Tweets[[author]],to="ASCII//TRANSLIT"))
  
  attributes(Tweets[[date]])$tzone <-tzone
  
  if (all(grepl("^$",Tweets$target))) {
    TT <- aggregate(target~author, FUN = length, data=Tweets[Tweets$target=="",])
    names(TT) <- c("name","Tweets")
  }
  
  Messages <-
    Tweets [Tweets$target=="" & Tweets$author!="",]
  Messages <- Messages[order(Messages$date),]
  if("location" %in% fields) Messages$location <- substr(Messages$location,1,50)
  Messages <- Messages[!is.na(date), c("date", "author", "text")]
  
  
  netCoTweet <- cotweet(Messages, searchers=searchers)
  # T.out <- as.data.frame(xtabs(X~Source,data=netCoTweet$links)); names(T.out) <- c("name","Mentions")
  # T.in <-  as.data.frame(xtabs(X~Target,data=netCoTweet$links)); names(T.in) <- c("name","Mentioned")
  Graph <- toIgraph(netCoTweet)
  
  
  
  #T4 <- Sys.time() 
  
  Community <- membership(cluster_walktrap(Graph))
  Authors <- data.frame(name=attr(Community,"name"), Walktrap=as.vector(Community), stringsAsFactors = FALSE)
  Authors$Walktrap <- paste0("G-",sprintf("%03d",Authors$Walktrap))
  highGroups <- names(sort(table(Authors$Walktrap),decreasing = TRUE))[1:16] # 16 number of high groups
  Authors$Walktrap =ifelse(Authors$Walktrap %in% highGroups,Authors$Walktrap,"Rest")
  
  
  tweets <- Tweets[,c("author", fields), drop=FALSE]
  AuthorsT <- aggregate(list(tweets[,fields]), by = list(tweets$author), max)
  names(AuthorsT)[1]<-"name"
  AuthorsT <- merge(AuthorsT, Authors[,-2, drop=FALSE], by="name", all.x=TRUE)
  AuthorsT <- AuthorsT[, c("name", fields), drop=FALSE]
  
  
  Authors$Degree   <- strength(Graph)
  Authors$label    <- ifelse(Authors$Degree>minlabel, as.character(Authors$name), "")
  Authors$size     <- ifelse(Authors$Degree>2, Authors$Degree, 0)
  Authors          <- merge(Authors, AuthorsT, by="name", all.x=TRUE)
  Authors$link <- linkTwitter(Authors$name)
  if(exists("TT")) {
    Authors<- merge(Authors, TT, by="name", all.x=TRUE)
    Authors$Tweets    <- ifelse(is.na(Authors$Tweets), 0, Authors$Tweets)
  }
  else Authors$Tweets <- 0
  # Authors <- merge(Authors, T.out, by="name", all.x=TRUE)
  # Authors <- merge(Authors, T.in ,  by="name", all.x=TRUE)
  Authors <- Authors[, c("name", "link", "Tweets", "Degree", 
                         fields, "Walktrap", "label", "size")]
  
  #T5 <- Sys.time() #E ----
  
  if (is.null(beginDate) || beginDate > max(Messages$date)) beginDate <- min(Messages$date)
  if (is.null(endDate)   || endDate   < min(Messages$date))   endDate <- max(Messages$date)
  
  if(!is.null(limits)) serie <- c(limits[limits > beginDate & limits <= endDate], as.POSIXct(endDate))
  else serie <- c(seq(as.POSIXct(beginDate)+ interval,as.POSIXct(endDate), interval), as.POSIXct(endDate))
  
  ncoin <- zoom <- main <- list()
  nFrames <- ifelse(nFrames==Inf,length(serie),nFrames)
  
  for (i in 1:nFrames) {
    serie <- serie[1:nFrames]
    names(serie)[i]<-paste0("S",i)
  }
  
  count <- 0
  for(i in names(serie)) {
    
    count=count+1; cat('\r', sprintf("%5.1f",(count)/nFrames*100), 
                       '% |', rep('=', count/nFrames*100), 
                       ifelse(count ==nFrames, '|\n',  '>'), sep = '')
    
    arguments$main <-  paste0(title, " ", as.character(serie[i], format="%d/%m/%Y, %H:%M")) 
    arguments$zoom  <- max(.50,arguments$zoom*(.985)^(count-1)) # .10 y .965
    rets  <- Messages[Messages$date <= serie[i],]
    
    
    tt <- # Original number of tweets by authorn
      as.data.frame(table(Tweets[Tweets[[date]]<= serie[i] & Tweets$target=="",author]))
    if(nrow(tt)==0) {
      tt <- as.data.frame(table(Tweets[Tweets[[date]]<=serie[i],author]))
      tt$Freq=0
      
    }
    names(tt) <- c("name","tweets")
    
    if(count < length(serie)) {
      ncoin[[i]] <- cotweet(Tweets[Tweets[[date]]<=serie[i],c(author, text)], searchers=searchers)
      # t.out <- as.data.frame(xtabs(X~Source, ncoin[[i]]$links)); names(t.out) <-c("name", "mentions")
      # t.in  <- as.data.frame(xtabs(X~Target,data=ncoin[[i]]$links)); names(t.in)  <-c("name", "mentioned")
      graph <- toIgraph(ncoin[[i]])
    }
    else {
      ncoin[[i]] <- netCoTweet
      graph=Graph
    }
    
    community <- membership(cluster_walktrap(graph))
    nodes <- data.frame(name=attr(community,"name"), walktrap=as.vector(community), stringsAsFactors = FALSE)
    nodes$walktrap  <- paste0("G-",sprintf("%03d", nodes$walktrap))
    # nodes <- merge(nodes, t.out, by="name", sort=FALSE, all.x=TRUE)
    # nodes <- merge(nodes, t.in, by="name", sort=FALSE, all.x=TRUE)
    nodes <- merge(nodes, Authors, by="name", sort=FALSE, all.x=TRUE)
    nodes <- merge(nodes, tt, by="name", all.x=TRUE)
    nodes$tweets    <- ifelse(is.na(nodes$tweets), 0, nodes$tweets)
    # nodes$mentions    <- ifelse(is.na(nodes$mentions), 0, nodes$mentions)
    # nodes$mentioned    <- ifelse(is.na(nodes$mentioned), 0, nodes$mentioned)
    nodes <- nodes[,c("name", "tweets", "Tweets", "Degree", fields,
                      "Walktrap", "walktrap", "size", "label")]
    
    ncoin[[i]]$nodes <- merge(ncoin[[i]]$nodes, nodes, by="name")
    arguments$nodes <- ncoin[[i]]
    ncoin[[i]] <- do.call(netCoin, arguments)
  }
  
  ncoin[["mode"]] <- "frame"
  if(exists("directory")) ncoin[["dir"]] <- directory
  
  #T7 <- Sys.time() #G ----
  
  # lista <- list("Set-up"= T2-T1, "Sample"= T3-T2, "Red"= T4-T3, 
  #              "Statistics"= T5-T4, "Loop"= T6-T5, "multigraph"= T7-T6, "Total"= T7-T1) # To check Sys.time() Add list to all <-
  Frames <- structure(ncoin, class="multigraph")
  all <- list(authors= Authors, messages= Messages, nets = Frames)
  return(all)
}

d_cotext <-function(data, text="text", sep=" ", min=1, date="date",
                     beginDate = NULL, endDate= NULL, interval= 3600, tzone = "Europe/Paris", limits=NULL,
                     nFrames = Inf, seed=1, n = min(5000,nrow(data)), minlabel=5, ...) {
  
  arguments <- list(...)
  
  if(!exists("minimum", arguments)) minimum=1 else minimum <- arguments$minimum; arguments$minimum <- NULL
  if(!exists("maxL", arguments)) maxL <-  Inf else maxL <- arguments$maxL; arguments$maxL <- NULL
  if(!exists("minL", arguments)) minL <- -Inf else minL <- arguments$minL; arguments$minL <- NULL
  if(!exists("support", arguments)) support <- -Inf else support <- arguments$support; arguments$support <- NULL
  if(!exists("procedures", arguments)) procedures="frequencies" else procedures <- arguments$procedures; arguments$procedures <- NULL
  if(!exists("criteria", arguments)) criteria="frequencies" else criteria <- arguments$criteria; arguments$criteria <- NULL
  if(!exists("community", arguments))  community <- "Walktrap"
  if(!exists("color", arguments))      color     <- "community"
   
  if(!exists("label",      arguments)) arguments$label <- "name"
  if(!exists("size",       arguments)) arguments$size <- "frequencies"
  if(!exists("labelSize",  arguments)) arguments$labelSize <- "degree"
  if(!exists("color",      arguments)) arguments$color <- "Walktrap"
  if(!exists("repulsion",  arguments)) arguments$repulsion <- 3
  if(!exists("distance",   arguments)) arguments$distance <- 3
  if(!exists("zoom",       arguments)) arguments$zoom <-1
  if(!exists("main",       arguments)) arguments$main <- "Title"
  if(!exists("note",       arguments)) arguments$note <- "Elaborated with netCoin"
  if(!exists("showArrows", arguments)) arguments$showArrows <- FALSE
  if( exists("dir",        arguments)) {
    directory <- arguments$dir 
    arguments$dir <- NULL
  }

  
  title <- arguments$main
  
  # T1 <- Sys.time() #A ----
  
  attributes(data[[date]])$tzone <-tzone
  incidences <- dichotomize(data, text, sep=sep, add=F, min=min, nas="None")
  graph <- allNet(incidences, procedures=procedures, criteria=criteria, minimum=minimum, minL=minL, maxL=maxL, support=support,
                  community=community, color=color)
  arguments$nodes <- graph
  graph <- do.call(netCoin, arguments)
  tnodes <- graph$nodes
  names(tnodes) <-sub("%","Acum-%",names(tnodes))
  names(tnodes) <-sub("community",community,names(tnodes))
  if(!is.null(graph$links)){
  Degree <- as.data.frame(table(data.frame(nodes=c(graph$links$Source,graph$links$Target)))); names(Degree) <- c("name", "Degree")
  }
  else Degree <- data.frame(Degree=NULL)
  tnodes <- merge(tnodes,Degree)

  #T5 <- Sys.time() #E ----
  
  if (is.null(beginDate) || beginDate > max(data[[date]])) beginDate <- min(data[[date]])
  if (is.null(endDate)   || endDate   < min(data[[date]]))   endDate <- max(data[[date]])
  
  if(!is.null(limits)) serie <- c(limits[limits > beginDate & limits <= endDate], endDate)
  else serie <- c(seq(as.POSIXct(beginDate)+ interval,as.POSIXct(endDate), interval), as.POSIXct(endDate))
  
  ncoin <- zoom <- main <- list()
  nFrames <- ifelse(nFrames==Inf,length(serie),nFrames)
  
  for (i in 1:nFrames) {
    serie <- serie[1:nFrames]
    names(serie)[i]<-paste0("S",i)
  }
  
  count <- 0
  for(i in names(serie)) {
    
    count=count+1; cat('\r', sprintf("%5.1f",(count)/nFrames*100), 
                       '% |', rep('=', count/nFrames*100), 
                       ifelse(count ==nFrames, '|\n',  '>'), sep = '')
    
    arguments$main <-  paste0(title, " ", as.character(serie[i], format="%d/%m/%Y, %H:%M")) 
    arguments$zoom  <- max(.50,arguments$zoom*(.985)^(count-1)) # .10 y .965
    incs  <- incidences[data[[date]] <= serie[i],]
    
    
    if(count<length(serie)){
      ncoin[[i]] <- allNet(incs, procedures=procedures, criteria=criteria, minimum=minimum, minL=minL, maxL=maxL, support=support,
                         community=community, color=color)
      ncoin[[i]]$nodes <- merge(ncoin[[i]]$nodes, tnodes)
      arguments$nodes <- ncoin[[i]]
      ncoin[[i]] <- do.call(netCoin, arguments)
    }
    else {
      ncoin[[i]] <- graph
      ncoin[[i]]$options$main <- paste0(title, " ", as.character(serie[i], format="%d/%m/%Y, %H:%M")) 
      ncoin[[i]]$nodes <- merge(ncoin[[i]]$nodes, tnodes)
      names(ncoin[[i]]$nodes) <- names(ncoin[[1]]$nodes)
    }
  }
  
  ncoin[["mode"]] <- "frame"
  if(exists("directory")) ncoin[["dir"]] <- directory
  
  #T7 <- Sys.time() #G ----
  
  # lista <- list("Set-up"= T2-T1, "Sample"= T3-T2, "Red"= T4-T3, 
  #              "Statistics"= T5-T4, "Loop"= T6-T5, "multigraph"= T7-T6, "Total"= T7-T1) # To check Sys.time() Add list to all <-
  all <- structure(ncoin, class="multigraph")
  return(all)
}



d_retweet <-function(Tweets, author="author", text="text", date="date",
                   fields=c("followers", "following", "statuses", "location"),
                   beginDate = NULL, endDate= NULL, interval= 3600, tzone = "Europe/Paris",
                   nFrames = Inf, seed=1, n = min(5000,nrow(Tweets)), minlabel=5, ...) {
  
  arguments <- list(...)
  
  if(!exists("label",      arguments)) arguments$label <- "name"
  if(!exists("size",       arguments)) arguments$size <- "Retweeted"
  if(!exists("labelSize",  arguments)) arguments$labelSize <- "size"
  if(!exists("color",      arguments)) arguments$color <- "Walktrap"
  if(!exists("repulsion",  arguments)) arguments$repulsion <- 3
  if(!exists("distance",   arguments)) arguments$distance <- 3
  if(!exists("zoom",       arguments)) arguments$zoom <-1
  if(!exists("main",       arguments)) arguments$main <- "Title"
  if(!exists("note",       arguments)) arguments$note <- "Elaborated with netCoin"
  if(!exists("showArrows", arguments)) arguments$showArrows <- TRUE
  if( exists("dir",        arguments)) {
    directory <- arguments$dir 
    arguments$dir <- NULL
  }
  if("location" %in% fields) Tweets$location <- substr(Tweets$location,1,50)
  
  title <- arguments$main
  
  # T1 <- Sys.time() #A ----
  
  Tweets$target<-ifelse(grepl("^RT @",Tweets[[text]]),sub(".*(@.*?):[ ].*","\\1",Tweets[[text]]),"")
  
  attributes(Tweets[[date]])$tzone <-tzone
  
 if (all(grepl("^$",Tweets$target))) {
   TT <- aggregate(target~author, FUN = length, data=Tweets[Tweets$target=="",])
   names(TT) <- c("name","Tweets")
   }
  
  Messages <-
    Tweets [Tweets$target=="" & Tweets$author!="",]
  Messages <- Messages[order(Messages$date),]
  if("location" %in% fields) Messages$location <- substr(Messages$location,1,50)
  Messages <- Messages[!is.na(date), c("date", "author", "text")]
  
  
  Retweets  <- toRetweets(Tweets, fields=fields)
  RetweetsC <- toRmessages(Retweets)
 
  
  #T2 <- Sys.time() #B----
  
  
  RetweetsN <- RetweetsC[1:min(nrow(RetweetsC),round(n/2)), c("text","Date","Source")]
  
  
  set.seed(2020)
  nsample <- min(nrow(Retweets),max(n-nrow(RetweetsN),n-round(n/2)))
  SRTweets <- rbind(Retweets[sample(nrow(Retweets),nsample),c("text","Date","Source")], RetweetsN)
  SRTweets$Target  <- sub(".*(@.*?):[ ].*","\\1",SRTweets$text)
  SRTweets$message <- sub(".*?:[ ](.*)","\\1",SRTweets$text)
  SRTweets <- SRTweets[,c("Source", "Target", "message", "Date")]
  
  PRetweets <- base::setdiff(SRTweets[, c("message","Date","Source", "Target")], RetweetsN) 
  
  #T3 <- Sys.time()
  
  Links <- PRetweets[order(PRetweets$Date),c("Source", "Target", "Date")]
  
  
  Edges <- 
    aggregate(Date~Source+Target, FUN = length, data=Links)
  names(Edges) <- c("Source","Target","Weight")
  
  Graph <- graph_from_edgelist(as.matrix(Edges[,1:2]),directed=TRUE) # All and every day igraph objects
  igraph::E(Graph)$weight <- Edges$Weight
  
  #T4 <- Sys.time() 
  
  Community <- membership(cluster_walktrap(Graph))
  Authors <- data.frame(name=attr(Community,"name"), Walktrap=as.vector(Community), stringsAsFactors = FALSE)
  Authors$Walktrap <- paste0("G-",sprintf("%03d",Authors$Walktrap))
  highGroups <- names(sort(table(Authors$Walktrap),decreasing = TRUE))[1:16] # 16 number of high groups
  Authors$Walktrap =ifelse(Authors$Walktrap %in% highGroups,Authors$Walktrap,"Rest")
  
  
  tweets <- Tweets[,c("author", fields)]
  AuthorsT <- aggregate(list(tweets[,fields]), by = list(tweets$author), max)
  names(AuthorsT)[1]<-"name"
  AuthorsT$link <- paste0('<a href="https://twitter.com/', 
                          sub("^@","", AuthorsT$name),
                          '" target="_blank">', 
                          sub("^@","",AuthorsT$name),'</a>')
  AuthorsT <- merge(AuthorsT, Authors, by="name", all.x=TRUE)
  AuthorsT <- AuthorsT[, c("name","link", fields)]
  
  
  Authors$Retweets  <- strength(Graph, mode="out")
  Authors$Retweeted <- strength(Graph, mode="in")
  Authors$Degree   <- strength(Graph)
  Authors$label    <- ifelse(Authors$Degree>minlabel, as.character(Authors$name), "")
  Authors$size     <- ifelse(Authors$Degree>2, Authors$Degree, 0)
  Authors          <- merge(Authors, AuthorsT, by="name", all.x=TRUE)
  if(exists("TT")) {
    Authors<- merge(Authors, TT, by="name", all.x=TRUE)
    Authors$Tweets    <- ifelse(is.na(Authors$Tweets), 0, Authors$Tweets)
  }
  else Authors$Tweets <- 0
  Authors <- Authors[, c("name", "link", "Tweets", "Retweets", "Retweeted", "Degree", 
                         fields, "Walktrap", "label", "size")]
  
  #T5 <- Sys.time() #E ----
  
  if (is.null(beginDate) || beginDate > max(SRTweets$Date)) beginDate <- min(SRTweets$Date)
  if (is.null(endDate)   || endDate   < min(SRTweets$Date))   endDate <- max(SRTweets$Date)
  
  serie <- c(seq(as.POSIXct(beginDate)+ interval,as.POSIXct(endDate), interval), as.POSIXct(endDate))
  
  ncoin <- zoom <- main <- list()
  nFrames <- ifelse(nFrames==Inf,length(serie),nFrames)
  
  for (i in 1:nFrames) {
    serie <- serie[1:nFrames]
    names(serie)[i]<-paste0("S",i)
  }
  
  count <- 0
  for(i in names(serie)) {
    
    count=count+1; cat('\r', sprintf("%5.1f",(count)/nFrames*100), 
                       '% |', rep('=', count/nFrames*100), 
                       ifelse(count ==nFrames, '|\n',  '>'), sep = '')
    
    arguments$main <-  paste0(title, " ", as.character(serie[i], format="%d/%m/%Y, %H:%M")) 
    arguments$zoom  <- max(.50,arguments$zoom*(.995)^(count-1)) # .10 y .965
    rets  <- PRetweets[PRetweets$Date <= serie[i],]
    
    
    tt <- # Original number of tweets by authorn
      as.data.frame(table(Tweets[Tweets[[date]]<= serie[i] & Tweets$target=="",author]))
    if(nrow(tt)==0) {
      tt <- as.data.frame(table(Tweets[Tweets[[date]]<=serie[i],author]))
      tt$Freq=0
      
    }
    names(tt) <- c("name","tweets")
    
    if(count<length(serie)) {
      edges <- aggregate(Date~Source+Target, FUN = length, data=rets) # Tweets linksn
      names(edges) <- c("Source","Target","Weight")
    
      graph<-graph_from_edgelist(as.matrix(edges[,1:2]),directed=TRUE) # All and every day igraph objects
      igraph::E(graph)$weight<-edges$Weight
    }
    else graph <- Graph 
    
    community <- membership(cluster_walktrap(graph))
    nodes <- data.frame(name=attr(community,"name"), walktrap=as.vector(community), stringsAsFactors = FALSE)
    nodes$walktrap  <- paste0("G-",sprintf("%03d", nodes$walktrap))
    nodes$retweets  <- strength(graph, mode="out")
    nodes$retweeted <- strength(graph, mode="in")
    nodes <- merge(nodes, Authors, by="name", sort=FALSE, all.x=TRUE)
    nodes <- merge(nodes, tt, by="name", all.x=TRUE)
    nodes$tweets    <- ifelse(is.na(nodes$tweets), 0, nodes$tweets)
    nodes <- nodes[,c("name","link", "tweets", "retweets", "retweeted",
                      "Tweets", "Retweets","Retweeted", "Degree", 
                      fields,
                      "Walktrap", "walktrap", "size", "label")]
    
    graph <- delete_edge_attr(graph, "weight")
    ncoin[[i]] <- fromIgraph(graph)
    ncoin[[i]]$nodes <- merge(ncoin[[i]]$nodes, nodes, by="name")
    arguments$nodes <- ncoin[[i]]
    ncoin[[i]] <- do.call(netCoin, arguments)
  }
  
  ncoin[["mode"]] <- "frame"
  if(exists("directory")) ncoin[["dir"]] <- directory
  
  #T7 <- Sys.time() #G ----
  
  # lista <- list("Set-up"= T2-T1, "Sample"= T3-T2, "Red"= T4-T3, 
  #              "Statistics"= T5-T4, "Loop"= T6-T5, "multigraph"= T7-T6, "Total"= T7-T1) # To check Sys.time() Add list to all <-
  Frames <- structure(ncoin, class="multigraph")
  all <- list(authors= Authors, messages= Messages, Retweets= RetweetsC, retweets= PRetweets, nets =Frames)
  return(all)
}

plot.multigraph <- function(multigraph) {
  do.call(multigraphCreate, multigraph)
}
toRetweets <- function(tweets, author="author", target="target", date="date", text="text", fields=NULL) {
  Retweets <- tweets[tweets[[target]]!="" & tweets[[author]]!="" & !is.na(tweets[[date]]),c(date,author, target, text, fields),]
  names(Retweets) <- c("Date","Source","Target", "text", fields)
  return(Retweets)
}


toRmessages <- function(retweets, author="author", target="target", date="date", text="text"){
  retweets  <- retweets[order(retweets$Date),]
  retweets$O  <- ave(as.numeric(retweets$Date), retweets$text, FUN = seq_along)
  retweets$Retweets <- as.integer(ave(retweets$Target, retweets$text, FUN = length))
  retweets$first.Date    <- ave(retweets$Date, retweets$text, FUN = function(x) {head(x,1)})
  retweets$last.Date    <- ave(retweets$Date, retweets$text, FUN = function(x) {tail(x,1)})
  RetweetsC <- retweets[retweets$O==1,]
  RetweetsC$message <- ave(as.numeric(RetweetsC$Date), RetweetsC$Target, FUN = seq_along)
  RetweetsC$message <- paste(sub("^@","",RetweetsC$Target),sprintf("%03d",RetweetsC$message),sep="#")
  RetweetsC <- RetweetsC[order(RetweetsC$Retweets, decreasing= TRUE),c("text","Source","Target", "message", "Retweets", "Date", "last.Date")]
}


