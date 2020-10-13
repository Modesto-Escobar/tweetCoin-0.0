credentials <- function(file = "credentials.txt") {
    credenciales <- read.csv(file, encoding="UTF-8", sep=";", quote="'", header=TRUE, stringsAsFactors=FALSE)
    appname <-credenciales$appname
    consumerkey <- credenciales$consumer_key
    consumersecret <- credenciales$consumer_secret
    accesstoken <- credenciales$access_key
    accesstokensecret <- credenciales$access_secret
    TT <- create_token(app = appname, consumer_key = consumerkey, consumer_secret = consumersecret, 
                       access_token = accesstoken, access_secret = accesstokensecret, set_renv = FALSE)
    assign("token", TT, envir = .GlobalEnv)
  }

load_tweets <- function(name, type = "user", format = "binary"){
  
  if (format == 'binary')
  {
    extension <- ".dat"
  } else  if (format == "delimited")
  {
    extension <- ".csv"
  }
  
  if (type == "user")
  {
    output_file_name <- paste0("user_tweets_", name, extension)
  }
  else if (type == "search")
  {
    output_file_name <- paste0("search_tweets_", name, extension)
  }
  else if (type == "stream")
  {
    output_file_name <- paste0("stream_tweets_", name, extension)
  }
  
  if (format == "binary")
  {
    load(output_file_name, .GlobalEnv )
    
  } else if (format == "delimited")
  {
    tuits<-read_twitter_csv(file = output_file_name, unflatten = FALSE)
    return(tuits)
    
  }
  
  
}

user_tweet <- function(user, maxtweets = 100, home = FALSE, parse = TRUE, check = TRUE, token = NULL, include_rts = FALSE, output_file_name = NULL, format = "binary"){
  if (format == 'binary')
  {
    extension <- ".dat"
  } else  if (format == "delimited")
  {
    extension <- ".csv"
  }
  
  if(is.null(output_file_name)){
    output_file_name <- paste0("user_tweets_", user, extension)
  }
  
  if(!(output_file_name %in% list.files())){
    datos_new <- get_timeline(user = user, n = maxtweets, home = home, parse = parse, check = check, token = token, include_rts = include_rts)
    if (format == 'binary')
    {
      save( datos_new, file=output_file_name)
    } else  if (format == "delimited")
    {
      write_as_csv(x = datos_new, file_name = output_file_name)
    }
    
    
    print(paste("Numero de tweets nuevos:", nrow(datos_new)))
    print("Nuevo fichero creado")
  }else{
    if (format == 'binary')
    {
      load(output_file_name, .GlobalEnv )
      datos_old <- datos_new
      
      
    } else  if (format == "delimited")
    {
      datos_old<-read_twitter_csv(file = output_file_name, unflatten = TRUE)
    }
    
    
    ultimo_id <- datos_old[1, ]$status_id
    ultimo_id = toString(as.bigz(ultimo_id) + 1)
    datos_new <- get_timeline(user = user, n = maxtweets, since_id = ultimo_id,
                              home = home, parse = parse, check = check, token = token, include_rts = include_rts)
    write_as_csv(x = datos_new, file_name = "./tmp.csv")
    datos_new<-read_twitter_csv(file = "./tmp.csv", unflatten = TRUE)
    tuits <- rbind(datos_new, datos_old)
    if (format == "binary")
    {
      
      save( tuits, file=output_file_name)
      
    } else if (format == "delimited")
    {
      write_as_csv(x = tuits, file_name = output_file_name)
      
    }
    
    print(paste("Numero total de tweets:", nrow(tuits)))
    print(paste("Numero de tweets nuevos:", nrow(datos_new)))
  }
}

search_tweet <- function(search, maxtweets = 300, type = "recent", include_rts = TRUE, geocode = NULL, max_id = NULL, parse = TRUE, token = NULL, retryonratelimit = FALSE, verbose = TRUE, output_file_name = NULL){
  if(is.null(output_file_name)){
    output_file_name <- paste0("search_tweets_", search, ".csv")
    
  }
  
  if(!(output_file_name %in% list.files())){
    datos_new <- search_tweets(search, n = maxtweets, type = type, include_rts = include_rts, geocode = geocode, max_id = max_id, parse = parse, token = token, retryonratelimit = retryonratelimit, verbose = verbose)
    write_as_csv(x = datos_new, file_name = output_file_name)
    
    print(paste("Numero de tweets nuevos:", nrow(datos_new)))
    print("Nuevo fichero creado")
  }else{
    
    datos_old<-read_twitter_csv(file = output_file_name, unflatten = TRUE)
    
    ultimo_id <- datos_old[nrow(datos_old),]$status_id
    
    ultimo_id = toString(as.bigz(ultimo_id) + 1)
   
    datos_new <- search_tweets(search, n = maxtweets, type = type, include_rts = include_rts, geocode = geocode, max_id = ultimo_id, parse = parse, token = token, retryonratelimit = retryonratelimit, verbose = verbose)
    write_as_csv(x = datos_new, file_name = "./tmp.csv")
    datos_new<-read_twitter_csv(file = "./tmp.csv", unflatten = TRUE)
    datos_concatenados <- rbind(datos_old, datos_new)
    write_as_csv(x = datos_concatenados, file_name = output_file_name)
    print(paste("Numero total de tweets:", nrow(datos_concatenados)))
    print(paste("Numero de tweets nuevos:", nrow(datos_new)))
  }

}

stream_tweet <- function(search, timeout = 120, parse = TRUE, token = NULL, output_file_name = search, verbose = TRUE, dir = NULL){
    datos <- stream_tweets(search, timeout = timeout, parse = parse, token = token, file_name = output_file_name, verbose = verbose)
    
    output_file_name_json <- paste0(output_file_name, ".json")
    datos_new <- parse_stream(output_file_name_json)
    output_file_name_csv <- paste0("stream_tweets_", output_file_name, ".csv")
    write_as_csv(x = datos_new, file_name = output_file_name_csv)
    
    print(paste("Numero de tweets:", nrow(datos_new)))
    
    
    if (file.exists(output_file_name_json))
    {
      file.remove(output_file_name_json)
    }
  
  
}

follow_tweet <- function(user, page = "-1", retryonratelimit = TRUE, parse = TRUE, verbose = TRUE, token = NULL){
  look <- lookup_users(user)
  datos_perfil <- users_data(look)
  num_seguidores <- datos_perfil$followers_count
  print(paste("Numero de seguidores:", num_seguidores))
  print ("Indica el numero de seguidores que quieres recuperar. Si se superan 75000 tendra periodos de espera de 15 minutos. Sera un proceso lento.")
  n <- scan()
  parte_entera <- floor(n/75000)
  parte_decimal <- (n/75000) - floor(n/75000)
  if ((parte_entera == 0) | ((parte_entera == 1) & (parte_decimal == 0)))
  {
    partes <- 1
  }
  else
  {
    partes <- parte_entera
    
    resto <- n - (partes * 75000)
    if (parte_decimal > 0)
    {
      partes <- partes + 1
    }
  }
  if (partes == 1)
  {
    
    seguidores_id <- get_followers(user, n, page = page, retryonratelimit = retryonratelimit, parse = parse, verbose = verbose, token = token)
    
  }
  else
  {
    for (i in 1:partes)
    {
      if (i == 1)
      {
        seguidores_id <- get_followers(user, n = 75000, page = page, retryonratelimit = retryonratelimit, parse = parse, verbose = verbose, token = token)
        next_page <- next_cursor(seguidores_id)
      }
      else if (i > 1 & i < partes)
      {
        seguidores_id2 <- get_followers(user, n = 75000, page = next_page, retryonratelimit = retryonratelimit, parse = parse, verbose = verbose, token = token)
        next_page <- next_cursor(seguidores_id2)
        seguidores_id <- rbind(seguidores_id, seguidores_id2)
      }
      else if (i == partes)
      {
        seguidores_id2 <- get_followers(user, n = resto, page = next_page, retryonratelimit = retryonratelimit, parse = parse, verbose = verbose, token = token)
        seguidores_id <- rbind(seguidores_id, seguidores_id2)
      }
    }
  }
  seguidores <- lookup_users(as_factor(seguidores_id$user_id))
  seguidores_final <- users_data(seguidores)
}

prepare <- function(data=data, info=c("fields", "cotweets", "inform"), original= TRUE, quote=FALSE, retweet=FALSE) {
  if ("fields" %in% info) fields <- c("screen_name","text","created_at","is_quote", "is_retweet") else fields <- NULL
  if ("cotweets" %in% info) cotweets <- c("quoted_screen_name", "retweet_screen_name") else cotweets <- NULL
  if ("inform" %in% info) inform <- c("followers_count", "friends_count", "statuses_count") else inform <- NULL
  Inform <- c("followers", "following", "stauses")
  data$intro <- ifelse(data$is_quote==FALSE & data$is_retweet==FALSE & original==TRUE, TRUE, FALSE)
  data$intro <- ifelse((data$is_quote==quote & quote==TRUE) | (data$is_retweet==retweet & retweet==TRUE), TRUE, data$intro)
  newdata <- data[data$intro, c(fields, cotweets, inform)]
  newdata$date  <- as.POSIXct(newdata$created_at)
  newdata$text  <- ifelse(newdata$is_retweet, paste0("RT @", newdata$retweet_screen_name, ": ", newdata$text), newdata$text)
  newdata$text  <- ifelse(newdata$is_quote  , paste0("RT @", newdata$quoted_screen_name, ": ", newdata$text), newdata$text)
  names(newdata) <- sub("^screen_name","author", names(newdata))
  newdata <- newdata[,c("author","text","date",inform)]
  names(newdata)[4:dim(newdata)[2]] <- Inform
  return(newdata)
}

prueba <- function (datos)
{
  print (datos)
}
  

