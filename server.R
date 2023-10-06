## server.R
library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)
library(Matrix)

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings <- function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
  print(dat$Rating)
  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  user_rgs <- sparseMatrix(i = dat$MovieID, 
                               j = rep(1,nrow(dat)), 
                               x = dat$Rating, 
                               dims = c(nrow(ratingmat), 1))
 return(user_rgs)
}



remommendlist<-function(ratings, movie, MovieGenres,listsize){
   
  tmp = ratings %>% 
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
    inner_join(movie, by = 'MovieID')
  
  #PreferGenrebyUser = MovieGenres
  
  BestRatingbyUser=tmp[tolower(tmp$Genres)%in%tolower(MovieGenres),]
  #head(sort(BestRatingbyUser$ave_ratings, decreasing=TRUE), 5)
  n = min(listsize, nrow(BestRatingbyUser))
  RecommendList <-head(BestRatingbyUser[order(-BestRatingbyUser$ave_ratings),], n) #BestRatingbyUser[sample(nrow(BestRatingbyUser),n ),]
  return(RecommendList)
}
# read in data
#books <- fread('data/books.csv')
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)

# convert accented characters
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
# extract year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))
#ratings <- fread('data/ratings_cleaned.csv')

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL

ratings <- ratings  %>%select('MovieID','UserID','Rating')

ratingmat <- sparseMatrix(ratings$MovieID, ratings$UserID, x=ratings$Rating,dimnames = list(MovieID =as.character(1:3952)
                                                                                            , UserID=as.character(sort(unique(ratings$UserID)))) ) # book x user matrix
ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] 


shinyServer(function(input, output, session) {
  
  # show the books to be rated
  
  #moviesselection<-
  output$ratings<- renderUI({
  # books per row
    
    moviesselection<-remommendlist(ratings,movies,input$select,10) 
    numberofmovies = nrow(moviesselection)
    if(numberofmovies <=5){
      num_rows <- 1
      num_movies <- numberofmovies 
      
    }else{
    num_rows <- 2
    num_movies <- 5 
    }
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = moviesselection$image_url[(i - 1) * num_movies + j], style = "max-height:150")),
                 div(style = "text-align:center; color: #999999; font-size: 80%", moviesselection$Genres[(i - 1) *num_movies + j]),
                 div(style = "text-align:center", strong(moviesselection$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", moviesselection$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)
        
        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        
        user_ratings <- get_user_ratings(value_list)
        
        # add user's ratings as first column to rating matrix
        rmat <- cbind(user_ratings, ratingmat)
        
        # get the indices of which cells in the matrix should be predicted
        # predict all books the current user has not yet rated
        items_to_predict <- which(rmat[, 1] == 0)
        prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
        
        # run the ubcf-alogrithm
        res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
        
        # sort, organize, and return the results
        user_results <- sort(res[, 1], decreasing = TRUE)[1:10]
        user_predicted_ids <- as.numeric(names(user_results))
        recom_results <- data.table(Rank = 1:10, 
                                    MovieID = user_predicted_ids, 
                                    Genres = movies$Genres[user_predicted_ids], 
                                    Title = movies$Title[user_predicted_ids], 
                                    Predicted_rating =  user_results)
    }) # still busy
    
  }) # clicked on button
  

  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_books <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_books, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_books + j),
            
          div(style = "text-align:center", 
              a(href = paste0('https://www.goodreads.com/book/show/', movies$MovieID[recom_result$MovieID[(i - 1) * num_books + j]]), 
                target='blank', 
                img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_books + j]], height = 150))
             ),
          div(style = "text-align:center; color: #999999; font-size: 80%", 
              movies$Genre[recom_result$MovieID[(i - 1) * num_books + j]]
             ),
          div(style="text-align:center; font-size: 100%", 
              strong(movies$Title[recom_result$MovieID[(i - 1) * num_books + j]])
             )
          
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function
