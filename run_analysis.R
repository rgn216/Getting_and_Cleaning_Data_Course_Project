run_analysis <- function(){
      
      # Importing the data
      
      features <- read.table("UCI HAR Dataset/features.txt",colClasses = "character")
      X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
      Y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
      subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
      X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
      Y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
      subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")      
      # Merging the datasets
      data_train <- cbind(  X_train,subject_train , Y_train  )            
      data_test <- cbind( X_test,subject_test , Y_test  )      
      data <- rbind(data_train , data_test )
      
      # renaming the variables
      
      names(data) <- c( features$V2 , "Subject" , "Activity")
      
      # select the variables that contain the words "mean" or "std"
      filtered_features <- filter( features , grepl( "mean()" , V2  )|grepl( "std()" , V2  ) )        
      filtered_data<- data[ , c(as.numeric(filtered_features$V1) , 562 , 563)]
      
      # modifying the activity variable
      
      fdata <- factor(filtered_data$Activity)
      levels(fdata) = c("WALKING" , "WALKING_UPSTAIRS" , "WALKING_DOWNSTAIRS" , "SITTING" ,"STANDING" ,"LAYING")
      filtered_data$Activity <- fdata      
      
      # summarizing by Subject and Activity, using the aggregate function
      
      tidy_data <- aggregate( . ~ Subject + Activity, data = filtered_data, FUN = 'mean')     
      tidy_data <- select( tidy_data ,1:79 )
      write.table( tidy_data , file = "tidy_data.txt" , row.name=FALSE) 

}