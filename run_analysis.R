  
## combine training and test datasets
  setwd("~/getting data/project")
  wd<-getwd()
  features<-read.table("features.txt",as.is=TRUE)
  setwd(paste(wd,"/train",sep=""))
  setwd("~/getting data/project")
  
  wd<-getwd()
  ##Create list of filed widths for read.fwf function - 16ch*561 variables
  ws<-c(16)
  for (i in 1:560) ws<-c(ws,16)
  ##load test data set
  setwd(paste(wd,"/test",sep=""))
  in_fwf1<-read.fwf("X_test.txt",ws,buffer=100)
  ##load training dataset
  setwd(paste(wd,"/train",sep=""))
  in_fwf<-read.fwf("X_train.txt",ws,buffer=100)
  ## combine them
  combined<-rbind(in_fwf,in_fwf1)
  ##write intermediate result to save debug time later
  write.table(combined,file="file.tbl")
  
##get labels
setwd("~/getting data/project")
activity_labels<-read.csv("activity_labels.txt", sep=" ",header=FALSE,stringsAsFactors = FALSE)
##get training data
setwd("~/getting data/project/train")
subject_train<-read.csv("subject_train.txt", sep=" ",header=FALSE,stringsAsFactors = FALSE)
Y_train<-read.csv("Y_train.txt", sep=" ",header=FALSE,stringsAsFactors = FALSE)
##get test data
setwd("~/getting data/project/test")
subject_test<-read.csv("subject_test.txt", sep=" ",header=FALSE,stringsAsFactors = FALSE)
Y_test<-read.table("Y_test.txt",header=FALSE,stringsAsFactors = FALSE)
  Y_test<-Y_test+5
setwd("~/getting data/project/train")
Y_train<-read.table("Y_train.txt",header=FALSE,stringsAsFactors = FALSE)
  setwd("~/getting data/project/train")
  subject_train<-read.csv("subject_train.txt", sep=" ",header=FALSE,stringsAsFactors = FALSE)  
Activity_id<-rbind(Y_test,Y_train)
Subject_id<-rbind(subject_test,subject_train)  

colnames(combined)<-c("activity_ID",features$V2)
selected_features<-grep("mean()|std()|activity",colnames(combined))
simplified<-combined[,selected_features]
  combined<-cbind(Activity_id,subject_id,combined)
##
  ##get training data
  setwd("~/train")
  subject_train<-read.csv("subject_train.txt", sep=" ",header=FALSE,stringsAsFactors = FALSE)
  X_train<-read.csv("X_train.txt", sep=" ",header=FALSE,stringsAsFactors = FALSE)
  Y_train<-read.csv("Y_train.txt", sep=" ",header=FALSE,stringsAsFactors = FALSE)
  ##get test data
  setwd("~/getting data/project/test")
  Y_test<-read.table("Y_test.txt",header=FALSE,stringsAsFactors = FALSE)
  setwd("~/getting data/project/train")
  Y_train<-read.table("Y_train.txt",header=FALSE,stringsAsFactors = FALSE)
  Activity_id<-rbind(Y_test,Y_train)
  colnames(Subject_id)<-"subject_id"
  simplified<-cbind(Subject_id,simplified)
  colnames(simplified)<-gsub("-","_",colnames(simplified))
  colnames(simplified)<-gsub("\\.","",make.names(colnames(simplified)))
  ##change activity_ID to a factor and change numbers to corresponding names
  simplified$activity_ID<-factor(simplified$activity_ID)
  library(plyr)
  simplified$activity_ID<-revalue(simplified$activity_ID, c("1"="WALKING", "2" ="WALKING_UPSTAIRS","3"="WALKING_DOWNSTAIRS","4"="SITTING","5"="STANDING","6"="LAYING"))
  ##change subject_ID to factor
  simplified$subject_id<-factor(simplified$subject_id)
  ## save simplified table
  write.table(simplified,file="simplified.tbl")
#now we create tables of averages by activity and subject
  ##calculate averages by subject
  by_activities<-split(simplified,simplified$activity_ID)
  average_by_activity<-NULL
  for (a in by_activities){
    for (r in 3:81){
      average_by_activity<-cbind(mean(a[,r],na.rm=TRUE),average_by_activity)  
    }
  }
  dim(average_by_activity)<-c(6,79)
  rownames(average_by_activity)<-activity_labels[,2]
  colnames(average_by_activity)<-colnames(simplified)[3:81]
  colnames(average_by_activity)<-paste("average_",colnames(average_by_activity),sep="")
##and now calculate averages by subject ID
  by_subject<-split(simplified,simplified$subject_id)
  average_by_subject<-NULL
  for (s in by_subject){
    for (r in 3:81){
      average_by_subject<-cbind(mean(a[,r],na.rm=TRUE),average_by_subject)  
    }
  }
  dim(average_by_subject)<-c(30,79)
  colnames(average_by_subject)<-colnames(simplified)[3:81]
  colnames(average_by_subject)<-paste("average_",colnames(average_by_subject),sep="")
  rownames(average_by_subject)<-paste("Subject", 1:30)
  write.table(average_by_activity,"average_by_activity.txt",)
  write.table(average_by_subject,"average_by_subject.txt",)
