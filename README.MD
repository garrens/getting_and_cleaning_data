---
title: "Read Me"
output: html_document
---

This Github repository contains my submission to complete my work on the class project for the "Getting and Cleaning Data" course by Jeff Leek, PhD, Brian Caffo, PhD, Roger D. Peng, PhD.   
The submission consists if the following components:  
1. Readme.MD - This Read Me file  
2. Codebook.MD - a description of the data elements in the included data sets  
3. Merged.tbl - a text file suitable to be read by 'read.table' that contains the extracted means and standard deviations labeled  as specified in the project requirements  
4. Average-by-subject.tbl -  text file suitable to be read by 'read.table' that contains the means of each extracted mean and standard deviation in file 3 above by experimental subject  
4. Average-by-activity.tbl -  text file suitable to be read by 'read.table' that contains the means of each extracted mean and standard deviation in file 3 above by activity  
5. run_analysis.R - an R script that creates the data sets above from the raw data files provided.  

I made a subjective decision on naming the variables that effect the results of my project. The source dataset contains names for the data that are not as descriptive as they might be and in some cases contain apparent errors where the word body is repeated. There is little information that would allow the names to be expanded to make them more clear and doing so would make connecting the processed data back to the source more difficult so I decided to use the provided names.  
The R script performs the following steps which are identified in comments in the script file itself in addition to here.

* reads the training and test data sets using `read.fwf` to process the 16 character fixed width fields  
* reads the features.txt file using as.is=TRUE to input the items as character strings for later processing rather than as the default factor type  
* combines the two sets into one table using rbind
* reads the activity_labels.txt file using header=FALSE,stringsAsFactors = FALSE
* get subjects data identifies the subject for each data item 
* converts the activity_ID to a factor and assigns the corresponding names  
* changes subject_ID to factor
* saves simplified table as simplified.txt 
* calculates the averages by activity and subject of the selected data types per the instructions by:  
+ using split to create data frame broken out by subject and activity
+ assigning appropriate row and column names
+ writing out the resulting tables as average_by_activity.txt and average_by_subject.txt
