run_analysis <- function() {
	library(reshape2)

	#Load data for 'train'
	print("loading x for train")
	xTr <- read.csv("UCI HAR Dataset/train/X_train.txt", sep = "", header = FALSE)
	print("loading y for train")
	yTr <- read.csv("UCI HAR Dataset/train/y_train.txt", sep = "", header = FALSE)
	print("loading subject for train")
	sTr <- read.csv("UCI HAR Dataset/train/subject_train.txt", sep = " ", header = FALSE)

	#Load data for 'test'
	print("loading x for test")
	xTe <- read.csv("UCI HAR Dataset/test/X_test.txt", sep = "", header = FALSE)
	print("loading y for test")
	yTe <- read.csv("UCI HAR Dataset/test/y_test.txt", sep = "", header = FALSE)
	print("loading subject for test")
	sTe <- read.csv("UCI HAR Dataset/test/subject_test.txt", sep = " ", header = FALSE)

	print("loading features")
	feat <- read.csv("UCI HAR Dataset/features.txt", sep = " ", header = FALSE)
	print("loading activities")
	act <- read.csv("UCI HAR Dataset/activity_labels.txt", sep = " ", header = FALSE)

	#Merge data
	print("merging test")
	test <- cbind(xTe, yTe, sTe)
	print("merging train")
	train <- cbind(xTr, yTr, sTr)
	print("merging test & train")
	data <- rbind(test, train)

	#Label data (removing special chars and apply as column names)
	print("turning features to column names")
	feat <- gsub("-", ".", gsub("[\\(\\)]", "", as.character(feat$V2)))
	columns <- c(feat, "activity", "subject")
	colnames(data) <- columns

	#Label activites
	print("applying activities as labels")
	for (i in 1:6) {
	  data$activity[data$activity == i] <- as.character(act$V2[[i]])
	}

	#Extract means and standard deviations
	print("extracting MEANs and STDs")
	m_and_s <- subset(data, select = columns[grep("mean\\.|mean$|std\\.|std$|activity|subject", columns)])

	#Now reshape as tidy and output
	print("reshaping/tidying")
	m_and_s_melt <- melt(m_and_s, id=c("subject", "activity"), measure.vars=colnames(m_and_s)[1:66])
	tidy_data <- dcast(m_and_s_melt, subject + activity ~ variable, mean)

	print("writing output")
	write.table(tidy_data, "getting_and_cleaning_data.txt", row.name=FALSE)
}