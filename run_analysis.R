require(plyr)

# Read the original data files

x_train <- read.table("train/X_train.txt", sep ="")
y_train <- read.table("train/y_train.txt", sep = "")
subject_train <- read.table("train/subject_train.txt", sep = "")

x_test <- read.table("test/X_test.txt", sep = "")
y_test <- read.table("test/y_test.txt", sep = "")
subject_test <- read.table("test/subject_test.txt", sep = "")

features <- read.table("features.txt", colClasses = c("character"))
activity_labels <- read.table("activity_labels.txt", col.names = c("ActivityId", "Activity"))

### 1. Merging the Test and Training data to create one complete data set

training_data <- cbind(x_train, subject_train, y_train)
test_data <- cbind(x_test, subject_test, y_test)
complete_data <- rbind(training_data, test_data)

# Label columns 562 and 563 that we merged above
sensor_labels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(complete_data) <- sensor_labels

### 2. Extracting the measurements that are mean and standard deviation from the complete data

mean_std_data<- complete_data[,grepl("mean|std|Subject|ActivityId", names(complete_data))]

### 3. Change names from activity index to descriptive name

mean_std_data <- join(mean_std_data, activity_labels, by = "ActivityId", match = "first")
mean_std_data <- mean_std_data[,-1] # remove the ActivityId column

### 4. Change labels to descriptive names

# remove "()/-" etc
names(mean_std_data) <- gsub('[()-]', '', names(mean_std_data))

# Clean up names
names(mean_std_data) <- gsub('\\.mean',".Mean",names(mean_std_data))
names(mean_std_data) <- gsub('\\.std',".Std",names(mean_std_data))

names(mean_std_data) <- gsub('Acc',"Acceleration",names(mean_std_data))
names(mean_std_data) <- gsub('GyroJerk',"AngularAcceleration",names(mean_std_data))
names(mean_std_data) <- gsub('Gyro',"AngularSpeed",names(mean_std_data))
names(mean_std_data) <- gsub('Mag',"Magnitude",names(mean_std_data))
names(mean_std_data) <- gsub('^t',"TimeDomain.",names(mean_std_data))
names(mean_std_data) <- gsub('^f',"FrequencyDomain.",names(mean_std_data))
names(mean_std_data) <- gsub('Freq\\.',"Frequency.",names(mean_std_data))
names(mean_std_data) <- gsub('Freq$',"Frequency",names(mean_std_data))
names(mean_std_data) <- gsub('BodyBody',"Body",names(mean_std_data))


### 5. Creates a second, independent tidy data set 

TidyData = ddply(mean_std_data, c("Subject","Activity"), numcolwise(mean))
write.table(TidyData, file = "TidyData.txt")