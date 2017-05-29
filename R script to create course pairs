
# R script to create pairs of courses 

# read the data 
data <- read.csv("mockEnrollmentData.csv", stringsAsFactors=FALSE)
data <- data %>%
  mutate(COURSE=paste0(COURSE_SUBJECT, COURSE_NUMBER)) %>%
  select(STUDENT_IDENTIFIER, COURSE, RELATIVE_TERM)

#test the function
testStudentID = ""
test <- data %>%
  filter(STUDENT_IDENTIFIER == testStudentID) %>%
  group_by(STUDENT_IDENTIFIER) %>%
  mutate(COURSE = list(as.data.frame(t(combn(COURSE, 2)), stringsAsFactors=FALSE))) %>%
  unnest(.sep=("_"))

#find a single student
#create a table with course pairs
#find the relative term for course1 and course2
#do for all students
duplicate.fn <- function(x) {
  thisStudent <- data %>%
    filter(STUDENT_IDENTIFIER == x)
  
  if (length(unique(thisStudent$COURSE)) >1) {
    
    new <- thisStudent %>%
      group_by(STUDENT_IDENTIFIER) %>%
      summarize(COURSE = list(as.data.frame(t(combn(COURSE, 2)), stringsAsFactors=FALSE))) %>%
      unnest(.sep=("_"))
    
    courseOrder <- thisStudent %>%
      select(COURSE, RELATIVE_TERM)
    
    courseOrder2 <- courseOrder
    colnames(courseOrder2) <- c("COURSE2", "RELATIVE_TERM2")
    
    new1 <- merge(new, courseOrder, by.x="COURSE_V1", by.y="COURSE")
    new2 <- merge(new1, courseOrder2, by.x="COURSE_V2", by.y="COURSE2")
    
  } else {
    new2 <- thisStudent %>%
      mutate(COURSE_V1 = COURSE, COURSE_V2=COURSE)
  }
  
  new2
}

#find list of students
allStudents <- as.list(unique(data$STUDENT_IDENTIFIER))

#apply function to all students
data_new <- ldply(allStudents, duplicate.fn)

#find the term difference (COURSE1 - COURSE2)
#change order of ouput
#create csv
data_new <- data_new %>%
  mutate(TERM_DIFFERENCE = RELATIVE_TERM-RELATIVE_TERM2) %>%
  select(STUDENT_IDENTIFIER, COURSE_V1, RELATIVE_TERM, COURSE_V2, RELATIVE_TERM2, TERM_DIFFERENCE)

write.csv(data_new, 'enrollmentData_coursePairs.csv', row.names=FALSE)
