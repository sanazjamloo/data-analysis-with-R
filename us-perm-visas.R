# Import necessary libraries
library(dplyr)
library(ggplot2)
library(e1071)
library(ggthemes)
library(gridExtra)
library(plotrix)
library(arules)
library(caret)
library(tidyverse)
library(magrittr)
library(stringr)
library(data.table)
require(scales)


# load the dataset
dataset <- read.csv("/Users/Sanaz/Downloads/us_perm_visas.csv", header = TRUE, sep = ",", stringsAsFactors= FALSE)

# Get the required columns for this analysis
application_type <- dataset$application_type
case_received_date <- dataset$case_received_date
case_status <- dataset$case_status
class_of_admission <- dataset$class_of_admission
country_of_citizenship <- dataset$country_of_citizenship
country_of_citizenship.1 <- dataset$country_of_citizenship.1
decision_date <- dataset$decision_date
employer_city <- dataset$employer_city
employer_name <- dataset$employer_name
employer_state <- dataset$state
foreign_worker_info_birth_country <- dataset$foreign_worker_info_birth_country
foreign_worker_info_birth_country.1 <-dataset$foreign_worker_info_birth_country.1
foreign_worker_info_education <- dataset$foreign_worker_info_education
foreign_worker_info_major <- dataset$foreign_worker_info_major
job_info_title <- dataset$job_info_job_title
job_info_work_state <- dataset$job_info_work_state
us_economic_sector <- dataset$us_economic_sector
wage_offer_from_9089   <- dataset$wage_offer_from_9089
wage <- suppressWarnings(as.numeric(dataset$pw_amount_9089))

#Create a new dataset with all the columns above
my_data <- data.frame(cbind(
  application_type,
  case_received_date,
  case_status,
  class_of_admission,
  country_of_citizenship,
  decision_date,
  employer_city,
  employer_name,
  employer_state,
  foreign_worker_info_birth_country,
  foreign_worker_info_education,
  foreign_worker_info_major,
  job_info_title,
  us_economic_sector,
  wage_offer_from_9089
))

# Observing the dataset structure, dimentions, column names 
nrow(dataset)
ncol(dataset)
ncol(my_data)
str(my_data)
dim(my_data)
colnames(my_data)

# Data Wrangling and clean up
# This dataset set has lot of NAs
sum(is.na(my_data))

# Removing NAs 
na.omit(my_data)



# Checking out first 10 rows and summary
head(my_data, 10)
summary(my_data)

# Case Status Values, avoiding duplicated values
(unique(my_data$case_status))

# Select variables for visualization
visa <- dataset[c('case_status', 'case_no', 'class_of_admission','country_of_citizenship','employer_name','employer_city','job_info_work_state','foreign_worker_info_education','foreign_worker_info_major','pw_amount_9089','wage_offer_from_9089','pw_soc_title')]

# Rename variables inside (visa) to make it easier to understand
colnames(visa) <- c('Case_Status','Case_Number', 'Visa_Type','Country_of_Citizenship','Employer', 'Employer_City', 'Job_Location','Education_Level','Degree_Major','Prevailing_Wage','Wage_offer_from_9089','Prevailing_Occupation')

# Allow variables to be accessed by their names
attach(visa)

# structure of the new dataset
str(visa)

# Summary of the visa
summary(visa)

# case status
qplot(Case_Status,
      main = 'Case Status',
      xlab = 'Status',
      ylab = 'Number of Cases',
      ylim = c(0, 200000),
      fill = factor(Case_Status)) + # bar color by Case Status
  scale_fill_manual(values = c("Dark Green","Orange","Red","Black")) +
  labs(fill = NULL) # legend title

# Top 10 visa types

visa %>% 
  filter(!(Visa_Type == "")) %>%
  group_by(Visa_Type) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(Visa_Type = reorder(Visa_Type, count)) %>%
  top_n(20, count) %>%
  
  ggplot(aes(x = Visa_Type, y = count)) +
  geom_bar(stat = 'identity', fill = "#ED553B") +
  geom_text(aes(x = Visa_Type, y = 1, label = paste0("(",count,")", sep = "")),
            hjust = -0.1, vjust = 0.4, size = 4, fontface = 'bold') +
  labs(x = 'Type', y = 'Number of Applicants', title = 'Class of Admission / Visa') +
  scale_y_continuous(labels = comma) +
  coord_flip()

# Top 20 country of citizenship 

visa %>% 
  filter(!is.na(Country_of_Citizenship)) %>% 
  group_by(Country_of_Citizenship) %>%
  summarize(CountOfCountry = n()) %>%
  arrange(desc(CountOfCountry)) %>%
  mutate(Country_of_Citizenship = reorder(Country_of_Citizenship, CountOfCountry)) %>%
  head(20) %>%
  
  ggplot(aes(x = Country_of_Citizenship, y = CountOfCountry)) +
  geom_bar(stat='identity',color="white", fill = "#ED553B") +
  geom_text(aes(x = Country_of_Citizenship, y = 1, label = paste0 ("(",CountOfCountry,")",sep="")),
            hjust=0, vjust=.5, size = 4, color = 'black',
            fontface = 'bold') +
  labs(x = 'Country', y = 'Count Of Visa Applications', title = 'Country and Visas') +
  coord_flip() + 
  theme_bw()

visa %>% 
  filter(!is.na(Country_of_Citizenship)) %>% 
  filter(Case_Status == "Certified") %>%
  group_by(Country_of_Citizenship) %>%
  summarize(CountOfCountry = n()) %>%
  arrange(desc(CountOfCountry)) %>%
  mutate(Country_of_Citizenship = reorder(Country_of_Citizenship, CountOfCountry)) %>%
  head(20) %>%
  
  ggplot(aes(x = Country_of_Citizenship, y = CountOfCountry)) +
  geom_bar(stat ='identity', color="white", fill = "#ED8E3B") +
  geom_text(aes(x = Country_of_Citizenship, y = 1, label = paste0("(",CountOfCountry,")",sep="")),
            hjust=0, vjust=.5, size = 4, color = 'black',
            fontface = 'bold') +
  labs(x = 'Country', y = 'Count Of Visa Applications', title = 'Country and Visas') +
  coord_flip() + 
  theme_bw()


col40 <-
  visa %>% 
  group_by(Employer) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(Employer = reorder(Employer, count)) %>%
  top_n(20, count)

ggplot(col40, aes(x = Employer, y = count)) +
  geom_bar(stat = 'identity', fill = "#4FB99F") +
  geom_text(aes(x = Employer, y = 1, label = paste0("(",count,")", sep = "")),
            hjust = -0.1, vjust = 0.4, size = 4, fontface = 'bold') +
  labs(x = 'Employer', y = 'Number of Applicants', title = 'Employers with Visa Applications') +
  coord_flip()

visa$Job_Location=gsub("California", "CA", visa$Job_Location)
california <- c("CA", "California")

col50 <-
  visa %>% 
  group_by(Job_Location) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(Job_Location = reorder(Job_Location, count)) %>%
  top_n(10, count)



ggplot(col50, aes(x = Job_Location, y = count)) +
  geom_bar(stat = 'identity', fill = "#913D8C") +
  geom_text(aes(x = Job_Location, y = 1, label = paste0("(",count,")", sep = "")),
            hjust = -0.1, vjust = 0.4, size = 4, fontface = 'bold') +
  labs(x = 'State', y = 'Number of Applicants', title = 'Employer State') +
  coord_flip()


col60 <-
  visa %>% 
  filter(!(Education_Level == "")) %>%
  group_by(Education_Level) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(Education_Level = reorder(Education_Level, count)) %>%
  head(7)

ggplot(col60, aes(x = Education_Level, y = count)) +
  geom_bar(stat='identity', fill ="#6F5778") +
  geom_text(aes(x = Education_Level, y = 1, label = paste0("(",count,")", sep = "")),
            hjust = -0.1, vjust = 0.4, size = 4, fontface = 'bold') +
  labs(x = 'Level', y = 'Number of Applicants', title = 'Education Level') +
  coord_flip()


col70 <-
  visa %>% 
  filter(!(Degree_Major == "")) %>%
  group_by(Degree_Major) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(Degree_Major = reorder(Degree_Major, count)) %>%
  top_n(15, count)

ggplot(col70, aes(x = Degree_Major, y = count)) +
  geom_bar(stat = 'identity', fill = "#FF3E96") +
  geom_text(aes(x = Degree_Major, y = 1, label = paste0("(",count,")", sep = "")),
            hjust = -0.1, vjust = 0.4, size = 4, fontface = 'bold') +
  labs(x = 'Major', y = 'Number of Applicants', title = 'Degree Major') +
  coord_flip()


visa %>% 
  filter(!(Prevailing_Occupation == "")) %>%
  group_by(Prevailing_Occupation) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(Prevailing_Occupation = reorder(Prevailing_Occupation, count)) %>%
  top_n(15, count)




ggplot(col90, aes(x = Prevailing_Occupation, y = count)) +
  geom_bar(stat = 'identity', fill = "#58D68D") +
  geom_text(aes(x = Prevailing_Occupation, y = 1, label = paste0("(",count,")", sep = "")),
            hjust = -0.1, vjust = 0.4, size = 4, fontface = 'bold') +
  labs(x = 'Occupation', y = 'Number of Applicants', title = 'Prevailing Occupation') +
  coord_flip()



visa %>% 
  filter(!is.na(country_of_citizenship)) %>% 
  group_by(Country_of_Citizenship, Visa_Type) %>%
  summarize(CountOfCountry = n()) %>%
  arrange(desc(CountOfCountry)) %>%
  head(20) %>%
  
  ggplot(aes(x = Country_of_Citizenship,y = CountOfCountry, fill = Visa_Type)) +
  geom_bar(stat='identity',color="white") +
  labs(x = 'Country', y = 'Count Of Visa Applications', title = 'Country and Visas with Class of Admission Classification') +
  coord_flip() + 
  theme_bw()






visa %>% 
  filter(Case_Status == "Certified") %>%
  filter(!is.na(Country_of_Citizenship)) %>% 
  group_by(Country_of_Citizenship, Visa_Type) %>%
  summarize(CountOfCountry = n()) %>%
  arrange(desc(CountOfCountry)) %>%
  head(20) %>%
  
  ggplot(aes(x = Country_of_Citizenship, y = CountOfCountry, fill = Visa_Type)) +
  geom_bar(stat = 'identity', color = "white") +
  labs(x = 'Country', y = 'Count Of Visa Applications', title = 'Country and Visas with Class of Admission Classification') +
  coord_flip() + 
  theme_bw()



# Filtering in a similar fashion and then grouping the data by employer name
Company_listings <- visa%>%
  filter(Case_Status == "Certified" & Visa_Type == "H-1B")%>% 
  group_by(Employer)%>%
  summarize(count=length(unique(Case_Number)))

Company_listings<- Company_listings[order(Company_listings$count,decreasing = TRUE),]
Company_listings<- Company_listings[c(1:10),]
#suggesting appropriate column names
colnames(Company_listings)<- c("Names of major employers","Number of H-1B visas certified/accepted")


ggplot(Company_listings, aes(x = Employer, y = count)) +
  geom_col(fill = 'dark red') +
  coord_flip() +
  ggtitle('Major Employers vs H1B Visas')




visa %>% 
  filter(!is.na(Employer_City)) %>% 
  group_by(Employer_City) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) %>%
  mutate(Employer_City = reorder(Employer_City, Count)) %>%
  head(20) %>%
  
  ggplot(aes(x = Employer_City, y = Count)) +
  geom_bar(stat='identity',color="white", fill ="#EDCA3B") +
  geom_text(aes(x = Employer_City, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, color = 'black',
            fontface = 'bold') +
  labs(x = 'Employer City', y = 'Count Of Visa Applications', title = 'Employer City and Visas') +
  coord_flip() + 
  theme_bw()


visa %>% 
  filter(!is.na(Employer_City)) %>% 
  filter(Case_Status == "Certified") %>%
  group_by(Employer_City) %>%
  summarize(CountOfCity = n()) %>%
  arrange(desc(CountOfCity)) %>%
  mutate(Employer_City = reorder(Employer_City, CountOfCity)) %>%
  head(20) %>%
  
  ggplot(aes(x = Employer_City, y = CountOfCity)) +
  geom_bar(stat='identity',color="white", fill ="#EDCA3B") +
  geom_text(aes(x = Employer_City, y = 1, label = paste0("(",CountOfCity,")",sep="")),
            hjust=0, vjust=.5, size = 4, color = 'black',
            fontface = 'bold') +
  labs(x = 'Employer City', y = 'Count Of Visa Applications', title = 'Employer City and Visas') +
  coord_flip() + 
  theme_bw()



# Top 20 Employers in City of New York
visa %>% 
  filter(Employer_City == "New York") %>% 
  group_by(Employer) %>%
  summarize(CountOfEmployerName = n()) %>%
  arrange(desc(CountOfEmployerName)) %>%
  mutate(Employer = reorder(Employer, CountOfEmployerName)) %>%
  head(20) %>%
  
  ggplot(aes(x = Employer, y = CountOfEmployerName)) +
  geom_bar(stat='identity', color="white", fill = "#F25234") +
  geom_text(aes(x = Employer, y = 1, label = paste0("(",CountOfEmployerName,")",sep = "")),
            hjust=0, vjust=.5, size = 4, color = 'black',
            fontface = 'bold') +
  labs(x = 'Employer Name', y = 'Count Of Visa Applications in New York', title = 'Employers in New York and Visas') +
  coord_flip() + 
  theme_bw()



#Top 20 Median for the salaries by employers for Certified Visa Applications
#I was expecting to see exciting values for this one but I'm having problems rendering this code because Prevailing_Wage(aka pw_amount_9089 ) variable has no value in it.  The one that has all the numerical values is in the .csv1 file.  I don't know how can I load that file or better to say I don't know why I have two versions of the same file.   It doesn't show up in my computer. I can only see that file in Tableau.


wage <- suppressWarnings(as.numeric(visa$Prevailing_wage))
visa %>% 
  filter(!is.na(wage)) %>%
  filter(Case_Status == "Certified" ) %>%
  group_by(Employer) %>%
  summarize(MedianEmployeeSalary = median(wage)) %>%
  arrange(desc(MedianEmployeeSalary)) %>%
  mutate(Employer = reorder(Employer, MedianEmployeeSalary)) %>%
  mutate(MedianEmployeeSalary2 = MedianEmployeeSalary)  %>%
  mutate(MedianEmployeeSalary = scales::dollar(MedianEmployeeSalary)) %>%
  head(20) %>%
  
  ggplot(aes(x = Employer, y = MedianEmployeeSalary2)) +
  geom_bar(stat='identity',color="white", fill = "red") +
  geom_text(aes(x = Employer, y = 1, label = paste0("(", MedianEmployeeSalary,")",sep="")),
            hjust=0, vjust=.5, size = 4, color = 'black',
            fontface = 'bold') +
  labs(x = 'Employer Name', y = 'Median Salary', title = 'Employee Salary in Visas') +
  coord_flip() + 
  theme_bw()


#The mean for the Previaling Wage 
wage <- suppressWarnings(as.numeric(dataset$pw_amount_9089))
mean(wage, na.rm=TRUE)
median(wage, na.rm=TRUE) 


Detecting Outliers for Wage Amount and Fivenum Summary

```{r}
boxplot(wage)

# Print outliers
boxplot.stats(wage)$out

#fivenume summary
summary(wage)
fivenum(wage)



Filetring based on certified H1B visa

```{r}
# Filtering aata based on status being "Certified" and calss of admission being "H1B"
na.omit(dataset)# omitting nas
h1b <- dataset %>% filter(case_status == "Certified" & 
                            class_of_admission == "H-1B")%>%
  group_by(country_of_citizenship)%>%
  summarize(count=length(unique(case_no)))

h1b


#Creating dataset for coorelation matrix and subsequent modelling for the data preparation, the attirbutes such as Caseno, Case status, Wage, Wage unit, Economic sector, Application type and class of admission have been considered. The employer name had too many levels,therefore, could not be considered. Out of 154 attributes, these were shorlisted based on the type and class of the attribute. Rest were not considered since they had too many levels, or were repetitions of same attributes or were simply empty list.

```{r}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
model<- data.frame(
  'Case_no'= trim(dataset$case_no),
  'Case_Status'= trim(dataset$case_status),
  'Wage'= trim(dataset$wage_offer_from_9089),
  'Wage type'= trim(dataset$wage_offer_unit_of_pay_9089),
  'Class'= trim(dataset$class_of_admission),
  'Economic Sector'= trim(dataset$us_economic_sector),
  'Application type'= trim(dataset$application_type))

head(model,100)



#Modeling some of the attributes to see the leves in each category

```{r}
unique(model$Application.type)
unique(model$Wage.type)
unique(model$Economic.Sector)

#To create the correlation matrix I convert the categorical variables to flags using for loops.
status<- model2$Case_Status
Applicationtype <- model2$Application.type
wagetype <- model2$Wage.type
economicsector <- model2$Economic.Sector
newcol1<-c()
newcol2<-c()
newcol3<-c()
newcol4<-c()
newcol5<-c()
newcol6<-c()
newcol7<-c()
newcol8<-c()
newcol9<-c()
newcol10<-c()
newcol11<-c()
newcol13<-c()
newcol14<-c()
newcol15<-c()
newcol16<-c()
newcol17<-c()
newcol18<-c()
newcol19<-c()
newcol20<-c()
newcol21<-c()
newcol22<-c()
newcol23<-c()
newcol24<-c()
newcol25<-c()
newcol26<-c()
newcol27<-c()
newcol28<-c()
newcol29<-c()
newcol30<-c()
newcol31<-c()
newcol32<-c()
newcol33<-c()
newcol34<-c()
for (i in (1:length(status)))
{
  if (status[i]=="Certified")
    newcol1<-c(newcol1,1)
  else
    newcol1<-c(newcol1,0)
}
for (i in (1:length(status)))
{
  if (status[i]=="Certified-Expired")
    newcol2<-c(newcol2,1)
  else
    newcol2<-c(newcol2,0)
}
for (i in (1:length(status)))
{
  if (status[i]=="Denied")
    newcol3<-c(newcol3,1)
  else
    newcol3<-c(newcol3,0)
}
for (i in (1:length(status)))
{
  if (status[i]=="Withdrawn")
    newcol4<-c(newcol4,1)
  else
    newcol4<-c(newcol4,0)
}
for (i in (1:length(Applicationtype)))
{
  if (status[i]=="PERM")
    newcol5<-c(newcol5,1)
  else
    newcol5<-c(newcol5,0)
}
for (i in (1:length(Applicationtype)))
{
  if (status[i]=="ONLINE")
    newcol6<-c(newcol6,1)
  else
    newcol6<-c(newcol6,0)
}
for (i in (1:length(Applicationtype)))
{
  if (status[i]=="MAILEDIN")
    newcol7<-c(newcol7,1)
  else
    newcol7<-c(newcol7,0)
}
for (i in (1:length(wagetype)))
{
  if (status[i]=="Year")
    newcol8<-c(newcol8,1)
  else
    newcol8<-c(newcol8,0)
}
for (i in (1:length(wagetype)))
{
  if (status[i]=="Week")
    newcol9<-c(newcol9,1)
  else
    newcol9<-c(newcol9,0)
}
for (i in (1:length(wagetype)))
{
  if (status[i]=="Month")
    newcol10<-c(newcol10,1)
  else
    newcol10<-c(newcol10,0)
}
for (i in (1:length(wagetype)))
{
  if (status[i]=="Hour")
    newcol11<-c(newcol11,1)
  else
    newcol11<-c(newcol11,0)
}
#for (i in (1:length(wagetype)))
# {
# if (status[i]=="Bi-Weekly")
#newcol12<-c(newcol12,1)
#else
#newcol12<-c(newcol12,0)
#}
model2$Certified <-newcol1
model2$Certified_Expired <-newcol2
model2$Denied <-newcol3
model2$Withdrawn <-newcol4
model2$PERM <-newcol5
model2$ONLINE <-newcol6
model2$MAILEDIN <-newcol7
model2$Yearly <-newcol8
model2$weekly <-newcol9
model2$monthly <-newcol10
model2$hourly <-newcol11
#model2$biweekly<-newcol12



head(model2)




