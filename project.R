library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(ggridges)
library(tidyr)
library(tidyverse)
library(DT)
library(plotrix)

# Importing Data Set ----------------------------------------------------
dataset <- read.csv("E:/R Programming-Resources/R Programming_codes/Codes From Trainer/Comcast Telecom Complaints data.csv")
View(dataset)


# Formatting Date ---------------------------------------------------------
as.character(dataset$Date) # converting date column to character type
# converting again to proper date format
dataset$proper_date <- dmy(dataset$Date)
View(dataset)
# extracting month from date column
dataset$month <- month(dataset$proper_date, label = TRUE, abbr = TRUE)
# extracting day from date column
dataset$day <- day(dataset$proper_date)


# CHART-1 (No.of. complaints at monthly and daily granularity level --------


b = ggplot(dataset, aes(x=dataset$month)) + 
  xlab("Months") + 
  ylab("Complaints")
b + geom_bar(fill="#48C9B0") + 
  geom_text(aes(label= ..count..), stat = "count", vjust=1.5, colour = "Black")



# trend chart for the number of complaints at monthly and daily granularity levels.

# Month wise Complaints
complaints <- table(dataset$month)
complaints
df_comp <- as.data.frame(complaints)
colnames(df_comp) <- c("Month","Count_complaints")
View(df_comp)

# Month ~ Day Complaints

bar <- ggplot(dataset, aes(x= day)) + 
  ylab("No.of.complaints") + facet_wrap(~month, ncol=4)
bar + geom_bar(color="black", fill="#48C9B0") +labs(title = "Month ~ day Complaints") 


# Frequency of complaint types  -------------------------------------------

freq_complaints <- table(dataset$Customer.Complaint)
freq_complaints
fix(freq_complaints)

freq_complaints_cc <- transform(freq_complaints, complaint.type = toupper(Var1)) %>%
  rename (Customer.Complaint = Var1) %>% select (complaint.type)
View(freq_complaints_cc)

Group <- ifelse(grepl("INTERNET", freq_complaints_cc$complaint.type),"INTERNET", 
ifelse(grepl("NETWORK", freq_complaints_cc$complaint.type),"NETWORK","OTHERS"))

Group

freq_complaints_cc$Complaint_group <- Group
                      
View(freq_complaints_cc)

table(freq_complaints_cc$Complaint_group)

# Frequency Table 

df <- as.data.frame(table(freq_complaints_cc$Complaint_group))
colnames(df) <- c("Complaint_Type","Frequency")
datatable(df)

# Frequency Bar Chart

b = ggplot(freq_complaints_cc, aes(x=Complaint_group)) + 
  xlab("Complaint Type") + 
  ylab("Counts")
b + geom_bar(fill="#48C9B0") + 
  geom_text(aes(label= ..count..), stat = "count", vjust=-1.2, colour = "Black")


# segregate values into Open & Closed -------------------------------------

dataset$Complaint_status <- ifelse((dataset$Status=="Open") |(dataset$Status=="Pending"), "OPEN","CLOSED")
dataset$Complaint_status

# Creating Stacked bar plot state wise using new categorical variable


status <- ggplot(dataset, aes(x=State, fill=Complaint_status)) + 
  geom_bar(position = position_stack()) + ylab("Count of Complaints") + 
  labs(title="Statewise Complaints and its Status") +
  theme(axis.text.x=element_text(angle=90, hjust=0.9))
status

## Which state has the maximum complaints 
df_total <- dataset %>% group_by(State) %>% tally()
df_total <- arrange(df_total,desc(State))
df_total
Max_complaints <- df_total %>% slice(which.max(n))
Max_complaints  ## Display  state with max complaints

##plot in bar chart

px <- ggplot(df_total,aes(x=reorder(State, -n),y = n, )) + 
  geom_bar(stat = "identity",fill="#48C9B0") + xlab("States") + 
  ylab("Total Complaints") +
  labs(title = "States ~ Maximum Complaints") + 
  geom_text(aes(label= n), vjust= 1.5,  ) +
  theme(axis.text.x=element_text(angle=90, hjust=0.9))
px

## Which state has the highest percentage of unresolved complaints 

df <- dataset %>% group_by(State, Complaint_status) %>% 
  filter(Complaint_status=="CLOSED") %>% 
  tally()
df <- arrange(df,desc(State))
df
df["unresolved"] <- round(((df_total$n-df$n)/df_total$n*100),2)
df
Max_unresolved <- which.max(df$unresolved)


piedata <- c((df_total$n[Max_unresolved]-df$n[Max_unresolved])/df_total$n[Max_unresolved]*100,
             df$n[Max_unresolved]/df_total$n[Max_unresolved]*100)
pielabel <- c("Unresolved","Solved")
class(piedata)
piedata
# 3d pie chart Max.unresolved Complaints

pie3D(piedata, mar = rep(1.75, 4), main=paste("Highest % of Unresolved Complaints--> (State - ",df$State[Max_unresolved],")" ),
      col = hcl.colors(length(piedata), "Spectral"),
      labels = paste(pielabel," ",piedata,"%"), 
      explode = 0.2)


# Provide the percentage of complaints resolved till date, 
# which were received through the Internet and customer care calls.

solved_complaints <- dataset %>% group_by(Received.Via) %>% filter(Complaint_status=="CLOSED") %>% tally()
solved_complaints
total_complaints <- nrow(dataset)
total_complaints
solved_percentage <- round(sum(solved_complaints$n)/total_complaints*100,2)
solved_percentage
piesolved <- c(100-solved_percentage, solved_percentage)
piesolved
pielabel <- c("Unresolved","Solved")
# pie Chart
pie3D(piesolved, mar = rep(1.75, 4), 
      main=paste(" % of Complaints Resolved Received via Internet & Customer care " ),
      col = c("#ff0000","#00ffff"),
      labels = paste(pielabel," ",piesolved,"%"), 
      explode = 0.2)

