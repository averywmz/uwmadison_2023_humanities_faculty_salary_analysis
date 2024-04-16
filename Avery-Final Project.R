# LIS 572 Final Project
# How Much does a Public University Humanities Faculty Earn Today?
# Avery Clare

# This project originally aimed at analyzing University of Washington salary data (source: https://fiscal.wa.gov/Staffing/Salaries) but the source dataset does not include departmental information
# So I have to switch to another university with a salary dataset having the department data included
# This is why we end up looking at UW-Madison salary data
# Source: https://github.com/wuu-madison/salary_data/raw/main/Updated%202023-10%20All%20Faculty%20and%20Staff%20Title%20and%20Salary%20Information.xlsx

install.packages("pillar")
install.packages ("dplyr")
install.packages("tidyverse")
library("dplyr")
install.packages("stringr")
library(stringr)

# Import uw-madison salary data
uwm_salary <- read.csv("C:/Users/stlp/Desktop/uwmadison_2023_salary_data.csv", stringsAsFactors = FALSE)

# Change income format from currency to numeric because slice_max seems neglecting amount over $99,999 in currency format
uwm_num_salary<-uwm_salary %>% 
  mutate(adj_salary = str_replace(Annual_FTE_Adjusted_Salary, "\\D", ""))

uwm_num_salary$num_salary <- 
  as.numeric(gsub(",","", uwm_num_salary$adj_salary))

# Check employee category
distinct(uwm_salary, Employee_Category)
distinct(uwm_salary, Title)

# Filter for faculty(FA) salary data
uwm_fa_salary<-uwm_num_salary %>% 
  filter(str_detect(Employee_Category, "FA"))

# Filter for academic staff(AS) salary data
uwm_as_salary<-uwm_num_salary %>% 
  filter(str_detect(Employee_Category, "AS"))


# Faculty amount: 2279
# Academic staff amount: 13055
# Faculty&academic staff amount: 15334
nrow(uwm_as_salary)
nrow(uwm_fa_salary)
nroe(uwm_ac_salary)

# Which department has the highest average income for faculty
dep_avg_fa_salary <- uwm_fa_salary %>% 
  group_by(Department) %>% 
  summarize(Salary = median(num_salary))

# Calculate the average faculty salary of UW-Madison
# $141,195.5
uwm_avg_fa_salary <- dep_avg_fa_salary %>% summarize(median(Salary))

# Which professor from which department earns the most
dep_max_fa_salary <- uwm_fa_salary %>% 
  group_by(Department) %>% 
  summarize(Salary = max(num_salary))

# check how many departments are there in UW-Madison
# department amount: 124
nrow(dep_avg_salary)


# Filter for the Humanities Departments
# UW-Madison Arts and Humanities Departments and Programs
# https://ls.wisc.edu/areas-of-study/arts-humanities

#African Cultural Studies, Department of
#Art History, Department of
#Asian Languages & Cultures, Department of
#Classical and Ancient Near Eastern Studies, Department of
#Creative Writing Program
#English, Department of
#English as a Second Language, Program in
#French and Italian, Department of
#Gender and Women's Studies, Department of
#German, Nordic, and Slavic+, Department of
#History, Department of
#Jewish Studies, Mosse/Weinstein Center for,
#Mead Witter School of Music
#Philosophy, Department of
#Spanish and Portuguese, Department of
#Honors Program
#Integrated Liberal Studies Program (ILS)
#Interdisciplinary Theatre Studies Program (ITS)
#Language Sciences Program
#Medieval Studies Program
#Religious Studies Program
#Second Language Acquisition, Doctoral Program in

# Humanities Faculty top salary
hum_max_salary <- dep_max_fa_salary %>% 
  filter(str_detect(Department, "African Cultural Studies|Anthropology|Art|Art History|Asian|Classic|English|French|Gender|German|History|Liberal|Jewish|Music|Philosophy|Religious|Spanish|Theatre"))

# Check how many humanities departments are there in UW-Madison
# Humanities amount: 20
nrow(hum_max_salary)

# Visualize humanities salary data to a bar chart
install.packages("ggplot2")
install.packages("colorspace")
install.packages("labeling")
library(ggplot2)

install.packages("RColorBrewer")
install.packages("ggthemes")
library(ggthemes)
install.packages("cartography")
install.packages("paletteer")
library(paletteer)

options(scipen =999)
ggplot_hum_max_salary<-
  ggplot(data = hum_max_salary)+
  geom_col(
    mapping = aes(
      x = Salary,
      y = reorder(Department, +Salary),
      text = paste("Salary:", paste0("$", formatC(Salary, format="f", digits=2, big.mark=","))),
      fill = Salary
      )
  )+
  labs(
    title = "Which Humanities Department Earns the Most?",
    subtitle = "2023 UW-Madison Humanities Department Faculty Salary",
    x = "Salary",
    y = "Department"
  )+
  scale_fill_distiller(palette = "Blues", direction = +1)+
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "black"),
    axis.title = element_text(face = "bold", color="black")
  )+
  scale_x_continuous(labels = scales::dollar_format())

install.packages("plotly")
library(plotly)

ggplotly(ggplot_hum_max_salary, tooltip = c("text"))


# Which humanities department has the highest average income level for faculty members?

# Humanities Faculty average salary level
hum_avg_salary <- dep_avg_fa_salary %>% 
  filter(str_detect(Department, "African Cultural Studies|Anthropology|Art|Art History|Asian|Classic|English|French|Gender|German|History|Liberal|Jewish|Music|Philosophy|Religious|Spanish|Theatre"))

ggplot_hum_avg_salary<-
  ggplot(data = hum_avg_salary)+
  geom_col(
    mapping = aes(
      x = Salary,
      y = reorder(Department, +Salary),
      text = paste("Salary:", paste0("$", formatC(Salary, format="f", digits=2, big.mark=","))),
      fill = Salary
    )
  )+
  labs(
    title = "Which Humanities Department Has the Highest Average Income for Faculty? ",
    subtitle = "2023 UW-Madison Humanities Department Faculty Salary",
    x = "Salary",
    y = "Department"
  )+
  scale_fill_distiller(palette = "Blues", direction = +1)+
  theme(panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 14, face = "bold", color = "black"),
        axis.title = element_text(face = "bold", color="black")
        )+
  scale_x_continuous(labels = scales::dollar_format())

install.packages("plotly")
library(plotly)

ggplotly(ggplot_hum_avg_salary, tooltip = c("text"))


# Use Datawrapper to visualize the income level of the Humanities department among all the departments of UW-Madison
# export department average faculty salary data to a csv file
write.csv(dep_avg_fa_salary, "dep_avg_fa_salary.csv")


#----------------------------------------
# How much does a begining-stage Humanities faculty earn?
# This is usually the case for assistant professors, who just graduated and got the job after fierce competition on the job market.

uwm_asst_prof_salary <- uwm_fa_salary %>% 
  filter(str_detect(Title, "Assistant Professor")) 

uwm_astprof_avg_salary<-uwm_asst_prof_salary%>% 
  group_by(Department) %>% 
  summarize(Salary = median(num_salary))

# Export as csv file, and upload to DataWrapper for visualization
write.csv(uwm_astprof_avg_salary,"uwm_astprof_avg_salary.csv")


#----------------------------------------
# How much does an advanced Humanities faculty earn?
# Looking at Professors
uwm_prof_salary <- uwm_fa_salary %>% 
  filter(Title == "Professor") 

uwm_prof_avg_salary<-uwm_prof_salary%>% 
  group_by(Department) %>% 
  summarize(Salary = median(num_salary))

# Export as csv file, and upload to DataWrapper for visualization
write.csv(uwm_prof_avg_salary,"uwm_prof_avg_salary.csv")


# Summary


#----------------------------------
# How many people are working in the Jewish Studies Center?
# Amount: 5
nrow(jew_salary)
# How many types of job are there in the Jewish Studies Center?
# four types: AS, CP, FA
distinct(jew_salary, Employee_Category)
# How many people for each type
employee_jew <- jew_salary %>% 
  count(Employee_Category)

# Zoom in to the History Department
hist_salary <- uwm_num_salary %>% 
  filter(Department == "History")

# How many people working in the History Department?
# Amount: 82
nrow(hist_salary)
# How many types of job are there in the History Department?
# four types: AS, CP, FA, ET1
distinct(hist_salary, Employee_Category)
# How many people for each type
employee_hist <- hist_salary %>% 
  count(Employee_Category)
#   Employee_Category  n
# 1                AS 20
# 2                CP  7
# 3               ET1  2
# 4                FA 53

hist_avg_salary<-hist_salary %>% 
  group_by(Employee_Category) %>% 
  summarize(avg_salary = median(num_salary))

# Zoom in to Jewish Studies Center
# 5 employees, among them 1 faculty member
jew_salary <- uwm_num_salary %>% 
  filter(str_detect(Department, "Jewish"))

# How many people are working in Anthropology?
# 24 people, among them 18 are faculty members
anth_salary <- uwm_num_salary %>% 
  filter(Department == "Anthropology")
nrow(anth_salary)
employee_anth <- anth_salary %>% 
  count(Employee_Category)
