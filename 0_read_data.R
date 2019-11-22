# 0_read_data.R
# get survey data
# Version using CSV data from Survey Monkey
# November 2019
library(dplyr)
library(stringr)
library(readxl)

## get data
# from Anna:
# Versions A, B, C, D were sent out to researchers.
# Versions H1, H2, H3, H4 were sent out to HRECs. 
# A is identical to H1, B is identical to H2, etc.

## get data, had to save in Excel as tab-delimited because of commas in text
# each file has its own pattern

# a) H4
survey = 'H4'
this.file = paste('Waste in Research Ethics Scenarios Survey version ', survey, '.txt', sep='')
h4 = read.table(paste('data/', this.file, sep=''), fill=TRUE, skip=2, header=FALSE, stringsAsFactors = FALSE, sep='\t')
names(h4) = c('id','collector_id','start_date','end_date','ip_address',
              'email','first_name','last_name','rubbish','respondent','changed','changed_text',
              'abandon','abandon_text',
              'scenario_notrt','scenario_notrt_reasons',
              'scenario_n1','scenario_n1_reasons',
              'scenario_interview_r1','scenario_interview_r2','scenario_interview_reasons',
              'scenario_surplus','scenario_surplus_reasons',
              'open_ended') 
h4 = select(h4, -collector_id, -ip_address, -email, -first_name, -last_name, -rubbish) %>%
  mutate(
    survey = survey,  
    # make start times
    start_date_time = 
      ifelse(nchar(start_date) > 20,
             as.POSIXct(strptime(start_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
             as.POSIXct(strptime(start_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
    end_date_time = 
      ifelse(nchar(end_date) > 20,
             as.POSIXct(strptime(end_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
             as.POSIXct(strptime(end_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
    start_date = as.Date(stringr::str_sub(start_date, 1, 10), format='%m/%d/%Y'), # calculate date
    time_taken = as.numeric(end_date_time-start_date_time)/60 ) %>% # calculate time taken (minutes)
  select(-end_date, -end_date_time, -start_date_time) 

# b) H3
survey = 'H3'
this.file = paste('Waste in Research Ethics Scenarios Survey version ', survey, '.txt', sep='')
h3 = read.table(paste('data/', this.file, sep=''), fill=TRUE, skip=2, header=FALSE, stringsAsFactors = FALSE, sep='\t')
names(h3) = c('id','collector_id','start_date','end_date','ip_address',
              'email','first_name','last_name','rubbish','respondent','changed','changed_text',
              'abandon','abandon_text',
              'scenario_n1','scenario_n1_reasons',
              'scenario_qa1','scenario_qa2','scenario_qa3','scenario_qa_reasons',
              'scenario_survey1','scenario_survey2','scenario_survey_reasons',
              'scenario_linked','scenario_linked_reasons',
              'open_ended') 
h3 = select(h3, -collector_id, -ip_address, -email, -first_name, -last_name, -rubbish) %>%
  mutate(
    survey = survey,  
    # make start times
    start_date_time = 
      ifelse(nchar(start_date) > 20,
             as.POSIXct(strptime(start_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
             as.POSIXct(strptime(start_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
    end_date_time = 
      ifelse(nchar(end_date) > 20,
             as.POSIXct(strptime(end_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
             as.POSIXct(strptime(end_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
    start_date = as.Date(stringr::str_sub(start_date, 1, 10), format='%m/%d/%Y'), # calculate date
    time_taken = as.numeric(end_date_time-start_date_time)/60 ) %>% # calculate time taken (minutes)
  select(-end_date, -end_date_time, -start_date_time) 

# c) H2
survey = 'H2'
this.file = paste('Waste in Research Ethics Scenarios Survey version ', survey, '.txt', sep='')
h2 = read.table(paste('data/', this.file, sep=''), fill=TRUE, skip=2, header=FALSE, stringsAsFactors = FALSE, sep='\t')
names(h2) = c('id','collector_id','start_date','end_date','ip_address',
              'email','first_name','last_name','rubbish','respondent','changed','changed_text',
              'abandon','abandon_text',
              'scenario_staff1','scenario_staff2','scenario_staff3','scenario_staff_reasons',
              'scenario_notrt','scenario_notrt_reasons',
              'scenario_survey1','scenario_survey2','scenario_survey_reasons',
              'scenario_surplus','scenario_surplus_reasons',
              'open_ended') 
h2 = select(h2, -collector_id, -ip_address, -email, -first_name, -last_name, -rubbish) %>%
    mutate(survey = survey,  
    # make start times
    start_date_time = 
      ifelse(nchar(start_date) > 20,
             as.POSIXct(strptime(start_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
             as.POSIXct(strptime(start_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
    end_date_time = 
      ifelse(nchar(end_date) > 20,
             as.POSIXct(strptime(end_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
             as.POSIXct(strptime(end_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
    start_date = as.Date(stringr::str_sub(start_date, 1, 10), format='%m/%d/%Y'), # calculate date
    time_taken = as.numeric(end_date_time-start_date_time)/60 ) %>% # calculate time taken (minutes)
  select(-end_date, -end_date_time, -start_date_time) 

# d) H1
survey = 'H1'
this.file = paste('Waste in Research Ethics Scenarios Survey version ', survey, '.txt', sep='')
h1 = read.table(paste('data/', this.file, sep=''), fill=TRUE, skip=2, header=FALSE, stringsAsFactors = FALSE, sep='\t')
names(h1) = c('id','collector_id','start_date','end_date','ip_address',
              'email','first_name','last_name','rubbish','respondent','changed','changed_text',
              'abandon','abandon_text',
              'scenario_linked','scenario_linked_reasons',
              'scenario_interview_r1','scenario_interview_r2','scenario_interview_reasons',
              'scenario_qa1','scenario_qa2','scenario_qa3','scenario_qa_reasons',
              'scenario_staff1','scenario_staff2','scenario_staff3','scenario_staff_reasons',
              'open_ended') 
h1 = select(h1, -collector_id, -ip_address, -email, -first_name, -last_name, -rubbish) %>%
  mutate(survey = survey,  
         # make start times
         start_date_time = 
           ifelse(nchar(start_date) > 20,
                  as.POSIXct(strptime(start_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
                  as.POSIXct(strptime(start_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
         end_date_time = 
           ifelse(nchar(end_date) > 20,
                  as.POSIXct(strptime(end_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
                  as.POSIXct(strptime(end_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
         start_date = as.Date(stringr::str_sub(start_date, 1, 10), format='%m/%d/%Y'), # calculate date
         time_taken = as.numeric(end_date_time-start_date_time)/60 ) %>% # calculate time taken (minutes)
  select(-end_date, -end_date_time, -start_date_time) 

# e) D
survey = 'D'
this.file = paste('Waste in Research Ethics Scenarios Survey version ', survey, '.txt', sep='')
D = read.table(paste('data/', this.file, sep=''), fill=TRUE, skip=2, header=FALSE, stringsAsFactors = FALSE, sep='\t')
names(D) = c('id','collector_id','start_date','end_date','ip_address',
              'email','first_name','last_name','rubbish','respondent','changed','changed_text',
              'abandon','abandon_text',
              'scenario_notrt','scenario_notrt_reasons',
              'scenario_n1','scenario_n1_reasons',
              'scenario_interview_r1','scenario_interview_r2','scenario_interview_reasons',
              'scenario_surplus','scenario_surplus_reasons',
              'open_ended') 
D = select(D, -collector_id, -ip_address, -email, -first_name, -last_name, -rubbish) %>%
  mutate(survey = survey,  
         # make start times
         start_date_time = 
           ifelse(nchar(start_date) > 20,
                  as.POSIXct(strptime(start_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
                  as.POSIXct(strptime(start_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
         end_date_time = 
           ifelse(nchar(end_date) > 20,
                  as.POSIXct(strptime(end_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
                  as.POSIXct(strptime(end_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
         start_date = as.Date(stringr::str_sub(start_date, 1, 10), format='%m/%d/%Y'), # calculate date
         time_taken = as.numeric(end_date_time-start_date_time)/60 ) %>% # calculate time taken (minutes)
  select(-end_date, -end_date_time, -start_date_time) 

# f) C
survey = 'C'
this.file = paste('Waste in Research Ethics Scenarios Survey version ', survey, '.txt', sep='')
C = read.table(paste('data/', this.file, sep=''), fill=TRUE, skip=2, header=FALSE, stringsAsFactors = FALSE, sep='\t')
names(C) = c('id','collector_id','start_date','end_date','ip_address',
             'email','first_name','last_name','rubbish','respondent','changed','changed_text',
             'abandon','abandon_text',
             'scenario_n1','scenario_n1_reasons',
             'scenario_qa1','scenario_qa2','scenario_qa3','scenario_qa_reasons',
             'scenario_survey1','scenario_survey2','scenario_survey_reasons',
             'scenario_linked','scenario_linked_reasons',
             'open_ended') 
C = select(C, -collector_id, -ip_address, -email, -first_name, -last_name, -rubbish) %>%
  mutate(survey = survey,  
         # make start times
         start_date_time = 
           ifelse(nchar(start_date) > 20,
                  as.POSIXct(strptime(start_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
                  as.POSIXct(strptime(start_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
         end_date_time = 
           ifelse(nchar(end_date) > 20,
                  as.POSIXct(strptime(end_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
                  as.POSIXct(strptime(end_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
         start_date = as.Date(stringr::str_sub(start_date, 1, 10), format='%m/%d/%Y'), # calculate date
         time_taken = as.numeric(end_date_time-start_date_time)/60 ) %>% # calculate time taken (minutes)
  select(-end_date, -end_date_time, -start_date_time) 

# g) B
survey = 'B'
this.file = paste('Waste in Research Ethics Scenarios Survey version ', survey, '.txt', sep='')
B = read.table(paste('data/', this.file, sep=''), fill=TRUE, skip=2, header=FALSE, stringsAsFactors = FALSE, sep='\t')
names(B) = c('id','collector_id','start_date','end_date','ip_address',
             'email','first_name','last_name','rubbish','respondent','changed','changed_text',
             'abandon','abandon_text',
             'scenario_staff1','scenario_staff2','scenario_staff3','scenario_staff_reasons',
             'scenario_notrt','scenario_notrt_reasons',
             'scenario_survey1','scenario_survey2','scenario_survey_reasons',
             'scenario_surplus','scenario_surplus_reasons',
             'open_ended') 
B = select(B, -collector_id, -ip_address, -email, -first_name, -last_name, -rubbish) %>%
  mutate(survey = survey,  
         # make start times
         start_date_time = 
           ifelse(nchar(start_date) > 20,
                  as.POSIXct(strptime(start_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
                  as.POSIXct(strptime(start_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
         end_date_time = 
           ifelse(nchar(end_date) > 20,
                  as.POSIXct(strptime(end_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
                  as.POSIXct(strptime(end_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
         start_date = as.Date(stringr::str_sub(start_date, 1, 10), format='%m/%d/%Y'), # calculate date
         time_taken = as.numeric(end_date_time-start_date_time)/60 ) %>% # calculate time taken (minutes)
  select(-end_date, -end_date_time, -start_date_time) 

# h) A
survey = 'A'
this.file = paste('Waste in Research Ethics Scenarios Survey version ', survey, '.txt', sep='')
A = read.table(paste('data/', this.file, sep=''), fill=TRUE, skip=2, header=FALSE, stringsAsFactors = FALSE, sep='\t')
names(A) = c('id','collector_id','start_date','end_date','ip_address',
             'email','first_name','last_name','rubbish','respondent','changed','changed_text',
             'abandon','abandon_text',
             'scenario_linked','scenario_linked_reasons',
             'scenario_interview_r1','scenario_interview_r2','scenario_interview_reasons',
             'scenario_qa1','scenario_qa2','scenario_qa3','scenario_qa_reasons',
             'scenario_staff1','scenario_staff2','scenario_staff3','scenario_staff_reasons',
             'open_ended') 
A = select(A, -collector_id, -ip_address, -email, -first_name, -last_name, -rubbish) %>%
  mutate(survey = survey,  
         # make start times
         start_date_time = 
           ifelse(nchar(start_date) > 20,
                  as.POSIXct(strptime(start_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
                  as.POSIXct(strptime(start_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
         end_date_time = 
           ifelse(nchar(end_date) > 20,
                  as.POSIXct(strptime(end_date, format='%m/%d/%Y %I:%M:%S %p'), tz="UTC"),
                  as.POSIXct(strptime(end_date, format='%m/%d/%Y %H:%M'), tz="UTC")),
         start_date = as.Date(stringr::str_sub(start_date, 1, 10), format='%m/%d/%Y'), # calculate date
         time_taken = as.numeric(end_date_time-start_date_time)/60 ) %>% # calculate time taken (minutes)
  select(-end_date, -end_date_time, -start_date_time) 

## concatenate and format ##
data = bind_rows(h3, h4, h2, h1, D, C, B, A) %>%
  # reformat so levels are in right order
  mutate(
    respondent = case_when( 
      str_detect(respondent, 'Ethics') ~ 1,
      str_detect(respondent, 'researcher') ~ 2,
      str_detect(respondent, 'Both') ~ 3
    ),
    respondent = factor(respondent, levels=1:3, labels=c('HREC','Researcher','Both')),
    changed = case_when( 
      changed=='Yes' ~ 1,
      changed=='No' ~ 2,
      changed=='This question does not apply to me' ~ 3
    ),
    changed = factor(changed, levels=1:3, labels=c('Yes','No','This question does not apply to me')),
    abandon = case_when( 
      abandon=='Yes' ~ 1,
      abandon=='No' ~ 2,
      abandon=='This question does not apply to me' ~ 3
    ),
    abandon = factor(abandon, levels=1:3, labels=c('Yes','No','This question does not apply to me'))
  )

# save
save(data, file='data/AnalysisReady.RData')
  