library(tidyverse)
library(httr)
library(jsonlite)

getReport <- function(x, values = "label", head="label"){
  df <- POST(url = 'https://redcap.ghid-kccr.org/api/',
             body = list(
               content='report',
               format='json',
               report_id= x,
               csvDelimiter='',
               rawOrLabel= values,
               token = "9C435B4AC255981BEFD49E32EB5AD2C2",
               rawOrLabelHeaders= head,
               exportCheckboxLabel='false',
               returnFormat='json'),
             encode = "form")
  content(df, as = "text") %>%
    fromJSON() %>%
    return()
}

pings <- getReport(167) %>% 
  mutate(
    across(approved:easy, ~fct_relevel(.x, "Completely disagree", "Disagree", "Neither agree nor disagree", "Agree",  "Completely agree")),
    across(approved:easy, as.numeric),
    occupation = fct_lump_prop(occupation, prop = 0.06)) %>% 
  rowwise() %>% 
  mutate(
    score = sum(across(approved:easy)) / 120 * 100,
    score = case_when(score <= 40 ~ 0, T ~ 1),
    score = factor(score, levels = 0:1, labels = c("Disapprove", "Approve"))
  ) %>% 
  select(-c(approved:easy))


<<<<<<< HEAD
library(Hmisc)
codebook <- read.csv("C:\\R Projects\\PINGS\\codebk.csv")
Hmisc::label(pings, self = F) <- codebook$label
=======
##  
colnames(dset)
# library(sjlabelled)
# library(Hmisc)
str(dset$record_id)
Hmisc::label(dset, self = F) <- readClipboard()
attributes(dset$record_id)
sjlabelled::add_labels()
summarytools::la
>>>>>>> 69f8c7f87a2b90f718dde5c1de92a7a29b832679
