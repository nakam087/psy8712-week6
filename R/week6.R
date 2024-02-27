# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(stringi) #had to look up where function was from



# Data Import
citations <-stri_read_lines("../docs/citations.txt", encoding = 'Windows-1252') #ISO-8859-1 is the most common encoding
citations_txt<-citations[!stri_isempty(citations)] #tried to use str_view with match=F but returned diff answers than classmates
print(length(citations)-length(citations_txt))
print(mean(str_length(citations_txt)))

# Data Cleaning
View(citations_tbl%>%
       sample_n(20))
citations_tbl <-tibble(line=1:length(citations_txt), cite=citations_txt)%>%
  mutate(cite=str_replace_all(cite,pattern="[\'\"]", replacement=""))%>% #is there a way with remove all?
  mutate(year=as.integer(str_extract(cite,pattern="\\d{4}"))) %>%
  mutate(page_start=as.integer(str_extract(cite, pattern="\\d+(?=-\\d+)")))%>% #(\\d+)-\\
  mutate(perf_ref=str_detect(cite,pattern=regex("performance", ignore_case=T)))%>% #can you also use "(?i)performance" here?; can do str_to_lower here
  mutate(title=str_match(cite, pattern= "\\)\\.\\s*([^\\.]+[\\.?!])")[,2])%>%# I used chatGPT to help me with these, but they still don't work...
  mutate(first_author=str_match(cite, pattern="^\\*?(([A-Z][a-z]+,?\\s[A-Z](?:\\.\\s?[A-Z])?\\.?))")[,2]) #still some NA's but I gave up

print(sum(!is.na(citations_tbl$first_author))) #is over 30000,but it's still not all of them...some are lacking a first author as well and my code might be over-generous here
