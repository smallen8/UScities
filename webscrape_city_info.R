library(rvest)
library(stringr)

#------------------------
# cities and populations
#------------------------
pop_url <- 'http://www.city-data.com/city/Missouri.html'
pop_page <- read_html(pop_url)
city_pops <- pop_page %>% html_nodes('.rS') %>% html_text()

cities <- gsub(x=city_pops, pattern='\\d.*', replacement='')
cities <- gsub(x=cities, pattern=', MO', replacement='')
pops <- as.numeric(gsub(x=cities_pops, pattern='\\D', replacement=''))

cities_pops_f <- data.frame(cities, pops)
cities_pops_f$cities <- as.character(cities_pops_f$cities)

#-----------------------
# cities and job info
#-----------------------
# get all cities from 3 pages
job_urls <- c('http://www.city-data.com/work/work-Missouri.html',
              'http://www.city-data.com/work/work-Missouri2.html',
              'http://www.city-data.com/work/work-Missouri3.html')
city_work_urls <- c()

for(url in job_urls) {
  job_page <- read_html(url)
  
  temp_city_work_urls <- job_page %>% html_nodes('.col-md-4') %>% html_nodes('a') %>% html_attr('href')
  temp_city_work_urls <- paste0('http://www.city-data.com/work/', temp_city_work_urls)
  
  city_work_urls <- c(city_work_urls, temp_city_work_urls)
}

city_work_urls <- city_work_urls[grepl('(Brewer)(Cherokee Pass)', city_work_urls)==FALSE]

# extract city information for each city
get_city_information <- function(city_work_urls) {
  i <- 1
  city_data <- data.frame()
  
  for(url in city_work_urls) {
    city_url <- paste0(url)
    city_work <- read_html(city_url)
    
    city_job_info <- city_work %>% html_nodes('.well') %>% html_text()
    
    city_name <- city_work %>% html_nodes('.city') %>% html_text()
    city_name <- gsub('Work and Jobs in ', '', city_name)
    city_name <- gsub(',.*', '', city_name)
    
    # get industries
    ind <- city_work %>% html_node(xpath='//*[@id="idTable"]') %>% html_text()
    ind <- gsub('([a-z]{1})([0-9]{1})', '\\1_\\2', ind) # add split between lowercase and number
    ind <- gsub('([)]{1})([0-9]{1})', '\\1_\\2', ind) # add split between parenthesis and number
    ind <- gsub('([)]{1})([A-Z]{1})', '\\1_\\2', ind) # add split between parenthesis and capital
    ind <- gsub('([a-z]{1})([A-Z]{1})', '\\1_\\2', ind)
    ind <- unlist(strsplit(ind, '_')) # split
    ind <- gsub('%.*', '', ind)
    ind <- ind[-c(1,2,3,4,5)]
    
    industry <- ind[seq(1, length(ind), 4)]
    all <- as.numeric(ind[seq(2, length(ind), 4)])
    males <- as.numeric(ind[seq(3, length(ind), 4)])
    females <- as.numeric(ind[seq(4, length(ind), 4)])
    most_common_industries <- data.frame(industry, all, males, females)
    most_common_industries$industry <- gsub('[,.]', '', most_common_industries$industry)
    most_common_industries$industry <- gsub(' ', '_', tolower(most_common_industries$industry))
    
    # get percentage of workers working in county
    perc_working_in_county <- as.numeric(unlist(regmatches(city_job_info[7], gregexpr('\\d{1,2}\\.\\d', city_job_info[7])))[1])
    
    # get percentage of people working at home
    perc_working_at_home <- as.numeric(unlist(regmatches(city_job_info[7], gregexpr('\\d{1,2}\\.\\d', city_job_info[7])))[2])
    
    # get travel time to work
    travel_time <- city_job_info[8]
    travel_time <- unlist(strsplit(travel_time, '\r\n'))
    travel_time <- gsub('%.*', '', travel_time)
    travel_time <- unlist(strsplit(travel_time, ':'))
    
    time <- travel_time[seq(1, length(travel_time), 3)]
    time <- gsub(' ', '_', tolower(time))
    perc <- as.numeric(travel_time[seq(3, length(travel_time), 3)])
    travel_time_to_work <- data.frame(time, perc)
    travel_time_to_work$time <- as.character(travel_time_to_work$time)
    
    # get type of transportation to work
    work_transportation <- city_job_info[12]
    work_transportation <- unlist(strsplit(work_transportation, '\r\n'))
    work_transportation <- gsub('%.*', '', work_transportation)
    work_transportation <- unlist(strsplit(work_transportation, ':'))
    
    type <- work_transportation[seq(1, length(work_transportation), 3)]
    type <- gsub(' ', '_', tolower(type))
    perc <- as.numeric(work_transportation[seq(3, length(work_transportation), 3)])
    means_of_transportation_to_work <- data.frame(type, perc)
    means_of_transportation_to_work$type <- as.character(means_of_transportation_to_work$type)
    
    # get type of employer
    employer <- city_job_info[18]
    employer <- unlist(strsplit(employer, '\r\n'))
    employer <- gsub('%.*', '', employer)
    employer <- unlist(strsplit(employer, ':'))
    
    type <- employer[seq(1, length(employer), 3)]
    type <- gsub(' ', '_', tolower(type))
    perc <- as.numeric(employer[seq(3, length(employer), 3)])
    class_of_workers <- data.frame(type, perc)
    class_of_workers$type <- as.character(class_of_workers$type)
    
    # combine city data
    city_data[i, 'city'] <- city_name
    
    for(row in 1:nrow(most_common_industries)) {
      city_data[i, most_common_industries[row, 'industry']] <- most_common_industries[row, 'all']
    }
    
    city_data[i, 'perc_working_in_county'] <- perc_working_in_county
    city_data[i, 'perc_working_at_home'] <- perc_working_at_home
    
    for(row in 1:nrow(travel_time_to_work)) {
      city_data[i, travel_time_to_work[row, 'time']] <- travel_time_to_work[row, 'perc']
    }
    
    for(row in 1:nrow(means_of_transportation_to_work)) {
      city_data[i, means_of_transportation_to_work[row, 'type']] <- means_of_transportation_to_work[row, 'perc']
    }
    
    for(row in 1:nrow(class_of_workers)) {
      city_data[i, class_of_workers[row, 'type']] <- class_of_workers[row, 'perc']
    }
    i <- i+1
  }
  
  return(city_data)
}

city_job_info <- get_city_information(city_work_urls)



### debug
i <- 1
city_data <- data.frame()

for(url in city_work_urls) {
  city_url <- paste0(url)
  city_work <- read_html(city_url)
  
  city_job_info <- city_work %>% html_nodes('.well') %>% html_text()
  
  city_name <- city_work %>% html_nodes('.city') %>% html_text()
  city_name <- gsub('Work and Jobs in ', '', city_name)
  city_name <- gsub(',.*', '', city_name)
  print(city_name)
  
  # get industries
  ind <- city_work %>% html_node(xpath='//*[@id="idTable"]') %>% html_text()
  ind <- gsub('([a-z]{1})([0-9]{1})', '\\1_\\2', ind) # add split between lowercase and number
  ind <- gsub('([)]{1})([0-9]{1})', '\\1_\\2', ind) # add split between parenthesis and number
  ind <- gsub('([)]{1})([A-Z]{1})', '\\1_\\2', ind) # add split between parenthesis and capital
  ind <- gsub('([a-z]{1})([A-Z]{1})', '\\1_\\2', ind)
  ind <- unlist(strsplit(ind, '_')) # split
  ind <- gsub('%.*', '', ind)
  ind <- ind[-c(1,2,3,4,5)]
  
  industry <- ind[seq(1, length(ind), 4)]
  all <- as.numeric(ind[seq(2, length(ind), 4)])
  males <- as.numeric(ind[seq(3, length(ind), 4)])
  females <- as.numeric(ind[seq(4, length(ind), 4)])
  most_common_industries <- data.frame(industry, all, males, females)
  most_common_industries$industry <- gsub('[,.]', '', most_common_industries$industry)
  most_common_industries$industry <- gsub(' ', '_', tolower(most_common_industries$industry))
  
  # get percentage of workers working in county
  perc_working_in_county_ind <- grep('Percentage of workers working in this county', city_job_info)[1]
  perc_working_in_county <- as.numeric(unlist(regmatches(city_job_info[perc_working_in_county_ind], gregexpr('\\d{1,2}\\.\\d', city_job_info[7])))[1])
  
  # get percentage of people working at home
  perc_working_at_home_ind <- grep('Percentage of workers working in this county', city_job_info)[1]
  perc_working_at_home <- as.numeric(unlist(regmatches(city_job_info[perc_working_at_home_ind], gregexpr('\\d{1,2}\\.\\d', city_job_info[7])))[2])
  
  # get travel time to work
  travel_time_ind <- grep('\\d{1,2} to \\d{1,2} minutes', city_job_info)[1]
  travel_time <- city_job_info[travel_time_ind]
  travel_time <- unlist(strsplit(travel_time, '\r\n'))
  travel_time <- gsub('%.*', '', travel_time)
  travel_time <- unlist(strsplit(travel_time, ':'))

  time <- travel_time[seq(1, length(travel_time), 3)]
  time <- gsub(' ', '_', tolower(time))
  perc <- as.numeric(travel_time[seq(3, length(travel_time), 3)])
  travel_time_to_work <- data.frame(time, perc)
  travel_time_to_work$time <- as.character(travel_time_to_work$time)
  
  # get type of transportation to work
  work_transportation_ind <- grep('Drove car alone', city_job_info)[1]
  work_transportation <- city_job_info[work_transportation_ind]
  work_transportation <- unlist(strsplit(work_transportation, '\r\n'))
  work_transportation <- gsub('%.*', '', work_transportation)
  work_transportation <- unlist(strsplit(work_transportation, ':'))
  
  type <- work_transportation[seq(1, length(work_transportation), 3)]
  type <- gsub(' ', '_', tolower(type))
  perc <- as.numeric(work_transportation[seq(3, length(work_transportation), 3)])
  means_of_transportation_to_work <- data.frame(type, perc)
  means_of_transportation_to_work$type <- as.character(means_of_transportation_to_work$type)
  
  # get type of employer
  employer_ind <- grep('Employee of', city_job_info)[1]
  employer <- city_job_info[employer_ind]
  employer <- unlist(strsplit(employer, '\r\n'))
  employer <- gsub('%.*', '', employer)
  employer <- unlist(strsplit(employer, ':'))
  
  type <- employer[seq(1, length(employer), 3)]
  type <- gsub(' ', '_', tolower(type))
  perc <- as.numeric(employer[seq(3, length(employer), 3)])
  class_of_workers <- data.frame(type, perc)
  class_of_workers$type <- as.character(class_of_workers$type)
  
  # combine city data
  city_data[i, 'city'] <- city_name
  
  for(row in 1:nrow(most_common_industries)) {
    city_data[i, most_common_industries[row, 'industry']] <- most_common_industries[row, 'all']
  }
  
  city_data[i, 'perc_working_in_county'] <- perc_working_in_county
  city_data[i, 'perc_working_at_home'] <- perc_working_at_home
  
  for(row in 1:nrow(travel_time_to_work)) {
    city_data[i, travel_time_to_work[row, 'time']] <- travel_time_to_work[row, 'perc']
  }
  
  for(row in 1:nrow(means_of_transportation_to_work)) {
    city_data[i, means_of_transportation_to_work[row, 'type']] <- means_of_transportation_to_work[row, 'perc']
  }
  
  for(row in 1:nrow(class_of_workers)) {
    city_data[i, class_of_workers[row, 'type']] <- class_of_workers[row, 'perc']
  }
  
  i <- i+1
}


# debug one city
city_data <- data.frame()

city_url <- paste0(city_work_urls[grep('Brewer', city_work_urls)])
city_work <- read_html(city_url)

city_job_info <- city_work %>% html_nodes('.well') %>% html_text()

city_name <- city_work %>% html_nodes('.city') %>% html_text()
city_name <- gsub('Work and Jobs in ', '', city_name)
city_name <- gsub(',.*', '', city_name)
print(city_name)

# get industries
ind <- city_work %>% html_node(xpath='//*[@id="idTable"]') %>% html_text()
ind <- gsub('([a-z]{1})([0-9]{1})', '\\1_\\2', ind) # add split between lowercase and number
ind <- gsub('([)]{1})([0-9]{1})', '\\1_\\2', ind) # add split between parenthesis and number
ind <- gsub('([)]{1})([A-Z]{1})', '\\1_\\2', ind) # add split between parenthesis and capital
ind <- gsub('([a-z]{1})([A-Z]{1})', '\\1_\\2', ind)
ind <- unlist(strsplit(ind, '_')) # split
ind <- gsub('%.*', '', ind)
ind <- ind[-c(1,2,3,4,5)]

industry <- ind[seq(1, length(ind), 4)]
all <- as.numeric(ind[seq(2, length(ind), 4)])
males <- as.numeric(ind[seq(3, length(ind), 4)])
females <- as.numeric(ind[seq(4, length(ind), 4)])
most_common_industries <- data.frame(industry, all, males, females)
most_common_industries$industry <- gsub('[,.]', '', most_common_industries$industry)
most_common_industries$industry <- gsub(' ', '_', tolower(most_common_industries$industry))

# get percentage of workers working in county
perc_working_in_county_ind <- grep('Percentage of workers working in this county', city_job_info)[1]
perc_working_in_county <- as.numeric(unlist(regmatches(city_job_info[perc_working_in_county_ind], gregexpr('\\d{1,2}\\.\\d', city_job_info[7])))[1])

# get percentage of people working at home
perc_working_at_home_ind <- grep('Percentage of workers working in this county', city_job_info)[1]
perc_working_at_home <- as.numeric(unlist(regmatches(city_job_info[perc_working_at_home_ind], gregexpr('\\d{1,2}\\.\\d', city_job_info[7])))[2])

# get travel time to work
travel_time_ind <- grep('\\d{1} to \\d{1} minutes', city_job_info)[1]
travel_time <- city_job_info[travel_time_ind]
travel_time <- unlist(strsplit(travel_time, '\r\n'))
travel_time <- gsub('%.*', '', travel_time)
travel_time <- unlist(strsplit(travel_time, ':'))

time <- travel_time[seq(1, length(travel_time), 3)]
time <- gsub(' ', '_', tolower(time))
perc <- as.numeric(travel_time[seq(3, length(travel_time), 3)])
travel_time_to_work <- data.frame(time, perc)
travel_time_to_work$time <- as.character(travel_time_to_work$time)

# get type of transportation to work
work_transportation_ind <- grep('Drove car alone', city_job_info)[1]
work_transportation <- city_job_info[work_transportation_ind]
work_transportation <- unlist(strsplit(work_transportation, '\r\n'))
work_transportation <- gsub('%.*', '', work_transportation)
work_transportation <- unlist(strsplit(work_transportation, ':'))

type <- work_transportation[seq(1, length(work_transportation), 3)]
type <- gsub(' ', '_', tolower(type))
perc <- as.numeric(work_transportation[seq(3, length(work_transportation), 3)])
means_of_transportation_to_work <- data.frame(type, perc)
means_of_transportation_to_work$type <- as.character(means_of_transportation_to_work$type)

# get type of employer
employer_ind <- grep('Employee of', city_job_info)[1]
employer <- city_job_info[employer_ind]
employer <- unlist(strsplit(employer, '\r\n'))
employer <- gsub('%.*', '', employer)
employer <- unlist(strsplit(employer, ':'))

type <- employer[seq(1, length(employer), 3)]
type <- gsub(' ', '_', tolower(type))
perc <- as.numeric(employer[seq(3, length(employer), 3)])
class_of_workers <- data.frame(type, perc)
class_of_workers$type <- as.character(class_of_workers$type)

# combine city data
city_data[i, 'city'] <- city_name

for(row in 1:nrow(most_common_industries)) {
  city_data[i, most_common_industries[row, 'industry']] <- most_common_industries[row, 'all']
}

city_data[i, 'perc_working_in_county'] <- perc_working_in_county
city_data[i, 'perc_working_at_home'] <- perc_working_at_home

for(row in 1:nrow(travel_time_to_work)) {
  city_data[i, travel_time_to_work[row, 'time']] <- travel_time_to_work[row, 'perc']
}

for(row in 1:nrow(means_of_transportation_to_work)) {
  city_data[i, means_of_transportation_to_work[row, 'type']] <- means_of_transportation_to_work[row, 'perc']
}

for(row in 1:nrow(class_of_workers)) {
  city_data[i, class_of_workers[row, 'type']] <- class_of_workers[row, 'perc']
}



######

i <- 1
city_data <- data.frame()

for(url in city_work_urls) {
  city_url <- paste0(url)
  city_work <- read_html(city_url)
  
  city_job_info <- city_work %>% html_nodes('.well') %>% html_text()
  
  city_name <- city_work %>% html_nodes('.city') %>% html_text()
  city_name <- gsub('Work and Jobs in ', '', city_name)
  city_name <- gsub(',.*', '', city_name)
  print(city_name)
  
  # get industries
  ind <- city_work %>% html_node(xpath='//*[@id="idTable"]') %>% html_text()
  ind <- gsub('([a-z]{1})([0-9]{1})', '\\1_\\2', ind) # add split between lowercase and number
  ind <- gsub('([)]{1})([0-9]{1})', '\\1_\\2', ind) # add split between parenthesis and number
  ind <- gsub('([)]{1})([A-Z]{1})', '\\1_\\2', ind) # add split between parenthesis and capital
  ind <- gsub('([a-z]{1})([A-Z]{1})', '\\1_\\2', ind)
  ind <- unlist(strsplit(ind, '_')) # split
  ind <- gsub('%.*', '', ind)
  ind <- ind[-c(1,2,3,4,5)]
  
  industry <- ind[seq(1, length(ind), 4)]
  all <- as.numeric(ind[seq(2, length(ind), 4)])
  males <- as.numeric(ind[seq(3, length(ind), 4)])
  females <- as.numeric(ind[seq(4, length(ind), 4)])
  most_common_industries <- data.frame(industry, all, males, females)
  most_common_industries$industry <- gsub('[,.]', '', most_common_industries$industry)
  most_common_industries$industry <- gsub(' ', '_', tolower(most_common_industries$industry))
  
  # get percentage of workers working in county
  perc_working_in_county <- as.numeric(unlist(regmatches(city_job_info[7], gregexpr('\\d{1,2}\\.\\d', city_job_info[7])))[1])
  
  # get percentage of people working at home
  perc_working_at_home <- as.numeric(unlist(regmatches(city_job_info[7], gregexpr('\\d{1,2}\\.\\d', city_job_info[7])))[2])
  
  # get travel time to work
  travel_time <- city_job_info[8]
  travel_time <- unlist(strsplit(travel_time, '\r\n'))
  travel_time <- gsub('%.*', '', travel_time)
  travel_time <- unlist(strsplit(travel_time, ':'))
  
  time <- travel_time[seq(1, length(travel_time), 3)]
  time <- gsub(' ', '_', tolower(time))
  perc <- as.numeric(travel_time[seq(3, length(travel_time), 3)])
  travel_time_to_work <- data.frame(time, perc)
  travel_time_to_work$time <- as.character(travel_time_to_work$time)
  
  # get type of transportation to work
  work_transportation <- city_job_info[12]
  work_transportation <- unlist(strsplit(work_transportation, '\r\n'))
  work_transportation <- gsub('%.*', '', work_transportation)
  work_transportation <- unlist(strsplit(work_transportation, ':'))
  
  type <- work_transportation[seq(1, length(work_transportation), 3)]
  type <- gsub(' ', '_', tolower(type))
  perc <- as.numeric(work_transportation[seq(3, length(work_transportation), 3)])
  means_of_transportation_to_work <- data.frame(type, perc)
  means_of_transportation_to_work$type <- as.character(means_of_transportation_to_work$type)
  
  # get type of employer
  employer <- city_job_info[18]
  employer <- unlist(strsplit(employer, '\r\n'))
  employer <- gsub('%.*', '', employer)
  employer <- unlist(strsplit(employer, ':'))
  
  type <- employer[seq(1, length(employer), 3)]
  type <- gsub(' ', '_', tolower(type))
  perc <- as.numeric(employer[seq(3, length(employer), 3)])
  class_of_workers <- data.frame(type, perc)
  class_of_workers$type <- as.character(class_of_workers$type)
  
  # combine city data
  city_data[i, 'city'] <- city_name
  
  for(row in 1:nrow(most_common_industries)) {
    city_data[i, most_common_industries[row, 'industry']] <- most_common_industries[row, 'all']
  }
  
  city_data[i, 'perc_working_in_county'] <- perc_working_in_county
  city_data[i, 'perc_working_at_home'] <- perc_working_at_home
  
  for(row in 1:nrow(travel_time_to_work)) {
    city_data[i, travel_time_to_work[row, 'time']] <- travel_time_to_work[row, 'perc']
  }
  
  for(row in 1:nrow(means_of_transportation_to_work)) {
    city_data[i, means_of_transportation_to_work[row, 'type']] <- means_of_transportation_to_work[row, 'perc']
  }
  
  for(row in 1:nrow(class_of_workers)) {
    city_data[i, class_of_workers[row, 'type']] <- class_of_workers[row, 'perc']
  }
  i <- i+1
}



####





