rm(list = ls())

library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)

andrews_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-and)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=150" %>%
  read_html()

arctic_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-arc)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=660" %>%
  read_html()

baltimore_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-bes)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=185" %>%
  read_html()

beaufort_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-ble)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=30" %>%
  read_html()

bonanza_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-bnz)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=670" %>%
  read_html()

california_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-cce)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=100" %>%
  read_html()

cedar_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-cdr)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=580" %>%
  read_html()

cenaz_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-cap)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=670" %>%
  read_html()

coweeta_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-cwt)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=250" %>%
  read_html()

florida_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-fce)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=205" %>%
  read_html()

georgia_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-gce)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=720" %>%
  read_html()

harvard_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-hfr)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=440" %>%
  read_html()

hubbard_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-hbr)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=290" %>%
  read_html()

jornada_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-jrn)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=375" %>%
  read_html()

# Many kellog datasets do not include start and end dates which throws error in funtion - workaround requires two seperate downloads
kellog_page1 <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-kbs)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=41" %>%
  read_html()

kellog_page2 <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-kbs)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=43&rows=80" %>%
  read_html()

konza_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-knz)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=170" %>%
  read_html()

luquillo_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-luq)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=200" %>%
  read_html()

mcmurdo_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-mcm)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=330" %>%
  read_html()

moorea_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-mcr)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=110" %>%
  read_html()

niwot_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-nwt)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=350" %>%
  read_html()

ninlet_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-nin)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=12" %>%
  read_html()

northeast_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-nes)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=30" %>%
  read_html()

ntl_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-ntl)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=320" %>%
  read_html()

palmer_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-pal)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=90" %>%
  read_html()

plum_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-pie)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=600" %>%
  read_html()

santabar_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-sbc)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=250" %>%
  read_html()

sevilleta_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-sev)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=220" %>%
  read_html()

shortgrass_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-sgs)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=110" %>%
  read_html()

vcr_page <-
  "http://portal.edirepository.org:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-vcr)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false&start=0&rows=330" %>%
  read_html()

scraper <- function(package_id) {
  cat("Scraping", package_id, "\n")
  data <- str_c("https://portal.edirepository.org/nis/metadataviewer?packageid=",
                package_id) %>%
    read_html() %>%
    html_elements(".subgroup.onehundred_percent") %>%
    pluck(1) %>%
    html_elements(".roweven") %>%
    html_text2() 
  
  tibble(begin = pluck(data, 1), 
         end = pluck(data, 1))
}

andrews_data <- andrews_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
arctic_data <- arctic_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
baltimore_data <- baltimore_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
beaufort_data <- beaufort_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
bonanza_data <- bonanza_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
california_data <- california_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
cedar_data <- cedar_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
cenaz_data <- cenaz_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
coweeta_data <- coweeta_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
florida_data <- florida_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
georgia_data <- georgia_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
harvard_data <- harvard_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
hubbard_data <- hubbard_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
jornada_data <- jornada_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
kellog_data1 <- kellog_page1 %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
kellog_data2 <- kellog_page2 %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
kellog_data <- rbind(kellog_data1,kellog_data2)
kellog_data <- kellog_data[-c(6:7,9,33:35,38,43:45,60,65,70:71),]
konza_data <- konza_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
luquillo_data <- luquillo_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
mcmurdo_data <- mcmurdo_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
moorea_data <- moorea_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
ninlet_data <- ninlet_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
niwot_data <- niwot_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
northeast_data <- northeast_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
ntl_data <- ntl_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
palmer_data <- palmer_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
plum_data <- plum_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
santabar_data <- santabar_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
sevilleta_data <- sevilleta_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
shortgrass_data <- shortgrass_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))
vcr_data <- vcr_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators))

# Combine datasets (exclude Harvard because they only provide year and not a date)
all_data <- rbind(andrews_data,arctic_data,baltimore_data,beaufort_data,
                  bonanza_data,california_data,cedar_data,cenaz_data,
                  coweeta_data,florida_data,georgia_data,
                  hubbard_data,jornada_data,kellog_data,konza_data,luquillo_data,
                  mcmurdo_data,moorea_data,ninlet_data,niwot_data,northeast_data,
                  ntl_data,palmer_data,plum_data,santabar_data,sevilleta_data,
                  shortgrass_data,vcr_data)

# Extract the three letter code for each site
all_data <- all_data %>%
  mutate(newcol = package_id) %>%
  separate(newcol, sep = "-", into = c('a','b','c')) %>%
  separate(c, into = c('name_short','e','f')) %>%
  select(!c(a,b,e,f)) %>%
  mutate(across(name_short, toupper),
         begin = as.Date(begin,"%Y-%m-%d"),
         end = as.Date(end,"%Y-%m-%d"))

# Correct start dates that were reported as end dates and vice versa
all_data <- all_data %>%
  transform(begin = pmin(begin, end),
            end   = pmax(begin, end))

# Calculate each datasets length in years
all_data <- all_data %>%
  mutate(diff = round(time_length(difftime(end,begin), "years"),3))

# Treat the Harvard Forest data the same as all_data
# Recall HF reports start and end as year which is why it is treated differently
harvard_data <- harvard_data %>%
  mutate(newcol = package_id) %>%
  separate(newcol, sep = "-", into = c('a','b','c')) %>%
  separate(c, into = c('name_short','e','f')) %>%
  select(!c(a,b,e,f)) %>%
  mutate(mutate(across(name_short, toupper),
         begin = as.numeric(begin),
         end = as.numeric(end),
         diff = (end-begin)+1))
harvard_data <- harvard_data[,c(1:4,6,5)]
harvard_data <- harvard_data %>%
  mutate(begin = as.Date(NA),
         end = as.Date(NA),
         diff = as.numeric(diff))

# Combine all_data w/ HF and combine with LTER description csv
all_data <- rbind(all_data,harvard_data)

setwd("D:/School/Applications/Job and Fellowship Applications/L&O Eco-DAS")

lter <- read.csv('lter_sites.csv')

all_data <- left_join(all_data,lter, by = 'name_short')

all_data <- all_data %>%
  mutate(current_year = ifelse(end_year > 2023, 2023, end_year),
         around_time = (current_year-start_year)+1,
         frac = (diff/around_time)*100)

# width = 800 height = 600
all_data %>%
  ggplot(aes(diff)) +
  stat_ecdf(geom = 'step', pad = F) +
  labs(x = 'Time Series Length (years)',
       y = 'ECDF') +
  scale_x_continuous(breaks = seq(0,40,5)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  coord_cartesian(xlim = c(0, 40)) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16, color = 'black'))

# width = 1000 height = 900
all_data %>%
  ggplot(aes(diff)) +
  geom_hline(yintercept = 0.5, color = 'red', linetype = 'longdash') +
  stat_ecdf(geom = 'step', pad = F) +
  labs(x = 'Time Series Length (years)',
       y = 'ECDF') +
  scale_x_continuous(breaks = seq(0,40,10)) +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  coord_cartesian(xlim = c(0, 40)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = 'black')) +
  facet_wrap(~name_long)

# width = 800 height = 600
all_data %>%
  ggplot(aes(frac)) +
  stat_ecdf(geom = 'step', pad = F) +
  labs(x = 'Fraction of Timeseries Length/Site Length',
       y = 'ECDF') +
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  coord_cartesian(xlim = c(0, 100)) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16, color = 'black'))

# width = 1000 height = 900
all_data %>%
  ggplot(aes(frac)) +
  geom_hline(yintercept = 0.5, color = 'red', linetype = 'longdash') +
  stat_ecdf(geom = 'step', pad = F) +
  labs(x = 'Fraction of Timeseries Length/Site Length',
       y = 'ECDF') +
  scale_x_continuous(breaks = seq(0,100,20)) +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  coord_cartesian(xlim = c(0, 100)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = 'black')) +
  facet_wrap(~name_long)
