rm(list = ls())

library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)

# Extract the Package Id for each sites datasets ----
# Could have been done in one step but in trial the page would time out

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

# Scraper function ----
# extracts the start and end date from each LTER sites metadata page

scraper <- function(package_id) {
  cat("Scraping", package_id, "\n")
  url <- paste0(
    "https://portal.edirepository.org/nis/metadataviewer?packageid=",
    package_id
  )
  webpage <- read_html(url)
  begin <- html_text(html_nodes(webpage, xpath = "//td[contains(text(), 'Begin:')]/following-sibling::td"))
  end <- html_text(html_nodes(webpage, xpath = "//td[contains(text(), 'End:')]/following-sibling::td"))
  
  if (length(begin) == 0 || length(end) == 0) {
    return(tibble(begin = NA, end = NA))
  }
  
  tibble(begin = begin[1], end = end[1])
}


# Apply the scraper function ----
# applies the scraper function to extract the datasets publication_date, package_id, begin, and end dates

andrews <- andrews_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
arctic <- arctic_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
baltimore <- baltimore_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
beaufort <- beaufort_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
bonanza <- bonanza_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
california <- california_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
cedar <- cedar_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
cenaz <- cenaz_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
coweeta <- coweeta_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
florida <- florida_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
georgia <- georgia_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
harvard <- harvard_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
hubbard <- hubbard_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
jornada <- jornada_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
kellog <- kellog_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
konza <- konza_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
luquillo <- luquillo_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
mcmurdo <- mcmurdo_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
moorea <- moorea_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
ninlet <- ninlet_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
niwot <- niwot_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
northeast <- northeast_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
ntl <- ntl_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
palmer <- palmer_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
plum <- plum_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
santabar <- santabar_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
sevilleta <- sevilleta_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
shortgrass <- shortgrass_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))
vcr <- vcr_page %>%
  html_table() %>%
  pluck(4) %>%
  clean_names() %>%
  mutate(across(title, ~ str_squish(str_remove_all(., "\\n")))) %>%
  mutate(date = map(package_id, scraper)) %>% 
  unnest(date) %>%
  select(!c(title,creators,publication_date))

# Combine datasets ----
# exclude Harvard because they only provide year and not a date

all_data <- rbind(andrews,arctic,baltimore,beaufort,
                  bonanza,california,cedar,cenaz,
                  coweeta,florida,georgia,harvard,
                  hubbard,jornada,kellog,konza,luquillo,
                  mcmurdo,moorea,ninlet,niwot,northeast,
                  ntl,palmer,plum,santabar,sevilleta,
                  shortgrass,vcr)

#* Extract the three letter code for each site ----

dat <- all_data %>%
  mutate(newcol = package_id) %>%
  separate(newcol, sep = "-", into = c('a','b','c')) %>%
  separate(c, into = c('name_short','e','f')) %>%
  select(!c(a,b,e,f)) %>%
  mutate(across(name_short, toupper),
         begin = parse_date_time(begin, orders = c("ymd", "-ymd", "y")),
         end = parse_date_time(end, orders = c("ymd", "-ymd", "y")),
         diff_years = as.numeric(round(interval(begin, end) / dyears(1),2)),
         diff_years = ifelse(diff_years < 0, diff_years*-1,diff_years))

setwd("D:/School/Applications/Job and Fellowship Applications/L&O Eco-DAS")

lter <- read.csv('lter_sites.csv')

dat <- left_join(dat,lter, by = 'name_short')

dat <- dat %>%
  mutate(current_year = ifelse(end_year > 2023, 2023, end_year),
         around_time = (current_year-start_year)+1,
         frac = diff_years/around_time)

# write.csv(all_data,'LTER_timeseriesLength.csv')

# Figures ----
# width = 800 height = 600

dat %>%
  ggplot(aes(diff_years)) +
  stat_ecdf(geom = 'step', pad = F) +
  labs(x = 'Time Series Length (years)',
       y = 'ECDF') +
  scale_x_continuous(breaks = seq(0,40,5)) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1)) +
  coord_cartesian(xlim = c(0, 40)) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16, color = 'black'))

# width = 800 height = 600

a <- ecdf(dat$diff_years)
quantile(a, probs = seq(0,0.95,0.01))

dat %>%
  ggplot(aes(diff_years)) +
  geom_segment(x = 2.18, y = -Inf, xend = 2.18, yend = 0.5, color = 'red', linetype = 'longdash') +
  geom_segment(x = -Inf, y = 0.5, xend = 2.18, yend = 0.5, color = 'red', linetype = 'longdash') +
  geom_segment(x = 6, y = -Inf, xend = 6, yend = 0.62, color = 'red', linetype = 'longdash') +
  geom_segment(x = -Inf, y = 0.62, xend = 6, yend = 0.62, color = 'red', linetype = 'longdash') +
  geom_segment(aes(x = 11, y = 0.5, xend = 7, yend = 0.58),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  annotate('text', label = 'LTER Funding Cycle \n(every 6 years)', x = 15.5, y = 0.45, size = 5) +
  stat_ecdf(geom = 'step', pad = F) +
  labs(x = 'Time Series Length (years)',
       y = 'ECDF') +
  scale_x_continuous(breaks = seq(0,40,5)) +
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1)) +
  coord_cartesian(xlim = c(0, 40)) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16, color = 'black'),
        plot.margin = grid::unit(c(2,5,0,0), "mm"))

# width = 1000 height = 900
dat %>%
  ggplot(aes(diff_years)) +
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
dat %>%
  ggplot(aes(frac)) +
  stat_ecdf(geom = 'step', pad = F) +
  labs(x = 'Ratio of Timeseries Duration:Site Duration',
       y = 'ECDF') +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  scale_y_continuous(breaks = seq(0,1,0.1),limits = c(0,1)) +
  coord_cartesian(xlim = c(0,1)) +
  theme_bw() +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16, color = 'black'))

# width = 1000 height = 900
dat %>%
  ggplot(aes(frac)) +
  geom_hline(yintercept = 0.5, color = 'red', linetype = 'longdash') +
  stat_ecdf(geom = 'step', pad = F) +
  labs(x = 'Fraction of Timeseries Length/Site Length',
       y = 'ECDF') +
  scale_x_continuous(breaks = seq(0,1,0.2)) +
  scale_y_continuous(breaks = seq(0,1,0.2),limits = c(0,1)) +
  coord_cartesian(xlim = c(0,1)) +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = 'black')) +
  facet_wrap(~name_long)
