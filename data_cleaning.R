library(RDota2)
library(httr)
library(steamR)
library(rlist)
library(tidyverse)
library(readr)
library(magrittr)
library(stringr)
library(tidytext)
data(stop_words)
library(scales)
library(maps)
library(leaflet)
Sys.setlocale("LC_CTYPE","eng")

full <- read_csv("df.csv")
full_us <- read_csv("full_us.csv")

########################## for 12 countries ##################
full %<>% filter(is.na(longitude)==F) %<>% filter(is.na(latitude)==F)
With_salary <- full %>% filter(is.na(salary_max)==F) %<>% filter(is.na(salary_min)==F)

full %<>% select(c("id","title","description","tag","label","area","location_name","latitude","longitude","salary_min","salary_max"))

# clean area
clean_1 <- full  %>% separate(area,into = c("country","state","county","city"),sep=",")
clean_1 %<>% separate(country,into = c("de1","country","de2"),sep = '"')
clean_1 %<>% separate(state,into = c("de3","state","de4"),sep = '"')
clean_1 %<>% separate(county,into = c("de5","county","de6"),sep = '"')
clean_1 %<>% separate(city,into = c("de5","city","de6"),sep = '"')
clean_1 %<>% select(-c("de1","de2","de3","de4","de5","de6"))

# clean title and description
clean_1$title <- str_replace_all(clean_1$title, "<strong>","")
clean_1$title <- str_replace_all(clean_1$title, "</strong>","")
clean_1$description <- str_replace_all(clean_1$description, "<strong>","")
clean_1$description <- str_replace_all(clean_1$description, "</strong>","")
clean_1$description <- str_replace_all(clean_1$description, "�","")
clean_1$title <- str_replace_all(clean_1$title, "�","")

#Is the vocabulary used to describe jobs for data scientists and statisticians 
#consistent between job boards?  Are there words that are specific to subfields 
#or geographic areas?  Are there words that are associated with salary levels?  
#Can you produce a list of descriptors that are commonly used?  

# over all
tidy_desc <- clean_1[,c(1,3)] %>%
  unnest_tokens(word, description)

tidy_desc <- tidy_desc %>%
  anti_join(stop_words) %>% filter(!(word %in% c("data","scientist","de","und","en","di","scientists","science","la","je","als","des","du","een","der","le","00e4")))

tidy_desc %>%
  count(word, sort = TRUE) %>%
  filter(n > 160) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

####################################### us only ########################################## 
full_us %<>% filter(is.na(longitude)==F) %<>% filter(is.na(latitude)==F)
With_salary <- full_us %>% filter(is.na(salary_max)==F) %<>% filter(is.na(salary_min)==F)

full_us %<>% select(c("id","title","description","tag","label","area","location_name","latitude","longitude","salary_min","salary_max"))

# clean area
us_clean <- full_us  %>% separate(area,into = c("country","state","county","city"),sep=",")
us_clean %<>% separate(country,into = c("de1","country","de2"),sep = '"')
us_clean %<>% separate(state,into = c("de3","state","de4"),sep = '"')
us_clean %<>% separate(county,into = c("de5","county","de6"),sep = '"')
us_clean %<>% separate(city,into = c("de5","city","de6"),sep = '"')
us_clean %<>% select(-c("de1","de2","de3","de4","de5","de6"))

# clean title and description
us_clean$title <- str_replace_all(clean_1$title, "<strong>","")
us_clean$title <- str_replace_all(clean_1$title, "</strong>","")
us_clean$description <- str_replace_all(clean_1$description, "<strong>","")
us_clean$description <- str_replace_all(clean_1$description, "</strong>","")
us_clean$description <- str_replace_all(clean_1$description, "�","")
us_clean$title <- str_replace_all(clean_1$title, "�","")

# over all
tidy_desc <- us_clean[,c(1,3)] %>%
  unnest_tokens(word, description)

tidy_desc <- tidy_desc %>%
  anti_join(stop_words) #%>% filter(!(word %in% c("data","scientist","de","und","en","di","scientists","science","la","je","als","des","du","een","der","le","00e4")))

tidy_desc %>%
  count(word, sort = TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

# leaflet

mapStates = map("state", fill = TRUE, plot = FALSE)

leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)%>%
  addMarkers(data=full_us,lng=~longitude,lat=~latitude,clusterOptions = markerClusterOptions())

# us
pop <- paste("Job title:",us_clean$title,"<br/>",
             "Description:",us_clean$description,"<br/>")
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)%>%
  addCircleMarkers(data=us_clean,lng=~longitude,
                   lat=~latitude,
                   clusterOptions = markerClusterOptions(),
                   weight = 2,
                   popup = pop)
# 12 country
pop1 <- paste("Job title:",clean_1$title,"<br/>",
             "Description:",clean_1$description,"<br/>")
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)%>%
  addCircleMarkers(data=clean_1,lng=~longitude,
                   lat=~latitude,
                   clusterOptions = markerClusterOptions(),
                   weight = 2,popup = pop1)

###################################### for England with detail ############################## 
gb_final <- read.csv("gb_final.csv")
gb_final %<>% filter(is.na(longitude)==F) %<>% filter(is.na(latitude)==F)
gb_final <- gb_final %>% filter(is.na(salary_max)==F) %<>% filter(is.na(salary_min)==F)
colnames(gb_final)[19] <- "company_name"
colnames(gb_final)[23] <- "location_name"

gb_final %<>% select(c("id","title","description","tag","label","area","location_name","latitude","longitude","salary_min","salary_max","detail"))

# clean area
clean_gb <- gb_final  %>% separate(area,into = c("country","state","county","city"),sep=",")
clean_gb %<>% separate(country,into = c("de1","country","de2"),sep = '"')
clean_gb %<>% separate(state,into = c("de3","state","de4"),sep = '"')
clean_gb %<>% separate(county,into = c("de5","county","de6"),sep = '"')
clean_gb %<>% separate(city,into = c("de5","city","de6"),sep = '"')
clean_gb %<>% select(-c("de1","de2","de3","de4","de5","de6"))

# clean title and description
clean_gb$title <- str_replace_all(clean_gb$title, "<strong>","")
clean_gb$title <- str_replace_all(clean_gb$title, "</strong>","")
clean_gb$description <- str_replace_all(clean_gb$description, "<strong>","")
clean_gb$description <- str_replace_all(clean_gb$description, "</strong>","")
clean_gb$description <- str_replace_all(clean_gb$description, "�","")
clean_gb$title <- str_replace_all(clean_gb$title, "�","")

tidygb_desc <- clean_gb[,c(1,15)] %>%
  unnest_tokens(word, detail)

tidygb_desc <- tidygb_desc %>%
  anti_join(stop_words) #%>% filter(!(word %in% c("data","scientist","de","und","en","di","scientists","science","la","je","als","des","du","een","der","le","00e4")))

tidygb_desc %>%
  count(word, sort = TRUE) %>%
  filter(n > 500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

write.csv(clean_gb,"clean_gb.csv",row.names = F)

# leaflet
mapStates = map("state", fill = TRUE, plot = FALSE)

leaflet() %>% addTiles() %>%
  addMarkers(data=clean_gb,lng=~longitude,lat=~latitude,clusterOptions = markerClusterOptions())

# gb
pop_gb <- paste("Job title : ",clean_gb$title,
             "Salary : ",clean_gb$salary_min,"~",clean_gb$salary_max,
             "Summary : ",clean_gb$description)
pop_gb <- sprintf(
  "<strong>Job title</strong>:%s</br><strong>Salary</strong>:%s~%s</br><strong>Summary</strong>:%s",
  clean_gb$title,clean_gb$salary_min,clean_gb$salary_max,clean_gb$description
) %>% lapply(htmltools::HTML)
cPal <- colorNumeric(palette = "Oranges",domain=clean_gb$salary_max)
leaflet(clean_gb) %>% addTiles() %>%
  addCircleMarkers(data=clean_gb,lng=~longitude,
                   lat=~latitude,
                   clusterOptions = markerClusterOptions(),
                   popup = pop_gb,
                   fillColor = ~cPal(clean_gb$salary_max),
                   stroke = FALSE,fillOpacity = 0.9)%>%
  addLegend("bottomright", pal = cPal, values = ~salary_max,title = "salary ()",opacity = 1)

