library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(magrittr)
library(stringr)
library(tidytext)
data(stop_words)
library(leaflet)
library(shinydashboard)
library(gridExtra)
library(scales)
library(tidyr)
library(wordcloud2)
library(DT)

clean_gb <- read_csv("clean_gb.csv")
indeed <- read_csv("indeed.csv")
indeed <- indeed[1:770,]
Encoding(clean_gb$description) <- "UTF-8"
clean_gb$description <- iconv(x=clean_gb$description,from = "UTF-8",to = "UTF-8",sub = "")
Encoding(clean_gb$title) <- "UTF-8"
clean_gb$title <- iconv(x=clean_gb$title,from = "UTF-8",to = "UTF-8",sub = "")
Encoding(clean_gb$detail) <- "UTF-8"
clean_gb$detail <- iconv(x=clean_gb$detail,from = "UTF-8",to = "UTF-8",sub = "")
# adzuna vs indeed
tidygb_desc <- clean_gb[,c(1,15)] %>%
  unnest_tokens(word, detail)

tidygb_desc <- tidygb_desc %>%
  anti_join(stop_words) #%>% filter(!(word %in% c("data","scientist","de","und","en","di","scientists","science","la","je","als","des","du","een","der","le","00e4")))

adzuna_text <- tidygb_desc %>% filter(word!="data") %>%
  count(word, sort = TRUE) %>%
  filter(n > 530) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word),height=650) +
  geom_col(fill="darkseagreen") +
  labs(y = NULL,title = "Description in Adzuna")+
  theme(axis.text.y=element_text(size=12))

tidyin_desc <- indeed[,c(1,6)] %>%
  unnest_tokens(word, description)

tidyin_desc <- tidyin_desc %>%
  anti_join(stop_words) #%>% filter(!(word %in% c("data","scientist","de","und","en","di","scientists","science","la","je","als","des","du","een","der","le","00e4")))

indeed_text <- tidyin_desc %>% filter(word!="data") %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word),height=650) +
  geom_col(fill="darkorange2") +
  labs(y = NULL,title = "Description in Indeed")+
  theme(axis.text.y=element_text(size=12))
  
a_vs_i <- grid.arrange(adzuna_text,indeed_text,nrow=1)

tidygb_desc %>% count(word, sort = T)
tidyin_desc %>% count(word, sort = T)

frequency <- bind_rows(mutate(tidygb_desc, author = "Adzuna"),
                       mutate(tidyin_desc, author = "Indeed")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, Indeed)

ggplot(frequency, aes(x = proportion, y = Adzuna, 
                      color = abs(Adzuna - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Adzuna", x = NULL)

# salary vs text
labels <- c("(0,35000]","(35000,45000]","(45000,60000]","(60000,216000]")
breaks <- c(-1,35000,45000,60000,216000)
clean_gb$salary_label <- cut(clean_gb$salary_max,breaks,labels)
tidy_salary <- clean_gb[,c(16,15)] %>% 
  unnest_tokens(word, detail) %>%anti_join(stop_words) %>%
  group_by(salary_label,word) %>% summarise(count=n())

tidy_salary %>% filter(word!="data") %>%
  filter(count > 150) %>%
  ggplot(aes(count, word) ) +
  geom_col(fill="darkseagreen") +
  labs(y = NULL,title = "Description in Adzuna")+
  theme(axis.text.y=element_text(size=12))+
  facet_grid(~salary_label)

frequency_2 <- tidy_salary %>%
  group_by(salary_label)  %>%
  mutate(proportion = count / sum(count)) %>% 
  select(-count) %>% 
  spread(salary_label, proportion) %>% 
  gather(salary_label, proportion,"(35000,45000]":"(60000,216000]")

g <- ggplot(frequency_2, aes(x = proportion, y = `(0,35000]`, 
                           color = abs(`(0,35000]` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~salary_label, ncol = 3) +
  theme(legend.position="none") +
  labs(y = "(0,35000]", x = NULL)

# different location
tidy_location <- clean_gb[,c(7,15)] %>% 
  unnest_tokens(word, detail) %>%anti_join(stop_words) %>%
  group_by(state,word) %>% summarise(count=n())

tidy_location %>% filter(word!="data") %>%
  filter(count > 180) %>%
  ggplot(aes(count, word) ) +
  geom_col(fill="darkseagreen") +
  labs(y = NULL,title = "Description in Adzuna")+
  theme(axis.text.y=element_text(size=12))+
  facet_grid(~state)

frequency_3 <- tidy_location %>%
  group_by(state)  %>%
  mutate(proportion = count / sum(count)) %>% 
  select(-count) %>% 
  spread(state, proportion) %>% 
  gather(state, proportion,"Eastern England":"Yorkshire And The Humber")

g <- ggplot(frequency_3, aes(x = proportion, y = `East Midlands`, 
                             color = abs(`East Midlands` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), 
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~state, ncol = 3) +
  theme(legend.position="none") +
  labs(y = "East Midlands", x = NULL)

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title="Jobs"),

    # Sidebar with a slider input for number of bins 
    dashboardSidebar(
      
      sidebarMenu(
        
        menuItem("Jobs Overview",
                 menuSubItem("Maps with salary",tabName = "ukm"),
                 menuSubItem("Data table and wordcloud",tabName = "adzuna_data")),
        menuItem("Text Mining",
                 menuSubItem("Adzuna vs Indeed",tabName = "avsi"),
                 menuSubItem("For different salary level",tabName = "salary_c"),
                 menuSubItem("For different location",tabName = "location_c"))
        
      )
      
    ),
    
    dashboardBody(
      
      tabItems(
        #mapping item
        tabItem(
          tabName = "ukm",
          fluidRow(
            box(width = 8,
                   title = "Mapping",
                   id = "tabset1",
                   leafletOutput("UKMC",height = 650)),
            box(width = 4,
                title = "Controls",
                status = "warning",
                footer = "If whole world map is showed, that means there are no 
                such range of salary in that state.",
                solidHeader = T,
                sliderInput("range","Range of years",min=0,max=168000,
                            value = c(0,168000)),
                selectInput("State",
                            label = "Choose a state to display",
                            choices = c("All","South East England","London",
                                        "South West England","Scotland",
                                        "Yorkshire And The Humber",
                                        "North West England","West Midlands",
                                        "Northern Ireland","Wales","North East England"))),
            box(width = 4,
                title = "Tips",background = "olive", 
                solidHeader = TRUE,collapsible = TRUE,
                div("1.Enlatge and click map to see more information."),
                div("2.Click '-' to hide Tips"))
          )
        ),
        tabItem(
          tabName = "adzuna_data",
          fluidRow(
            box(title = "wordcloud of descriptions in Adzuna",
                wordcloud2Output("wc1")),
            box(width=12,title = "Adzuna data table",
                DT::dataTableOutput("adzuna"))
          )),
        # adzuna vs indeed
        tabItem(
          tabName = "avsi",
          fluidRow(
            box(width = 8,
                title = "words in descriptions for Adzuna and Indeed",
                plotOutput("avsip"),
                uiOutput("p1")),
            box(width = 4,
                title = "proportion of word",
                plotOutput("pai"),
                uiOutput("p2"))
          )
        ),
        # salary
        tabItem(
          tabName = "salary_c",
          fluidRow(
            box(width = 12,
                title = "words in descriptions in different levels of salary",
                plotOutput("salary_cc",
                           height=650),
                uiOutput("p3")),
            box(width = 12,
                title = "proportion of word",
                plotOutput("salary_ccc",height = 400),
                uiOutput("p4"))
          )
        ),
        # location
        tabItem(
          tabName = "location_c",
          fluidRow(
            box(width = 12,
                title = "words in descriptions in different states",
                plotOutput("location_cc",height = 800),
                uiOutput("p5"))
      
          )
        )
        
      )
      
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #UKM
  output$UKMC <- renderLeaflet({
    clean_gb %<>% filter(salary_min>=input$range[1]&salary_max<=input$range[2])
    pop_gb <- paste("<strong>Job title</strong>: ",clean_gb$title,"<br/>",
                   "<strong>Salary</strong> :",clean_gb$salary_min,"~",clean_gb$salary_max,"<br/>",
                    "<strong>Summary</strong>: ",clean_gb$description)
    cPal <- colorNumeric(palette = "Oranges",domain=clean_gb$salary_max)
    if(input$State=="All"){
      leaflet(clean_gb) %>% addTiles() %>%
        addCircleMarkers(data=clean_gb,lng=~longitude,
                         lat=~latitude,
                         clusterOptions = markerClusterOptions(),
                         popup = pop_gb,
                         fillColor = ~cPal(clean_gb$salary_max),
                         stroke = FALSE,fillOpacity = 0.9)%>%
        addLegend("bottomright", pal = cPal, values = ~salary_max,title = "salary ",opacity = 1)
    }else{
      clean_gb %<>% filter(clean_gb$state==input$State)
      leaflet(clean_gb) %>% addTiles() %>%
        addCircleMarkers(data=clean_gb,lng=~longitude,
                         lat=~latitude,
                         clusterOptions = markerClusterOptions(),
                         popup = pop_gb,
                         fillColor = ~cPal(clean_gb$salary_max),
                         stroke = FALSE,fillOpacity = 0.9)%>%
        addLegend("bottomright", pal = cPal, values = ~salary_max,title = "salary",opacity = 1)
    }
  })
  
  
  output$avsip <- renderPlot({
    grid.arrange(adzuna_text,indeed_text,nrow=1)
  })
  output$pai <- renderPlot({
    ggplot(frequency, aes(x = proportion, y = Adzuna, 
                          color = abs(Adzuna - proportion))) +
      geom_abline(color = "gray40", lty = 2) +
      geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
      scale_color_gradient(limits = c(0, 0.001), 
                           low = "darkslategray4", high = "gray75") +
      facet_wrap(~author, ncol = 2) +
      theme(legend.position="none") +
      labs(y = "Adzuna", x = NULL)
  })
  output$salary_cc <- renderPlot({
    tidy_salary %>% filter(word!="data") %>%
      filter(count > 150) %>%
      ggplot(aes(count, word) ) +
      geom_col(fill="darkseagreen") +
      labs(y = NULL,title = "Description in Adzuna")+
      theme(axis.text.y=element_text(size=12))+
      facet_grid(~salary_label)
  })
  output$salary_ccc <- renderPlot({
    ggplot(frequency_2, aes(x = proportion, y = `(0,35000]`, 
                            color = abs(`(0,35000]` - proportion))) +
      geom_abline(color = "gray40", lty = 2) +
      geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
      scale_color_gradient(limits = c(0, 0.001), 
                           low = "darkslategray4", high = "gray75") +
      facet_wrap(~salary_label, ncol = 3) +
      theme(legend.position="none") +
      labs(y = "(0,35000]", x = NULL)
  })
  output$location_cc <- renderPlot({
    ggplot(frequency_3, aes(x = proportion, y = `East Midlands`, 
                            color = abs(`East Midlands` - proportion))) +
      geom_abline(color = "gray40", lty = 2) +
      geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
      scale_color_gradient(limits = c(0, 0.001), 
                           low = "darkslategray4", high = "gray75") +
      facet_wrap(~state, ncol = 3) +
      theme(legend.position="none") +
      labs(y = "East Midlands", x = NULL)
  })
  output$wc1 <- renderWordcloud2({
    tidygb_desc %>% count(word)%>%filter(n>300)%>%wordcloud2(size = 2)
  })
  output$adzuna <- DT::renderDataTable({ clean_gb })
  output$p1 <- renderUI({
    p("Comparing the count of words in two website, they are almost the same. Words like 'experience', 'team', 'skills' are all have high frequency in both two websites. And we can see that python and machine learning are always mentioned so they are really important skills.")
  })
  output$p2 <- renderUI({
    p("In frequency plot, in Adzuna, 'UK' and 'London' are frequently mentioned cause jobs in that data is all from UK, so it makes sense. And also 'modeling' is mentioned more in Indeed. ")
  })
  output$p3 <- renderUI({
    p("It is clear that in different level of salary, the distribution of words are almost the same but in some group there are some words not mentioned, it may because the number of jobs is small in that group. But also may because they are not important in that level.")
  })
  output$p4 <- renderUI({
    p("In frequency plot, it looks almost the same for each level. But it is obvious that 'graduates' are mentioned more in level (0,35000].")
  })
  output$p5 <- renderUI({
    p("It is hard to have some conclusion from this plot, but London is obviously different cause majority of jobs are in London; And in Yorkshire And The Humber, 'financial' is mentioned more than it in East Midlands.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
