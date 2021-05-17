# Load R packages
library(shiny)
library(shinythemes)
library(shinysense)  
library(shiny)
library(googledrive)
library(googlesheets4)
library(dplyr)
library(readxl)
library(tidyr)
#remotes::install.github("ropenscilabs/icon")
#library(icon)  
library(readxl)
#upload the data 
#Dannye_mentorstvo <- read_excel("love/ll/Dannye_mentorstvo.xlsx")

love = as.data.frame(Dannye_mentorstvo)

love = love %>% mutate(id = row_number())
info = love %>%  select(1,2, 3, 4, 33)
names = c("name", "company", "position", "about_me", "id")
colnames(info) = names
data = love %>% select(6, 30:33) 
names = c("edu","channels", "freq","formats", "id")
colnames(data) = names
texts = love[,27:29]
library(stopwords)
library(tm)
library(tidytext)
rustopwords = data.frame(word=c(stopwords("ru"), "это", "бла", "твой", "свой", "наш", "сл", "б", "ла", "д", "ду"), stringsAsFactors=FALSE)
names = c("teach","skills", "traits")
colnames(texts) = names
texts = texts %>%   mutate(id = row_number())
library(udpipe)
library(ruimtehol)
what_to_teach = ""
what_to_skill = ""

anno <- udpipe(texts$teach, "russian")
anno[, c("doc_id", "sentence_id", "token", "lemma", "upos")]
library(stopwords)
rustopwords = data.frame(lemma=c(stopwords("ru"), "это", "бла", "твой", "свой", "наш", "сл", "б", "ла", "д", "ду"), stringsAsFactors=FALSE)
library(stringr)
anno = anno %>% 
    anti_join(rustopwords) %>% 
    filter(! str_detect(lemma, "[0-9]+")) %>% 
    filter(!str_detect(lemma,"[:punct:]"))

set.seed(123456789)
model <- embed_sentencespace(anno, dim = 15, epoch = 20)


anno1 <- udpipe(texts$skills, "russian")
anno1[, c("doc_id", "sentence_id", "token", "lemma", "upos")]

anno1 = anno1 %>% 
    anti_join(rustopwords) %>% 
    filter(! str_detect(lemma, "[0-9]+")) %>% 
    filter(!str_detect(lemma,"[:punct:]"))

set.seed(123456789)
model1 <- embed_sentencespace(anno1, dim = 15, epoch = 20)


rm_words <- function(string, words) {
    stopifnot(is.character(string), is.character(words))
    spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
    vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
    
}
op = love[,7:26]
op = op %>% mutate(id = row_number())
names = c("vostok", "disign", "business", "politology", "math", "soc", "physics1", "philology", "economics", "law", 
          "vostok_m", "history", "math_mech", "politology_regions", "soc_sci", "media", "physics2", "economics_m", "law_m", "ling", "id")
colnames(op) = names
op[is.na(op)] <- 0
op$business = as.numeric(op$business)
op$politology = as.numeric(op$politology)
op$math = as.numeric(op$math)
op$soc = as.numeric(op$soc)
op$economics = as.numeric(op$economics)

# Google Sheet unique ID
ss <- "1_cJKLTjkLSyrWnXjtDc3Se3IqCSmmWGDosrQiNZM-z8"
ss1 = "1S1sxZ5eKPwE82yXJWuFw5WESaVz33BAEHwmhSNIP-RI"
# Communicate with Google Sheets
options(gargle_oauth_cache = ".secrets")
#gargle::gargle_oauth_cache()
drive_auth(cache = ".secrets", email = "aasmolyaninova@gmail.com")
gs4_auth(token = drive_token())

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(id = "inTabset",
                           theme = "superhero",
                           strong("Тиндер для менторов"),
                           tabPanel("Для начала опрос!",value = "panel1",  actionButton('jumpToP2', 'Заполнили анкету? Переходите к выбору!'),
                                    sidebarPanel(
                                        tags$h3("Расскажи немного о себе"),
                                        textInput("what_to_teach", "Опиши свои цели", ""),
                                        textInput("what_to_skill", "Какие профессиональные качества ты мог бы выделить в себе?", ""),
                                        radioButtons("edu", label = h3("Укажи свой уровень образования"),
                                                     choices = list("Бакалавриат" = "Бакалавриат", "Магистратура" = "Магистратура"), selected = 0),
                                        radioButtons("op1", label = h3("Укажи свою ОП"),
                                                     choices = list("Анализ данных в финансах" = "Анализ данных в финансах", "Востоковедение" = "Востоковедение", "Дизайн" = "Дизайн",
                                                                    "Востоковедение М" = "Востоковедение М", "История и археология" = "История и археология", 
                                                                    "Математика и механика" = "Математика и механика", "Медиапроизводство и медиааналитика" = "Медиапроизводство и медиааналитика",
                                                                    "Международный бизнес и менеджмент" = "Международный бизнес и менеджмент", "Политические науки и регионоведение" = "Политические науки и регионоведение", "Политология и мировая политика" = "Политология и мировая политика",
                                                                    "Прикладная математика и информатика" = "Прикладная математика и информатика", "Социальные науки" = "Социальные науки", 
                                                                    "Социология и социальная информатика" = "Социология и социальная информатика", "Физика...13" = "Физика...13", "Физика...23" = "Физика...23",
                                                                    "Филология" = "Филология", "Экономика" = "Экономика", "Экономика М" = "Экономика М", "Юриспруденция" = "Юриспруденция",
                                                                    "Юриспруденция М" = "Юриспруденция М", "Языкознание и литературоведение" = "Языкознание и литературоведение"), selected = 0),
                                        selectInput("channels", label = h3("В каком формате ты хочешь взаимодействовать с ментором?"), 
                                                    choices = list("Телефон" = "Телефон", "Facebook" = "Facebook", "WhatsApp" = "WhatsApp", "Telegram" = "Telegram", "Skype" = "Skype", "Личные общения" = "Личные общения", 
                                                                   "Zoom" = "Zoom", "VK" = "VK", "MS Teams" = "MS Teams", "Email" = "Email" ),
                                                    selected = 0, multiple = TRUE),
                                        selectInput("freq", label = h3("Как часто ты готов видеться с ментором?"),
                                                    choices = list("1 раз в месяц" = "1 раз в месяц", "Постоянно на связи" = "Постоянно на связи", "Чаще, чем 1 раз в месяц" = "Чаще, чем 1 раз в месяц",
                                                                   "1 раз в два месяца" = "1 раз в два месяца", "Обсуждается лично" = "Обсуждается лично", "По необходимости" = "По необходимости", "1 раз в квартал" = "1 раз в квартал"),
                                                    selected = 0, multiple = TRUE), 
                                        selectInput("formats", label = h3("Как бы ты хотел общаться с ментором?"),
                                                    choices = list("Консультации" = "Консультации", "Общение в мессенджерах" = "Общение в мессенджерах", "Стажировка" = "Стажировка",
                                                                   "Экскурсия в компанию" = "Экскурсия в компанию", "Вовлечение менти в проектную деятельность" = "Вовлечение менти в проектную деятельность",
                                                                   "Помощь в подготовке резюме/портфолио" = "Помощь в подготовке резюме/портфолио", "Видео-консультации по Skype" = "Видео-консультации по Skype",
                                                                   "Личное общение" = "Личное общение", "Установочная личная встреча" = "Установочная личная встреча", "Рекомендательное письмо от ментора по итогам программы" = 
                                                                       "Рекомендательное письмо от ментора по итогам программы", "Содействие в поиске научных и образовательный мероприятий (конференций, летних школ, выездных семинаров)" = 
                                                                       "Содействие в поиске научных и образовательный мероприятий (конференций, летних школ, выездных семинаров)", "«Один рабочий день ментора» - знакомство с профессиональной деятельностью ментора" =
                                                                       "«Один рабочий день ментора» - знакомство с профессиональной деятельностью ментора"),
                                                    selected = 0, multiple = TRUE), 
                                        
                                        
                                        
                                    ), # sidebarPanel
                                    mainPanel(
                                        h2("Что такое менторство в НИУ ВШЭ?"),
                                        h4("Менторство в Вышке— программа развития лидерского и профессионального потенциала студентов и выпускников через общение и сотрудничество.

Студенты получают поддержку и советы опытных коллег при выборе карьерной траектории и первого шага в профессиональной деятельности. Наставник не только передает свои навыки и делится опытом с молодым специалистом, но также помогает по-новому раскрыть его способности, перестроить образовательную и карьерную траектории, разработать план личного развития. Лучшими наставниками для студентов становятся именно выпускники, так как они близки студентам по духу и опыту обучения в Вышке."),
                                        br(), hr(), br(),
                                        h2("Зачем это нужно?"),
                                        h4("Менти получает не только доступ к знаниям человека, находящегося уже внутри отрасли, но и заряд мотивации и уверенности в своих силах, так необходимый всем, кто собирается строить карьеру или развивать собственный бизнес.

Участие в программе помогает студенту системно и осознанно выстраивать траекторию собственного развития и трудоустройства.

Ментор не только реализует социальную миссию, помогая встать на ноги новому поколению молодых профессионалов, но и сам вдохновляется новыми идеями, получает интересные инсайты от активных и умных студентов (реверсивное наставничество), собирает вокруг себя сети из единомышленников, получает доступ к лучшим кадрам для своей компании.

Программа наставничества для выпускника — вклад в развитие человеческого капитала в самом широком смысле.")
                                        
                                    ) # mainPanel
                                    
                           ), # Navbar 1, tabPanel
                           tabPanel("Tinder", "Пора выбирать!", value = "panel2", actionButton('jumpToP3', 'Уже выбрали? Оцените наше приложение!'),
                                    fluidPage(  
                                        shinyswiprUI(
                                            
                                            "acnh_swipe",
                                            class = "text-center",
                                            p(
                                                
                                                HTML("Dislike • Like"),
                                                
                                            ),
                                            fluidRow(
                                                column(3),
                                                column(
                                                    6,
                                                    column(
                                                        6,
                                                        h4("Имя:"),
                                                        textOutput("name"),
                                                        h4("Компания:"),
                                                        textOutput("company"),
                                                        h4("Должность:"),
                                                        textOutput("position"),
                                                    ),
                                                    column(
                                                        6,
                                                        h4("О себе:"),
                                                        textOutput("about_me")
                                                    ),
                                                ),
                                                column(3)
                                            )
                                            
                                        ),
                                        br(), hr(),
                                        
                                        h4("Top 10"),
                                        p("Эта таблица хранит информацию о выборе и собирает статистику по самым интересным менторам"),
                                        column(12, align = "center", tableOutput("table"))
                                    ),
                                    
                           ),
                           tabPanel("Оцени наше приложение (пожалуйста)", value = "panel3",
                                    sliderInput("slider1", label = h3("Что скажешь?"),  min = 0, 
                                                max = 10, value = 0), 
                                    br(), hr(),
                                    textInput("meh", "Также оставляйте идеи и пожелания для улучшения:)", ""),
                                    actionButton('jumpToP4', 'Отправить ответ!')
                           )
                           
                ) # navbarPage
) # fluidPage


# Define server function  

server <- function(input, output, session) {
    
    observeEvent(input$jumpToP2, {
        updateTabsetPanel(session, "inTabset",
                          selected = "panel2")
    })
    
    card_swipe <- callModule(shinyswipr, "acnh_swipe")
    
    
    # Sample one villager
    library(data.table)
    eventReactive(input$jumpToP2, {
        if(!is.null(input$channels) && !is.null(input$freq) && !is.null(input$formats) && !is.null(input$edu) && input$jumpToP2 >0){
            newrow = data.table(edu = as.character(input$edu),
                                channels=as.character(input$channels),
                                freq = as.character(input$freq), 
                                formats = as.character(input$formats),
                                id = as.numeric(137)
            )
            #data = as.data.frame(t(data))
            data <<- rbind(data, newrow)
        }
        data
    }, ignoreNULL = FALSE)
    
    
    eventReactive(input$jumpToP2, {
        if(!is.null(input$op1) && input$jumpToP2 >0){
            op =  op %>%    
                filter(
                    if (input$op1 == "Дизайн") {
                        op$disign <= 7 & op$disign != 0 
                    } else if(input$op1 =="Востоковедение"){
                        op$vostok <= 7 & op$vostok != 0 
                    } else if(input$op1 =="Международный бизнес и менеджмент"){
                        op$business <= 7 & op$business != 0 
                    } else if(input$op1 =="Политология и мировая политика"){
                        op$politology <= 7 & op$politology != 0 
                    } else if(input$op1 =="Прикладная математика и информатика"){
                        op$math <= 7 & op$math != 0 
                    } else if(input$op1 =="Социология и социальная информатика"){
                        op$soc <= 7 & op$soc != 0 
                    } else if(input$op1 =="Физика...13"){
                        op$physics1 <= 7 & op$physics1 != 0 
                    } else if(input$op1 =="Филология"){
                        op$philology <= 7 & op$philology != 0 
                    } else if(input$op1 =="Экономика"){
                        op$economics <= 7 & op$economics != 0 
                    } else if(input$op1 =="Юриспруденция"){
                        op$law <= 7 & op$law != 0 
                    } else if(input$op1 =="Востоковедение М"){
                        op$vostok_m <= 7 & op$vostok_m != 0 
                    } else if(input$op1 =="История и археология"){
                        op$history <= 7 & op$history != 0 
                    } else if(input$op1 =="Математика и механика"){
                        op$math_mech <= 7 & op$math_mech != 0 
                    } else if(input$op1 =="Политические науки и регионоведение"){
                        op$politology_regions <= 7 & op$politology_regions != 0 
                    } else if(input$op1 =="Социальные науки"){
                        op$soc_sci <= 7 & op$soc_sci != 0 
                    } else if(input$op1 =="Медиапроизводство и медиааналитика"){
                        op$media <= 7 & op$media != 0 
                    } else if(input$op1 =="Физика...23"){
                        op$physics2 <= 7 & op$physics2 != 0 
                    } else if(input$op1 =="Экономика М"){
                        op$economics_m <= 7 & op$economics_m != 0 
                    } else if(input$op1 =="Юриспруденция М"){
                        op$law_m <= 7 & op$law_m != 0 
                    } else{ op$ling <= 7 & op$ling != 0}
                )}
        op
    }, ignoreNULL = FALSE)
    
    
    data = data %>% left_join(op)
    
    data = data %>%  mutate(tel = sub(".*?(Телефон|$).*", "\\1", channels)) %>% 
        mutate(whatsapp = sub(".*?(WhatsApp|$).*", "\\1", channels)) %>% 
        mutate(telegram = sub(".*?(Telegram|$).*", "\\1", channels)) %>% 
        mutate(personal_com = sub(".*?(Личное общение|$).*", "\\1", channels)) %>% 
        mutate(skype = sub(".*?(Skype|$).*", "\\1", channels)) %>% 
        mutate(msteams = sub(".*?(MS Teams|$).*", "\\1", channels)) %>% 
        mutate(facebook = sub(".*?(Facebook|$).*", "\\1", channels)) %>% 
        mutate(vk = sub(".*?(VK|$).*", "\\1", channels)) %>% 
        mutate(zoom = sub(".*?(Zoom|$).*", "\\1", channels)) %>% 
        mutate(email = sub(".*?(Email|$).*", "\\1", channels)) %>% 
        mutate(two_month = sub(".*?(1 раз в два месяца|$).*", "\\1", freq)) %>% 
        mutate(every_month = sub(".*?(Чаще, чем 1 раз в месяц|$).*", "\\1", freq)) %>% 
        mutate(one_month = sub(".*?(1 раз в месяц|$).*", "\\1", freq)) %>% 
        mutate(smth_else = sub(".*?(1 раз в квартал|$).*", "\\1", freq)) %>% 
        
        mutate(consult = sub(".*?(Консультации|$).*", "\\1", formats)) %>% 
        mutate(pers_meet = sub(".*?(Установочная личная встреча|$).*", "\\1", formats)) %>% 
        mutate(networks = sub(".*?(общение в мессенджерах, социальных сетях|$).*", "\\1", formats)) %>% 
        mutate(personal_com2 = sub(".*?(Личное общение|$).*", "\\1", formats)) %>% 
        mutate(video = sub(".*?(Видео-консультации по Skype|$).*", "\\1", formats)) %>% 
        mutate(cv = sub(".*?(Помощь в подготовке резюме/портфолио|$).*", "\\1", formats)) %>% 
        mutate(project = sub(".*?(Вовлечение менти в проектную деятельность|$).*", "\\1", formats)) %>% 
        mutate(excursion = sub(".*?(Экскурсия в компанию|$).*", "\\1", formats)) %>% 
        mutate(internship = sub(".*?(Стажировка|$).*", "\\1", formats))  %>% 
        mutate(rec = sub(".*?(Рекомендательное письмо от ментора по итогам программы|$).*", "\\1", formats))  %>% 
        mutate(sci = sub(".*?(Содействие в поиске научных и образовательный мероприятий (конференций, летних школ, выездных семинаров)|$).*", "\\1", formats))  %>% 
        mutate(pro = sub(".*?(«Один рабочий день ментора» - знакомство с профессиональной деятельностью ментора|$).*", "\\1", formats))  %>% 
        
        mutate(bachelor = sub(".*?(бакалавриат|$).*", "\\1", edu)) %>% 
        mutate(mag = sub(".*?(магистратура|$).*", "\\1", edu)) 
    
    
    data$tel = if_else(data$tel == "", 0, 1)
    data$email = if_else(data$email == "", 0, 1)
    data$whatsapp = if_else(data$whatsapp == "", 0, 1)
    data$telegram = if_else(data$telegram == "", 0, 1)
    data$personal_com = if_else(data$personal_com == "", 0, 1)
    data$msteams = if_else(data$msteams == "", 0, 1)
    data$facebook = if_else(data$facebook == "", 0, 1)
    data$vk = if_else(data$vk == "", 0, 1)
    data$zoom = if_else(data$zoom == "", 0, 1)
    data$skype = if_else(data$skype == "", 0, 1)
    data$two_month = if_else(data$two_month == "", 0, 1)
    data$every_month = if_else(data$every_month == "", 0, 1)
    data$one_month = if_else(data$one_month == "", 0, 1)
    data$smth_else = if_else(data$smth_else == "", 0, 1)
    
    data$consult = if_else(data$consult == "", 0, 1)
    data$pers_meet = if_else(data$pers_meet == "", 0, 1)
    data$networks = if_else(data$networks == "", 0, 1)
    data$personal_com2 = if_else(data$personal_com2 == "", 0, 1)
    data$video = if_else(data$video == "", 0, 1)
    data$cv = if_else(data$cv == "", 0, 1)
    data$project = if_else(data$project == "", 0, 1)
    data$excursion = if_else(data$excursion == "", 0, 1)
    data$internship = if_else(data$internship == "", 0, 1)
    data$rec = if_else(data$rec == "", 0, 1)
    data$sci = if_else(data$sci == "", 0, 1)
    data$pro = if_else(data$pro == "", 0, 1)
    
    data$bachelor = if_else(data$bachelor == "", 0, 1)
    data$mag = if_else(data$mag == "", 0, 1)
    
    n = as.numeric(data[nrow(data),5])  
    rownames(data) = data$id
    data_spread = data %>% dplyr::select(-id,-formats, -freq, - channels, -edu) %>% as.matrix()
    sim = lsa::cosine(t(data_spread))
    diag(sim) = 0
    
    simCut = sim[,as.character(n)]
    mostSimilar = head(sort(sim[,n], decreasing = T), n = 20)
    mostSimilar = data.frame(similar = mostSimilar)
    mostSimilar$id = as.numeric(rownames(mostSimilar))
    
    
    eventReactive(input$jumpToP2, {
        if(input$what_to_teach != "" && input$jumpToP2 >0){
            what_to_teach <<- as.character(input$what_to_teach)
        }
        what_to_teach
    }, ignoreNULL = FALSE)
    
    
    what_to_teach = rm_words(what_to_teach, tm::stopwords("ru"))
    
    embeddings <- starspace_embedding(model, unique(anno$sentence), type = "document")
    embedding_sentence <- starspace_embedding(model, what_to_teach, type = "document")
    mostsimilar <- embedding_similarity(embeddings, embedding_sentence)
    mostsimilar = data.frame(mostsimilar)
    mostsimilar <- tibble::rownames_to_column(mostsimilar, "sentence")
    anno = anno %>%  select(doc_id, sentence_id, sentence) %>% distinct()
    mostsimilar = mostsimilar  %>% left_join(anno)
    mostsimilar$doc_id<-gsub("doc","",as.character(mostsimilar$doc_id))
    mostsimilar$doc_id = as.numeric(mostsimilar$doc_id)
    mostsimilar = mostsimilar[, 2:3]
    mostsimilar[,1][is.nan(mostsimilar[,1])] <- 0
    names(mostsimilar)[1] <- "result"
    mostsimilar =mostsimilar %>% group_by(doc_id) %>% top_n(1, result) %>%  distinct()
    mostsimilar_all = mostsimilar %>%  left_join(texts, by = c("doc_id" = "id"))
    
    output$txtout = renderText(what_to_teach)
    
    eventReactive(input$jumpToP2, {
        if(input$what_to_skill != "" && input$jumpToP2 >0){
            what_to_teach <<- as.character(input$what_to_skill)
        }
        what_to_teach
    }, ignoreNULL = FALSE)
    
    what_to_skill = rm_words(what_to_skill, tm::stopwords("ru"))
    embeddings1 <- starspace_embedding(model1, unique(anno1$sentence), type = "document")
    embedding_sentence1 <- starspace_embedding(model1, what_to_skill, type = "document")
    mostsimilar1 <- embedding_similarity(embeddings1, embedding_sentence1)
    mostsimilar1 = data.frame(mostsimilar1)
    mostsimilar1 <- tibble::rownames_to_column(mostsimilar1, "sentence")
    anno1 = anno1 %>%  select(doc_id, sentence_id, sentence) %>% distinct()
    mostsimilar1 = mostsimilar1  %>% left_join(anno1)
    mostsimilar1$doc_id<-gsub("doc","",as.character(mostsimilar1$doc_id))
    mostsimilar1$doc_id = as.numeric(mostsimilar1$doc_id)
    mostsimilar1 = mostsimilar1[, 2:3]
    mostsimilar1[,1][is.nan(mostsimilar1[,1])] <- 0
    names(mostsimilar1)[1] <- "result1"
    mostsimilar1 =mostsimilar1 %>% group_by(doc_id) %>% top_n(1, result1) %>%  distinct()
    mostsimilar_all = mostsimilar_all %>%  left_join(mostsimilar1)
    
    
    mostsimilar_all  = mostsimilar_all  %>% left_join(info, by = c( "doc_id" = "id")) 
    mostSimilar = mostSimilar %>% full_join(mostsimilar_all, by = c( "id" = "doc_id"))
    
    mostSimilar = mostSimilar %>% arrange(desc(similar, result, result1)) %>%  top_n( 10) %>% select(id, name, company, position, about_me)
    

    villager <- sample_n(mostSimilar, 1)
    
    
    # Render the villlager variables
    output$id <-         renderText({ villager$id })
    output$name <-        renderText({ villager$name })
    output$company <-     renderText({ villager$company })
    output$position <- renderText({ villager$position })
    output$about_me <-       renderText({ villager$about_me })
    
    # Render the table of swipes
    output$resultsTable <- renderDataTable({ appVals$swipes })
    
    # Set up reactive values object
    appVals <- reactiveValues(
        villager  = villager,
        swipes = data.frame(
            id = numeric(),
            name = character(),
            company = character(),
            position = character(),
            about_me = character(),
            swipe = character()
        )
    )
    
    observeEvent(card_swipe(), {
        
        # Record the last swipe result
        latest_result <- data.frame(
            name = appVals$villager$name,
            swipe = card_swipe()
        )
        
        # Send to Google Sheets
        date_col <- data.frame(date = Sys.time())  # capture datetime
        sheet_append(  # add a row to the sheet
            ss,  # the Google Sheet unique ID
            cbind(date_col, latest_result)
        )  
        
        # Add to table of all swipe results
        appVals$swipes <- rbind(latest_result, appVals$swipes)
        
        # Send results to the output
        output$resultsTable <- renderTable({ appVals$swipes })
        
        # Update the villager
        appVals$villager <- sample_n(mostSimilar, 1)
        
        # Send update to ui
        output$id <-         renderText({ appVals$villager$id })
        output$name <-        renderText({ appVals$villager$name })
        output$company <-     renderText({ appVals$villager$company })
        output$position <- renderText({ appVals$villager$position })
        output$about_me <-       renderText({ appVals$villager$about_me })
        
    }) # Close event observe.
    
    # Read latest data from the Google Sheet
    the_data <- eventReactive(
        { card_swipe() },
        read_sheet(ss) %>%
            count(name, swipe) %>%
            pivot_wider(names_from = swipe, values_from = n) %>% 
            replace_na(list(right = 0L, left = 0L)) %>% 
            arrange(desc(right), left) %>% 
            mutate(Rank = row_number()) %>% 
            select(
                Rank, Name = name, Liked = right, Disliked = left
            ) %>% 
            dplyr::filter(!is.na("Liked") & !is.na("Disliked")) %>% 
            slice(1:10),
        ignoreNULL = FALSE
    )
    
    # Render the latest results as a table
    output$table <- renderTable(the_data())
    observeEvent(input$jumpToP3, {
        updateTabsetPanel(session, "inTabset",
                          selected = "panel3")
        
        
    })
    
    
    observeEvent(input$jumpToP4 ,  {
        
        # Record the last swipe result
        latest_result1 <- data.frame(
            mark = input$slider1, 
            wish = input$meh
        )
        
        # Send to Google Sheets
        date_col <- data.frame(date = Sys.time())  # capture datetime
        sheet_append(  # add a row to the sheet
            ss1,  # the Google Sheet unique ID
            cbind(date_col, latest_result1)
        )  
        
        
    }) # Close event observe.
}



# Create Shiny object
shinyApp(ui = ui, server = server)
