# Challenge 9
# P. Kessling, 29.04.2021

# Heute wollen wir das Kommunikationsverhalten von Twitter-User*innen zu einem
# bestimmtem Thema untersuchen. Suche dafür ein aktuelles Thema und nutze die Twitter-API.
# [+] Durchsuche Twitter nach dem Thema, beachte dabei, dass wir zu mindestens 1500 Tweet
# brauchen.
# [+]Für die einhundert aktivsten Nutzer bestimmen wir das Kommunikations verhalten genauer.
# Lade für diese Nutzer jeweils die letzten 1000 Tweets, falls so viele vorhanden sind
# und Werte visuell nach der Zeit aus.
# ACHTUNG: das Laden der Daten kann eine ganze Weile in Anspruch nehmen.
# *[+] Erstelle eine Visualisierung des gesamten Kommunikatinosaufkommen, aufgelöst in 5 Minuten-Intervallen.
# *[+] Erstelle eine Visualisierung der aktivsten Nutzer.
# *[+] Erstelle eine Visualisierung der von diesen Nutzern am häufigsten genutzen Hashtags.

library(rtweet)
library(dplyr)
library(ggplot2)
library(stopwords)
library(plotly)
install.packages("ggwordcloud")
library(ggwordcloud)

ha <- search_tweets("gendern", n=1500)

write_rds(ha, "ha.rds")


token<-get_token()

end <- ha %>% 
  group_by(screen_name) %>% 
  summarise(gesamt=n()) %>% 
  arrange(desc(gesamt)) %>% 
  head(100)

time <- get_timeline(end$screen_name, n=1000, token=token, retryonratelimit=T)


write_rds(time, "time.rds")

# Visualisierung des Kommunikationsaufkommens in 5 min. Intervallen 
#vom 15.05. 10:45 bis 16.05 14:51 = 28.06 
#das Ergebnis ist sehr interessant, denn es ist deutlich auf dem Graphen zu erkennen, 
#dass die Nutzer zum Thema 'gendern' viel mehr eigene Gedanken twittern, als andere User zitieren, 
#die zu diesem Thema was geschrieben haben. 

ggplot(ha) +
  geom_freqpoly(aes(created_at, color = is_quote), position = "stack", bins = 336.72)+ 
  scale_x_datetime(date_breaks = "4 hours")+ 
  labs(
    x = "Zeit der Veröffentlichung",
    y = "Tweets Gesamtanzahl",
    color = "Wurde zitiert",
    title = "Gesamter Kommunikationsaufkommen zum Thema 'gendern'"
  )

#gezogene Tweets nach meinem Thema (gendern) filtern 


filtert_tweets <- time %>% 
  filter(str_detect(text, paste ("gendern")))

#Visualisierung der aktivsten Nutzer
active <- filtert_tweets %>% 
  group_by(screen_name) %>% 
  summarise(tweet_gesamt_pro_user=n()) %>% 
  arrange(desc(tweet_gesamt_pro_user)) %>% 
  head(10)


plot_ly(data=active, x=~reorder(screen_name, -tweet_gesamt_pro_user),
        marker=list(color=toRGB("#B88FC6")), y=~tweet_gesamt_pro_user,type="bar") %>% 
  layout(title="10 aktivsten Nutzer", xaxis=list(title="Name der Twitter-Nutzer"), yaxis=list(title="Tweeetsanzahl"))


# nach den Hashtags filtern 

hashtag_ <- "#[a-zA-Z0-9_-ー\\.]+"
hashtag <- str_extract_all(filtert_tweets$text, hashtag_)


hashtag_word <- unlist(hashtag)

hashtag_word

df<- data.frame(hashtag_word)
df<- df %>% 
  group_by(hashtag_word) %>% 
  summarise(anzahl=n()) %>% 
  arrange(desc(anzahl)) 
  
b <-df %>% 
  head(20) %>% 
  ggplot(aes(label=hashtag_word, size=anzahl))+
  geom_text_wordcloud(shape="diamond")+
  theme_light()+
  scale_size_area(max_size = 20)

b

