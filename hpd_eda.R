library(tidyverse)
library(tidytext)
library(rjson)
library(wordcloud2)
library(magick)
library(tm)

hpd <- fromJSON(file="en_train_set.json")

hpd[1:2]
class(hpd)
hpd[[1]]$dialogue
hpd[[1]]$position

#map + pluck
extracted_dialogue <- map(hpd, pluck, "dialogue")

#queremos session, locutores e dialogo
session_names <- rep(names(extracted_dialogue), 
                     times = sapply(extracted_dialogue, length))

extracted_dialogue$`Session-1`

 str_split_fixed(string = tibble(
                          dialogue = unlist(extracted_dialogue)
                          )$dialogue,
                 pattern = ':',n=2) |> head()

 #memo: fazer todos os códigos passo a passo
 # explicando porque usamos str_trim
 
 dialogo_tb <- str_split_fixed(string = tibble(
   dialogue = unlist(extracted_dialogue)
 )$dialogue,
 pattern = ':',n=2) |> 
   as_tibble() |> 
   mutate(session = session_names,
          V1 = str_trim(V1)) |> 
   select(session, personagem = V1, dialogo = V2)
 
 dialogo_tb |> 
   filter(stringr::str_detect(dialogo,'\\d')) 
 
 dialogo_tb |> 
   filter(stringr::str_detect(dialogo,'[0-9]')) 
############# outra forma de fazer ##################
 
 # extracted_dialogue <- tibble(dialogue = unlist(extracted_dialogue)) |> 
 #   mutate(personagem = str_extract(pattern = "^[^:]+",string = dialogue))
 
 ############# se estiver pesando pro seu pc, rode isto ##################
  
#rm(hpd) 

 
 ############### questao 1: personagem mais mencionado nos dialogos #############



 character_mentions <- sapply( #aplica função em cada elemento
   unique(dialogo_tb$personagem), #lista person. únicos
   function(char) { #func que detecta mencoes a personagens em cada dialogo
   sum(
     str_detect(dialogo_tb$dialogo, fixed(char)) # fixed() = correspondencia exata
     )
 }
 )
 
 # Creating a data frame for the results
 mentions_df <- data.frame(character = unique(dialogo_tb$personagem),
                           mentions = character_mentions)
 
 # Displaying the results
 mentions_df |> 
   arrange(desc(mentions)) |> 
   slice(1:20) |> 
   ggplot(aes(y = character, x = mentions)) +
   geom_bar(stat = 'identity')
 
 ###################### questao 2: most frequent greetings #####################

 greetings <- c("Hello", "Hi", "Greetings", "Hey")
 

 greetings_found <- sapply(greetings, function(greet) {
   unlist(str_extract_all(dialogo_tb$dialogo, fixed(greet)))
 })
 

 greetings_found
 
 
 #############questao 3: How many dialogues contain questions?#################

 question_dialogues_count <- sum(str_detect(dialogo_tb$dialogo, fixed("?")))
 

 question_dialogues_count/nrow(dialogo_tb)
 
 ##########questao 4: Which dialogue is the longest and who said it?############
 

 dialogo_tb$length <- str_length(dialogo_tb$dialogo)
 

 longest_dialogue <- dialogo_tb |> 
   arrange(desc(length)) |>
   slice(1)
 
 longest_dialogue |> View()
 
 ################## how many times hermione  vs ron mentions harry #######
 

 ron_mentions_potter <- sum(str_detect(
   dialogo_tb$dialogo[dialogo_tb$personagem == "Ron"], "Harry"))
 

 hermione_mentions_harry <- sum(str_detect(
   dialogo_tb$dialogo[dialogo_tb$personagem == "Hermione"], "Harry"))
 

 cat("Ron mentions 'Potter':", ron_mentions_potter, "times\n")
 cat("Hermione mentions 'Harry':", hermione_mentions_harry, "times")
 ##################################################################
 
################ mencoes 

character_mentions <- sapply(unique(dialogo_tb$personagem), function(char) {
  sum(str_detect(dialogo_tb$dialogo, fixed(char)))
})


mentions_df <- data.frame(personagem = unique(dialogo_tb$personagem),
                          mentions = character_mentions) |> 
  filter(personagem != "hat")


mentions_df |> 
  arrange(desc(mentions)) |> 
  slice(1:20) |> 
  ggplot(aes(y = reorder(character, +mentions), x = mentions)) +
  geom_bar(stat = 'identity') +
  ggtitle('Character mentions')


############ wordcloud ###########
#https://excessivepepper.com/?p=94
# uma das fontes
#exemplo em python:https://amueller.github.io/word_cloud/auto_examples/masked.html


# Exemplo de dados de texto
texto <- dialogo_tb$dialogo[50:300]

# Processar e criar uma tabela de frequência das palavras
corpus <- Corpus(VectorSource(texto))

# fazer pre processamentos, como por exemplo
to_space <- content_transformer(function(x, pattern)
{ 
  return (gsub(pattern, " ", x))
}
)
# exemplo, remover simbolos indesejados
docs <- tm_map(corpus, to_space, "’re")

dtm <- TermDocumentMatrix(corpus)
inspect(dtm[1:10,])
matriz <- as.matrix(dtm)

frequencia <- sort(rowSums(matriz), decreasing = TRUE)
dados <- data.frame(word = names(frequencia), freq = frequencia) |> 
  anti_join(stop_words) |> 
  filter(word != "—")

wordcloud2(dados,shape = 'star',size = 0.5)
wordcloud2(dados, figPath = "img/Thunderbolt.jpg", size = 0.5)

##############################
# adicionando outra forma de tratar os dados
Bing <- get_sentiments("bing")


tm <- Corpus(VectorSource(dialogo_tb$dialogo))
tm <- tm_map(tm, content_transformer(tolower))
tm <- tm_map(tm, removeNumbers)
tm <- tm_map(tm, removeWords, stopwords("english"))
tm <- tm_map(tm, removePunctuation)
tm <- tm_map(tm, stripWhitespace)
tdm <- TermDocumentMatrix(tm)

tdm <- as.matrix(tdm)
tdm <- sort(rowSums(tdm), decreasing = T)
tdm <- data.frame(Word = names(tdm), Number = tdm)


remover <- c("’s", "’ve", "’re", "’ll","—")
gsub("’", "", head(tdm$Word))
gsub(".*’.*", "", head(tdm$Word))

tdm <- tdm |> 
  filter(!str_detect("’",Word)) |> 
  filter(!str_detect("—",Word))  
wordcloud2(tdm,shape = 'star',size = 1)


dialogo_tb |>
  unnest_tokens(output = word, input = dialogo, token = "ngrams", n = 2) |>
  filter(is.na(word)==F) |>
  separate(word, c("word1", "word2"), sep = " ") |> 
  filter(!word1 %in% c("on", "in", "the", "be", "are", "i", "you", "is", "to", "a", "has", "of", "it", "he", "was", "it's")) |>
  filter(!word2 %in% c("on", "in", "the", "be", "are", "i", "you", "is", "to", "a", "has", "of", "it", "he", "was", "it's")) |> 
  unite(word,word1, word2, sep = " ") |> 
  count(word, sort = T) |> 
  slice(1:15) |>
  ggplot(., aes(reorder(word, +n), n))+
  geom_bar(stat = "identity", width = 0.65, fill = "#a1d76a", alpha = 0.85)+
  coord_flip()+
  labs(title = "top 15 bigramas mais populares",
       subtitle = "Top 15",
       x = "Bigram content", y = "Frequency", 
       caption = "© feito por Ían Muliterno e inspirado por Michau96/Kaggle")+
  theme_minimal()



Sentiment <- dialogo_tb |>
  unnest_tokens(output = word, input = dialogo) |>
  left_join(Bing, "word") |>
  filter(is.na(sentiment)==F)

Sentiment |> 
  group_by(word, sentiment) |>
  summarise(count = n(), .groups = 'drop') |>
  arrange(desc(count)) |>
  slice(1:20) |>
  ggplot(., aes(reorder(word, +count), count, fill = sentiment))+
  geom_bar(stat = "identity", width = 0.62, alpha = 0.9)+
  scale_fill_brewer(palette = "Set1")+
  coord_flip()+
  labs(title = "Most popular words with assigned sentiment",
       subtitle = "Top 20",
       x = "Word", y = "Frequency", fill = "Sentiment",
       caption = "© Feito por Ían, inspirado por Michau96/Kaggle")+
  guides(fill = guide_legend(reverse = T))+
  theme_minimal()+
  theme(legend.title.align = 0.5, legend.position = "right", legend.direction = "vertical")



Sentiment |> 
  filter(personagem %in% c("Harry", "Ron", "Hermione", "Hagrid", "Dumbledore", "Lupin", "McGonagall", "Malfoy", "George",
                          "Snape", "Mrs. Weasley", "Tom", "Sirius", "Dobby"))  |>
  group_by(personagem, sentiment) |>
  summarise(count = n(), .groups = 'drop') |>
  ggplot(., aes(personagem, count, fill = sentiment))+
  geom_bar(stat = "identity", position = "fill", width = 0.57, alpha = 0.9)+
  scale_x_discrete(limits = c("Dobby", "Sirius", "Tom", "Mrs. Weasley", "Snape", "George", "Malfoy", 
                              "McGonagall", "Lupin", "Dumbledore", "Hagrid",  "Hermione", "Ron", "Harry"))+
  scale_fill_brewer(palette = "Set1")+
  scale_y_continuous(labels = scales::percent)+
  coord_flip()+
  labs(title = "proporção de palavras com sentimento positivo e negativo",
       subtitle = "por personagem", fill = "Sentiment",
       x = "Character", y = "Ratio",
       caption = "© Feito por Ían inspirado por Michau96/Kaggle")+
  guides(fill = guide_legend(reverse = T))+
  theme_minimal()+
  theme(legend.title.align = 0.5, legend.position = "right", legend.direction = "vertical")
