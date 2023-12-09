letras <- c('abc','abcde','xyz')

str_view(letras,'a')
str_view(letras,'5')

acentos <- c(
  'eu nao curto nao.',
  'eu acho inconcebível não gostar de acentos!'
)

str_view(acentos,'não')
str_view(acentos,'n.o')
str_view(acentos,'n[aã]o')

str_match(acentos,'n[aã]o') |> class()
str_match_all(acentos,'n[aã]o')

match_nao <- str_match_all(acentos,'n[aã]o')

str_view(acentos,'\\D')

str_view('www.estesite.com.br','\\.')
str_view('www.estesite.com.br','[.]')

frase <- c('gosto do numero 42 e 24',
           "eu tenho 29 anos")

str_match_all(frase,'\\d')
str_match_all(frase,"\\d+")
str_match_all(frase,'\\D+')

str_match_all(frase,"\\w+")
#exercicio 1
str_match_all(frase,"\\W+")


unlist(str_split("Eu amo programar em R!", "\\W+"))


nome_usuario <- " "

if(
  
    str_detect(
      nome_usuario,'\\W+'
    )
    
){
  stop('por favor insira seu nome')
}


fruit <- c('apple','blueberry','apricot','banana')
str_match(fruit,'.+')
str_match(fruit,'ap.+')
str_match(fruit,'[A-Z].+')

fruit <- c('apple','blueberry','apricOt','Banana')

str_match(fruit,'[A-Z].+')

empty <- ""
str_match(empty,".+") #quantificador um ou mais
str_match(empty,".*") # quantificador zero ou mais

grape_things = c("uva", "uvafruta", "uvavinhedo", "uva oleo")
str_match(grape_things,'uva.+')
str_match(grape_things,'uva.*')

#exercicio 3
str_view("organizeeeeee, organização, organiz", "organiz(e|ação)?")
str_view("organizeeeeee, organização, organiz", "organiz(e|ação)+")
str_view("organizeeeeee, organização, organiz", "organiz(e|ação)*")

cat_things = c("caterpillar", "catapult", "cattle", "house cat","cat")
str_match(cat_things,"^cat.*")
str_match(cat_things,".*cat")
str_match(cat_things,".*cat$")

coisas_a_capturar <- c("macarrao, maca, item3")
str_match(coisas_a_capturar,"(\\w*)\\W*")
str_match(coisas_a_capturar,"(\\w*)\\W*(\\w*)\\W*")
str_match(coisas_a_capturar,"(\\w*)\\W*(\\w*)\\W*(\\w*)")

str_match_all(coisas_a_capturar,'\\w*')



pattern = '.*(\\d{3}).*(\\d{3}).*(\\d{4})'

phone_numbers = c(
  "(541) 471 3918",
  "(603)281-0308",
  "Home: 425-707-7220",
  "(814)-462-8074",
  "9704443106",
  "I don't have a phone."
)

str_match(phone_numbers,pattern)


library(tidyverse)
library(tidytext)
library(rjson)
library(wordcloud2)
library(tm)


hpd <- fromJSON(file = "en_train_set.json")
hpd[1:2]
class(hpd)
hpd[[1]]$dialogue
hpd[[1]]$position

extracted_dialogue <- map(hpd,pluck,"dialogue")
extracted_dialogue[1:2]

session_names <- rep(names(extracted_dialogue),
                     times = sapply(extracted_dialogue, length))

session_names[1:10]

tibble_hp <- tibble(dialogue = unlist(extracted_dialogue))

extracted_dialogue$`Session-1`

str_split_fixed(tibble_hp$dialogue,pattern = ":" ,n=2) |> head()

class(str_split_fixed(tibble_hp$dialogue,pattern = ":" ,n=2))

dialogo_tb <- str_split_fixed(tibble_hp$dialogue,
                              pattern = ":" ,
                              n=2) |> 
  as_tibble() |> 
  mutate(session = session_names,
         personagem = str_trim(V1)) |> 
  select(session, personagem, dialogo = V2)


##  questao 1HPD: personagem mais mencionado nos dialogos

char_mentions <- sapply(unique(dialogo_tb$personagem),
                        function(char){
                          sum(
                          str_detect(dialogo_tb$dialogo,
                                     fixed(char)
                                     ))
                        })


mentions_df <- data.frame(personagem = unique(dialogo_tb$personagem),
                          mentions = char_mentions)


mentions_df |> 
  arrange(desc(mentions)) |> 
  filter(personagem != "hat") |> 
  slice(1:20) |> 
  ggplot(aes(y= reorder(personagem,+mentions), x = mentions)) +
  geom_bar(stat = "identity")


## questao 4HPD: Quem mais menciona Harry, o Ron ou a Hermione?

dialogos_do_Ron <- dialogo_tb |> 
  filter(personagem == "Ron")

Ron_fala_do_harry <- sum(str_detect(dialogos_do_Ron$dialogo,
                                "Harry"))

Ron_fala_do_harry <- sum(str_detect(dialogos_do_Ron$dialogo,
                                    "Harry"))

dialogos_do_Hermione <- dialogo_tb |> 
  filter(personagem == "Hermione")

Hermione_fala_do_harry <- sum(str_detect(dialogos_do_Hermione$dialogo,
                                         "Harry"))


texto <- dialogo_tb$dialogo[50:400]


corpus <- Corpus(VectorSource(texto))
dtm <- TermDocumentMatrix(corpus)
matriz <- as.matrix(dtm)
frequencia <- sort(rowSums(matriz), decreasing = T)
dados <- data.frame(word = names(frequencia),
                    freq = frequencia) |> 
  anti_join(stop_words, by = 'word') |> 
  filter(word != "—")

wordcloud2(dados,shape = "cardioid", size = 0.5)

Bing <- get_sentiments("bing")

Sentiment <- dialogo_tb |> 
  unnest_tokens(output =  word, input = dialogo) |> 
  left_join(Bing,'word') |> 
  filter(is.na(sentiment) == F)


Sentiment |> 
  group_by(word,sentiment) |> 
  summarise(count = n(), .groups = 'drop') |> 
  arrange(desc(count)) |> 
  slice(1:20) |> 
  ggplot(aes(y = reorder(word, +count), x = count, fill = sentiment)) +
  geom_bar(stat = "identity",width = 0.62, alpha = 0.7)


###############

dialogo_tb |> 
  unnest_tokens(output = word, input = dialogo, token = "ngrams", n = 2) |> 
  filter(is.na(word) == F) |> 
  separate(word, c('word1','word2'), sep = " ") |> 
  filter(!word1 %in% unique(stop_words$word)) |> 
  filter(!word2 %in% unique(stop_words$word)) |> 
  unite(word, word1, word2, sep = " ") |> 
  count(word, sort = T) |> 
  slice(1:15) |>
  ggplot(aes(reorder(word,+n),n))+
  geom_bar(stat = 'identity',width = 0.62, alpha = 0.7)+
  coord_flip() 

  
  
filtro1 <-   dialogo_tb |> 
  unnest_tokens(output = word, input = dialogo, token = "ngrams", n = 2) |> 
  filter(is.na(word) == F) |> 
  separate(word, c('word1','word2'), sep = " ") |> 
  filter(!word1 %in% unique(stop_words$word)) |> 
  filter(!word2 %in% unique(stop_words$word))


filtro2 <- dialogo_tb |> 
  unnest_tokens(output = word, input = dialogo, token = "ngrams", n = 2) |> 
  filter(is.na(word) == F) |> 
  separate(word, c('word1','word2'), sep = " ") |> 
  anti_join(stop_words, c('word1' = 'word')) |> 
  anti_join(stop_words,c('word2' = 'word'))


all.equal(filtro1,filtro2)
