library(tidyverse)
library(tidytext)
library(rjson)


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
############# outra forma de fazer ##################
 
 # extracted_dialogue <- tibble(dialogue = unlist(extracted_dialogue)) |> 
 #   mutate(personagem = str_extract(pattern = "^[^:]+",string = dialogue))
 
 ############# se estiver pesando pro seu pc, rode isto ##################
  
#rm(hpd) 

 
 ############### questao 1: personagem mais mencionado nos dialogos #############
 # Sample dialogue data (replace this with the actual dataset)

 # Counting mentions of each character
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
 # Common greetings
 greetings <- c("Hello", "Hi", "Greetings", "Hey")
 
 # Extracting greetings from dialogues
 greetings_found <- sapply(greetings, function(greet) {
   unlist(str_extract_all(dialogo_tb$dialogo, fixed(greet)))
 })
 
 # Displaying the results
 greetings_found
 
 
 #############questao 3: How many dialogues contain questions?#################
 # Counting dialogues that contain questions
 question_dialogues_count <- sum(str_detect(dialogo_tb$dialogo, fixed("?")))
 
 # Displaying the count
 question_dialogues_count/nrow(dialogo_tb)
 
 ##########questao 4: Which dialogue is the longest and who said it?############
 
 # Calculating the length of each dialogue
 dialogo_tb$length <- str_length(dialogo_tb$dialogo)
 
 # Identifying the longest dialogue
 longest_dialogue <- dialogo_tb %>% 
   arrange(desc(length)) %>%
   slice(1)
 
 # Displaying the result
 longest_dialogue |> View()
 
 ################## how many times hermione  vs ron mentions harry #######
 
 # Count ron's mentions of "harry"
 ron_mentions_potter <- sum(str_detect(
   dialogo_tb$dialogo[dialogo_tb$personagem == "Ron"], "Harry"))
 
 # Count Hermione's mentions of "Harry"
 hermione_mentions_harry <- sum(str_detect(
   dialogo_tb$dialogo[dialogo_tb$personagem == "Hermione"], "Harry"))
 
 # Displaying the results
 cat("Ron mentions 'Potter':", ron_mentions_potter, "times\n")
 cat("Hermione mentions 'Harry':", hermione_mentions_harry, "times")
 ##################################################################
 
 library(wordcloud2)
 library(magick)
 library(tm)
 
 # Exemplo de dados de texto
 texto <- dialogo_tb$dialogo[50:300]
 
 # Processar e criar uma tabela de frequência das palavras
 corpus <- Corpus(VectorSource(texto))
 dtm <- TermDocumentMatrix(corpus)
 matriz <- as.matrix(dtm)
 frequencia <- sort(rowSums(matriz), decreasing = TRUE)
 dados <- data.frame(word = names(frequencia), freq = frequencia) |> 
   anti_join(stop_words) |> 
   filter(word != "—")
 
 wordcloud2(dados,shape = 'star',size = 0.5)
 wordcloud2(dados, figPath = "img/raio.png", size = 0.5)
 
 # wordcloud2(dados, figPath = "img/raio.png", size = 3,
 #            widgetsize = c(1200,1200), ellipticity = .9, gridSize = 10,
 #            fontFamily = "Julee", backgroundColor = "white")
# dataframe
df <- data.frame(
  numbers = 1:5,
  letters = letters[1:5]
)

# tibble
tb <- tibble(
  numbers = 1:5,
  letters = letters[1:5]
)

df
tb

# repare que tibble traz de cara, mais informações que o dataframe, 
#ele também é mais ajustado as aplicações do tidyverse, se tornando menos inclinado a erros do que  o dataframe

#fonte: https://www.tidytextmining.com/tidytext

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text_df <- tibble(line = 1:length(text), text = text)

text_df

text_df %>%
  unnest_tokens(word, text)
################ mentions 

character_mentions <- sapply(unique(dialogo_tb$personagem), function(char) {
  sum(str_detect(dialogo_tb$dialogo, fixed(char)))
})

# Creating a data frame for the results
mentions_df <- data.frame(personagem = unique(dialogo_tb$personagem),
                          mentions = character_mentions) |> 
  filter(personagem != "hat")

# Displaying the results
mentions_df |> 
  arrange(desc(mentions)) |> 
  slice(1:20) |> 
  ggplot(aes(y =  reorder(charac, -mentions), x = mentions)) +
  geom_bar(stat = 'identity') +
  ggtitle('Character mentions')
