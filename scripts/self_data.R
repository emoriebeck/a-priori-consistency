library(dostats)
library(plyr)
library(tidyverse)

###Informant
####Load Data
old_cols <- c("participant ID", "talkative", "find fault", "thorough", "depressed", "original", "reserved", "helpful",
              "careless", "relaxed", "curious", "energy", "quarrels", "reliable", "be tense", "ingenious",
              "enthusiasm", "forgiving", "disorganized", "worries", "imagination", "quiet", "trusting",
              "lazy", "emotionally stable", "inventive", "assertive", "cold", "perseveres", "moody", 
              "aesthetic", "shy", "considerate", "efficient", "calm", "routine", "outgoing", "rude",
              "plans", "nervous", "reflect", "few artistic", "cooperate", "distracted", "sophisticated", 
              "positive", "negative", "happy", "self-esteem")

new_cols <- c(paste(rep(c("E", "A", "C", "N", "O"), times = 8), 
                    rep(seq(1,8,1), each = 5), sep = "_"), 
                    "O_9", "A_9", "C_9", "O_10")

## put the path to the files here and the rest should run
wd <- "~/Box/network/other projects/Correlated Change/data"
files <- list.files(sprintf("%s/target data/", wd))

codebook_fun <- function(file){
  print(file)
  # read in the data
  file <- paste(wd, "/target data/", file, sep = "")
  data_in <- read.csv(file, stringsAsFactors = F)
  # sid
  q_num     <- colnames(data_in)[grepl("participant ID", t(data_in[1,]))]
  item_text <- t(data_in[1,])[grepl("participant ID", t(data_in[1,]))]
  # in general items
  q_num     <- c(q_num, colnames(data_in)[grepl("in general", t(data_in[1,]))])
  item_text <- t(data_in[1,])[grepl("in general", t(data_in[1,]))]
  short_name <- str_remove_all(item_text, "Are you")
  short_name <- str_remove_all(short_name, "Do you experience ")
  short_name <- str_remove_all(short_name, " in general?")
  short_name <- str_remove_all(short_name, "someone who is ")
  short_name <- str_remove_all(short_name, " How would you rate your ")
  short_name <- str_remove_all(short_name, "[\\_\\t?]")
  short_name <- str_remove_all(short_name, "^ *|(?<= ) | *$")
  short_name <- word(short_name, 1)
  item_text <- c(t(data_in[1,])[grepl("participant ID", t(data_in[1,]))], item_text)
  # consistency items
  q_num     <- c(q_num, colnames(data_in)[grepl("consistent", t(data_in[1,]))])
  item_text <- c(item_text, t(data_in[1,])[grepl("consistent", t(data_in[1,]))])
  short_name <- c("SID", paste("general", short_name, sep = "_"), paste("consistent", short_name, sep = "_"))
  # BFI items
  for (j in old_cols[2:45]){
    q_num <- c(q_num, colnames(data_in)[grepl("I am someone who", t(data_in[1,])) & str_detect(t(data_in[1,]), j)])
    item_text <- c(item_text, t(data_in[1,])[grepl("I am someone who", t(data_in[1,])) & str_detect(t(data_in[1,]), j)])
  }
  short_name <- c(short_name, paste("trait", old_cols[2:45], sep = "_"))
  item_text <- str_remove_all(item_text, "^ *|(?<= ) | *$")
  item_text <- str_remove_all(item_text, "\\t")
  tibble(q_num = q_num, item_text = item_text, short_name = short_name)
}

data_all <- tibble(file = files[-1], month = str_sub(file, 1, 3)) %>%
  filter(!month %in% c("M09", "M13", "M17", "M21", "M25")) %>%
  mutate(codebook = map(file, codebook_fun))


for(i in 1:length(files)){
  file <- files[i]
  month <- str_sub(file, 1, 3)
  file <- paste(wd, "/target data/", file, sep = "")
  data_in <- read.csv(file, stringsAsFactors = F)
  col_nums <- rep(NA,length(old_cols))
  for (j in 1:length(old_cols)){
      col_num <- which((grepl("am someone who", t(data_in[1,])) & str_detect(t(data_in[1,]), old_cols[j])) |
                            (str_detect(t(data_in[1,]), old_cols[j])) |
                            (grepl("How long have you known", t(data_in[1,])) & str_detect(t(data_in[1,]), old_cols[j])))
      col_nums[j] <- ifelse(length(col_num) != 0, col_num, NA)
  }
  col_nums <- tibble(item = old_cols, num = col_nums) %>%
    filter(!is.na(col_nums)) %>%
    mutate(item = gsub("[ ]","", item))
  data_in <- tbl_df(data_in[-c(1:2),]) %>%
    select(as.vector(col_nums$num)) %>%
    setNames(c("SID", col_nums$item[-1])) %>%
    mutate(wave = month) 
  if(i == 1){target_data <- data_in}
  else{target_data <- full_join(data_in, target_data)}
}


target_data <- target_data %>%
  filter(!str_detect(SID, "[a-z A-z]") & !(SID %in% c("", "0", "0000", "1212")))

####Clean Data
rev_code <- c("E_2", "E_5", "E_7", "A_1","A_3","A_6","A_8",
              "C_2","C_4","C_5","C_9", "N_2","N_5","N_7",
              "O_7", "O_9")

target_data$NA_num <- apply(target_data[,2:45], 1, function(x) sum(is.na(as.numeric(x))))

target_data <- target_data %>% filter(NA_num != 44) %>%
  mutate_at(vars(talkative:Howmuchdoyoulike), funs(as.numeric(as.character(.)))) %>%
  gather(key = item, value = value, talkative:sophisticated) %>%
  mutate(item = factor(mapvalues(item, from = gsub("[ ]", "", old_cols[2:45]), 
                                 to = new_cols, warn_missing = F), levels = new_cols),
         wave = mapvalues(wave, from = c("M01", "M05", "M09", "M13", "M17", "M21", "M25"),
                          to = paste("T", seq(1,7,1), sep = ""), warn_missing = F)) %>%
  select(-NA_num)

## save a file with only personality data
write.csv(inf_data, "~/Downloads/target_sevenwaves.csv", row.names = F)
