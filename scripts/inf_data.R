###Informant
####Load Data
old_cols <- c("talkative", "find fault", "thorough", "depressed", "original", "reserved", "helpful",
              "careless", "relaxed", "curious", "energy", "quarrels", "reliable", "tense", "ingenious",
              "enthusiasm", "forgiving", "disorganized", "worries", "imagination", "quiet", "trusting",
              "lazy", "emotionally stable", "inventive", "assertive", "cold", "perseveres", "moody", 
              "aesthetic", "shy", "considerate", "efficient", "calm", "routine", "outgoing", "rude",
              "plans", "nervous", "reflect", "few artistic", "cooperate", "distracted", "sophisticated",
              
              "Years", "Months", "How close", "How important", "How satisfied", "How much do you like")
## put the path to the files here and the rest should run
wd <- "~/Box/network/other projects/Correlated Change/data"
files <- list.files(sprintf("%s/informant data/", wd))


for(i in 1:length(files)){
  file <- files[i]
  month <- str_sub(file, 1, 3)
  file <- paste(wd, "/informant data/", file, sep = "")
  data_in <- read.csv(file, stringsAsFactors = F)
  col_nums <- rep(NA,length(old_cols))
  for (j in 1:length(old_cols)){
      col_num <- which((grepl("is someone who", t(data_in[1,])) & str_detect(t(data_in[1,]), old_cols[j])) |
                            (str_detect(t(data_in[1,]), old_cols[j])) |
                            (grepl("How long have you known", t(data_in[1,])) & str_detect(t(data_in[1,]), old_cols[j])))
      col_nums[j] <- ifelse(length(col_num) != 0, col_num, NA)
  }
  col_nums <- tibble(item = old_cols, num = col_nums) %>%
    filter(!is.na(col_nums)) %>%
    mutate(item = gsub("[ ]","", item))
  data_in <- tbl_df(data_in[-c(1:2),]) %>%
    select(17, as.vector(col_nums$num)) %>%
    setNames(c("SID", col_nums$item)) %>%
    mutate(wave = month) 
  if(i == 1){inf_data <- data_in}
  else{inf_data <- full_join(data_in, inf_data)}
}


####Clean Data
remove <- c("sara", "CXZ", "vcx", "hgf", "fsd", "agjeffrey@wustl.edu","99999", "gf", "xfc", "fdfdfs",
            "ASDF", "https://wustlpsych.qualtrics.com/SE/?SID=SV_1FyiNxQLoYOB1tj", "ew", "fds", "adf", 
            "aggregate", "516928410@qq.com", "Keegan Hughes", "habanah1@gmail.com", "Mira Hanfling", "wd",
            "ted", "Vehicula proin at. Rhoncus magna vel ipsum, risus turpis sit blandit, quam elit eu, venenatis.",
            "Ante ridiculus venenatis orci lorem eu non sapien, bibendum ut urna ridiculus.",
            "kb", "jaf;kjoq;wiejopt", "ktest", "lkjl", "sara gi test", "sdfa", "delete", ",jkl",
            "boo", "dfdfg", "ljk", "sdf", "dsf", "sara bf test", "david.oei@oeidesign.com", 
            "katie.glazebrook@gmail.com", "https://wustlpsych.az1.qualtrics.com/SE/?SID=SV_0IFY5hwSWtUxGSx",
            "Margaret McNally", "test20140903", "Test on 9/3/14", "id", "zoom", "2121", "dfg", 
            "Lucas Boatwright - \"I accidentally filled out the first set of questions in my survey wrong. They were supposed to be about me, but I filled them in as if they were about Jo. By the time I realized, I couldn't go back. I believe it was just the first set of questions, though. Please disregard them.\"", "test", "test_Seth", "0000","Test 9/3/14",
            "testrp20140903", "dfg", "zoom", "id", "2121", "cbvxcvx", "9999", "marissanodiff@sbclobal.net", "Corey clark", "lkj", "practic", "petar_salakov@yahoo.com", "trerth",
            "test_delete", "TEST", "Test", "sdfsdf", "sdfasf", "sadf", "saasasas", "sa", "ry", "practice1",
            "lkjl'", "f", "dsaa", "cbvxcvxcv","asdf", "9999999", "999999", "pauline.lkw@gmail.com", "Katericart", "")

rev_code <- c("E_2", "E_5", "E_7", "A_1","A_3","A_6","A_8",
              "C_2","C_4","C_5","C_9", "N_2","N_5","N_7",
              "O_7", "O_9")

inf_data$NA_num <- apply(inf_data[,2:45], 1, function(x) sum(is.na(as.numeric(x))))

inf_data <- inf_data %>% filter(!SID %in% remove & NA_num != 44) %>%
  mutate(SID = gsub("[ ]", "", SID)) %>%
  separate(SID, c("SID", "inf"), -2) %>%
  group_by(SID, inf) %>%
  mutate(freq = n()) %>%
  ungroup() %>%
  group_by(SID) %>%
  mutate(rank = dense_rank(desc(freq))) %>%
  ungroup() %>%
  mutate_at(vars(talkative:Howmuchdoyoulike), funs(as.numeric(as.character(.)))) %>%
  filter(!(SID %in% c("", "0"))) %>%
  mutate(wave = mapvalues(wave, from = c("M01", "M05", "M09", "M13", "M17", "M21", "M25"),
                          to = paste("T", seq(1,7,1), sep = ""), warn_missing = F)) %>%
  filter(!(SID %in% remove) & NA_num != 44)

## save a file with only personality data
write.csv(inf_data, "~/Downloads/inf_sevenwaves.csv", row.names = F)