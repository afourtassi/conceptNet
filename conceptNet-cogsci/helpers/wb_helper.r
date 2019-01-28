# This file contains functions used to get item data, admin data, etc. , and generate dataframes for analysis
import_all_library<- function(){
  library(tidyr)
  library(purrr)
  library(readr)
  library(ggplot2)
  library(langcog)
  library(boot)
  library(lazyeval)
  library(dplyr)
  library(wordbankr)
  library(directlabels)
  library(scales)
  library(stringr)
  library(lmtest)
}
######################################################################################################################
get_lang_item_data <- function(lang, lang_form = "WS", lex_class = "nouns") {

  item_data <- get_item_data(language = lang, form = lang_form) %>%
    select(num_item_id, definition, type, lexical_class, uni_lemma, category) %>%
    filter(type == "word", lexical_class == lex_class) %>%
    rename(item = num_item_id)
  
  #Initialize every item as NA (NOT yet learnt)
  lang_item <- item_data %>%
    select(item, definition, uni_lemma, category) %>%
    mutate(age = NA)
  
  return(lang_item)
}

######################################################################################################################
# get only "useful" admin data (where production is not NA)
get_lang_admin_data <- function(lang, lang_form = "WS") {
  #get the kids' data_id from every age
  admin_data <- get_administration_data() %>%
    ## get_administration_data() %>% filter(form == "WS", !is.na(production), language == "German") %>% .$source_name %>% unique()
    filter(form == lang_form, !is.na(production), language == lang) %>%
    select(data_id, age) %>%
    arrange(age)
  
  return(admin_data)
}

######################################################################################################################
# See how many kids we have in every age
get_kids_by_age <- function(admin_data) {
  #get number of kids by age
  nkids_by_age <- admin_data %>%
    group_by(age) %>%
    summarise(n = n())
  return(nkids_by_age)
}

######################################################################################################################
#get "produces" instrument data 
get_lang_instr_data <- function(lang, lang_form = "WS") {
  instr_data <- get_instrument_data(language = lang,
                                    form = lang_form) %>%
    filter(value == "produces") %>%
    arrange(num_item_id) %>%
    rename(item = num_item_id)
  
  return(instr_data)
}

######################################################################################################################
#calculate the age of acquisition
get_lang_aoa <- function(item_data, admin_data, instr_data) {
  nkids_by_age <- get_kids_by_age(admin_data)
  ages<- nkids_by_age$age
  
  for (cur_age in ages) {
    rem_item <- item_data %>% filter(is.na(age))
    current_age_id <- admin_data %>% filter(age == cur_age)
    current_instr <- instr_data %>% 
      filter(data_id %in% current_age_id$data_id)
    for (w in rem_item$item) {
      proportion <- sum(current_instr$item == w) / nkids_by_age$n[which(ages==cur_age)]
      if (proportion >= 0.5) {
        item_data$age[which(item_data$item == w)] = cur_age
      }
    }
  }
  word_aoa<- item_data %>% filter(!is.na(age))
  return(word_aoa)
}

######################################################################################################################

#check how many words are learnt each age
get_nwords_by_age <- function(word_aoa) {
  nwords_by_age <- word_aoa %>%
    arrange(age) %>%
    group_by(age) %>%
    summarise(n = n())
  
  return(nwords_by_age)
}

######################################################################################################################
#make a final dataframe
make_aoa_dataframe_helper <- function(word_aoa) {
  ages<- get_nwords_by_age(word_aoa)$age
  df <- data.frame()
  for (i in ages) {
    rem_words <- word_aoa %>% filter(age >= i)
    rem_lemma <- c(rem_words$uni_lemma)
    rem_def <- c(rem_words$definition)
    rem_item<- c(rem_words$item)
    corr_age <- rep(i, times = length(rem_lemma))
    curr_df <- data.frame(corr_age, rem_item, rem_lemma, rem_def)
    df <- rbind(df, curr_df)
  }  
  df <- df %>% rename(uni_lemma = rem_lemma, definition=rem_def, item=rem_item)%>%
    left_join(word_aoa %>% select(item, age)) %>%
    mutate(learned = as.numeric(age == corr_age)) %>%
    select(corr_age, item, definition, uni_lemma,learned) %>%
    rename(age = corr_age) %>%
    arrange(age, item)
  return(df)
}

######################################################################################################################
make_aoa_dataframe <- function(lang, lang_form = "WS", lex_class = "nouns") {
  item_data <- get_lang_item_data(lang = lang,
                                  lang_form = lang_form,
                                  lex_class = lex_class)
  admin_data <-
    get_lang_admin_data(lang = lang, lang_form = lang_form)
  instr_data <-
    get_lang_instr_data(lang = lang, lang_form = lang_form)
  word_aoa <-
    get_lang_aoa(item_data = item_data,
                 admin_data = admin_data,
                 instr_data = instr_data)
  
  print(word_aoa)
  
  return(make_aoa_dataframe_helper(word_aoa))
}

######################################################################################################################
make_vocab_dataframe <- function(lang, lang_form = "WS", lex_class = "nouns") {
  item_data <- get_lang_item_data(lang = lang,
                                  lang_form = lang_form,
                                  lex_class = lex_class)
  admin_data <-
    get_lang_admin_data(lang = lang, lang_form = lang_form)
  instr_data <-
    get_lang_instr_data(lang = lang, lang_form = lang_form)
  word_aoa <-
    get_lang_aoa(item_data = item_data,
                 admin_data = admin_data,
                 instr_data = instr_data)
  
  return(arrange(word_aoa, age))
}

######################################################################################################################
trim_all_unilemma<-function(unilemma_list){
  unilemma_list<- unilemma_list %>%
    mutate(uni_lemma=gsub(" \\s*\\([^\\)]+\\)","", uni_lemma)) %>%
    mutate(uni_lemma=gsub("[*].*$","", uni_lemma)) %>%
    filter(!is.na(uni_lemma))
  return(unilemma_list)
}

######################################################################################################################
trim_all_definition<-function(def_list){
  def_list<- def_list %>%
    mutate(definition= gsub(" \\s*\\([^\\)]+\\)","", definition)) %>%
    mutate(definition= gsub("[*].*$","", definition)) %>%
    mutate(definition= gsub("\\/.*", "", definition)) %>%
    filter(definition!= "babysitter's name", 
           definition!= "child's own name", 
           definition!= "pet's name") %>%
    mutate(definition= gsub("[[:punct:]]", "", definition)) 
  
  return(def_list)
}
######################################################################################################################
trim_unilemma<-function(unilemma_list){
  unilemma_list<- unilemma_list %>%
    mutate(uni_lemma= gsub("[*].*$","", uni_lemma)) %>%
    mutate(uni_lemma= gsub("\\/.*", "", uni_lemma)) %>%
    #    mutate(uni_lemma=if_else(grepl(" \\(animal\\)",uni_lemma),gsub(" \\(animal\\)","", uni_lemma),uni_lemma)) %>%
    #    mutate(uni_lemma=if_else(grepl(" \\(object\\)",uni_lemma),gsub(" \\(object\\)","", uni_lemma),uni_lemma)) %>%
    filter(!is.na(uni_lemma))
  return(unilemma_list)
}

######################################################################################################################
trim_definition<-function(def_list){
  def_list<- def_list %>%
    mutate(definition= gsub("[*].*$","", definition)) %>%
    mutate(definition= gsub("\\/.*", "", definition)) 
  return(def_list)
}
######################################################################################################################
write_out_csv<-function(name){
  write.csv(paste(getwd(),"/out_files/",name, sep = "" ))
}
######################################################################################################################
write_out_csv<- function(var, lang, type){
  write.csv(var, paste(getwd(),"/out_files/",lang, "_",type,".csv",sep = ""), row.names = F)
}
