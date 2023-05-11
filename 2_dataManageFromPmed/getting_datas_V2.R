
setwd("C:/Users/Federico/Desktop/8_Abstracts/2_dataManageFromPmed")

library(readtext)
library(stringr)
library(dplyr)
library(tidyverse)
library(stringi)
library(quanteda)
library(readxl)

`%notin%` <- Negate(`%in%`)
plain_text <- readtext("C:/Users/Federico/Desktop/8_Abstracts/1_textDataFromPmed/iron_ckd_3584art.txt" ,encoding = "utf-8") ## we should do this here ,  encoding = "utf-8" but we loose an article

plain_text2 <-  plain_text[,-1] %>% as.data.frame() %>% 
  rename("txt" = 1)

noNewLine <- str_replace_all(plain_text2, pattern = "\n",replacement = " ")
### split studies

studies <- str_extract_all(noNewLine, "(PMID-.*?)\\s*(?=(PMID-|SO\\s*-))") %>% 
  unlist() %>% as_tibble()   %>% rename("text" = 1)


data <- data.frame(studies) %>% 
  mutate(text = str_squish(text))

### Pubmed ID ####

data2 <- data %>% mutate(
  PMID = str_extract(text, pattern = "(?<=PMID-)\\s*(.*?)\\s*(?=OWN)"), ## positive lookbehind, matches what starts with PMID- without including it in the match and
  PMID = str_extract(PMID, "\\d+"))  ## accept only numbers
# ends with OWN, without including it in the match

## Titles ####

data3 <- data2 %>% mutate(
  TITLE = str_extract(text, "TI\\s*-\\s(.*?)\\s*(PG|LI)"),
  TITLE = str_remove(TITLE, "^TI\\s*-"), ## Remove all TI - at beginning of string
  TITLE = str_remove(TITLE, "PG$"),  ## remove all PG at end of the string
  TITLE = str_replace(TITLE, "\\s{2,}",' '), ## replace any number of spaces grater than 2 with one space
) %>% 
  relocate(TITLE, .before = text)

## abstracts ####

data4 <- data3 %>% 
  mutate(ABSTRACT = str_extract(text, "AB\\s*(.*?)\\s*(?=(FAU\\s*-|CI\\s*-|AU\\s*-|AD\\s*-))"),
         ABSTRACT = str_remove(ABSTRACT, "AB\\s*-"),
         ABSTRACT = str_replace(ABSTRACT, "\\s*",' '),
         ABSTRACT = stri_enc_toutf8(ABSTRACT),
         ABSTRACT = str_squish(ABSTRACT),
        #ABSTRACTS = str_remove_all(ABSTRACTS, "‰|â|€|Â"), ## need unicode or start with UTF8
         ABSTRACT = gsub(ABSTRACT,pattern = "\u00B7", replacement = ".")   ### gsub fonctionne mieux avec unicodes
         ) %>% 
  relocate(ABSTRACT, .after = TITLE)

## year of publication ####

data5 <- data4 %>% 
  mutate(publicationDate = str_extract(text, "(?<=DP)(.*?)\\s*(?=TI)"),
         publicationDate = str_remove(publicationDate, "-"),
         publicationDate = str_remove(publicationDate, "^\\s"),
         publicationYear = str_extract(publicationDate,"\\d{4}")
         # ABSTRACTS = str_replace(ABSTRACTS, "\\s{2,}",' ') 
         ) %>% 
  relocate(publicationDate, .before = TITLE)

## Mesh terms ####

data6 <- data5 %>% 
  mutate(keywords = str_extract_all(text, "(?<=(MH\\s.-|MH\\s-))(.*?)(?=(MH|PMC) )") ) %>% 
  group_by(PMID) %>% 
  mutate(
    keyword1 = keywords[[1]][1], 
    keyword2 = keywords[[1]][2],
    keyword3 = keywords[[1]][3],
    keyword4 = keywords[[1]][4],
    keyword5 = keywords[[1]][5],
    keyword6 = keywords[[1]][6],
    keyword7 = keywords[[1]][7], 
    keyword8 = keywords[[1]][8],
    keyword9 = keywords[[1]][9],
    keyword10 = keywords[[1]][10],
    keyword11 = keywords[[1]][11],
    keyword12 = keywords[[1]][12],
    keyword13 = keywords[[1]][13],
    keyword14 = keywords[[1]][14], 
    keyword15 = keywords[[1]][15],
    keyword16 = keywords[[1]][16],
    keyword17 = keywords[[1]][17],
    keyword18 = keywords[[1]][18],
    keyword19 = keywords[[1]][19]) %>% 
  ungroup()

data6b <- data6 %>% 
  mutate(
    keyword1bis = keyword1,
    keyword2bis = keyword2,
    keyword3bis = keyword3,
    keyword4bis = keyword4,
    keyword5bis = keyword5,
    keyword6bis = keyword6,
    keyword7bis = keyword7,
    keyword8bis = keyword8,
    keyword9bis = keyword9,
    keyword10bis = keyword10,
    keyword11bis = keyword11,
    keyword12bis = keyword12,
    keyword13bis = keyword13,
    keyword14bis = keyword14,
    keyword15bis = keyword15,
    keyword16bis = keyword16,
    keyword17bis = keyword17,
    keyword18bis = keyword18,
    keyword19bis = keyword19
  ) %>%  
  separate(keyword1bis, into = c("key1a","key1b","key1c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword2bis, into = c("key2a","key2b","key2c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword3bis, into = c("key3a","key3b","key3c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword4bis, into = c("key4a","key4b","key4c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword5bis, into = c("key5a","key5b","key5c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword6bis, into = c("key6a","key6b","key6c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword7bis, into = c("key7a","key7b","key7c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword8bis, into = c("key8a","key8b","key8c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword9bis, into = c("key9a","key9b","key9c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword10bis, into = c("key10a","key10b","key10c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword11bis, into = c("key11a","key11b","key11c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword12bis, into = c("key12a","key12b","key12c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword13bis, into = c("key13a","key13b","key13c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword14bis, into = c("key14a","key14b","key14c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword15bis, into = c("key15a","key15b","key15c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword16bis, into = c("key16a","key16b","key16c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword17bis, into = c("key17a","key17b","key17c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword18bis, into = c("key18a","key18b","key18c"), sep = c("\\/|\\,") )  %>% 
  separate(keyword19bis, into = c("key19a","key19b","key19c"), sep = c("\\/|\\,") )  %>% 
  mutate(across(
    contains("key"),
    ~ str_remove(.,"\\*")
  ))

  
## getting protocole type ####

data7 <- data6b %>% 
  mutate(protocolType = str_extract_all(text, "(?<=(PT\\s.-|PT\\s-) )(.*?)(?=(PT\\s*-|DEP\\s*-) )")) %>% 
  group_by(PMID) %>% 
  mutate(
    protocol1 = protocolType[[1]][1], 
    protocol2 = protocolType[[1]][2],
    protocol3 = protocolType[[1]][3],
    protocol4 = protocolType[[1]][4],
    protocol5 = protocolType[[1]][5],
    protocol6 = protocolType[[1]][6],
    protocol7 = protocolType[[1]][7], 
    protocol8 = protocolType[[1]][8],
    protocol9 = protocolType[[1]][9],
    protocol10 = protocolType[[1]][10] ) %>% 
  ungroup()


data8 <- data7 %>% 
  mutate(FAU_yes = ifelse(str_detect(text, "FAU\\s")==FALSE,0,1) ,   
         Authors = str_extract_all(text, "(?<=(FAU\\s.-|FAU\\s-))(.*?)(?=(AU\\s*-|AD\\s*-) )")) %>% 
  group_by(PMID)  %>% 
  mutate(
    author1 = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][1]), 
    author2 = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][2]),
    author3 = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][3]),
    author4 = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][4]),
    author5 = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][5]),
    author6 = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][6]),
    authorn = ifelse(FAU_yes ==0,NA_character_, Authors[[1]][length(Authors[[1]])] ) ) %>% 
    mutate(across(c(author1,author2, author3, author4, author5,author6,authorn), str_squish )) %>% 
    mutate(across(c(author1,author2, author3, author4, author5,author6,authorn), str_remove, "," )) %>% 
    mutate(across(c(author1,author2, author3, author4, author5,author6,authorn), str_remove, "(?<=(\\s.|\\s\\s.))(.*)" )) %>% 
  ungroup()

## Other keywords ####

data9 <- data8 %>%  # "(?<=(MH\\s.-|MH\\s-))(.*?)(?=(MH|PMC) )"
  mutate(OT = str_extract_all(text, "(?<=(OT\\s-))(.*?)(?=(OT\\s*-|EDAT\\s*-|MH\\s*-|COIS\\s*-) )") ) %>%  
  group_by(PMID) %>% 
  mutate(
    OT1 = OT[[1]][1], 
    OT2 = OT[[1]][2],
    OT3 = OT[[1]][3],
    OT4 = OT[[1]][4],
    OT5 = OT[[1]][5],
    OT6 = OT[[1]][6],
    OT7 = OT[[1]][7],
    OT8 = OT[[1]][8]
    ) %>% 
  mutate(across(contains("OT"), str_squish)) %>% 
  mutate(across(contains("OT"), tolower)) %>%
  mutate(across(contains("OT"), ~replace_na(.,"0"))) %>%   
  ungroup()
  
## journal ####

data10 <- data9 %>% 
  mutate(journal = str_extract(text,"(?<=(JT\\s-|JT\\s\\s-))(.*?)(?=(JID\\s+-))"),
         journal = str_squish(journal)) %>% 
  select( PMID,journal ,TITLE, publicationYear,protocolType , keywords ,publicationDate,ABSTRACT, contains("Author"),contains("key"),contains("OT"), -c(text, Authors,OT,protocolType,keywords))


data11 <- data10 %>% 
  mutate(containsHR = str_detect(ABSTRACT, "\\bHR\\b"),
         containsOR = str_detect(ABSTRACT, "\\bOR\\b"),
         containsRR = str_detect(ABSTRACT, "\\bRR\\b"),
         containsIncidence = str_detect(ABSTRACT, regex("incidence",ignore_case=TRUE) ), 
         containsPrevalence = str_detect(ABSTRACT, regex("prevalence",ignore_case=TRUE) ),
  )



## country ####


countrylist <- read_xls("all_countries.xls", col_names = F) %>% rename( "country" = 1) %>% 
  mutate(countryLow = tolower(country))

countryRegex <- paste0(countrylist$country, collapse = "|")
countryRegexLow <- paste0(countrylist$countryLow, collapse = "|")


data11Country <- data11 %>% 
 ungroup() %>% 
  mutate(country = str_extract_all(tolower(ABSTRACT), countryRegexLow),
         rowID = row_number())  %>% 
  group_by(rowID) %>% 
  mutate(countryUnique = paste0(unique(unlist(country ) ),collapse = ", " )) %>% 
  ungroup()


data12 <- data11 %>% 
  left_join(select(data11Country,PMID, countryUnique), by = "PMID")


## Study Objective ####


studyObjective <- data12 %>% 
  select(PMID, ABSTRACT,TITLE) %>% 
  filter(!is.na(PMID))  %>%
  filter(!is.na(ABSTRACT)) %>% 
  mutate(doc_id = as.character(PMID) )

corp <- corpus(studyObjective, text_field = "ABSTRACT", docid_field = "doc_id")

toks <- tokens(corp)

## Objectives

kw_object <- kwic(toks, pattern =  c("objectiv*", "aim*", "scope",
                                     phrase("we investigated"),"purpose*",
                                     phrase("study was designed to"),
                                     phrase("this article reviews"),
                                     phrase("we studied"),
                                     phrase("this study reviews"),phrase("we review"),phrase("we sought to"),
                                     phrase("not understood"), phrase("not weel understood"), 
                                     phrase("is unknown"),phrase("little is known"),
                                     phrase("to investigate")), window = 50) %>% as.data.frame()


kw_object2 <- kw_object %>% 
  mutate(pre2 = str_remove_all(pre, ".*(\\.|\\:)"),
         post2 = str_remove(post, "(\\.)(.*)"),
         studyObjective = paste(pre2, keyword, post2))


kw_object3 <- kw_object2 %>% group_by(docname) %>% slice(1) %>% select( "PMID" = docname, studyObjective)

data13 <- data12 %>% left_join(kw_object3)


## Attach definitions


corpdefinition <- corpus(data13, text_field = "ABSTRACT", docid_field = "doc_id")

toks <- tokenize_sentence(corpdefinition) %>% tokens()

define<- kwic(toks, pattern =   c(phrase("was defined"), 
                                      phrase("defined as"),
                                      phrase("defined by")) ,valuetype = "regex",
                  window = 1, case_insensitive = TRUE) %>%
  rename("PMID"  = 1) %>% as.data.frame() %>% 
  group_by(PMID) %>% 
  slice(1) %>% 
  ungroup() %>% select(PMID, keyword) %>% 
  rename( "definition" = keyword)

data14 <- data13 %>% 
  left_join( select(define, definition, PMID) )


saveRDS(data14, "C:/Users/Federico/Desktop/8_Abstracts/2_dataManageFromPmed/2a_Rdatabases/ckd_iron_full.rds")
