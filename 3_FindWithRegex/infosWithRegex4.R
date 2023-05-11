

dataB <- readRDS("C:/Users/Federico/Desktop/8_Abstracts/2_dataManageFromPmed/2a_Rdatabases/ckd_iron_full.rds")

setwd("C:/Users/Federico/Desktop/8_Abstracts/3_FindWithRegex")

# check modifications
library(quanteda)

library(dplyr)
library(tidyverse)
library(stringr)

library(readxl)

`%notin%` <- Negate(`%in%`)

### acronyms ####

data <- dataB %>% ### ok than we need to transform it to 4 variables like for authors, and from there we put in a regex with str_count we find the number of words to find.
  group_by(PMID) %>%
  mutate(ABSTRACT2 = ABSTRACT,
         ABSTRACT2 = str_replace_all(ABSTRACT2, pattern = "-|/", replacement = " "),
         ABSTRACT2 = str_remove_all(ABSTRACT2, pattern = "'"),
         acronymsParent = str_extract_all(ABSTRACT2,"\\(\\s?[A-Z]+\\s?\\)"),
         acro1 = acronymsParent [[1]][1],
         acro2 = acronymsParent [[1]][2],
         acro3 = acronymsParent [[1]][3],
         acro4 = acronymsParent [[1]][4] ,
         lengthacro1 = ifelse(is.na(acro1),0, str_count(str_remove_all(acro1, "\\(|\\)| " ) ) ),
         lengthacro2 = ifelse(is.na(acro2),0, str_count(str_remove_all(acro2, "\\(|\\)| " ) ) ),
         lengthacro3 = ifelse(is.na(acro3),0, str_count(str_remove_all(acro3, "\\(|\\)| " ) ) ),
         lengthacro4 = ifelse(is.na(acro4),0, str_count(str_remove_all(acro4, "\\(|\\)| " ) ) ),
         acroComp1 = str_extract(ABSTRACT2, paste0("(\\w+\\s+){0,",lengthacro1,"}\\(" ,acro1) ),
         acroComp2 = str_extract(ABSTRACT2, paste0("(\\w+\\s+){0,",lengthacro2,"}\\(" ,acro2) ),
         acroComp3 = str_extract(ABSTRACT2, paste0("(\\w+\\s+){0,",lengthacro3,"}\\(" ,acro3) ),
         acroComp4 = str_extract(ABSTRACT2, paste0("(\\w+\\s+){0,",lengthacro4,"}\\(" ,acro4) ),
         acroComp1 = tolower(str_remove(acroComp1, "\\(.*") ),
         acroComp2 = tolower(str_remove(acroComp2, "\\(.*") ),
         acroComp3 = tolower(str_remove(acroComp3, "\\(.*") ),
         acroComp4 = tolower(str_remove(acroComp4, "\\(.*") ),
         acroComp1 = str_trim(tolower(str_remove(acroComp1, "\\band\\b|\\bor\\b|\\bwith\\b|\\bin\\b|\\bof\\b|\\bfor\\b|\\ba\\b|\\bthat\\b|[1-9]") ) ),
         acroComp2 = str_trim(tolower(str_remove(acroComp2, "\\band\\b|\\bor\\b|\\bwith\\b|\\bin\\b|\\bof\\b|\\bfor\\b|\\ba\\b|\\bthat\\b|[1-9]") ) ),
         acroComp3 = str_trim(tolower(str_remove(acroComp3, "\\band\\b|\\bor\\b|\\bwith\\b|\\bin\\b|\\bof\\b|\\bfor\\b|\\ba\\b|\\bthat\\b|[1-9]") ) ),
         acroComp4 = str_trim(tolower(str_remove(acroComp4, "\\band\\b|\\bor\\b|\\bwith\\b|\\bin\\b|\\bof\\b|\\bfor\\b|\\ba\\b|\\bthat\\b|[1-9]") ) )
  )


# Paste STOP build ####
### to clean up the population

stop <- stopwords() %>% append(c("s","0", "the", "frequents?", "p", "vs","n", "relationship", "definitions?", "conclusions?"))
stop2 <- paste0("\\b",stop , "\\b")
pasteStop <- paste(stop2,collapse = '|' )

stopSymbol <- c("\\.",",", "\\;",
                "\\(","\\)","\\[","\\]",
                "%","\\<","\\>","\\?",
                "\\=", "\\+","\\-","\\/"
               )
stopSymbol2 <- paste0(stopSymbol, collapse = "|")
pasteStopSymbol <- paste0(pasteStop,"|", stopSymbol2)


### regex to remove numbers

regClean1 <- c("[:digit:]+", "'s", "\\d+")
regClean2 <-  paste0(regClean1, collapse = "|")



## 1. Title ####

### a. Title decomposition

### Title decomposition

library(tidytext)

titleExprDF2 <- data %>%  unnest_tokens(titleExpr, TITLE,
                                               token = stringr::str_split, pattern = pasteStopSymbol) %>%
  select(PMID, titleExpr) %>%
  mutate(detectTitleExpr = str_detect(titleExpr, "^\\s*$")) %>%
  filter(detectTitleExpr == "FALSE") %>%
  group_by(PMID) %>%
  mutate(titleExpr = str_trim(titleExpr),
         eRank = seq(1,n()),
         titExprRank = paste0("titExprRank",eRank))


titleExpressions <- titleExprDF2 %>%
  pivot_wider(values_from = titleExpr,
              names_from = titExprRank,
              id_cols = PMID)


### a. Exposure from Title ####
exposureRegexPost <- c("(\\bassociations?\\b\\s+of\\s+((\\w+ ){1,4}))",
                       "(effects?\\s+of\\s+((\\w+ ){1,4}))",
                       "(predictives?\\s+values?\\s+of\\s+((\\w+ ){1,4}))",
                       "(impact\\s+of\\s+((\\w+ ){1,4}))",
                       "(use\\s+of\\s+((\\w+ ){1,4}))",
                       "(administration\\s+of\\s+((\\w+ ){1,4}))",
                       "(prognostic\\s+value\\s+of\\s+((\\w+ ){1,4}))",
                       "(impact\\s+of\\s+((\\w+ ){2,3}and\\s+(\\w+ ){1,4}))",
                       "(efficacy\\s+of\\s+((\\w+ ){1,4}))",
                       "(efficacy\\s+and\\s+safety\\s+of\\s+((\\w+ ){1,4}))",
                       "(\\bassociations?\\b\\s+between\\s+((\\w+ ){1,4}))", ### apr?s il y a and et c'est l'outcome
                       "(relationships?\\b\\s+between\\s+((\\w+ ){1,4}))", ### apr?s il y a and et c'est l'outcome
                       "(the\\s+\\brole?\\b\\s+of\\s+((\\w+ ){1,4}))")




exposureRegexPost6 <- c("(\\bassociations?\\b\\s+of\\s+((\\w+ ){6}))",
                        "(effects?\\s+of\\s+((\\w+ ){6}))",
                        "(predictives?\\s+values?\\s+of\\s+((\\w+ ){6}))",
                        "(impact\\s+of\\s+((\\w+ ){6}))",
                        "(use\\s+of\\s+((\\w+ ){6}))",
                        "(administration\\s+of\\s+((\\w+ ){6}))",
                        "(prognostic\\s+value\\s+of\\s+((\\w+ ){6}))",
                        "(impact\\s+of\\s+((\\w+ ){2,3}and\\s+(\\w+ ){3}))",
                        "(efficacy\\s+of\\s+((\\w+ ){6}))",
                        "(efficacy\\s+and\\s+safety\\s+of\\s+((\\w+ ){6}))",
                        "(the\\s+\\brole?\\b\\s+of\\s+((\\w+ ){6}))")


exposureRegexPre <- c("(\\w+\\s+){1,3}(?=(is|are) associated)",
                      "(\\w+\\s+){1,3}(?=(is|are) not associated)",
                      "(\\w+\\s+){1,3}(?=(is|are) related to)",
                      "(\\w+\\s+){1,3}(?=(is|are) not related to)",
                      "(\\w+\\s+){1,3}(?=associates?\\s+with)",
                      "(\\w+\\s+){1,4}(?=in\\sassociation\\swith\\s)",
                      "(\\w+\\s+){1,3}(?=and risk of)",
                      "(\\w+\\s+){1,4}(?=predicts?\\b)",
                      "(\\w+\\s+){1,4}(?=in relation to?\\b)",
                      "(\\w+\\s+){1,3}(?=correlate with)",
                      "(\\w+\\s+){1,3}(?=as a prognostic indicator for)",
                      "(\\w+\\s+){1,3}(?=are predictive biomarkers of)",
                      "(\\w+\\s+){1,3}(?=as an independent\\s+\\w+\\s+predictor)",
                      "((\\w+\\s+)|(\\w+\\.\\s+)){1,3}(?=(may be associated|may not be associated))", ## matches h. pylori
                      "(are\\s+|is\\s+)(\\w+\\s+){1,5}associated",
                      "(\\w+\\s+){1,4}(?=for\\sthe\\streatment\\sof)"
) ## are meat and heme iron intake associated with ?

exposureRegexPre6 <- c("(\\w+\\s+){1,6}(?=(is|are) associated)",
                       "(\\w+\\s+){1,6}(?=(is|are) not associated)",
                       "(\\w+\\s+){1,6}(?=(is|are) related to)",
                       "(\\w+\\s+){1,6}(?=(is|are) not related to)",
                       "(\\w+\\s+){1,3}(?=associates?\\s+with)",
                       "(\\w+\\s+){1,6}(?=and risk of)",
                       "(\\w+\\s+){1,6}(?=predicts?\\b)",
                       "(\\w+\\s+){1,6}(?=in\\sassociation\\swith\\s)",
                       "(\\w+\\s+){1,6}(?=in relation to?\\b)",
                       "(\\w+\\s+){1,6}(?=correlate with)",
                       "(\\w+\\s+){1,6}(?=as a prognostic indicator for)",
                       "(\\w+\\s+){1,6}(?=are predictive biomarkers of)",
                       "((\\w+\\s+)|(\\w+\\.\\s+)){1,6}(?=(may be associated|may not be associated))",
                       "(are\\s+|is\\s+)(\\w+\\s+){1,6}associated",
                       "(\\w+\\s+){1,6}(?=for\\sthe\\streatment\\sof)")



exposure <- data  %>%
  select(PMID,TITLE) %>% group_by(PMID) %>% slice(1) %>% ungroup() %>%
  mutate(TITLE = tolower(TITLE),
         TITLE = str_remove_all(TITLE, ","),
         TITLE = str_replace_all(TITLE, "-", replacement = " "),
         exposureTitlePost2 = reduce(map(exposureRegexPost, str_extract, string = TITLE),`paste` ),
         exposureTitlePost = ifelse(str_detect(exposureTitlePost2, "\\sand|and\\s")==TRUE,
                                    reduce(map(exposureRegexPost6, str_extract, string = TITLE),`paste` ),exposureTitlePost2)

         ) %>%
  mutate(exposureTitlePost = str_replace_all(exposureTitlePost, pattern = "NA\\s+|\\s+NA", replacement =""),
         exposureTitlePost = str_remove_all(exposureTitlePost, "\\s+on\\s+.*|\\s+in\\s+.*|\\s+with\\s+.*|\\s+for\\s+.*|\\s+among\\s+.*|\\bto\\b|\\bvs\\b.*")) %>% ### remove after
  mutate(exposureTitlePost = str_remove_all(exposureTitlePost, ".*\\s+of|.*\\s+between|\\bis\\b|\\bare\\b|\\bassociated\\b|\\bnot\\b|\\bcan\\b"),
         exposureTitlePost = str_trim(exposureTitlePost)) %>%

  mutate(exposureTitlePre2 = reduce(map(exposureRegexPre, str_extract, string = TITLE),`paste` ),
         exposureTitlePre = ifelse(str_detect(exposureTitlePre2, "\\sand|and\\s")==TRUE,
         reduce(map(exposureRegexPre6, str_extract, string = TITLE),`paste` ), exposureTitlePre2)
         ) %>% select(-c(exposureTitlePre2, exposureTitlePost2)) %>%
  mutate(exposureTitlePre = str_replace_all(exposureTitlePre, pattern = "NA\\s+|\\s+NA", replacement =""),
         exposureTitlePre = str_remove_all(exposureTitlePre, "\\s+on\\s+.*|\\s+in\\s+.*|\\s+with\\s+.*|\\s+for\\s+.*|\\s+among\\s+.*|\\bto\\b|\\bvs\\b.*")) %>% ### remove after
  mutate(exposureTitlePre = str_remove_all(exposureTitlePre, ".*\\s+of|.*\\s+between|\\bis\\b|\\bare\\b|\\bassociated\\b|\\bnot\\b|\\bcan\\b"),
         exposureTitlePre = str_trim(exposureTitlePre))


### b: Outcome from Title ####

### all connection words between exposure and outcome
findoutcome1 <- c("on", "with", "for", "with an increased risk for")

# find all possible combinations between exposures and the connection words and create the regex

regexOutcomePost <- crossing(exposure$exposureTitlePost, findoutcome1) %>%
  rename("exposure" = 1) %>% filter(exposure!="") %>%
  mutate(finalRegexPost = paste0(exposure,"\\s+",findoutcome1,"\\s+\\w+ \\w+ \\w+ \\w+"))

finalRegexPost <- regexOutcomePost$finalRegexPost

regexOutcomePost6 <- crossing(exposure$exposureTitlePost, findoutcome1) %>%
  rename("exposure" = 1) %>% filter(exposure!="") %>%
  mutate(finalRegexPost = paste0(exposure,"\\s+",findoutcome1,"\\s+\\w+ \\w+ \\w+ \\w+ \\w+ \\w+ \\w+ "))

finalRegexPost6 <- regexOutcomePost6$finalRegexPost

### also outcomes are generally mentioned after the exposure Regex pre


outcomeRegexPre2 <- c("((?<=associated?\\s(with\\s|to\\s))((\\w+\\s?){1,4}))",
                      "((?<=associates?\\s(with\\s|to\\s))((\\w+\\s?){1,4}))",
                      "((?<=(is|are)\\srelated\\s(with\\s|to\\s))((\\w+\\s?){1,4}))",
                      "((?<=(is|are)\\snot\\srelated\\s(with\\s|to\\s))((\\w+\\s?){1,4}))",
                      "((?<=and\\srisk\\sof\\s)((\\w+\\s?){1,4}))",
                      "((?<=predicts?\\b\\s)((\\w+\\s?){1,4}))",
                      "((?<=in\\srelation\\sto\\b\\s)((\\w+\\s?){1,4}))",
                      "((?<=correlated?\\s(with\\s|to\\s))((\\w+\\s?){1,4}))",
                      "((?<=correlates?\\s(with\\s|to\\s))((\\w+\\s?){1,4}))",
                      "((?<=prognostic\\sindicator\\sfor\\s)((\\w+\\s?){1,4}))",
                      "((?<=predictive\\sbiomarkers?\\sof\\s)((\\w+\\s?){1,4}))",
                      "((?<=predictive\\s(of\\s|for\\s))((\\w+\\s?){1,4}))",
                      "((?<=predictor\\s(of\\s|for\\s))((\\w+\\s?){1,4}))",
                      "((?<=predict\\sthe\\soutcome\\sof\\s)((\\w+\\s?){1,4}))",
                      "((?<=in\\sassociation\\swith\\s)((\\w+\\s?){1,4}))",
                      "((?<=for\\sthe\\streatment\\sof)((\\w+\\s?){1,4}))"
                      )


### apply all Regex combination to the TITLE string and clean it up

outcome <- exposure %>%
  mutate(outcomeTitlePost2 = reduce(map(finalRegexPost, str_extract, string = TITLE),`paste` ),
         outcomeTitlePost2 = str_replace_all(outcomeTitlePost2, pattern = "NA\\s+|\\s+NA", replacement =""),

         outcomeTitlePost = ifelse(str_detect(outcomeTitlePost2, "\\sand|and\\s")==TRUE,
                                     reduce(map(finalRegexPost6, str_extract, string = TITLE),`paste` ),outcomeTitlePost2),
         outcomeTitlePost = str_replace_all(outcomeTitlePost, pattern = "NA\\s+|\\s+NA", replacement ="") ) %>%
  select(-c(outcomeTitlePost2)) %>%
  mutate(
         ### str remove all before words
         outcomeTitlePost = str_remove(outcomeTitlePost, pattern = ".*(\\bfor\\b|\\bon\\b|\\bwith\\b)"),
         ### str remove all after words
         outcomeTitlePost = str_remove(outcomeTitlePost, pattern = "(\\bin\\b).*")
         )%>%
  mutate(
         outcomeTitlePre = reduce(map(outcomeRegexPre2, str_extract, string = TITLE),`paste` ),
         outcomeTitlePre = str_replace_all(outcomeTitlePre, pattern = "NA\\s+|\\s+NA", replacement =""),
         outcomeTitlePre = str_remove(outcomeTitlePre, pattern = "(\\bin\\b).*"))


### faut encore bien checker mais c'est pas mal

### c: population from Title ####

populationTitleRegex <- c("\\sin\\s)((\\w+\\s?){1,8}))")


population <- outcome %>% mutate(
  exposureTitle = ifelse(!is.na(exposureTitlePost), exposureTitlePost, exposureTitlePre),
  exposureTitle = str_remove(exposureTitle, pasteStopSymbol),
  outcomeTitle = ifelse(!is.na(outcomeTitlePost), outcomeTitlePost, outcomeTitlePre),
  exposureTitle = str_trim(exposureTitle),
  outcomeTitle = str_trim(outcomeTitle)
  ) %>%

  mutate(popTitleDynamicRegexExpo = paste0("((?<=",exposureTitle, populationTitleRegex),
         popTitleDynamicRegexOutcome = paste0("((?<=",outcomeTitle, populationTitleRegex),
         populationTitleExp = str_extract(TITLE, pattern = popTitleDynamicRegexExpo  ),
         populationTitleOut = str_extract(TITLE, pattern = popTitleDynamicRegexOutcome  ),
         populationTitle3 = str_extract(TITLE, "in\\s+patients\\s+with\\s+(\\w+(\\s+|\\.|\\:)){1,8}"),
         populationTitleIn = str_extract(TITLE, "(?<=in ).{10,30}(\\.?|\\,?)"),
         populationTitleIn = str_remove_all(populationTitleIn, pasteStopSymbol),
         populationTitle = ifelse(!is.na(populationTitle3), populationTitle3,
                                  ifelse(!is.na(populationTitleOut), populationTitleOut,
                                         ifelse(!is.na(populationTitleExp), populationTitleExp,populationTitleIn)) )
  )



finalTitleInfos <- population %>%
  select(PMID, TITLE, exposureTitle, outcomeTitle,populationTitle) %>%
  left_join(titleExpressions, by ="PMID")

finalTitleInfos[finalTitleInfos == "NA"] <- NA



## 2. Frequencies %, filtered sample ####

#### Extract the phrases mentioning prevalence or incidence with a window of 60 words around it

prevInc <- data %>%
  filter(!is.na(PMID)) %>%
  filter(
    containsPrevalence == TRUE |
      containsIncidence==TRUE) %>%
  mutate(doc_id = as.character(PMID) ) %>%
  filter(grepl("%", ABSTRACT)) %>%
  arrange(PMID)


corp <- corpus(prevInc, text_field = "ABSTRACT", docid_field = "doc_id")

toks <- tokens(corp)

prevInc2 <- kwic(toks, pattern =   c(phrase("\\d+ %"),phrase("\\d+\\%") ),valuetype = "regex",
                 window = 60, case_insensitive = TRUE) %>%
  rename("PMID"  = 1) %>% as.data.frame()


### create a full phrase with results and a ranked ID for each phrase inside a study

frequencies <- prevInc2 %>% mutate(
  # pre = tolower(pre),
  # post= tolower(post),
  fullPhrase = paste(pre,keyword,post)) %>%
  group_by(PMID) %>% arrange(PMID) %>%
  mutate(IDRank = seq(1:length(PMID)),
         IDletter = letters[IDRank],
         doc_id = paste0(PMID,"_",IDRank) ) %>%  ## creating new ID cuz in corpus, doc_ID must be unique
  mutate(shortPost = str_remove(post, "(?<!\\d)\\.(?!\\d).*?(?=(\\.\\d|\\.$|$))"),
         shortPre = str_extract(pre, "(?<= \\.\\s?).[:alpha:].*"),
         shortFullPhrase = paste(shortPre,keyword,shortPost)) %>%
  mutate(ntokShort = ntoken(shortFullPhrase),
         ntokLong = ntoken(fullPhrase))



## 2.1 frequencies clean

### 2.1a filter CI freqs and doubles ####

frequencies2 <- frequencies   %>%  ### some sentence could have been created in double because prevalence is mentioned twice
  mutate(PMID = str_remove(doc_id, "_.")) %>%
  group_by(PMID) %>% distinct(keyword, .keep_all = TRUE) %>%  ## we filter here the doubles
  ungroup() %>%
  mutate(isKeywordaCI =  ifelse(tolower(str_extract(post,"..") ) == "ci", "yes",
                                ifelse(tolower(str_extract(post,"....") ) == "conf", "yes","no") ) ) %>%    ## filter percentages that are confidence intervals
  filter(isKeywordaCI == "no")%>% mutate(
    # pre = tolower(pre),
    # post= tolower(post),
    fullPhrase = paste(pre,keyword,post)) %>%
  mutate(isKeyCILow1 = str_detect( tolower(fullPhrase), paste0("(\\bci\\b|\\binterval\\b)\\s*", keyword)),
         isKeyCILow2 = str_detect( tolower(fullPhrase), paste0("(\\bci\\b|\\binterval\\b).{1,20}", keyword,".{1,20}\\)")),  # excepting the parenthesis from the match
         isKeyCILow3 = ifelse(str_detect( tolower(keyword), "\\-"), str_detect(tolower(fullPhrase), paste0("(\\bci\\b|\\binterval\\b).{1,20}", keyword) ),FALSE ),
         keyword = str_remove(keyword, "\\-")
  ) %>%
  filter(isKeyCILow1 == FALSE & isKeyCILow2 == FALSE &  isKeyCILow3 == FALSE) %>%
  select(doc_id, keyword, fullPhrase, shortFullPhrase, contains("ntok")) %>%
  mutate(PMID = str_remove(doc_id, "_."))



### 2.1b freq1 Vs freq2 ####

### some frequencies are meant to be compared with others, so we put on same row, than filter and manage questions separately

Vss <- c("\\bv\\b","\\bV\\b","\\bvs\\b","\\bVs\\b","\\bVS\\b","\\bversus\\b",
         "compared to", "compared with")
pasteVss <- paste0(Vss, collapse = "|")

frequencies3 <- frequencies2 %>%
  mutate(
    isVS = str_detect(fullPhrase, paste0(keyword, "\\s*(",pasteVss,")")),
    freqVS1 = ifelse(isVS == TRUE, keyword, NA),
    freqVS2 = ifelse(isVS == TRUE, str_extract( fullPhrase , paste0("(?<=",keyword,"\\s?(",pasteVss,"))\\s*\\d+\\.?\\d+?\\s?\\%?") ), NA ),
    toDelete = str_detect(fullPhrase,  paste0( "(",pasteVss,")\\s*", keyword) ) ,
    pVal = ifelse(isVS == TRUE, str_extract(fullPhrase,paste0("(?<=", freqVS2,").*\\s*(\\bp\\b|\\bP\\b).*\\)"  )), NA ),
    pVal = str_remove(pVal, ".*?(?=(\\bp\\b|\\bP\\b))") ,
    pVal = str_remove(pVal, "\\).*"),
    pVal = str_remove(pVal, "\\,.*"),
    pVal = str_remove(pVal, "\\;.*"),
    #pVal = str_remove(pVal, "(?<=(\\bp\\b|\\bP\\b)\\s?(\\<|\\>|\\=)\\s?\\d{1,2}\\.?\\d?).*"),
  ) %>%
  filter(toDelete == FALSE)

### 2.1c freq in range ####

## frequencies are expressed in a Range (difference in formulation where increases or decreases (talking about subgroups) and ranging from to giving sort of confint)

frequencies4 <- frequencies3 %>%
  mutate(freqIsRange = str_extract(fullPhrase, "(from|to)?\\s?((\\d{1,5}\\s?\\.?(\\d{1,5})?\\s?)(p\\s?\\.\\s?1(0{1,5})|%))"),
         freqIsRange2 = str_detect(fullPhrase, paste0("from\\s*",keyword) ),
         freqTo =str_detect(fullPhrase, paste0("to ", keyword)),

         freqRange1 = ifelse(freqIsRange2 == TRUE, keyword, NA),
         freqRange2 = ifelse(freqIsRange2 == TRUE, str_extract( fullPhrase , paste0(keyword,".*to \\d+\\.?\\d+?\\s?%") ), NA ),
         freqRange2 = str_remove(freqRange2, ".*?to\\s?"),
         freqRange2 = str_remove(freqRange2, "(?<=%).*"),
         pastePMIDfreqRange = paste(PMID, freqRange2),
         pastePMIDkeyword = paste(PMID, keyword)
  ) %>%
  filter(pastePMIDkeyword %notin% pastePMIDfreqRange) %>%  ## to eliminate the doubles rows of the range
  mutate(
    pVal = ifelse(freqIsRange2 == TRUE, str_extract(fullPhrase,paste0("(?<=", freqRange2,").*\\(\\s*(p|P).*\\)"  )), pVal ),
    pVal = str_remove(pVal, ".*?\\(") ,
    pVal = str_remove(pVal, "\\).*")
  )


frequencies5 <- frequencies4 %>% select(doc_id, PMID, keyword, fullPhrase,
                                        isVS, freqVS1, freqVS2 , pVal,
                                        freqIsRange2 , freqRange1, freqRange2, shortFullPhrase, contains("nTok") )



### 2.2 Numeric Frequencies in full phrase ####

frequencies6 <- frequencies5 %>%  ### extraction
  mutate(singleFreq = ifelse(isVS == FALSE & freqIsRange2 == FALSE, as.numeric(str_remove(keyword, "\\%") ),NA ) )

### 2.3: Populations from Full phrase ####

prevalencePopulationRegex <- c("((\\w+\\s?){1,2})(population)",
                               "(among\\spatients\\s)(\\w+\\s?){1,5}",
                               "(?<=(study|studies)\\son\\s).*?(patients)",
                               "(?<=in\\spatients\\swith\\s)(\\w+\\s?){1,5}",
                               "((prevalence|incidence)\\sof\\s.*?\\bin\\b)(\\w+\\s?){1,5}",
                               "(\\w+\\s+){1,5}patients",
                               "patients\\s+with\\s+(\\w+\\s+?){1,5}"
)
prevPopRegex <- paste0(prevalencePopulationRegex, collapse = "|")


###########################""

prevalencePopulation <- frequencies6 %>%
  mutate(fullPhraseText = tolower(str_replace_all(fullPhrase, "\\-", " ")),  # I remove it after the frequencies
         prevPop1 = str_extract(fullPhraseText, prevPopRegex),
         popFullPhrase =str_remove_all(prevPop1, pasteStopSymbol),
         rowID = row_number())  %>%
  ungroup()



### c: Exposure from Full phrase ####

regexExpFPhrase<- c("(?<=(\\.|\\,)\\s)(.){1,100}(?=was\\b.*\\bprevalent\\b)",
                    "(.){1,100}(?=with\\s+(\\ban\\b|\\ba\\b)\\s+(incidence|prevalence)\\s+\\bof\\b)",
                    "(?<=\\bthe\\b\\soverall\\s(prevalence|incidence)\\sof\\s)(\\w+\\s+?){1,5}(?=was)",
                    "(prevalence|incidence)\\sof\\s(\\w+\\s+?){1,5}",
                    "(.){1,100}(?=has\\sa\\s(prevalence|incidence)\\sof)",
                    ".{1,100}(?=with a prevalence reaching up to)",
                    ".{1,100}(?=has an estimated prevalence of)",
                    "(a|an) (prevalence|incidence) of \\d+\\.?\\d+?.*for (\\w+\\s+?){1,5}"
)

regexAllExposureFPhrase <- paste0(regexExpFPhrase, collapse = "|")

prevalenceExposure <- prevalencePopulation %>%
  mutate(ExposurefullPhrase = str_extract(fullPhraseText, regexAllExposureFPhrase ),
         ExposurefullPhrase = str_replace_all(ExposurefullPhrase, "\\s\\s", ", "))





# ### 3: Final freq values ####
#
# FullPhraseAndTitle <- prevalenceExposure %>%
#   left_join(select(finalTitleInfos, PMID, exposureTitle,outcomeTitle, populationTitle),
#             by = "PMID") %>%
#   mutate(rangeFreqHigh = as.numeric(rangeFreqHigh),
#          rangeFreqLow = as.numeric(rangeFreqLow),
#          freqMean = (rangeFreqHigh + rangeFreqLow)/2,
#          freqFin = ifelse(is.na(freqMean), singleFreq, freqMean) )


### 2.4. Suggestions for GPT full phrase ####

## replaceing acronyms in suggestions

replaceAcro <- prevalenceExposure %>%
  left_join( select(data,PMID,contains("acro") ), by = "PMID") %>%
  mutate(acro1p = paste0("\\s",acro1,"\\s"),
         acro2p = paste0("\\s",acro2,"\\s"),
         acro3p = paste0("\\s",acro3,"\\s"),
         acro4p = paste0("\\s",acro4,"\\s"),
         ) %>%
  ## exposure full phrase
  mutate( ExposurefullPhrase = ifelse(is.na(acro1), ExposurefullPhrase,
                                     str_replace_all(ExposurefullPhrase, pattern =  tolower(acro1p ), replacement =  paste0(" ",acroComp1, " ") ) ),

          ExposurefullPhrase = ifelse(is.na(acro2), ExposurefullPhrase,
                                      str_replace_all(ExposurefullPhrase, pattern =  tolower(acro2p), replacement =  paste0(" ",acroComp2, " ") ) ),

          ExposurefullPhrase = ifelse(is.na(acro3), ExposurefullPhrase,
                                      str_replace_all(ExposurefullPhrase, pattern =  tolower(acro3p), replacement =  paste0(" ",acroComp3, " ") ) ),

          ExposurefullPhrase = ifelse(is.na(acro4), ExposurefullPhrase,
                                      str_replace_all(ExposurefullPhrase, pattern =  tolower(acro4p), replacement =  paste0(" ",acroComp4, " ") ) ) ) %>%
  mutate(
    ExposurefullPhrase = str_remove_all(ExposurefullPhrase, paste0(pasteStopSymbol,"|prevalence|incidence|among,|children|adults?|patients?|estimated" ) ) ) %>%

    ### population
    mutate( popFullPhrase = ifelse(is.na(acro1), popFullPhrase,
                                        str_replace_all(popFullPhrase, pattern =  tolower(acro1p ), replacement =  paste0(" ",acroComp1, " ") ) ),

            popFullPhrase = ifelse(is.na(acro2), popFullPhrase,
                                        str_replace_all(popFullPhrase, pattern =  tolower(acro2p), replacement =  paste0(" ",acroComp2, " ") ) ),

            popFullPhrase = ifelse(is.na(acro3), popFullPhrase,
                                        str_replace_all(popFullPhrase, pattern =  tolower(acro3p), replacement =  paste0(" ",acroComp3, " ") ) ),

            popFullPhrase = ifelse(is.na(acro4), popFullPhrase,
                                        str_replace_all(popFullPhrase, pattern =  tolower(acro4p), replacement =  paste0(" ",acroComp4, " ") ) ) ) %>%
      mutate(
        popFullPhrase = str_remove_all(popFullPhrase, paste0(pasteStopSymbol,"|prevalence|incidence" ) ),

  )




suggestionsGPT <- replaceAcro %>%
  left_join( select(finalTitleInfos, PMID, TITLE, exposureTitle, populationTitle) ) %>%
  mutate(pasteAcronym = paste(acroComp1,acroComp2,acroComp3,acroComp4, sep=","),
         pasteAcronym = str_remove_all(pasteAcronym, pattern ="NA,|NA"),
         pasteAcronym = ifelse(str_detect(pasteAcronym,"^\\s*$"), NA, pasteAcronym)
         ) %>%
  mutate(exposureSuggestion =paste(exposureTitle, ExposurefullPhrase, pasteAcronym, sep = " "),
         exposureSuggestion =
           str_remove_all(exposureSuggestion,c( "\\bNA\\b|NA\\b|\\?" ) ),
         exposureSuggestion =
           str_remove_all(exposureSuggestion, pasteStop),
         exposureSuggestion = str_trim(exposureSuggestion),
         exposureSuggestion = str_replace_all(exposureSuggestion, "  ", ", ")) %>%
  mutate(populationSuggestion =
             paste(populationTitle, popFullPhrase),
         populationSuggestion =
               str_remove_all(populationSuggestion,c( "\\bNA\\b|NA\\b|\\?" ) ),
         populationSuggestion =
               str_remove_all(populationSuggestion, pasteStop),
      ) %>% mutate(
        populationSuggestion = ifelse(is.na(populationSuggestion), pasteAcronym, populationSuggestion)
        ) %>%
  mutate(
    exposureSuggestion = str_remove_all(exposureSuggestion, "NA")
  )


suggestionsGPT2 <- suggestionsGPT %>%
  select(doc_id,PMID, keyword, fullPhrase, isVS, freqVS1, freqVS2, pVal, freqIsRange2, freqRange1, freqRange2, singleFreq,
         exposureSuggestion, populationSuggestion, shortFullPhrase, contains("nTok") ) %>%
  left_join(select(data, PMID, ABSTRACT, TITLE))





### need to join with all rest

saveRDS(suggestionsGPT2 , "suggestionsGPTCKD3.rds")


