library(devtools)
library(quanteda)
library(gptchatteR)
library(readxl)
library(tidyverse)
library(quanteda)

`%notin%` <- Negate(`%in%`)

setwd("C:/Users/Federico/Desktop/8_Abstracts/4_FindWithGPT")


chatter.auth("your key")

chatter.create()

data <- readRDS("C:/Users/Federico/Desktop/8_Abstracts/3_FindWithRegex/suggestionsGPTCKD3.rds")


## Questions full abstract ####

data2 <- data %>%
  group_by(PMID) %>% arrange(PMID, doc_id) %>%
  mutate(questionRank = seq(1:n()) ) %>% ungroup() %>%
  mutate(questionFullAbstract =
           ifelse(isVS ==TRUE,
                  paste0(
                    "Considering the sudy with title: ", TITLE, "and abstract: ", ABSTRACT,". ",
                    "Answer to the following questions as shortly as possible separating each answer with a semicolon symbol:",
                    "1) Are: ",freqVS1, " and ", freqVS2, " related to a measure of incidence or prevalence? (possible answers: incidence, prevalence, no). If yes,",
                    "2) Which  caracteristic is linked to", freqVS1 , " ? (for example:", exposureSuggestion , ")",
                    "; 3) Which unit is related to ", freqVS1 , " ? for ex.(percentage, per 100 persons/ year...)",
                    "; 4) ",freqVS1," is linked to a group best described by which carachteristics? for example: patients age, sex, or patients with: ", populationSuggestion , ")",
                    "; 5) confidence interval of",freqVS1, "[low, high] (if reported) ; ",

                    " 5) Characteristic related to the second frequency measure ",freqVS2, "% (for example:", exposureSuggestion , ");",
                    " 6) measure unit (percentage, per 100 persons/ year...);",
                    " 7)  characteristics of group related to ", freqVS2,";",
                    " 8) confidence interval of",freqVS2, "[low, high] (if reported) ;"),
                  ifelse(isVS == FALSE & freqIsRange2==FALSE & questionRank==1,

              paste0("Considering the sudy with title: ", TITLE, "and abstract: ", ABSTRACT,". ",
                                "Answer to the following questions as shortly as possible separating each answer with a semicolon symbol:",
                                " 1) Is ", keyword, " related to a measure of incidence or prevalence? (possible answers: incidence, prevalence, no). If yes,",
                                "; 2) Which  caracteristic or exposure  is linked to", keyword , " (for example:", exposureSuggestion , ")",
                                "; 3) Which unit is related to ", keyword , " for ex.(percentage, per 100 persons/ year...)",
                                "; 4) ",keyword," is linked to a group best described by which carachteristics? for example: patients age, sex, or patients with: ", populationSuggestion , ")",
                                "; 5) confidence interval of the frequency measure [low, high] (if reported) ; ",
                                "Thanks"
                                ),
              ifelse(isVS == FALSE & freqIsRange2 & questionRank>1,
                     paste0("In the same study: ",
                            "Answer to the following questions as shortly as possible separating each answer with a semicolon symbol:",
                            " 1) Is ", keyword, " related to a measure of incidence or prevalence? (possible answers: incidence, prevalence, no). If yes,",
                            "; 2) Which  caracteristic or exposure is linked to", keyword , " (for example:", exposureSuggestion , ")",
                            "; 3) Which unit is related to ", keyword , " for ex.(percentage, per 100 persons/ year...)",
                            "; 4) ",keyword," is linked to a group best described by which carachteristics? for example: patients age, sex, or patients with: ", populationSuggestion , ")",
                            "; 5) confidence interval of the frequency measure [low, high] (if reported) ; ",
                            "Thanks"),
                     paste0("In the same study: ",
                            "Answer to the following questions as shortly as possible separating each answer with a semicolon symbol:",
                            " 1) Is ", keyword, " related to a measure of incidence or prevalence? (possible answers: incidence, prevalence, no). If yes,",
                            "; 2) Which  caracteristic or exposure is linked to", keyword , " (for example:", exposureSuggestion , ")",
                            "; 3) Which unit is related to ", keyword , " for ex.(percentage, per 100 persons/ year...)",
                            "; 4) ",keyword," is linked to a group best described by which carachteristics? for example: patients age, sex, or patients with: ", populationSuggestion , ")",
                            "; 5) confidence interval of the frequency measure [low, high] (if reported) ; ",
                            "Thanks") )))
    ) # end mutate



## Questions full phrase ####

data3 <- data2 %>% ungroup() %>%
  mutate(questionFullPhrase =
           ifelse(freqIsRange2 == TRUE,
               paste0(
               "Considering the piece of text that follows, please answer to the questions mentioned below as simply and shortly as possible and separate each answer with a semicolon.",
               " If one information is not available, write NA. ","TEXT: ",
               fullPhrase, " etc...",
               ". QUESTIONS: ",
               "1) Is ", freqRange1 ," related to a measure of prevalence or incidence? (possible answers: incidence, prevalence, no). if yes:",
               " 2) Characteristic or exposure related to ",freqRange1 , " (for example:", exposureSuggestion , ");",
               " 3) measure unit (percentage, per 100 persons/ year...);",
               " 4) group characteristics ( for example: patients age, sex, or patients with", populationSuggestion , ")",

               " 5) Characteristic or exposure related to ",freqRange2 , "% (for example:", exposureSuggestion , ");",
               " 6) measure unit (percentage, per 100 persons/ year...);",
               " 7)  group characteristics related to ",freqRange2," ( for example: patients age, sex, country, or patients with: ", populationSuggestion , ")",
               "Thanks"
             ),

           ifelse( isVS ==TRUE ,

           paste0(
             "Considering the piece of text that follows, please answer to the questions mentioned below as simply and shortly as possible and separate each answer with a semicolon.",
             " If one information is not available, write NA. ","TEXT: ",
             fullPhrase, " etc...",
             ". QUESTIONS: ",
             "1) Is ", freqVS1 ," related to a measure of prevalence or incidence? (possible answers: incidence, prevalence, no). if yes:",
             " 2) Characteristic or exposure related to ",freqVS1 , " (for example:", exposureSuggestion , ");",
             " 3) measure unit (percentage, per 100 persons/ year...);",
             " 4) group characteristics ( for example: patients age, sex, or patients with", populationSuggestion , ")",

             " 5) Characteristic related to ",freqVS2 , "% (for example:", exposureSuggestion , ");",
             " 6) measure unit (percentage, per 100 persons/ year...);",
             " 7)  group characteristics related to ",freqVS2," ( for example: patients age, sex, country, or patients with: ", populationSuggestion , ")",
             "Thanks"
           ),
           paste0(
          "Considering the piece of text that follows, please answer to the questions mentioned below as simply and shortly as possible and separate each answer with a semicolon.",
          " If one information is not available, write NA. ","TEXT: ",
           fullPhrase, " etc... ",
          ". QUESTIONS: ",
           " 1) Is ", keyword, " related to a measure of incidence or prevalence? (possible answers: incidence, prevalence, no). If yes,",
           "; 2) Which  caracteristic or exposure is linked to ", keyword , " (for example:", exposureSuggestion , ")",
           "; 3) Which unit is related to ", keyword , " for ex.(percentage, per 100 persons/ year...)",
           "; 4) ",keyword," is linked to a group best described by which carachteristics? for example: patients age, sex, or patients with: ", populationSuggestion , ")",
           "; 5) confidence interval of the frequency measure [low, high] (if reported) ; ",
           "Thanks" ) ) )

        )


data3b <- data3 %>% ungroup() %>%
  mutate(questionFullPhraseShort =
           ifelse(freqIsRange2 == TRUE,
                  paste0(
                    "Considering the text that follows:",
                    shortFullPhrase,
                    ". QUESTIONS: ",
                    "1) Is ", freqRange1 ," related to a measure of prevalence or incidence? (possible answers: incidence, prevalence, no). if yes:",
                    " 2) Characteristic or exposure related to ",freqRange1 , " (for example:", exposureSuggestion , ");",
                    " 3) measure unit (percentage, per 100 persons/ year...);",
                    " 4) group characteristics ( for example: patients age, sex, or patients with", populationSuggestion , ")",

                    " 5) Characteristic or exposure related to ",freqRange2 , "% (for example:", exposureSuggestion , ");",
                    " 6) measure unit (percentage, per 100 persons/ year...);",
                    " 7)  group characteristics related to ",freqRange2," ( for example: patients age, sex, country, or patients with: ", populationSuggestion , ")",
                    " If one information is not available, write NA and separate each answer with a semicolon "

                  ),

                  ifelse( isVS ==TRUE ,

                          paste0(
                            "Considering the text that follows:",
                            shortFullPhrase,
                            ". QUESTIONS: ",
                            "1) Is ", freqVS1 ," related to a measure of prevalence or incidence? (possible answers: incidence, prevalence, no). if yes:",
                            " 2) Characteristic or exposure related to ",freqVS1 , " (for example:", exposureSuggestion , ");",
                            " 3) measure unit (percentage, per 100 persons/ year...);",
                            " 4) group characteristics ( for example: patients age, sex, or patients with", populationSuggestion , ")",

                            " 5) Characteristic related to ",freqVS2 , "% (for example:", exposureSuggestion , ");",
                            " 6) measure unit (percentage, per 100 persons/ year...);",
                            " 7)  group characteristics related to ",freqVS2," ( for example: patients age, sex, country, or patients with: ", populationSuggestion , ")",
                            " If one information is not available, write NA and separate each answer with a semicolon "
                          ),
                          paste0(
                            "Considering the text that follows:",
                            shortFullPhrase,
                            ". QUESTIONS: ",
                            " 1) Is ", keyword, " related to a measure of incidence or prevalence? (possible answers: incidence, prevalence, no). If yes,",
                            "; 2) Which  caracteristic or exposure is linked to ", keyword , " (for example:", exposureSuggestion , ")",
                            "; 3) Which unit is related to ", keyword , " for ex.(percentage, per 100 persons/ year...)",
                            "; 4) ",keyword," is linked to a group best described by which carachteristics? for example: patients age, sex, or patients with: ", populationSuggestion , ")",
                            "; 5) confidence interval of the frequency measure [low, high] (if reported) ; "
                            ) ) )

  )

data3c <- data3b %>% ungroup() %>%
  mutate(questionFullPhraseShortNoSuggest =
           ifelse(freqIsRange2 == TRUE,
                  paste0(
                    "Considering the text that follows:",
                    shortFullPhrase,
                    ". QUESTIONS: ",
                    "1) Is ", freqRange1 ," related to a measure of prevalence or incidence? (possible answers: incidence, prevalence, no). if yes:",
                    " 2) Characteristic or exposure related to ",freqRange1 ,
                    " 3) measure unit (percentage, per 100 persons/ year...);",
                    " 4) group characteristics ",

                    " 5) Characteristic or exposure related to ",freqRange2 , "%" ,
                    " 6) measure unit (percentage, per 100 persons/ year...);",
                    " 7)  group characteristics related to ",freqRange2,"%",
                    " If one information is not available, write NA and separate each answer with a semicolon. Answer as shortly as possible."

                  ),

                  ifelse( isVS ==TRUE ,

                          paste0(
                            "Considering the text that follows:",
                            shortFullPhrase,
                            ". QUESTIONS: ",
                            "1) Is ", freqVS1 ," related to a measure of prevalence or incidence? (possible answers: incidence, prevalence, no). if yes:",
                            " 2) Characteristic or exposure related to ",freqVS1 ,
                            " 3) measure unit (percentage, per 100 persons/ year...);",
                            " 4) group characteristics",

                            " 5) Characteristic related to ",freqVS2 , "%",
                            " 6) measure unit (percentage, per 100 persons/ year...);",
                            " 7)  group characteristics related to ",freqVS2,"%",
                            " If one information is not available, write NA and separate each answer with a semicolon. Answer as shortly as possible."
                          ),
                          paste0(
                            "Considering the text that follows:",
                            shortFullPhrase,
                            ". QUESTIONS: ",
                            " 1) Is ", keyword, " related to a measure of incidence or prevalence? (possible answers: incidence, prevalence, no). If yes,",
                            "; 2) Which  caracteristic or exposure is linked to ", keyword ,
                            "; 3) Which unit is related to ", keyword , " for ex.(percentage, per 100 persons/ year...)",
                            "; 4) ",keyword," is linked to a group best described by which carachteristics? ",
                            "; 5) confidence interval of the frequency measure [low, high] (if reported) ; ",
                            " If one information is not available, write NA and separate each answer with a semicolon. Answer as shortly as possible."
                          ) ) )

  )

data4 <- data3c %>% group_by(PMID) %>%
  mutate(docRank = seq(1,n())) %>% ungroup() %>%
  mutate(doc_id = paste0(PMID, "_", docRank))

corp <- corpus(data4, text_field = "questionFullPhrase", docid_field = "doc_id")

corp2 <- corpus(data4, text_field = "questionFullAbstract", docid_field = "doc_id")

corp3 <- corpus(data4, text_field = "questionFullPhraseShort", docid_field = "doc_id")

corp4 <- corpus(data4, text_field = "questionFullPhraseShortNoSuggest", docid_field = "doc_id")

data5 <- data4 %>%
  mutate(nTokenfPhrase = ntoken(corp),
         nTokenAbstract = ntoken(corp2),
         nTokenShortFPhrase = ntoken(corp3),
         nTokenShortFPhraseNoSugg = ntoken(corp4),
         )

## to compare which one consumes less tokensfor GPT
sum(data5$nTokenfPhrase)
sum(data5$nTokenAbstract)
sum(data5$nTokenShortFPhrase)
sum(data5$nTokenShortFPhraseNoSugg)

## lets check one by one in the open AI website
data6 <- data5 %>% arrange(doc_id) %>%
  select(doc_id, PMID, TITLE, ABSTRACT,exposureSuggestion, populationSuggestion ,isVS,freqIsRange2,fullPhrase,keyword,questionFullPhrase , questionFullAbstract,
         questionFullPhraseShortNoSuggest)

write.csv(data6, "questionsCKD3.csv")


### need to check the filtering because in PMID 10414620 the VS keywords disappeared
