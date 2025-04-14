# Load libraries ===============================================================

library(readxl)
library(dplyr)
library(quanteda)
library(ggplot2)

# Read in data =================================================================

# Unfortunately due to copyright restrictions on abstracts we cannot share our
# full search results which are used below.

# Data was exported from WOS in Excel format. Others attempting to replicate
# our process will have to replicate our search strategy and download the
# search results to run our code.

# However we would still like to share our code to show how we processed and
# analysed the data, and so that others may use the code in similar projects.

# List all wos raw files
mylist <- list.files("wos_20240115", full.names = T)
# Read in all files
myfiles <- lapply(mylist, read_excel)
# Fomat as dataframe
wos <- do.call("rbind", myfiles)
# Remove any duplicate copies of records accidentally downloaded
wos <- unique(wos)
# Format column names
names(wos) <- gsub(" ", "", names(wos))
# Make sure publication year is from 1990 - 2023
wos <- wos %>%
  filter(PublicationYear >= 1990 & PublicationYear <= 2023)
# Filter out publications with no abstract
wos <- wos %>%
  filter(!is.na(Abstract))

# Save combined file
# Number of records (1990 - 2023) = 118,177
# Number of records (with abstract) = 98,678
write.csv(wos, "data/wos_search_results_combined_20240115.csv", row.names = F)

# Tokenise text by sentences ===================================================

# Read in combined data
wos <- read.csv("data/wos_search_results_combined_20240115.csv", 
                stringsAsFactors = F)
# Combine title and abstract text into a column called Text
wos <- wos %>% mutate(Text = paste(ArticleTitle," ",Abstract))
# Create subset of data on selected journals only
wos_selected_genetics <- wos %>% filter(SourceTitle == "NATURE GENETICS" |
                                 SourceTitle == "GENOME RESEARCH" |
                                 SourceTitle == "GENOME BIOLOGY" |
                                 SourceTitle == "NUCLEIC ACIDS RESEARCH" |
                                 SourceTitle == "CELL STEM CELL")
wos_selected_autism <- wos %>% filter(SourceTitle == "MOLECULAR AUTISM" |
                                        SourceTitle == "JOUNAL OF AUTISM AND DEVELOPMENTAL DISORDERS" |
                                        SourceTitle == "AUTISM" |
                                        SourceTitle == "AUTISM RESEARCH" |
                                        SourceTitle == "JOURNAL OF NEURODEVELOPMENTAL DISORDERS")
# Number of records from wos total = 98,678
# Number of records from selected journals = 4,852
# Number of records from selected journals (genetics) = 180
# Number of records from selected journals (autism) = 4,672
# Create corpi
corpus_wos <- quanteda::corpus(wos$Text, docnames = wos$UT.UniqueWOSID.)
corpus_genetics <- quanteda::corpus(wos_selected_genetics$Text, docnames = wos_selected_genetics$UT.UniqueWOSID.)
corpus_autism <- quanteda::corpus(wos_selected_autism$Text, docnames = wos_selected_autism$UT.UniqueWOSID.)
# Tokenize by sentence
tokens_wos <- quanteda::tokens(corpus_wos, what = "sentence", remove_punct = T)
tokens_genetics <- quanteda::tokens(corpus_genetics, what = "sentence", remove_punct = T)
tokens_autism <- quanteda::tokens(corpus_autism, what = "sentence", remove_punct = T)

# Run keyword in context to locate sentences ===================================

# KWIC / WOS / Neurodiversity
kwic_wos_neurodiversity <- quanteda::kwic(tokens_wos,
                                  pattern = "neuro(| |-)diver(se|sity|gent|gence|gency|gencies)",
                                  valuetype = c("regex"),
                                  case_insensitive = T)
# KWIC / Genetics / Neurodiversity
kwic_genetics_neurodiversity <- kwic(tokens_genetics, 
                                  pattern = "neuro(| |-)diver(se|sity|gent|gence|gency|gencies)", 
                                  valuetype = c("regex"),
                                  case_insensitive = T)
# KWIC / Autism / Neurodiversity
kwic_autism_neurodiversity <- kwic(tokens_autism, 
                                     pattern = "neuro(| |-)diver(se|sity|gent|gence|gency|gencies)", 
                                     valuetype = c("regex"),
                                     case_insensitive = T)
# KWIC / WOS / Identity First
kwic_wos_identityfirst <- kwic(tokens_wos, 
                                  pattern = "autistic (person|people|individual|population|group|sample|patient|paticipant|subject|woman|women|female|girl|man|men|male|boy|non-binary|nonbinary|infant|child|youth|teen|adult)", 
                                  valuetype = c("regex"),
                                  case_insensitive = T)
# KWIC / Genetics / Identity First
kwic_genetics_identityfirst <- kwic(tokens_genetics, 
                                     pattern = "autistic (person|people|individual|population|group|sample|patient|paticipant|subject|woman|women|female|girl|man|men|male|boy|non-binary|nonbinary|infant|child|youth|teen|adult)", 
                                     valuetype = c("regex"),
                                     case_insensitive = T)
# KWIC / Autism / Identity First
kwic_autism_identityfirst <- kwic(tokens_autism, 
                                    pattern = "autistic (person|people|individual|population|group|sample|patient|paticipant|subject|woman|women|female|girl|man|men|male|boy|non-binary|nonbinary|infant|child|youth|teen|adult)", 
                                    valuetype = c("regex"),
                                    case_insensitive = T)
# KWIC / WOS / Spectrum
kwic_wos_spectrum <- kwic(tokens_wos, 
                                 pattern = "on the (|autism |autistic )spectrum", 
                                 valuetype = c("regex"),
                                 case_insensitive = T)
# KWIC / Genetics / Spectrum
kwic_genetics_spectrum <- kwic(tokens_genetics, 
                                    pattern = "on the (|autism |autistic )spectrum", 
                                    valuetype = c("regex"),
                                    case_insensitive = T)
# KWIC / Autism / Spectrum
kwic_autism_spectrum <- kwic(tokens_autism, 
                               pattern = "on the (|autism |autistic )spectrum", 
                               valuetype = c("regex"),
                               case_insensitive = T)
# KWIC / WOS / Person First
kwic_wos_personfirst <- kwic(tokens_wos, 
                            pattern = "(person|people|individuals?|participants?|woman|women|females?|girls?|man|men|males?|boys?|infants?|child|children|youths?|teens?|teenagers?|adults?) (with|who has|who have) (autism|ASD)", 
                            valuetype = c("regex"),
                            case_insensitive = T)
# KWIC / Genetics / Person First
kwic_genetics_personfirst <- kwic(tokens_genetics, 
                               pattern = "(person|people|individuals?|participants?|woman|women|females?|girls?|man|men|males?|boys?|infants?|child|children|youths?|teens?|teenagers?|adults?) (with|who has|who have) (autism|ASD)", 
                               valuetype = c("regex"),
                               case_insensitive = T)
# KWIC / Autism / Person First
kwic_autism_personfirst <- kwic(tokens_autism, 
                                  pattern = "(person|people|individuals?|participants?|woman|women|females?|girls?|man|men|males?|boys?|infants?|child|children|youths?|teens?|teenagers?|adults?) (with|who has|who have) (autism|ASD)", 
                                  valuetype = c("regex"),
                                  case_insensitive = T)

# Save data
write.csv(kwic_wos_neurodiversity, "results/kwic_wos_neurodiversity.csv", row.names = F)
write.csv(kwic_genetics_neurodiversity, "results/kwic_genetics_neurodiversity.csv", row.names = F)
write.csv(kwic_autism_neurodiversity, "results/kwic_autism_neurodiversity.csv", row.names = F)

write.csv(kwic_wos_identityfirst, "results/kwic_wos_identityfirst.csv", row.names = F)
write.csv(kwic_genetics_identityfirst, "results/kwic_genetics_identityfirst.csv", row.names = F)
write.csv(kwic_autism_identityfirst, "results/kwic_autism_identityfirst.csv", row.names = F)

write.csv(kwic_wos_spectrum, "results/kwic_wos_spectrum.csv", row.names = F)
write.csv(kwic_genetics_spectrum, "results/kwic_genetics_spectrum.csv", row.names = F)
write.csv(kwic_autism_spectrum, "results/kwic_autism_spectrum.csv", row.names = F)

write.csv(kwic_wos_personfirst, "results/kwic_wos_personfirst.csv", row.names = F)
write.csv(kwic_genetics_personfirst, "results/kwic_genetics_personfirst.csv", row.names = F)
write.csv(kwic_autism_personfirst, "results/kwic_autism_personfirst.csv", row.names = F)


# Format for plotting ==========================================================

# Read back in data and format
# WOS / Neurodiversity
kwic_wos_neurodiversity <- read.csv("results/kwic_wos_neurodiversity.csv", stringsAsFactors = F) %>%
  select(docname, keyword) %>%
  rename(UT.UniqueWOSID. = docname) %>%
  left_join(wos, by = "UT.UniqueWOSID.") %>%
  select(docname = "UT.UniqueWOSID.", keyword, year = PublicationYear) %>%
  group_by(year) %>%
  summarise(num_docs = n_distinct(docname),
            num_keywords = n()) %>%
  mutate(keyword = "Neurodiversity")

# WOS/ Identity first
kwic_wos_identityfirst <- read.csv("results/kwic_wos_identityfirst.csv", stringsAsFactors = F) %>%
  select(docname, keyword) %>%
  rename(UT.UniqueWOSID. = docname) %>%
  left_join(wos, by = "UT.UniqueWOSID.") %>%
  select(docname = "UT.UniqueWOSID.", keyword, year = PublicationYear) %>%
  group_by(year) %>%
  summarise(num_docs = n_distinct(docname),
            num_keywords = n()) %>%
  mutate(keyword = "Identity First")

# WOS / Spectrum
kwic_wos_spectrum <- read.csv("results/kwic_wos_spectrum.csv", stringsAsFactors = F) %>%
  select(docname, keyword) %>%
  rename(UT.UniqueWOSID. = docname) %>%
  left_join(wos, by = "UT.UniqueWOSID.") %>%
  select(docname = "UT.UniqueWOSID.", keyword, year = PublicationYear) %>%
  group_by(year) %>%
  summarise(num_docs = n_distinct(docname),
            num_keywords = n()) %>%
  mutate(keyword = "On the Spectrum")

# WOS / Person first
kwic_wos_personfirst <- read.csv("results/kwic_wos_personfirst.csv", stringsAsFactors = F) %>%
  select(docname, keyword) %>%
  rename(UT.UniqueWOSID. = docname) %>%
  left_join(wos, by = "UT.UniqueWOSID.") %>%
  select(docname = "UT.UniqueWOSID.", keyword, year = PublicationYear) %>%
  group_by(year) %>%
  summarise(num_docs = n_distinct(docname),
            num_keywords = n()) %>%
  mutate(keyword = "Person First")

# Genetics / Neurodiversity # NO DATA
kwic_genetics_neurodiversity <- read.csv("results/kwic_genetics_neurodiversity.csv", stringsAsFactors = F)

# Genetics / Identity first
kwic_genetics_identityfirst <- read.csv("results/kwic_genetics_identityfirst.csv", stringsAsFactors = F) %>%
  select(docname, keyword) %>%
  rename(UT.UniqueWOSID. = docname) %>%
  left_join(wos_selected_genetics, by = "UT.UniqueWOSID.") %>%
  select(docname = "UT.UniqueWOSID.", keyword, year = PublicationYear) %>%
  group_by(year) %>%
  summarise(num_docs = n_distinct(docname),
            num_keywords = n()) %>%
  mutate(keyword = "Identity First")

# Genetics / Spectrum # NO DATA
kwic_genetics_spectrum <- read.csv("results/kwic_genetics_spectrum.csv", stringsAsFactors = F)

# Genetics / Person first
kwic_genetics_personfirst <- read.csv("results/kwic_genetics_personfirst.csv", stringsAsFactors = F) %>%
  select(docname, keyword) %>%
  rename(UT.UniqueWOSID. = docname) %>%
  left_join(wos_selected_genetics, by = "UT.UniqueWOSID.") %>%
  select(docname = "UT.UniqueWOSID.", keyword, year = PublicationYear) %>%
  group_by(year) %>%
  summarise(num_docs = n_distinct(docname),
            num_keywords = n()) %>%
  mutate(keyword = "Person First")

# Autism / Neurodiversity
kwic_autism_neurodiversity <- read.csv("results/kwic_autism_neurodiversity.csv", stringsAsFactors = F) %>%
  select(docname, keyword) %>%
  rename(UT.UniqueWOSID. = docname) %>%
  left_join(wos_selected_autism, by = "UT.UniqueWOSID.") %>%
  select(docname = "UT.UniqueWOSID.", keyword, year = PublicationYear) %>%
  group_by(year) %>%
  summarise(num_docs = n_distinct(docname),
            num_keywords = n()) %>%
  mutate(keyword = "Neurodiversity")

# Autism / Identity first
kwic_autism_identityfirst <- read.csv("results/kwic_autism_identityfirst.csv", stringsAsFactors = F) %>%
  select(docname, keyword) %>%
  rename(UT.UniqueWOSID. = docname) %>%
  left_join(wos_selected_autism, by = "UT.UniqueWOSID.") %>%
  select(docname = "UT.UniqueWOSID.", keyword, year = PublicationYear) %>%
  group_by(year) %>%
  summarise(num_docs = n_distinct(docname),
            num_keywords = n()) %>%
  mutate(keyword = "Identity First")

# Autism / Spectrum
kwic_autism_spectrum <- read.csv("results/kwic_autism_spectrum.csv", stringsAsFactors = F) %>%
  select(docname, keyword) %>%
  rename(UT.UniqueWOSID. = docname) %>%
  left_join(wos_selected_autism, by = "UT.UniqueWOSID.") %>%
  select(docname = "UT.UniqueWOSID.", keyword, year = PublicationYear) %>%
  group_by(year) %>%
  summarise(num_docs = n_distinct(docname),
            num_keywords = n()) %>%
  mutate(keyword = "On the Spectrum")

# Autism / Person first
kwic_autism_personfirst <- read.csv("results/kwic_autism_personfirst.csv", stringsAsFactors = F) %>%
  select(docname, keyword) %>%
  rename(UT.UniqueWOSID. = docname) %>%
  left_join(wos_selected_autism, by = "UT.UniqueWOSID.") %>%
  select(docname = "UT.UniqueWOSID.", keyword, year = PublicationYear) %>%
  group_by(year) %>%
  summarise(num_docs = n_distinct(docname),
            num_keywords = n()) %>%
  mutate(keyword = "Person First")

# Plot data ====================================================================

#Combine data
kwic_wos <- rbind(kwic_wos_neurodiversity, kwic_wos_identityfirst, kwic_wos_spectrum, kwic_wos_personfirst)
kwic_genetics <- rbind(kwic_genetics_identityfirst, kwic_genetics_personfirst)
kwic_autism <- rbind(kwic_autism_neurodiversity, kwic_autism_identityfirst, kwic_autism_spectrum, kwic_autism_personfirst)

# Plot line graph
num_wos_publications_line <- ggplot(kwic_wos, aes(x = year, y = num_docs, color = keyword)) +
  geom_line() +
  labs(title = "A",
       x = "Year",
       y = "Number of Publications Using Term",
       color = "Terms") +
  scale_color_manual(values = c("#490092","#b66dff","#006ddb","#6db6ff")) +
  theme_minimal()
# Plot bar graphs
num_wos_publications <- ggplot(kwic_wos, aes(x = year, y = num_docs, fill = keyword)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#490092","#b66dff","#006ddb","#6db6ff")) +
  labs(title = "B",
       x = "Year",
       y = "Number of Publications Using Term",
       fill = "Terms") +
  theme_minimal()
num_genetics_publications <- ggplot(kwic_genetics, aes(x = year, y = num_docs, fill = keyword)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#490092","#6db6ff")) +
  labs(title = "C",
       x = "Year",
       y = "Number of Publications Using Term",
       fill = "Terms") +
  theme_minimal()
num_autism_publications <- ggplot(kwic_autism, aes(x = year, y = num_docs, fill = keyword)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#490092","#b66dff","#006ddb","#6db6ff")) +
  labs(title = "D",
       x = "Year",
       y = "Number of Publications Using Term",
       fill = "Terms") +
  theme_minimal()

# Save plots
ggsave("figs/num_wos_line.jpeg", num_wos_publications_line)
ggsave("figs/num_wos.jpeg", num_wos_publications)
ggsave("figs/num_genetics.jpeg", num_genetics_publications)
ggsave("figs/num_autism.jpeg", num_autism_publications)
