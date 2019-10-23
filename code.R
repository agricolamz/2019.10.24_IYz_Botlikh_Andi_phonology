setwd("/home/agricolamz/work/materials/2019.10.23-24_IJaz_Alekseev/prezi")
library(tidyverse)
library(ggforce)
library(extrafont)
loadfonts()
theme_set(theme_bw()+
            theme(text = element_text(size = 19, family = "Brill")))

# create qrcodes ----------------------------------------------------------
library(qrcode)
qrcode_gen("https://github.com/agricolamz/2019.10.24_IYz_Botlikh_Andi_phonology/raw/master/2019.10.24_IYz_Botlikh_Andi_phonology.pdf")
qrcode_gen("https://github.com/agricolamz/2019.10.24_IYz_Botlikh_Andi_phonology/raw/master/2019.10.24_IYz_Botlikh_Andi_phonology.pdf",
           wColor = "#0099CC", bColor = "white")



# create a map ------------------------------------------------------------

map <- read_csv("data/andi_botlikh_villages.csv")
map$eng[3] <- " "
map$eng[c(1, 2, 4, 7, 9, 10, 14, 16, 17)] <- paste(".      ", map$eng[c(1, 2, 4, 7, 9, 10, 14, 16, 17)])

library(lingtypology)
map.feature(map$language,
            features = map$language,
            latitude = map$lat,
            longitude = map$lon,
            label = map$eng,
            label.hide = FALSE,
            width = log(map$population)*1.5,
            tile = "Stamen.TerrainBackground",
            zoom.level = 11) ->
  m1

map.feature(map$language,
            latitude = map$lat,
            longitude = map$lon,
            label = map$eng,
            width = log(map$population)*2,
            tile = "Esri.WorldStreetMap",
            minimap = TRUE, 
            zoom.level = 5) ->
  m2

leafsync::latticeView(m1, m2)


# calculate amount of people ----------------------------------------------

map %>% 
  group_by(language) %>% 
  summarise(sum(population))

# create botlikh venn diagram ---------------------------------------------
botlikh <- read_csv("data/botlikh.csv")

botlikh %>% 
  count(bind_id) %>% 
  na.omit() %>% 
  count() %>% 
  summarise(n = n,
            sa = 8464 - n,
            aa = 6821 - n) %>% 
  pivot_longer(names_to = "type", values_to = "number", n:aa) %>% 
  mutate(prop = round(number/sum(number), 3),
         label = paste0(number, " words\n", prop*100, "%"),
         x = c(-0.04, 0.25, -0.3), 
         r = 0,
         y  = 0) ->
  values

data.frame(x = c(0.15, -0.15),
           y = 0,
           y2 = c(0.39, 0.31),
           r = c((0.45+0.24)/2, (0.31+0.24)/2),
           label = c('Saidova, Abusov 2012\n8464 words', 
                     'Alekseev, Azaev 2019\n6821 words')) %>% 
  ggplot(aes(x0 = x, y0 = y, r = r, label = label)) +
  geom_circle(aes(fill = label), alpha = .2, size = 1, show.legend = FALSE) +
  geom_text(aes(x=x*1.9, y2), family = "Brill", size = 5)+
  geom_text(data = values, aes(x = x, y = y, label = label), family = "Brill", size = 6)+
  coord_fixed()+
  theme_void()+
  ylim(-0.4, 0.4) ->
  venn

ggsave(filename = "images/03_venn.png", venn, device = "png", width = 6, height = 5)

# analyse phonology -------------------------------------------------------
stress <- "́"

botlikh %>% 
  group_by(bind_id, reference) %>% 
  mutate(n = n()) %>% 
  filter(n<2) %>% 
  select(bind_id, reference, lemma, text) %>%
  pivot_wider(names_from = reference, values_from = c(lemma, text)) %>% 
  na.omit() %>% 
  rename(lemma_alekseev_azaev = `lemma_Alekseev 2006`,
         lemma_saidova_abusov = `lemma_Saidova, Abusov 2012`,
         text_alekseev_azaev = `text_Alekseev 2006`,
         text_saidova_abusov = `text_Saidova, Abusov 2012`) %>% 
  ungroup() %>% 
  mutate(new_id = 1:n()) %>%   # filter(new_id == 488)
  #  pivot_longer(names_to = "reference", values_to = "lemma") %>%
  mutate(lemma_alekseev_azaev = str_replace_all(lemma_alekseev_azaev, "i", "I"),
         lemma_alekseev_azaev = str_replace_all(lemma_alekseev_azaev, "І", "I"),
         ipa_aa = str_remove_all(lemma_alekseev_azaev, " ?//.*"),
         ipa_aa = str_replace_all(ipa_aa, paste0("-а", stress, "ᴴ"), "-ʔ-a`-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("-а", "ᴴ"), "-ʔ-ã-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("-а", stress), "-ʔ-a`-"),
         ipa_aa = str_replace_all(ipa_aa, "-а:", "-ʔ-aː-"),
         ipa_aa = str_replace_all(ipa_aa, "-а", "-ʔ-a-"),
         ipa_aa = str_remove_all(ipa_aa, "[-/=\\\\]"),
         ipa_aa = str_replace_all(ipa_aa, "ʔa", "-ʔ-a-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("а", stress, "ᴴ"), "-ã`-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("а", "ᴴ"), "-ã-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("а", stress), "-a`-"),
         ipa_aa = str_replace_all(ipa_aa, "а", "-a-"),
         ipa_aa = str_replace_all(ipa_aa, "б", "-b-"),
         ipa_aa = str_replace_all(ipa_aa, "гъв", "-ʁʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "гъ", "-ʁ-"),
         ipa_aa = str_replace_all(ipa_aa, "гI", "-ʕ-"),
         ipa_aa = str_replace_all(ipa_aa, "гь", "-h-"),
         ipa_aa = str_replace_all(ipa_aa, "гь", "-h-"),
         ipa_aa = str_replace_all(ipa_aa, "гв", "-ɡʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "г", "-ɡ-"),
         ipa_aa = str_replace_all(ipa_aa, "дж", "-dʒ-"),
         ipa_aa = str_replace_all(ipa_aa, "д", "-d-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("е", stress, "ᴴ"), "-ẽ`-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("е", "ᴴ"), "-ẽ-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("е", stress), "-e`-"),
         ipa_aa = str_replace_all(ipa_aa, "е", "-e-"),
         ipa_aa = str_replace_all(ipa_aa, "ж", "-ʒ-"),
         ipa_aa = str_replace_all(ipa_aa, "зв", "-zʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "з", "-z-"),
         ipa_aa = str_replace_all(ipa_aa, "з", "-z-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("и", stress, "ᴴ"), "-ĩ`-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("и", "ᴴ"), "-ĩ-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("и", stress), "-i`-"),
         ipa_aa = str_replace_all(ipa_aa, "и", "-i-"),
         ipa_aa = str_replace_all(ipa_aa, "й", "-j-"),
         ipa_aa = str_replace_all(ipa_aa, "кIкIв", "-k'ːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "кькьв", "-tɬ'ːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "кIкI", "-k'ː-"),
         ipa_aa = str_replace_all(ipa_aa, "кькь", "-tɬ'ː-"),
         ipa_aa = str_replace_all(ipa_aa, "ккв", "-kːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "къв", "-qχ'ʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "кIв", "-k'ʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "кьв", "-tɬ'ʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "кк", "-kː-"),
         ipa_aa = str_replace_all(ipa_aa, "кI", "-k'-"),
         ipa_aa = str_replace_all(ipa_aa, "кв", "-kʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "къ", "-qχ'-"),
         ipa_aa = str_replace_all(ipa_aa, "кь", "-tɬ'-"),
         ipa_aa = str_replace_all(ipa_aa, "к", "-k-"),
         ipa_aa = str_replace_all(ipa_aa, "лълъв", "-ɬːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "лIлIв", "-tɬːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "лълъ", "-ɬː-"),
         ipa_aa = str_replace_all(ipa_aa, "лIлI", "-tɬː-"),
         ipa_aa = str_replace_all(ipa_aa, "лъв", "-ɬʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "лIв", "-tɬʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "лъ", "-ɬ-"),
         ipa_aa = str_replace_all(ipa_aa, "лI", "-tɬ-"),
         ipa_aa = str_replace_all(ipa_aa, "л", "-l-"),
         ipa_aa = str_replace_all(ipa_aa, "м", "-m-"),
         ipa_aa = str_replace_all(ipa_aa, "н", "-n-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("о", stress, "ᴴ"), "-õ`-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("о", "ᴴ"), "-õ-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("о", stress), "-o`-"),
         ipa_aa = str_replace_all(ipa_aa, "о", "-o-"),
         ipa_aa = str_replace_all(ipa_aa, "п", "-p-"),
         ipa_aa = str_replace_all(ipa_aa, "пI", "-p'-"),
         ipa_aa = str_replace_all(ipa_aa, "пп", "-pː-"),
         ipa_aa = str_replace_all(ipa_aa, "р", "-r-"),
         ipa_aa = str_replace_all(ipa_aa, "сс", "-sː-"),
         ipa_aa = str_replace_all(ipa_aa, "св", "-sʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "с", "-s-"), 
         ipa_aa = str_replace_all(ipa_aa, "тI", "-t'-"),
         ipa_aa = str_replace_all(ipa_aa, "тт", "-tː-"),
         ipa_aa = str_replace_all(ipa_aa, "т", "-t-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("у", stress, "ᴴ"), "-ũ'-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("у", "ᴴ"), "-ũ-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("у", stress), "-u`-"),
         ipa_aa = str_replace_all(ipa_aa, "у", "-u-"),
         ipa_aa = str_replace_all(ipa_aa, "ф", "-f-"),
         ipa_aa = str_replace_all(ipa_aa, "ххв", "-χːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "хьв", "-çʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "хъв", "-qχʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "хх", "-χː-"),
         ipa_aa = str_replace_all(ipa_aa, "хI", "-ħ-"),
         ipa_aa = str_replace_all(ipa_aa, "хъ", "-qχ-"),
         ipa_aa = str_replace_all(ipa_aa, "хь", "-ç-"),
         ipa_aa = str_replace_all(ipa_aa, "х", "-χ-"),
         ipa_aa = str_replace_all(ipa_aa, "цIцIв", "-ts'ːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "цIцI", "-ts'ː-"),
         ipa_aa = str_replace_all(ipa_aa, "цIв", "-ts'ʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "ццв", "-tsːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "цI", "-ts'-"),
         ipa_aa = str_replace_all(ipa_aa, "цц", "-tsː-"),
         ipa_aa = str_replace_all(ipa_aa, "цв", "-tsʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "ц", "-ts-"),
         ipa_aa = str_replace_all(ipa_aa, "чIчIв", "-tʃ'ːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "чIчI", "-tʃ'ː-"),
         ipa_aa = str_replace_all(ipa_aa, "чIв", "-tʃ'ʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "ччв", "-tʃːʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "чI", "-tʃ'-"),
         ipa_aa = str_replace_all(ipa_aa, "чч", "-tʃː-"),
         ipa_aa = str_replace_all(ipa_aa, "чв", "-tʃʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "ч", "-tʃ-"),
         ipa_aa = str_replace_all(ipa_aa, "ш", "-ʃ-"),
         ipa_aa = str_replace_all(ipa_aa, "щ", "-ʃː-"),
         ipa_aa = str_replace_all(ipa_aa, "ъв", "-ʔʷ-"),
         ipa_aa = str_replace_all(ipa_aa, "ъ", "-ʔ-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("э", stress, "ᴴ"), "-ʔ-ẽ`-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("э", "ᴴ"), "-ʔ-ẽ-"),
         ipa_aa = str_replace_all(ipa_aa, paste0("э", stress), "-ʔ-e`-"),
         ipa_aa = str_replace_all(ipa_aa, "э", "-ʔ-e-"),
         ipa_sa = str_replace_all(ipa_aa, "я", "-j-a-"),
         ipa_aa = str_replace_all(ipa_aa, "в", "-w-"),
         ipa_aa = str_replace_all(ipa_aa, "-:", "ː"),
         ipa_aa = str_replace_all(ipa_aa, "-{2,}", "-"),
         ipa_aa = str_replace_all(ipa_aa, "-\\(", "\\("),
         ipa_aa = str_replace_all(ipa_aa, "-\\)", "\\)"),
         ipa_aa = str_remove_all(ipa_aa, "^-"),
         ipa_aa = str_remove_all(ipa_aa, "^ʔ-"),
         ipa_aa = str_remove_all(ipa_aa, "-$"),
         lemma_saidova_abusov = str_replace_all(lemma_saidova_abusov, "i", "I"),
         lemma_saidova_abusov = str_replace_all(lemma_saidova_abusov, "І", "I"),
         ipa_sa = str_remove_all(lemma_saidova_abusov, " ?//.*"),
         ipa_sa = str_replace_all(ipa_sa, paste0("-а", stress, "ᴴ"), "-ʔ-a`-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("-а", "ᴴ"), "-ʔ-ã-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("-а", stress), "-ʔ-a`-"),
         ipa_sa = str_replace_all(ipa_sa, "-а:", "-ʔ-aː-"),
         ipa_sa = str_replace_all(ipa_sa, "-а", "-ʔ-a-"),
         ipa_sa = str_remove_all(ipa_sa, "[-/=\\\\]"),
         ipa_sa = str_replace_all(ipa_sa, "ʔa", "-ʔ-a-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("а", stress, "ᴴ"), "-ã`-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("а", "ᴴ"), "-ã-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("а", stress), "-a`-"),
         ipa_sa = str_replace_all(ipa_sa, "а", "-a-"),
         ipa_sa = str_replace_all(ipa_sa, "б", "-b-"),
         ipa_sa = str_replace_all(ipa_sa, "гъв", "-ʁʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "гъ", "-ʁ-"),
         ipa_sa = str_replace_all(ipa_sa, "гI", "-ʕ-"),
         ipa_sa = str_replace_all(ipa_sa, "гь", "-h-"),
         ipa_sa = str_replace_all(ipa_sa, "гь", "-h-"),
         ipa_sa = str_replace_all(ipa_sa, "гв", "-ɡʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "г", "-ɡ-"),
         ipa_sa = str_replace_all(ipa_sa, "дж", "-dʒ-"),
         ipa_sa = str_replace_all(ipa_sa, "д", "-d-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("е", stress, "ᴴ"), "-ẽ`-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("е", "ᴴ"), "-ẽ-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("е", stress), "-e`-"),
         ipa_sa = str_replace_all(ipa_sa, "е", "-e-"),
         ipa_sa = str_replace_all(ipa_sa, "ж", "-ʒ-"),
         ipa_sa = str_replace_all(ipa_sa, "зв", "-zʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "з", "-z-"),
         ipa_sa = str_replace_all(ipa_sa, "з", "-z-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("и", stress, "ᴴ"), "-ĩ`-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("и", "ᴴ"), "-ĩ-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("и", stress), "-i`-"),
         ipa_sa = str_replace_all(ipa_sa, "и", "-i-"),
         ipa_sa = str_replace_all(ipa_sa, "й", "-j-"),
         ipa_sa = str_replace_all(ipa_sa, "кIкIв", "-k'ːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "кькьв", "-tɬ'ːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "кIкI", "-k'ː-"),
         ipa_sa = str_replace_all(ipa_sa, "кькь", "-tɬ'ː-"),
         ipa_sa = str_replace_all(ipa_sa, "ккв", "-kːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "къв", "-qχ'ʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "кIв", "-k'ʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "кьв", "-tɬ'ʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "кк", "-kː-"),
         ipa_sa = str_replace_all(ipa_sa, "кI", "-k'-"),
         ipa_sa = str_replace_all(ipa_sa, "кв", "-kʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "къ", "-qχ'-"),
         ipa_sa = str_replace_all(ipa_sa, "кь", "-tɬ'-"),
         ipa_sa = str_replace_all(ipa_sa, "к", "-k-"),
         ipa_sa = str_replace_all(ipa_sa, "лълъв", "-ɬːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "лIлIв", "-tɬːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "лълъ", "-ɬː-"),
         ipa_sa = str_replace_all(ipa_sa, "лIлI", "-tɬː-"),
         ipa_sa = str_replace_all(ipa_sa, "лъв", "-ɬʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "лIв", "-tɬʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "лъ", "-ɬ-"),
         ipa_sa = str_replace_all(ipa_sa, "лI", "-tɬ-"),
         ipa_sa = str_replace_all(ipa_sa, "л", "-l-"),
         ipa_sa = str_replace_all(ipa_sa, "м", "-m-"),
         ipa_sa = str_replace_all(ipa_sa, "н", "-n-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("о", stress, "ᴴ"), "-õ`-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("о", "ᴴ"), "-õ-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("о", stress), "-o`-"),
         ipa_sa = str_replace_all(ipa_sa, "о", "-o-"),
         ipa_sa = str_replace_all(ipa_sa, "п", "-p-"),
         ipa_sa = str_replace_all(ipa_sa, "пI", "-p'-"),
         ipa_sa = str_replace_all(ipa_sa, "пп", "-pː-"),
         ipa_sa = str_replace_all(ipa_sa, "р", "-r-"),
         ipa_sa = str_replace_all(ipa_sa, "сс", "-sː-"),
         ipa_sa = str_replace_all(ipa_sa, "св", "-sʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "с", "-s-"), 
         ipa_sa = str_replace_all(ipa_sa, "тI", "-t'-"),
         ipa_sa = str_replace_all(ipa_sa, "тт", "-tː-"),
         ipa_sa = str_replace_all(ipa_sa, "т", "-t-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("у", stress, "ᴴ"), "-ũ'-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("у", "ᴴ"), "-ũ-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("у", stress), "-u`-"),
         ipa_sa = str_replace_all(ipa_sa, "у", "-u-"),
         ipa_sa = str_replace_all(ipa_sa, "ф", "-f-"),
         ipa_sa = str_replace_all(ipa_sa, "ххв", "-χːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "хьв", "-çʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "хъв", "-qχʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "хх", "-χː-"),
         ipa_sa = str_replace_all(ipa_sa, "хI", "-ħ-"),
         ipa_sa = str_replace_all(ipa_sa, "хъ", "-qχ-"),
         ipa_sa = str_replace_all(ipa_sa, "хь", "-ç-"),
         ipa_sa = str_replace_all(ipa_sa, "х", "-χ-"),
         ipa_sa = str_replace_all(ipa_sa, "цIцIв", "-ts'ːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "цIцI", "-ts'ː-"),
         ipa_sa = str_replace_all(ipa_sa, "цIв", "-ts'ʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "ццв", "-tsːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "цI", "-ts'-"),
         ipa_sa = str_replace_all(ipa_sa, "цц", "-tsː-"),
         ipa_sa = str_replace_all(ipa_sa, "цв", "-tsʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "ц", "-ts-"),
         ipa_sa = str_replace_all(ipa_sa, "чIчIв", "-tʃ'ːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "чIчI", "-tʃ'ː-"),
         ipa_sa = str_replace_all(ipa_sa, "чIв", "-tʃ'ʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "ччв", "-tʃːʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "чI", "-tʃ'-"),
         ipa_sa = str_replace_all(ipa_sa, "чч", "-tʃː-"),
         ipa_sa = str_replace_all(ipa_sa, "чв", "-tʃʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "ч", "-tʃ-"),
         ipa_sa = str_replace_all(ipa_sa, "ш", "-ʃ-"),
         ipa_sa = str_replace_all(ipa_sa, "щ", "-ʃː-"),
         ipa_sa = str_replace_all(ipa_sa, "ъв", "-ʔʷ-"),
         ipa_sa = str_replace_all(ipa_sa, "ъ", "-ʔ-"),
         ipa_sa = str_replace_all(ipa_sa, "я", "-j-a-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("э", stress, "ᴴ"), "-ʔ-ẽ`-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("э", "ᴴ"), "-ʔ-ẽ-"),
         ipa_sa = str_replace_all(ipa_sa, paste0("э", stress), "-ʔ-e`-"),
         ipa_sa = str_replace_all(ipa_sa, "э", "-ʔ-e-"),
         ipa_sa = str_replace_all(ipa_sa, "в", "-w-"),
         ipa_sa = str_replace_all(ipa_sa, "-:", "ː"),
         ipa_sa = str_replace_all(ipa_sa, "-{2,}", "-"),
         ipa_sa = str_replace_all(ipa_sa, "-\\(", "\\("),
         ipa_sa = str_replace_all(ipa_sa, "-\\)", "\\)"),
         ipa_sa = str_remove_all(ipa_sa, "^-"),
         ipa_sa = str_remove_all(ipa_sa, "^ʔ-"),
         ipa_sa = str_remove_all(ipa_sa, "-$"))  %>% 
  filter(!str_detect(ipa_aa, " "),
         !str_detect(ipa_sa, " ")) %>% 
  mutate(str_aa = str_detect(ipa_aa, "`"),
         str_sa = str_detect(ipa_sa, "`")) %>% 
  filter(str_aa, str_sa) %>% # remove rows with unstressed words
  mutate(ipa_aa = str_remove_all(ipa_aa, "`"),
        ipa_sa = str_remove_all(ipa_sa, "`")) %>%
  mutate(simmilar = ipa_aa == ipa_sa) %>% View()
  count(simmilar)


