setwd("/home/agricolamz/work/materials/2019.10.23-24_IJaz_Alekseev/prezi")
library(tidyverse)
library(ggforce)
library(extrafont)
loadfonts()
theme_set(theme_bw()+
            theme(text = element_text(size = 19, family = "Brill")))

# read data ---------------------------------------------------------------

botlikh <- read_csv("data/botlikh.csv")


# create qrcodes ----------------------------------------------------------
library(qrcode)
qrcode_gen("https://github.com/agricolamz/2019.10.24_IYz_Botlikh_Andi_phonology/raw/master/2019.10.24_IYz_Botlikh_Andi_phonology.pdf")
qrcode_gen("https://github.com/agricolamz/2019.10.24_IYz_Botlikh_Andi_phonology/raw/master/2019.10.24_IYz_Botlikh_Andi_phonology.pdf",
           wColor = "#0099CC", bColor = "white")


# create botlikh venn diagram ---------------------------------------------

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
