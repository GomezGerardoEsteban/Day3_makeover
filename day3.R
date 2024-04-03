
rm(list = ls())

library(tidyverse)
library(readxl)
library(ggtext)

# Descarga de todos los datos del banco mundial mediante API

# url <- "https://databank.worldbank.org/data/download/WDI_EXCEL.zip"
# 
# filename <- basename(url)
# 
# download.file(url = url,
#               filename = filename,
#               mode = "wb")
# 
# unzip(filename)

# Una vez extraida la base la guarde como "baseWDI.xlsx", por eso el levantado es a partir de ese nombre
# Si no modificas el nombre simplemente ejecuta 'base <- read_excel(filename)' para levantar la base

base <- read_excel("rmd/bases/baseWDI.xlsx")

cod_API_varible <- "EG.ELC.ACCS.RU.ZS" # Codigo API de la variable porcentaje de población rural con acceso a electricidad

# Vector con filtro de países de interes

country_code <- c("WLD",
                  "COL",
                  "PER",
                  "BRA",
                  "ECU",
                  "MEX",
                  "BOL",
                  "CHL",
                  "CRI",
                  "ARG",
                  "URY",
                  "PAR",
                  "VEN")


# Filtrado de base

base_rural <- base %>% 
  filter(IndicatorCode == cod_API_varible & CountryCode %in% country_code)

# base en formato largo

base_rural_L <- base_rural %>% 
  gather(key = year, value = elecRural, 5:length(base_rural))

base_rural_L <- base_rural_L %>% 
  mutate(year = as.numeric(year))

base_rural_L <- base_rural_L %>% 
  filter(year > 1999) %>% 
  group_by(year) %>% 
  mutate(Latam = mean(elecRural, na.rm = T))

grafico <- base_rural_L %>% 
  filter(CountryCode %in% c("BOL", "PER")) %>% 
  ggplot() +
  geom_line(mapping = aes(x = year, y = elecRural, color = CountryCode), show.legend = F) +
  geom_line(mapping = aes(x = year, y = Latam), color = "#11677A", ) +
  geom_point(mapping = aes(x = year, y = Latam), color = "#11677A", shape = 17, size = 2) +
  geom_point(mapping = aes(x = year, y = elecRural, color = CountryCode), show.legend = F) +
  scale_color_manual(values = c("#137A11", "#7A1121")) +
  scale_x_continuous(breaks = 2000:2021, limits = c(2000, 2021)) +
  labs(title = "Evolución de la electrificación rural en Latinoamérica",
       subtitle = "<span style='color:#137A11;'>**Bolivia**</span> <span style='color:#4B5253;'>y</span> <span style='color:#7A1121;'>**Perú**</span> <span style='color:#4B5253;'>los dos países andinos que cerraron la brecha frente al</span> <span style='color:#11677A;'>**Promedio Latinoamericano**</span>",
       y = "Porcentaje de población rural con acceso a electricidad",
       x = NULL,
       caption = "Fuente: World Development Indicators (WDI), World Bank <br> **#30DayChartChallenge #Day3** <br> **@GEstebanGomez**") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.0, size = 12, face = "bold"),
        plot.subtitle = element_markdown(hjust = 0.0, size = 11),
        plot.caption = element_markdown(size=8, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


ggsave(plot = grafico, filename = "../rmd/resultados/graficos/30DayChartChallenge/3Day_makeover.png",
       dpi = 300, width = 8.01, height = 4.81)


