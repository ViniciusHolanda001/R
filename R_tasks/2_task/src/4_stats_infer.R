

dados <- read.csv('data/Banco de Dados 2.csv', sep = ";")


dados
View(dados)
glimpse(dados)



prop.table(table(dados$Genero))*100
prop.table(table(dados$Estuda))*100
prop.table(table(dados$Genero, dados$Estuda))*100


dados %>%
  ggplot(aes(Matematica))+
  geom_histogram(aes(y = stat(count) / sum(count)), bins = 20)+
  geom_density()+
  facet_wrap(~Estuda)
scale_y_continuous(labels = scales::percent)



dados %>%
  ggplot(aes(sample = Matematica))+
  facet_wrap(~Estuda)+
  stat_qq_band(fill = "gray")+
  stat_qq_line(col = "red")+
  stat_qq_point()

dados %>%
  group_by(Estuda) %>%
  summarise(Estatistica = shapiro.test(Portugues)$statistic,
            Valor_de_p = shapiro.test(Portugues)$p.value)




