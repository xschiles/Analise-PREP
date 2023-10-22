#Importando e fazendo os devidos tratamentos necessários na base de dados;
#ativando pacotes necessários pra a manipulação da base;
#modificando os valores das linhas 200 a 280 para um novo valor
#alterando os valores que as variáveis podem assumir 

setwd(dir = "d:\\Desktop\\script PREP")
library(readr) #pacote readr para realizar a importação da base de dados e tratamento de valores faltantes
library(dplyr) #pacote dplyr para seleção de linhas e colunas e para obtenção de medidas resumo 
library(ggplot2) #pacote ggplot2, para construção de gráficos
library(expss) #pacote que possibilita a construção de tabelas de frequencia e de dupla entrada

base_prep = read_delim(file = "base_PREp.txt", #read_delim porque o arquivo é em formato txt
                       na = c("88888","77777","1500","1900","10,5",
                              "10,4","10,2","10,1","10,9","-0,1","-0,2","-1,4"),
                       locale = locale(decimal_mark = ","))
                       
base_prep$municipio[201:280] = 4 #escolhemos 4 para posteriormente substituir o 4 por Rio de Janeiro

base_prep$municipio = factor(base_prep$municipio, #modificando valores que as variáveis irão assumir com a função factor
                             labels = c("Salvador","São Paulo","Belo Horizonte","Rio de Janeiro"))

base_prep$descontinuou = factor(base_prep$descontinuou,
                               labels = c("Não","Sim"))

base_prep$populacao = factor(base_prep$populacao,
                                labels = c("HSH","Mulher Trans"))

base_prep$discriminacao = factor(base_prep$discriminacao,
                                labels = c("Não","Sim"))

base_prep$descontinuou = factor(base_prep$descontinuou,
                                labels = c("Não","Sim"))

base_prep$mora_familia = factor(base_prep$mora_familia,
                                labels = c("Não","Sim"))

base_prep$violencia_sexual = factor(base_prep$violencia_sexual,
                                labels = c("Não","Sim"))

base_prep$uso_camisinha = factor(base_prep$uso_camisinha,
                                labels = c("Raramente","Ocasionalmente","Sempre"))

base_prep$pessoas_velhas = factor(base_prep$pessoas_velhas,
                                labels = c("Não","Sim"))

base_prep$sexo_grupo = factor(base_prep$sexo_grupo,
                                labels = c("Não","Sim"))

base_prep$teste_HIV = factor(base_prep$teste_HIV,
                                labels = c("Não","Sim"))

base_prep$usou_PEP = factor(base_prep$usou_PEP,
                                labels = c("Não","Sim"))

base_prep$adesao = factor(base_prep$adesao,
                          labels = c("Não","Sim"))

#=============================================================================================
#1 – Preciso que seja traçado um perfil dos participantes do estudo 
#(quais características mais se destacam, por exemplo) em todas as variáveis.

#Resumindo a base de dados para avaliação rápida
summary(base_prep)

#construindo tabelas de frequencia para cada variável
base_prep |> 
  select(municipio) |> 
  fre()

base_prep |> 
  select(descontinuou) |> 
  fre()

base_prep |> 
  select(populacao) |> 
  fre()

base_prep |> 
  ggplot(aes(x = populacao)) +
  geom_bar(fill = "black") +
  theme_classic()


base_prep |> 
  select(idade) |> 
  fre()

base_prep |> 
  select(escolaridade) |> 
  fre()

base_prep |> 
  select(discriminacao) |> 
  fre()

base_prep |> 
  select(mora_familia) |> 
  fre()

base_prep |> 
  select(violencia_sexual) |> 
  fre()

base_prep |> 
  select(uso_camisinha) |> 
  fre()

base_prep |> 
  select(pessoas_velhas) |> 
  fre()

base_prep |> 
  select(sexo_grupo) |> 
  fre()

base_prep |> 
  select(teste_HIV) |> 
  fre()

base_prep |> 
  select(usou_PEP) |> 
  fre()

base_prep |> 
  select(adesao) |> 
  fre()

#================================================================================================
#2 – Preciso fazer o mesmo que o item anterior, mas traçar somente o perfil dos 
#participantes de Salvador (quais características mais se destacam, por exemplo) em todas as variáveis.

#primeiramente criando uma sub-base apenas com os individos da cidade de Salvador 
Salvador = base_prep |> 
  filter(municipio == "Salvador")
#sumarizando a nova sub-base obtida para obtenção de algumas medidas resumo simples 
summary(Salvador)
#construindo tabelas de frequencia para a analise de cada variavel dos individuos de Salvador  
Salvador |> 
  select(descontinuou) |> 
  fre()

Salvador |> 
  select(populacao) |> 
  fre()

Salvador |> 
  select(idade) |> 
  fre()

Salvador |> 
  select(escolaridade) |> 
  fre()

Salvador |> 
  select(discriminacao) |> 
  fre()

Salvador |> 
  select(mora_familia) |> 
  fre()

Salvador |> 
  select(violencia_sexual) |> 
  fre()

Salvador |> 
  select(uso_camisinha) |> 
  fre()

Salvador |> 
  select(pessoas_velhas) |> 
  fre()

Salvador |> 
  select(sexo_grupo) |> 
  fre()

Salvador |> 
  select(teste_HIV) |> 
  fre()

Salvador |> 
  select(usou_PEP) |> 
  fre()

Salvador |> 
  select(adesao) |> 
  fre()

#aqui construimos um gráfico para analisar de forma visual o comportamento
#da variável uso_camisinha an cidade de Salvador, porque as três opcões que são
#raramente, ocasionalmente e sempre, são muito semelhantes graficamente e numericamente observando
Salvador |> 
  filter(!is.na(uso_camisinha)) |> 
  ggplot(mapping = aes(x = uso_camisinha)) +
  geom_bar(fill = "black",
           colour = "yellow") +
  labs(x = "usou camisinha?",
       y = "Contagem") +
  theme_dark()

#===============================================================================================
      #COMEÇANDO AS ANALISES DE ASSOCIAÇÃO, RELAÇÃO OU CORRELAÇÃO ENTRE AS VARIÁVEIS 
#===============================================================================================
# 4 – Existe relação entre adesão e as variáveis descontinuou, população e qualidade_sono?

#4.1 - verificando se há associação entre as variáveis adesão/descontinuou
base_prep |> #construindo uma tabela de dupla entrada com os valores das variaveis 
  select(adesao,descontinuou) |> 
  cross_cases(adesao,descontinuou)

base_prep |> #construindo um grafico para uma primeira analise visual de como se comportam os dados
  filter(!is.na(adesao)) |> 
  ggplot(mapping = aes(x = adesao, 
                              fill = descontinuou)) +
  geom_bar(position = "fill") +
  labs(x = "Adesão",
       y = "%",
       fill = "Descontinuou") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_dark()

#calculando o qui quadrado
qui4.1 = chisq.test(table(base_prep$adesao , base_prep$descontinuou),
                 correct = FALSE)
qui4.1$statistic 
##aqui estamos calculando o coeficiente de contingencia usando como n, 280, 
##já que é o numero de individuos sendo observados em toda a base 
C = sqrt(qui4.1$statistic/(qui4.1$statistic+280))
C
#calculando limite superior do coeficiente de contingência
LS1 = sqrt((2-1)/2)
LS1

#=================================================================================================
#4.2 - verificando se há relação entre as variáveis adesão e população
base_prep |> 
  select(adesao,populacao) |> 
  cross_cases(adesao,populacao)

base_prep |> 
  filter(!is.na(adesao)) |> 
  ggplot(mapping = aes(x = adesao,
                       fill = populacao)) +
  geom_bar(position = "fill") +
  labs(x = "Adesão",
       y = "%",
       fill = "População") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_dark()

qui4.2 = chisq.test(table(base_prep$adesao , base_prep$populacao),
                        correct = FALSE) #aqui estamos calculando o qui quadrado novamente para essa associação
qui4.2$statistic

C = sqrt(qui4.2$statistic/(qui4.2$statistic+280))
C
LS2 = sqrt((2-1)/2)
LS2

#=================================================================================================
#4.3 - verificando se há RELAÇÃO entre as variáveis adesão e qualidade_sono
base_prep |> 
  filter(!is.na(adesao)) |>
  ggplot(mapping = aes(x = adesao, 
                       y = qualidade_sono)) +
  geom_boxplot() + #usando boxplot, pois uma variável é qualitativa e outra quantitativa
  theme_dark()

#obtendo media, variancia, media ponderada e calculando o R² das variáveis,
#já que adesão é qualitativa e qualidade_sono e quantitativa 
tab4.3 = base_prep |> 
  filter(!is.na(adesao),
         !is.na(qualidade_sono)) |> 
  group_by(adesao) |> 
  summarise(media = mean(qualidade_sono , na.rm = TRUE),
            variancia = var(qualidade_sono , na.rm = TRUE),
            n = n())
tab4.3
#calculando a media ponderada entre as variãncias da qualidade de sono
Var_med4.3 = sum((tab4.3$variancia*tab4.3$n))/sum(tab4.3$n)
Var_med4.3
#calculando o coeficiente de determinação (R²)
R2 = 1 - Var_med4.3/var(base_prep$qualidade_sono, na.rm = TRUE) 
R2

#===========================================================================================
#5 – Existe relação entre município e as variáveis descontinuou, população, 
#violencia_sexual e uso_camisinha?
#===========================================================================================
#5.1 - municipio x descontinuou
base_prep |> 
  select(municipio,descontinuou) |> 
  cross_cases(municipio,descontinuou)

base_prep |> 
  ggplot(mapping = aes(x = municipio,
                       fill = descontinuou)) +
  geom_bar(position = "fill") +
  labs(x = "Município",
       y = "%",
       fill = "Descontinuou") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_dark()

#calculando o qui quadrado (as duas variáveis são qualitativas)
qui5.1 = chisq.test(table(base_prep$municipio , base_prep$descontinuou),
                  correct = FALSE)
qui5.1$statistic

V = sqrt((qui5.1$statistic)/280*(min(4-1,2-1)))
V

#==========================================================================================
#5.2 - municipio x população
base_prep |> 
  ggplot(mapping = aes(x = municipio,
                       fill = populacao)) +
  geom_bar(position = "fill") +
  labs(x = "Município",
       y = "%",
       fill = "População") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_dark()

qui5.2 = chisq.test(table(base_prep$municipio , base_prep$populacao),
                  correct = FALSE)
qui5.2$statistic

V = sqrt((qui5.2$statistic)/280*(min(4-1,2-1)))
V

#===========================================================================================
#5.3 - municipio x violencia sexual
base_prep |> 
  ggplot(mapping = aes(x = municipio,
                       fill = violencia_sexual)) +
  geom_bar(position = "fill") +
  labs(x = "Município",
       y = "%",
       fill = "Violẽncia Sexual") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_dark()

qui5.3 = chisq.test(table(base_prep$municipio , base_prep$violencia_sexual),
                  correct = FALSE)
qui5.3

V = sqrt((qui5.3$statistic)/280*(min(4-1,2-1)))
V

#============================================================================================
#5.4 - municipio x uso_camisinha
base_prep |>
  filter(!is.na(uso_camisinha)) |> 
  ggplot(mapping = aes(x = municipio,
                       fill = uso_camisinha)) +
  geom_bar(position = "fill") +
  labs(x = "Município",
       y = "%",
       fill = "Uso de camisinha") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_dark()

qui5.4 = chisq.test(table(base_prep$municipio , base_prep$uso_camisinha),
                  correct = FALSE)
qui5.4

V = sqrt((qui5.4$statistic)/280*(min(4-1,2-1)))
V

#==================================================================================================
#6 – Existe relação entre município e as variáveis idade, escolaridade, depressao e qualidade_sono?
#==================================================================================================
#6.1 - municipio x idade
tab6.1 = base_prep |> 
  group_by(municipio) |> 
  summarise(media = mean(idade , na.rm = TRUE),
            variancia = var(idade , na.rm = TRUE),
            n = n())

base_prep |> 
  ggplot(mapping = aes(x = municipio, 
                       y = idade)) +
  geom_boxplot() +
  theme_dark()

Var_med6.1 = sum((tab6.1$variancia*tab6.1$n))/sum(tab6.1$n)
Var_med6.1
R2 = 1 - Var_med6.1/var(base_prep$idade, na.rm = TRUE)
R2

#=====================================================================================
#6.2 - municipio x escolaridade
tab6.2 = base_prep |> 
  group_by(municipio) |> 
  summarise(media = mean(escolaridade , na.rm = TRUE),
            variancia = var(escolaridade , na.rm = TRUE),
            n = n())

base_prep |> 
  ggplot(mapping = aes(x = municipio, 
                       y = escolaridade)) +
  geom_boxplot() +
  theme_dark()

Var_med6.2 = sum((tab6.2$variancia*tab6.2$n))/sum(tab6.2$n)
Var_med6.2
R2 = 1 - Var_med6.2/var(base_prep$escolaridade, na.rm = TRUE)
R2

#=======================================================================================
#6.3 - municipio x depressão
tab6.3 = base_prep |> 
  group_by(municipio) |> 
  summarise(media = mean(depressao , na.rm = TRUE),
            variancia = var(depressao , na.rm = TRUE),
            n = n())

base_prep |> 
  ggplot(mapping = aes(x = municipio, 
                       y = depressao)) +
  geom_boxplot() +
  theme_dark()

Var_med6.3 = sum((tab6.3$variancia*tab6.3$n))/sum(tab6.3$n)
Var_med6.3
R2 = 1 - Var_med6.3/var(base_prep$depressao, na.rm = TRUE)
R2

#=====================================================================================
#6.4 - municipio x qualidade sono
tab6.4 = base_prep |> 
  group_by(municipio) |> 
  summarise(media = mean(qualidade_sono , na.rm = TRUE),
            variancia = var(qualidade_sono , na.rm = TRUE),
            n = n())

base_prep |> 
  ggplot(mapping = aes(x = municipio, 
                       y = qualidade_sono)) +
  geom_boxplot() +
  theme_dark()

Var_med6.4 = sum((tab6.4$variancia*tab6.4$n))/sum(tab6.4$n)
Var_med6.4
R2 = 1 - Var_med6.4/var(base_prep$qualidade_sono, na.rm = TRUE)
R2

#==============================================================================================
#7 – Existe relação entre a idade e as escalas (depressão, qualidade do sono e ansiedade) 
#consideradas no estudo?
#==============================================================================================
#7.1 - construindo gráfico de dispersão para as variáveis idade e depressão (duas variáveis quantitativas)
base_prep |> 
  ggplot(mapping = aes(x = idade,
                       y = depressao)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm", colour = "blue") +
  labs(x = "Idade",
       y = "Depressão") +
  theme_dark()

#calculando o coeficiente de correlação 
cor(base_prep$idade,
    base_prep$depressao,
    use = "na.or.complete") #usamos "na.or.complete" na funcao "use" para que ele não considere no calculo os NA's presentes

#7.2 - idade x qualidade sono
base_prep |> 
  ggplot(mapping = aes(x = idade,
                       y = qualidade_sono)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm", colour = "blue") +
  labs(x = "Idade",
       y = "Qualidade do sono") +
  theme_dark()

cor(base_prep$idade,
    base_prep$qualidade_sono,
    use = "na.or.complete")

#7.3 - idade x ansiedade
base_prep |> 
  ggplot(mapping = aes(x = idade,
                       y = ansiedade)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm", colour = "blue") +
  labs(x = "Idade",
       y = "Ansiedade") +
  theme_dark()

cor(base_prep$idade,
    base_prep$ansiedade,
    use = "na.or.complete")

#==========================================================================================
#8 – Apresente medidas apropriadas para todas as variáveis quantitativas para HSH_ 
#que usa camisinha raramente, HSH que usa camisinha ocasionalmente, HSH que usa camisinha_ 
#sempre, MT que usa camisinha raramente, MT que usa camisinha ocasionalmente e MT que usa camisinha sempre.
#===========================================================================================

#calcularemos como medidas apropriadas: media, mediana, quartil, variância, desvio padrão 
#e desvio absoluto mediano. As variáveis quantitativas observadas são: Idade, Escolaridade,
#Depressão, Ansiedade, Qualidade sono e Risco. Então calcularemos as medidas apropriadas em cada uma delas

#criando um objeto apenas com população HSH que usam camisinha raramente
HSHraramente = base_prep |> 
  filter(populacao == "HSH",
         uso_camisinha == "Raramente")

#criando um resumo rápido da base de dados
summary(HSHraramente)
#obtendo medidas resumo da variável idade
HSHraramente |>
  summarise(media = mean(idade, na.rm = TRUE),
            mediana = median(idade, na.rm = TRUE),
            quartil = quantile(idade, probs = .25, na.rm = TRUE),
            variancia = var(idade, na.rm = TRUE),
            desvio = sd(idade, na.rm = TRUE),
            dam = mad(idade, na.rm = TRUE))
#obtendo medida resumo da variável escolaridade
HSHraramente |>
  summarise(media = mean(escolaridade, na.rm = TRUE),
            mediana = median(escolaridade, na.rm = TRUE),
            quartil = quantile(escolaridade, probs = .25, na.rm = TRUE),
            variancia = var(escolaridade, na.rm = TRUE),
            desvio = sd(escolaridade, na.rm = TRUE),
            dam = mad(escolaridade, na.rm = TRUE)) 
#obtendo medida resumo da variável depre2ssão
HSHraramente |>
  summarise(media = mean(depressao, na.rm = TRUE),
            mediana = median(depressao, na.rm = TRUE),
            quartil = quantile(depressao, probs = .25, na.rm = TRUE),
            variancia = var(depressao, na.rm = TRUE),
            desvio = sd(depressao, na.rm = TRUE),
            dam = mad(depressao, na.rm = TRUE)) 
#obtendo medida resumo da variável ansiedade
HSHraramente |>
  summarise(media = mean(ansiedade, na.rm = TRUE),
            mediana = median(ansiedade, na.rm = TRUE),
            quartil = quantile(ansiedade, probs = .25, na.rm = TRUE),
            variancia = var(ansiedade, na.rm = TRUE),
            desvio = sd(ansiedade, na.rm = TRUE),
            dam = mad(ansiedade, na.rm = TRUE)) 
#obtendo medida resumo da variável qualidade_sono
HSHraramente |>
  summarise(media = mean(qualidade_sono, na.rm = TRUE),
            mediana = median(qualidade_sono, na.rm = TRUE),
            quartil = quantile(qualidade_sono, probs = .25, na.rm = TRUE),
            variancia = var(qualidade_sono, na.rm = TRUE),
            desvio = sd(qualidade_sono, na.rm = TRUE),
            dam = mad(qualidade_sono, na.rm = TRUE)) 

#=========================================================================================
#criando um objeto apenas com HSH que usam camisinha ocasionalmente
HSHocasionalmente = base_prep |> 
  filter(populacao == "HSH",
         uso_camisinha == "Ocasionalmente")

summary(HSHocasionalmente)

HSHocasionalmente |>
  summarise(media = mean(idade, na.rm = TRUE),
            mediana = median(idade, na.rm = TRUE),
            quartil = quantile(idade, probs = .25, na.rm = TRUE),
            variancia = var(idade, na.rm = TRUE),
            desvio = sd(idade, na.rm = TRUE),
            dam = mad(idade, na.rm = TRUE)) 

HSHocasionalmente |>
  summarise(media = mean(escolaridade, na.rm = TRUE),
            mediana = median(escolaridade, na.rm = TRUE),
            quartil = quantile(escolaridade, probs = .25, na.rm = TRUE),
            variancia = var(escolaridade, na.rm = TRUE),
            desvio = sd(escolaridade, na.rm = TRUE),
            dam = mad(escolaridade, na.rm = TRUE))

HSHocasionalmente |>
  summarise(media = mean(depressao, na.rm = TRUE),
            mediana = median(depressao, na.rm = TRUE),
            quartil = quantile(depressao, probs = .25, na.rm = TRUE),
            variancia = var(depressao, na.rm = TRUE),
            desvio = sd(depressao, na.rm = TRUE),
            dam = mad(depressao, na.rm = TRUE))

HSHocasionalmente |>
  summarise(media = mean(ansiedade, na.rm = TRUE),
            mediana = median(ansiedade, na.rm = TRUE),
            quartil = quantile(ansiedade, probs = .25, na.rm = TRUE),
            variancia = var(ansiedade, na.rm = TRUE),
            desvio = sd(ansiedade, na.rm = TRUE),
            dam = mad(ansiedade, na.rm = TRUE))

HSHocasionalmente |>
  summarise(media = mean(qualidade_sono, na.rm = TRUE),
            mediana = median(qualidade_sono, na.rm = TRUE),
            quartil = quantile(qualidade_sono, probs = .25, na.rm = TRUE),
            variancia = var(qualidade_sono, na.rm = TRUE),
            desvio = sd(qualidade_sono, na.rm = TRUE),
            dam = mad(qualidade_sono, na.rm = TRUE))

#criando um objeto apenas com HSH que usam camisinha sempre
HSHsempre = base_prep |> 
  filter(populacao == "HSH",
         uso_camisinha == "Sempre")

summary(HSHsempre)

HSHsempre |>
  summarise(media = mean(idade, na.rm = TRUE),
            mediana = median(idade, na.rm = TRUE),
            quartil = quantile(idade, probs = .25, na.rm = TRUE),
            variancia = var(idade, na.rm = TRUE),
            desvio = sd(idade, na.rm = TRUE),
            dam = mad(idade, na.rm = TRUE)) 

HSHsempre |>
  summarise(media = mean(escolaridade, na.rm = TRUE),
            mediana = median(escolaridade, na.rm = TRUE),
            quartil = quantile(escolaridade, probs = .25, na.rm = TRUE),
            variancia = var(escolaridade, na.rm = TRUE),
            desvio = sd(escolaridade, na.rm = TRUE),
            dam = mad(escolaridade, na.rm = TRUE))

HSHsempre |>
  summarise(media = mean(depressao, na.rm = TRUE),
            mediana = median(depressao, na.rm = TRUE),
            quartil = quantile(depressao, probs = .25, na.rm = TRUE),
            variancia = var(depressao, na.rm = TRUE),
            desvio = sd(depressao, na.rm = TRUE),
            dam = mad(depressao, na.rm = TRUE))

HSHsempre |>
  summarise(media = mean(ansiedade, na.rm = TRUE),
            mediana = median(ansiedade, na.rm = TRUE),
            quartil = quantile(ansiedade, probs = .25, na.rm = TRUE),
            variancia = var(ansiedade, na.rm = TRUE),
            desvio = sd(ansiedade, na.rm = TRUE),
            dam = mad(ansiedade, na.rm = TRUE))

HSHsempre |>
  summarise(media = mean(qualidade_sono, na.rm = TRUE),
            mediana = median(qualidade_sono, na.rm = TRUE),
            quartil = quantile(qualidade_sono, probs = .25, na.rm = TRUE),
            variancia = var(qualidade_sono, na.rm = TRUE),
            desvio = sd(qualidade_sono, na.rm = TRUE),
            dam = mad(qualidade_sono, na.rm = TRUE))

#aqui sairemos de população = HSM e obteremos as medidas apropriadas para população = Mulher Trans,
#e que usam camisinha raramente, ocasionalmente e sempre

#criando um objeto apenas com MT que usam camisinha raramente
MTraramente = base_prep |> 
  filter(populacao == "Mulher Trans",
         uso_camisinha == "Raramente")

summary(MTraramente)

#calculando medidas de cada variável quantitativa
MTraramente |>
  summarise(media = mean(idade, na.rm = TRUE),
            mediana = median(idade, na.rm = TRUE),
            quartil = quantile(idade, probs = .25, na.rm = TRUE),
            variancia = var(idade, na.rm = TRUE),
            desvio = sd(idade, na.rm = TRUE),
            dam = mad(idade, na.rm = TRUE)) 

MTraramente |>
  summarise(media = mean(escolaridade, na.rm = TRUE),
            mediana = median(escolaridade, na.rm = TRUE),
            quartil = quantile(escolaridade, probs = .25, na.rm = TRUE),
            variancia = var(escolaridade, na.rm = TRUE),
            desvio = sd(escolaridade, na.rm = TRUE),
            dam = mad(escolaridade, na.rm = TRUE))

MTraramente |>
  summarise(media = mean(depressao, na.rm = TRUE),
            mediana = median(depressao, na.rm = TRUE),
            quartil = quantile(depressao, probs = .25, na.rm = TRUE),
            variancia = var(depressao, na.rm = TRUE),
            desvio = sd(depressao, na.rm = TRUE),
            dam = mad(depressao, na.rm = TRUE))

MTraramente |>
  summarise(media = mean(ansiedade, na.rm = TRUE),
            mediana = median(ansiedade, na.rm = TRUE),
            quartil = quantile(ansiedade, probs = .25, na.rm = TRUE),
            variancia = var(ansiedade, na.rm = TRUE),
            desvio = sd(ansiedade, na.rm = TRUE),
            dam = mad(ansiedade, na.rm = TRUE))

MTraramente |>
  summarise(media = mean(qualidade_sono, na.rm = TRUE),
            mediana = median(qualidade_sono, na.rm = TRUE),
            quartil = quantile(qualidade_sono, probs = .25, na.rm = TRUE),
            variancia = var(qualidade_sono, na.rm = TRUE),
            desvio = sd(qualidade_sono, na.rm = TRUE),
            dam = mad(qualidade_sono, na.rm = TRUE))

#criando um objeto apenas com MT que usam camisinha ocasionalmente
MTocasionalmente = base_prep |> 
  filter(populacao == "Mulher Trans",
         uso_camisinha == "Ocasionalmente")

summary(MTocasionalmente)

MTocasionalmente |>
  summarise(media = mean(idade, na.rm = TRUE),
            mediana = median(idade, na.rm = TRUE),
            quartil = quantile(idade, probs = .25, na.rm = TRUE),
            variancia = var(idade, na.rm = TRUE),
            desvio = sd(idade, na.rm = TRUE),
            dam = mad(idade, na.rm = TRUE)) 

MTocasionalmente |>
  summarise(media = mean(escolaridade, na.rm = TRUE),
            mediana = median(escolaridade, na.rm = TRUE),
            quartil = quantile(escolaridade, probs = .25, na.rm = TRUE),
            variancia = var(escolaridade, na.rm = TRUE),
            desvio = sd(escolaridade, na.rm = TRUE),
            dam = mad(escolaridade, na.rm = TRUE))

MTocasionalmente |>
  summarise(media = mean(depressao, na.rm = TRUE),
            mediana = median(depressao, na.rm = TRUE),
            quartil = quantile(depressao, probs = .25, na.rm = TRUE),
            variancia = var(depressao, na.rm = TRUE),
            desvio = sd(depressao, na.rm = TRUE),
            dam = mad(depressao, na.rm = TRUE))

MTocasionalmente |>
  summarise(media = mean(ansiedade, na.rm = TRUE),
            mediana = median(ansiedade, na.rm = TRUE),
            quartil = quantile(ansiedade, probs = .25, na.rm = TRUE),
            variancia = var(ansiedade, na.rm = TRUE),
            desvio = sd(ansiedade, na.rm = TRUE),
            dam = mad(ansiedade, na.rm = TRUE))

MTocasionalmente |>
  summarise(media = mean(qualidade_sono, na.rm = TRUE),
            mediana = median(qualidade_sono, na.rm = TRUE),
            quartil = quantile(qualidade_sono, probs = .25, na.rm = TRUE),
            variancia = var(qualidade_sono, na.rm = TRUE),
            desvio = sd(qualidade_sono, na.rm = TRUE),
            dam = mad(qualidade_sono, na.rm = TRUE))

#criando um objeto apenas com MT que usam camisinha sempre
MTsempre = base_prep |> 
  filter(populacao == "Mulher Trans",
         uso_camisinha == "Sempre")

summary(MTsempre)

MTsempre |>
  summarise(media = mean(idade, na.rm = TRUE),
            mediana = median(idade, na.rm = TRUE),
            quartil = quantile(idade, probs = .25, na.rm = TRUE),
            variancia = var(idade, na.rm = TRUE),
            desvio = sd(idade, na.rm = TRUE),
            dam = mad(idade, na.rm = TRUE)) 
MTsempre |> 
  filter(!is.na(idade)) |> 
  ggplot(mapping = aes(x = idade)) +
  geom_bar(fill = "black") +
  labs(x = "Idades") +
  theme_classic()
  

MTsempre |>
  summarise(media = mean(escolaridade, na.rm = TRUE),
            mediana = median(escolaridade, na.rm = TRUE),
            quartil = quantile(escolaridade, probs = .25, na.rm = TRUE),
            variancia = var(escolaridade, na.rm = TRUE),
            desvio = sd(escolaridade, na.rm = TRUE),
            dam = mad(escolaridade, na.rm = TRUE))

MTsempre |> 
  filter(!is.na(escolaridade)) |> 
  ggplot(mapping = aes(x = escolaridade)) +
  geom_boxplot(fill = "red") +
  labs(x = "escolaridade") +
  theme_classic() +
  coord_flip()


MTsempre |>
  summarise(media = mean(depressao, na.rm = TRUE),
            mediana = median(depressao, na.rm = TRUE),
            quartil = quantile(depressao, probs = .25, na.rm = TRUE),
            variancia = var(depressao, na.rm = TRUE),
            desvio = sd(depressao, na.rm = TRUE),
            dam = mad(depressao, na.rm = TRUE))

MTsempre |> 
  filter(!is.na(depressao)) |> 
  ggplot(mapping = aes(x = depressao)) +
  geom_histogram(bins = 5,
                 fill = "black") +
  labs(x = "depressão") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic()

MTsempre |>
  summarise(media = mean(ansiedade, na.rm = TRUE),
            mediana = median(ansiedade, na.rm = TRUE),
            quartil = quantile(ansiedade, probs = .25, na.rm = TRUE),
            variancia = var(ansiedade, na.rm = TRUE),
            desvio = sd(ansiedade, na.rm = TRUE),
            dam = mad(ansiedade, na.rm = TRUE))

MTsempre |>
  summarise(media = mean(qualidade_sono, na.rm = TRUE),
            mediana = median(qualidade_sono, na.rm = TRUE),
            quartil = quantile(qualidade_sono, probs = .25, na.rm = TRUE),
            variancia = var(qualidade_sono, na.rm = TRUE),
            desvio = sd(qualidade_sono, na.rm = TRUE),
            dam = mad(qualidade_sono, na.rm = TRUE))

#==========================================================================================
#9 – Considerado somente os indivíduos de Salvador, você diria que existe relação entre a 
#idade e as escalas (depressão, qualidade do sono e ansiedade) consideradas no estudo?
#==========================================================================================
#9.1 - idade x depressão 
Salvador |> #utilizando a base já criada apenas com indivíduos de Salvador
  ggplot(mapping = aes(x = idade,
                       y = depressao)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm", colour = "blue") + #usando gráfico de pontos pois as duas variáveis são quantitativas
  labs(x = "idade",
       y = "depressão") +
  theme_dark()

cor(Salvador$idade, #coeficiente de correlação das variáveis
    Salvador$depressao,
    use = "na.or.complete")

#9.2 - idade x qualidade do sono
Salvador |> 
  ggplot(mapping = aes(x = idade,
                       y = qualidade_sono)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm", colour = "blue") +
  labs(x = "idade",
       y = "qualidade sono") +
  theme_dark()

cor(Salvador$idade,
    Salvador$qualidade_sono,
    use = "na.or.complete")

#9.3 - idade x ansiedade
Salvador |> 
  ggplot(mapping = aes(x = idade,
                       y = ansiedade)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm", colour = "blue") +
  labs(x = "idade",
       y = "ansiedade") +
  theme_dark()

cor(Salvador$idade,
    Salvador$ansiedade,
    use = "na.or.complete")

#============================================================================================
#10 – Considerando somente adolescentes com percepção de risco maior do que 5, 
#existe relação entre uso_camisinha e sexo_grupo?
#============================================================================================
#como são duas variáveis qualitativas então irei começar construindo um gráfico de barras empilhadas
#para analisar de forma visual como irão se comportar os dados
base_prep |> 
  filter(risco > 5,
         !is.na(uso_camisinha)) |> 
  ggplot(mapping = aes(x = uso_camisinha, 
                       fill = sexo_grupo)) +
  geom_bar(position = "fill") +
  labs(x = "Usa camisinha?",
       y = "%",
       fill = "fez sexo em grupo?") + 
  scale_y_continuous(labels = scales::percent_format())+
  theme_dark()

#aqui estou criando um objeto com as variáveis uso-camisinha, sexo_grupo,
#risco e idade. Nela irei selecionar apenas as linhas que tem risco maior que 5
sep = base_prep |>#nomeei como "sep", de "separado"
  filter(!is.na(uso_camisinha),
         risco > 5) #fiz as especificações de cada um dos valores que desejo que sejam filtrados para essa nova base de dados
         
sep |> 
  select(uso_camisinha,sexo_grupo) |> 
  cross_cases(uso_camisinha,sexo_grupo)

#aqui já estou calculando o qui quadrado desse objeto
qui10 = chisq.test(table(sep$uso_camisinha , sep$sexo_grupo),
                  correct = FALSE)
qui10

V = sqrt((qui10$statistic)/172*(min(3-1,2-1)))
V

#Fim :)