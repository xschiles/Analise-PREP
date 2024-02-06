# Analise-PREP

## Introdução
Trabalho realizado no primeiro período da faculdade com o objetivo de analisar dados reais sobre uma forma de tratamento para o HIV.
O objetivo era entender principalmente o perfil dos pacientes, como as áreas em que habitavam e a distribuição de idades, além de traçar relações entre características individuais de cada paciente e observar possíveis padrões entre os dados. Para isso, foram feitos os seguintes questionamentos:

## Processos a serem analisados
-> 1 – Preciso que seja traçado um perfil dos participantes do estudo (quais características mais se destacam, por exemplo) em todas as variáveis.
-- Para isso, criamos tabelas de frequência e gráficos de uma variável para cada uma a fim de verificar o comportamento numérico de cada informação. Extraímos informações que mais chamavam atenção e comportamentos distoantes do padrão.

-> 2 – Preciso fazer o mesmo que o item anterior, mas traçar somente o perfil dos participantes de Salvador (quais características mais se destacam, por exemplo) em todas as variáveis.
-- Também analisamos com tabelas de frequência e plotagem de gráficos de barras simples. Aqui os dados se tornam mais escassos, então as análises tem uma proporção de mudança mais sensível do que quando analisados todas as cidades em conjunto.

-> 3 – Comparar descritivamente os dois perfis traçados. O paciente atendido em Salvador tem características semelhantes ao paciente atendido pelo estudo?
-- Neste tópico já foi possível observar a sensibilidade na mudança dos gráficos quando comparados.

-> 4 – Existe relação entre adesão e as variáveis descontinuou, população e qualidade_sono?
-- A partir daqui já realizamos análises bivariadas e plotamos gráficos mais complexos, além do cálculo de coeficientes como coeficientes de contingência, V de Cramer, Qui-Quadrado, R² (coeficiente de determinação) e coeficiente de correlação.

-> 5 – Existe relação entre município e as variáveis descontinuou, população, violencia_sexual e uso_camisinha?

-> 6 – Existe relação entre município e as variáveis idade, escolaridade, depressao e qualidade_sono?

-> 7 – Existe relação entre a idade e as escalas (depressão, qualidade do sono e ansiedade) consideradas no estudo?

-> 8 – Apresente medidas apropriadas para todas as variáveis quantitativas para HSH que usa camisinha raramente, HSH que usa camisinha ocasionalmente, HSH que usa camisinha sempre, MT que usa camisinha raramente, MT que usa camisinha ocasionalmente e MT que usa camisinha sempre.

-> 9 – Considerado somente os indivíduos de Salvador, você diria que existe relação entre a idade e as escalas (depressão, qualidade do sono e ansiedade) consideradas no estudo?

-> 10 – Considerando somente adolescentes com percepção de risco maior do que 5, existe relação entre uso_camisinha e sexo_grupo?

## Conclusão do projeto
Verificou-se que o município com mais candidatos no estudo foi São Paulo, seguido de Rio de Janeiro, Salvador e Belo Horizonte. Os pacientes se dividiam em majoritariamente, homens (212 homens) e 68 mulheres trans. Considerando toda a população do estudo, observou-se que 60% descontinuaram o tratamento. Um dado alarmante observado foi que 92,5% dos participantes afirmaram já ter sofrido algum tipo de violência sexual. 
Em Salvador, 50% dos pacientes descontinuaram o tratamento. Foi possível verificar que o perfil dos participantes de Salvador era bem semelhante ao perfil dos candidatos quando analisou-se todos os municípios juntos. 
Em geral, vemos que 77,5% dos pacientes aderiram ao tratamento no fim do estudo mas apenas 12,5% da população utilizou o PrEP alguma vez na vida após o contato com o vírus do HIV. Também foi possível verificar que a ansiedade e depressão estão fortemente relacionados à idade, enquanto a qualidade do sono não teve nenhuma relação com estas variáveis.

## Pacotes e conhecimentos utilizados
Ferramentas utilizadas: Excel e Rstudio. 
Foram feitas análises uni e bivariadas, plotagem de gráficos, criação de tabelas de frequência e cálculo de coeficientes estatísticos para verificar a relação entre diferentes tipos de variáveis usando pacotes dplyr, ggplot2, expss e scales.
No Excel, vimosos gráficos de forma mais direta e pudemos editar algumas colunas de forma que fossem importadas da forma correta para o RStudio.
No RStudio, utilizamos os pacotes readr e readxl para poder importar a base de dados. O pacote dplyr utilizamos para poder criar objetos apenas com as variáveis que escolhemos para direcionar as análises. Além disso, podemos utilizar par filtrar apenas por informações específicas e selecionar os dados excluindo células com dados faltantes (contendo NA ou NULL). ggplot2 foi utilizado com scales e expss para toda a parte de visualização de dados e verificação de correlação entre varíaveis. Foi específicamente utilizado para avaliar a existência ou não de relação, correlação e associação entre as diversas variáveis quantitativas e qualitativas.
