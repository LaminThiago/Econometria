####
#AULA ESPECIFICAÇÃO DE MODELOS
#Uso de variáveis categóricas (dummies), transformações de variáveis
#e teste de hipótese

install.packages("AER")
library(AER)
data("CPS1988")

#descrição dos dados
#https://rdrr.io/cran/AER/man/CPS1988.html

summary(CPS1988)
#dados em cross section para determinantes de salários para março de
#1988 coletados pelo US Census Bureau.
View(CPS1988)
summary(CPS1988)
#wage: salário em dólares por semana
#educação: é medida em anos de escolaridade
#experiencia: também é medida em anos
#É obtida por meio da idade menos o tempo de escolaridade menos 6;
#Por isso, é possível ter valores negativos (foi construída)

#Variáveis dummies
#ethnicity (caucasian e african-American = afrodescendentes)
#smsa: indica se a residência pertence a uma região metropolitana 
#padrão dos EUA.
#região: indica a região em que o indivíduo reside
#northeast (região nordeste), midwest (centro-oeste), west (região oeste - California)
#south (sul)
#parttime: individuos trabalhando meio período

#Fazendo um modelo semilogaritmo ou loglinear

cps_lm = lm(log(wage) ~ experience + I(experience^2)+ education + ethnicity, data = CPS1988)
cps_lm2 = lm(log(wage) ~ experience + I(experience^2)+ education + ethnicity + smsa + parttime + region , data = CPS1988)

#Uso da função indicadora: é utilizada, pois o ":,*, ^" tem significados
#específicos dentro da função lm. 
#O uso da função indicadora faz com que o chapéu signifique elevar ao quadrado. 


cps_lm
summary(cps_lm)
summary(cps_lm2)

View(data)

#afrodescente está setado como 1
#Logo a diferença de salários se dá em relação aos afro-descendentes

#A experiencia influencia o salário até certo ponto depois tem uma 
#espécie de derivada negativa (parâmetro negativo)
#A partir de determinado ponto, a experiência não contribui mais positivamente

#Para ver o impacto da experiencia sobre o salário tem que somar os dois coeficientes
#soma dos dois coeficientes: 0.077-2(0.0013)*X
#como vimos em aula, agora o efeito marginal depende do X

#com 18 anos de experiência (media):
#0.077 -0,0468= 0,031 (3,1%)

#com 5 anos de experiência
#0,077 - 0,013 =0,064 (6,4%)

#com 25 anos
#0,077 - 0,065 = 0,012 (1,2%)

#o efeito marginal de exper em salário é positivo, porém decrescente.
#revelando uma curva com concavidade para baixo.

install.packages("stargazer")
library(stargazer)
stargazer(cps_lm, type = "text")

#para ver o efeito mg de educação: pega o coeficiente e multiplica por 100
# o impacto de um ano a mais de educação é de (0,086*100) um aumento de 8,6% 
#no salário semanal

#Para a variável de educação, temos que considerar o coeficiente multiplicado
#por 100. Logo 0.086*100 = 8.6%. Logo, o impacto de um ano a mais de estudo 
#é de um aumento de 8,6% sobre o salário semanal.

#Variável dummies: ethnicityafam representa que os afro-americanos
#ganham menos que os caucasianos (afrodescendentes é igual a 1)

#A interpretação correta de um coeficiente de uma variável dummy
#é (exp(Beta)-1)*100= -21,6%    

#Logo, fazendo esse cálculo temos que a mudança no salário semanal é 
#de -21,6% quando consideramos os afrodescentes em relação ao grupo
#dos caucasianos (chamado de grupo de controle).
#[Exp(-0,243)-1]*100=-21,6%
v1=(exp(-0.243)-1)*100
v1


#ADICIONANDO INTERAÇÕES

#Fazendo agora uma interação entre educação e a variável etnica
#Apenas colocando "education*ethnicity" estou adicionando: educação, etnia e a interação entre elas
#Só colocando o "*" adiciono 3 variáveis.
#Lembrar que não é o vezes que estamos acostumados. Para isso, 
#seria preciso colocar a variável indicadora

cps_lm3 = lm(log(wage) ~ experience + I(experience^2)+ education*ethnicity, data = CPS1988)
cps_lm3
stargazer(cps_lm3, type = "text")

#Efeito da Educação sobre o salário dos afroamericanos (não caucasianos) vai ser: 0.086 - 0.01
#0.086 - 0.01 = 0.076, aproximadamente, 7,6%

#Efeito da Educação para o salário dos brancos vai ser: 0.086
#0.086*100 = 8.6%

#Percebe-se que o retorno da educação sobre os salários é maior para
#os brancos.

#-0.124 = é a diferença salarial média entre afro-americanos e caucasianos
#ou seja, os primeiros ganham 12,4% menos que os segundos. Para o valor exato:
#nesse modelo (cps_lm3)
v2=(exp(-0.124)-1)*100
v2
#-11,6%

#É um valor de aproximadamente -11,7%

stargazer(cps_lm2, type = "text")
#quando incluo as outras variáveis binárias

#smsayes = pertencer à região metropolitanta
#aumenta o salário em 16,5%

#parttimeyes = trabalhar meio período
#diminui o salário 88,1%

#Na análise das regiões ele compara 
#midwest (centro-oeste), west (região oeste - California)
#south (sul) com região nordeste (northeast). 
#A região Nordeste, conforme definição do Departamento do Censo, 
#é a região mais rica dos Estados Unidos.
#A região foi responsável por aproximadamente 25% do produto interno bruto americano de 2007.
#As maiores cidades dessa região incluem Boston, Filadélfia, Nova Iorque e Pittsburgh.

#É possível perceber que pertencer à qualquer uma das 3 regiões impacta um 
#menor salário percentual EM RELAÇÃO à pertencer à região Nordeste.


#####################################################################
