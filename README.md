# IDEOLOGICAL CLEAVAGES IN THE EUROPEAN PARLIAMENT: THE POLITICIZATION OF ENVIRONMENTAL LEGISLATION, 2009-2022.

#### Prof. Dr. Rodrigo Barros de Albuquerque - Federal University of Sergipe.

#### Prof. MSc. Christian de Almeida Brandão - Uninassau. PhD Candidate at Federal University of Pernambuco.

## Instructions for Reproduction.

## Contents of Each Folder:
- data_cleaning_and_preparation: Contains all scripts needed to transform the raw data from the databases into data used in the analysis.
- data_analysis: Contains all scripts needed to perform multilevel logistic regression, calculate the difference in predicted probabilities, calculate the intraclass correlation coefficient, and the likelihood ratio.
- visualization: Contains all scripts needed to generate the graph presented in the article, as well as the graphs for alternative models. This folder also contains the graphs in PNG format and the CSV files used to generate them.
  
### The files in the "data_cleaning_and_preparation" folder should be executed in the following order:
1. "first_r_script".
2. "first_sql_script".
3. "second_r_script".
4. "second_sql_script".
5. "third_r_script".

### Script Naming in the "data_analysis" Folder:
- Files starting with "mean" use the average of the 10% most extreme values.
- Files starting with "mlr" contain the code to perform multilevel logistic regressions.

# CLIVAGENS IDEOLÓGICAS NO PARLAMENTO EUROPEU: A POLITIZAÇÃO DA LEGISLAÇÃO AMBIENTAL, 2009-2022.

#### Prof. Dr. Rodrigo Barros de Albuquerque - Universidade Federal de Sergipe.

#### Prof. Me. Christian de Almeida Brandão - Uninassau. Doutorando - Universidade Federal de Pernambuco.

## Instruções para reprodução.

### Conteúdo de cada pasta:
- data_cleaning_and_preparation: Contém todos os scripts necessários para transformar os dados brutos dos bancos de dados nos dados que serão utilizados na análise.
- data_analysis: Contém todos os scripts necessários para realizar a regressão logística multinível, calcular a diferença das probabilidades previstas, calcular o coeficiente de correlação intraclasse e a razão de verossimilhança.
- visualization: Contém todos os scripts necessários para gerar o gráfico apresentado no artigo, bem como os gráficos dos modelos alternativos. Esta pasta também contém os gráficos em formato PNG e os arquivos CSV utilizados para gerá-los.

### Os arquivos da pasta "data_cleaning_and_preparation" devem ser executados na seguinte ordem:
1. "first_r_script".
2. "first_sql_script".
3. "second_r_script".
4. "second_sql_script".
5. "third_r_script".

### Nomenclatura dos scripts da pasta "data_analysis"
- arquivos iniciados com "mean" utilizam a média dos 10% mais extremos.
- arquivos iniciados com "mlr" contém o código para realizar as regressões lógisticas multinível.
