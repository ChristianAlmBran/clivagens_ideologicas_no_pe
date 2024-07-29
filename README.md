# CLIVAGENS IDEOLÓGICAS NO PARLAMENTO EUROPEU: A POLITIZAÇÃO DA LEGISLAÇÃO AMBIENTAL, 2009-2022.

#### Prof. Dr. Rodrigo Barros de Albuquerque - Universidade Federal de Sergipe.

#### Me. Christian de Almeida Brandão - Universidade Federal de Pernambuco.

## Instruções para reprodução.

### Conteúdo de cada pasta:
- data_cleaning_and_preparation: Contém todos os scripts necessários para transformar os dados brutos dos bancos de dados nos dados que serão utilizados na análise.
- data_analysis: Contém todos os scripts necessários para realizar a regressão logística multinível, calcular a diferença das probabilidades previstas, calcular o coeficiente de correlação intraclasse e a razão de verossimilhança.
- visualization: Contém todos os scripts necessários para gerar o gráfico apresentado no artigo, bem como os gráficos dos modelos alternativos. Esta pasta também contém os gráficos em formato PNG e os arquivos CSV utilizados para gerá-los.

### Os arquivos da pasta "data_cleaning_and_preparation" devem ser executados na seguinte ordem:
1. "first_r_script"
2. "first_sql_script"
3. "second_r_script"
4. "second_sql_script"
5. "third_r_script"

### Nomenclatura dos scripts da pasta "data_analysis"
- arquivos iniciados com "mean" utilizam a média dos 10% mais extremos
- arquivos iniciados com "mlr" contém o código para realizar as regressões lógisticas multinível


