setwd("D:\\OneDrive\\DEV\\cnpj_pi\\qsa_cnpj")


# os arquivos da receita federal são muito grandes. Até computadores mais potentes
# terão dificuldades de carregar e manipular os arquivos. Como queremos somente os dados 
# para o Piaui. Tivemos que encontrar uma alternativa.
# Com uma forma de minimizar a necessidade
# de potência, o código abaixo carrega os arquivos um por um e realizar a filtragem e 
# exclui da memória os arquivos quando não serão mais utilizados, dessa forma, podemos 
# trabalhar com a memória disponibilizada e as limitações que a máquina tem. 

# listando os arquicos com os estabelecimenstos
files = list.files(pattern = "ESTABELE")

# função para carregar todos os arquivos
load_files = function(i){
  df = read.csv(i, header = FALSE, sep = ";")
  return(df)
}

# lista para receber os arquivos 
list_estabelecimentos = list()

# construindo a função para carregar os arquivos na lista
for(i in files){
  list_estabelecimentos[[i]] =  load_files(i)
}

# Selecionando as empresas do Piaui de cada arquivo
df1 = list_estabelecimentos$K3241.K03200Y0.D21217.ESTABELE.csv|>
      dplyr::filter(V20 == "PI")

df2 = list_estabelecimentos$K3241.K03200Y1.D21217.ESTABELE.csv|>
  dplyr::filter(V20 == "PI")

df3 = list_estabelecimentos$K3241.K03200Y2.D21217.ESTABELE.csv|>
  dplyr::filter(V20 == "PI")

df4 = list_estabelecimentos$K3241.K03200Y3.D21217.ESTABELE.csv|>
dplyr::filter(V20 == "PI")
  
df5 = list_estabelecimentos$K3241.K03200Y4.D21217.ESTABELE.csv|>
dplyr::filter(V20 == "PI")

df6 = list_estabelecimentos$K3241.K03200Y5.D21217.ESTABELE.csv|>
dplyr::filter(V20 == "PI")

df7 = list_estabelecimentos$K3241.K03200Y6.D21217.ESTABELE.csv|>
dplyr::filter(V20 == "PI")

df8 = list_estabelecimentos$K3241.K03200Y7.D21217.ESTABELE.csv|>
dplyr::filter(V20 == "PI")

df9 = list_estabelecimentos$K3241.K03200Y8.D21217.ESTABELE.csv|>
dplyr::filter(V20 == "PI")

df10 = list_estabelecimentos$K3241.K03200Y9.D21217.ESTABELE.csv|>
dplyr::filter(V20 == "PI")

# jutando os arquivos
estabelecimentos = rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10)

# renomeando as variáveis
id_municipios = read.csv("id_municipios.csv")

estabelecimentos|>
  dplyr::rename(cnpj_basico = V1,
                cnpj_ordem = V2,
                cnpj_dv = V3,
                identificador_matriz_filial = V4,
                nome_fantasia = V5,
                situacao_cadastral = V6,
                data_situacao_cadastral = V7,
                motivo_situacao_cadastral = V8,
                nome_cidade_cadastral = V9,
                id_pais = V10,
                data_inicio_atividade = V11,
                cnae_fiscal_principal = V12,
                cnae_fiscal_secundararia = V13,
                tipo_logradouro = V14,
                logradouro = V15,
                numero = V16,
                complemento = V17,
                bairro = V18,
                cep = V19,
                sigla_uf = V20,
                id_municipio_rf = V21,
                ddd_1 = V22,
                telefone_1 = V23,
                ddd_2 = V24,
                telefone_2 = V25,
                ddd_fax = V26,
                fax = V27,
                email = V28,
                situacao_especial = V29,
                data_situacao_especial = V30)|>
  dplyr::left_join(id_municipios, by = "id_municipio_rf")|> #identificando os códigos do municipio IBGE
  dplyr::mutate(data_inicio_atividade = lubridate::ymd(data_inicio_atividade),# transformando as variáveis de data
                data_situacao_cadastral = lubridate::ymd(data_situacao_cadastral))|>
  write.csv("estabelecimentos_pi.csv")

# excluindo os arquios que não serão mais necessários. 
rm(list_estabelecimentos, df1, df2, df3, df4, df5, df6, df7, df8, df9, df10)

# Agora, carregando os arquivos de empresas
list_empresas <- list.files(pattern = "EMPRECSV")

# criando a lista para pegar os arquivos das empresas
empresas = list()

# construindo a função para carregar os arquivos na lista
for(i in list_empresas){
  empresas[[i]] =  load_files(i)
}

# retirando as listas dos cnpjs dos estabelecimento do Piaui
cnpj_basico = as.vector(estabelecimentos$V1)

# filtrando os arquivos para pegar somente as empresas do piaui
empresa_0 = subset(empresas$K3241.K03200Y0.D21217.EMPRECSV.csv, V1 %in% cnpj_basico)
empresa_1 = subset(empresas$K3241.K03200Y1.D21217.EMPRECSV.csv, V1 %in% cnpj_basico)
empresa_2 = subset(empresas$K3241.K03200Y2.D21217.EMPRECSV.csv, V1 %in% cnpj_basico)
empresa_3 = subset(empresas$K3241.K03200Y3.D21217.EMPRECSV.csv, V1 %in% cnpj_basico)
empresa_4 = subset(empresas$K3241.K03200Y4.D21217.EMPRECSV.csv, V1 %in% cnpj_basico)
empresa_5 = subset(empresas$K3241.K03200Y5.D21217.EMPRECSV.csv, V1 %in% cnpj_basico)
empresa_6 = subset(empresas$K3241.K03200Y6.D21217.EMPRECSV.csv, V1 %in% cnpj_basico)
empresa_7 = subset(empresas$K3241.K03200Y7.D21217.EMPRECSV.csv, V1 %in% cnpj_basico)
empresa_8 = subset(empresas$K3241.K03200Y8.D21217.EMPRECSV.csv, V1 %in% cnpj_basico)
empresa_9 = subset(empresas$K3241.K03200Y9.D21217.EMPRECSV.csv, V1 %in% cnpj_basico)

# juntando os arquivos
empresas_pi = rbind(empresa_0, empresa_1, empresa_2, empresa_3, empresa_4, empresa_5,
                    empresa_6, empresa_7, empresa_8, empresa_9)

# renomeando as variáveis salvando o arquivos
empresas_pi|>
  dplyr::rename(cnpj_basico = V1,
                razao_social = V2,
                qualificacao_responsavel = V4,
                natureza_juridica = V3,
                capital_social = V5,
                porte = V6,
                ente_federativo = V7)|>
  write.csv("empresas_pi.csv")

# exlcuindo os arquivos.
rm(empresas, empresa_0, empresa_1, empresa_2, empresa_3, empresa_4, empresa_5,
   empresa_6, empresa_7, empresa_8, empresa_9)