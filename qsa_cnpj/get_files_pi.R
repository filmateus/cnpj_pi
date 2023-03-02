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
df1 = list_estabelecimentos[[1]]|>
    dplyr::filter(V20 == "PI")

df2 = list_estabelecimentos[[2]]|>
    dplyr::filter(V20 == "PI")

df3 = list_estabelecimentos[[3]]|>
    dplyr::filter(V20 == "PI")

df4 = list_estabelecimentos[[4]]|>
    dplyr::filter(V20 == "PI")
  
df5 = list_estabelecimentos[[5]]|>
    dplyr::filter(V20 == "PI")

df6 = list_estabelecimentos[[6]]|>
    dplyr::filter(V20 == "PI")

df7 = list_estabelecimentos[[7]]|>
    dplyr::filter(V20 == "PI")

df8 = list_estabelecimentos[[8]]|>
    dplyr::filter(V20 == "PI")

df9 = list_estabelecimentos[[9]]|>
    dplyr::filter(V20 == "PI")

df10 = list_estabelecimentos[[10]]|>
    dplyr::filter(V20 == "PI")

# jutando os arquivos
estabelecimentos = rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10)

# renomeando as variáveis
id_municipios = read.csv("id_municipios.csv")

estabelecimentos = estabelecimentos|>
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
                data_situacao_cadastral = lubridate::ymd(data_situacao_cadastral))
  
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
empresa_1 = subset(empresas[[1]], V1 %in% cnpj_basico)
empresa_2 = subset(empresas[[2]], V1 %in% cnpj_basico)
empresa_3 = subset(empresas[[3]], V1 %in% cnpj_basico)
empresa_4 = subset(empresas[[4]], V1 %in% cnpj_basico)
empresa_5 = subset(empresas[[5]], V1 %in% cnpj_basico)
empresa_6 = subset(empresas[[6]], V1 %in% cnpj_basico)
empresa_7 = subset(empresas[[7]], V1 %in% cnpj_basico)
empresa_8 = subset(empresas[[8]], V1 %in% cnpj_basico)
empresa_9 = subset(empresas[[9]], V1 %in% cnpj_basico)
empresa_10 = subset(empresas[[10]], V1 %in% cnpj_basico)

# juntando os arquivos
empresas_pi = rbind(empresa_10, empresa_1, empresa_2, empresa_3, empresa_4, empresa_5,
                    empresa_6, empresa_7, empresa_8, empresa_9)

# renomeando as variáveis salvando o arquivos
empresas_pi =empresas_pi|>
  dplyr::rename(cnpj_basico = V1,
                razao_social = V2,
                qualificacao_responsavel = V4,
                natureza_juridica = V3,
                capital_social = V5,
                porte = V6,
                ente_federativo = V7)

# exlcuindo os arquivos.
rm(empresas, empresa_10, empresa_1, empresa_2, empresa_3, empresa_4, empresa_5,
   empresa_6, empresa_7, empresa_8, empresa_9)


nat_jud = qsacnpj::tab_natureza_juridica
nat_jud$cod_subclass_natureza_juridica = as.numeric(nat_jud$cod_subclass_natureza_juridica)

# salvando arquivo final com as informações que usados no dash
estabelecimentos|>
  dplyr::inner_join(empresas_pi, by = "cnpj_basico")|>
  dplyr::filter(identificador_matriz_filial == 1 &
                  situacao_cadastral == 2 | situacao_cadastral == 8)|>
  dplyr::left_join(nat_jud, 
                   by = c("natureza_juridica" = "cod_subclass_natureza_juridica"))|>
  dplyr::filter(nm_natureza_juridica %in% c("Entidades Empresariais"))|>
  dplyr::select(cnpj_basico, situacao_cadastral, id_municipio,  data_inicio_atividade,
                cnae_fiscal_principal, natureza_juridica, data_situacao_cadastral)|>
  write.csv("file_dash.csv")

