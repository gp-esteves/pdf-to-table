inst_dependencies <- function() {
  print("Checking for dependencies...")
  dep <- c("shiny", "shinythemes", "pdftools",
           "tidyverse", "stringr")
  needed <- setdiff(dep, rownames(installed.packages()))
  
  if (identical(needed, character(0)) == FALSE) {
    print("Installing necessary packages...")
    install.packages(needed)
  } else {
    print("Packages already installed.")
  }
}

clean_forearm <- function(raw) {
  patientName <- str_trim(str_extract_all(raw, "(?<=Nome: ).+(?=Sexo)"), side="both")
  examDate <- str_extract_all(raw, "(?<=Data da varredura: ).+(?= ID)")
  examScan <- str_trim(str_extract_all(raw, "(?<=Tipo de varredura:).+(?=\n)"))
  
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())
  raw <- reduce(raw, c)
  raw <- str_replace(raw, "Imagem não destinada a diagnóstico", "")
  
  table_start <- stringr::str_which(tolower(raw), "rádio")
  table_end <- stringr::str_which(tolower(raw), "total")
  table_end <- table_end[min(which(table_end > table_start))]
  
  table <- raw[(table_start):(table_end)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|")
  
  data_table <- data_table %>% 
    select(-1, -7, -8) %>% 
    slice(-1) %>%
    separate(CMO.DMO, c("CMO", "DMO"), " ")
  
  colnames(data_table) <- c("regiao", "area", "CMO", "DMO", "zscore", "AM")
  
  data_table <- data_table %>%
    mutate(paciente = patientName,
           data = examDate,
           varredura = examScan)
  
  data_table
}

clean_femur <- function(raw) {
  patientName <- str_trim(str_extract_all(raw, "(?<=Nome: ).+(?=Sexo)"), side="both")
  examDate <- str_extract_all(raw, "(?<=Data da varredura: ).+(?= ID)")
  examScan <- str_trim(str_extract_all(raw, "(?<=Tipo de varredura:).+(?=\n)"))
  
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())
  raw <- reduce(raw, c)
  raw <- str_replace(raw, "Imagem não destinada a diagnóstico", "")
  
  table_start <- stringr::str_which(tolower(raw), "região")
  table_end <- stringr::str_which(tolower(raw), "de ward")
  table_end <- table_end[min(which(table_end > table_start))]
  
  table <- raw[(table_start):(table_end)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|")
  
  data_table <- data_table |> 
    slice(2:5, 7) |> 
    mutate(`Área` = as.numeric(stringr::str_replace(`Área`, "de Ward", ""))) |> 
    separate(CMO.DMO, c("CMO", "DMO"), " ") |> 
    select(2:7)
  
  data_table[5,1] <- "Triangulo_Ward"
  
  colnames(data_table) <- c("regiao", "area", "CMO", "DMO", "zscore", "AM")
  
  data_table <- data_table %>%
    mutate(paciente = patientName,
           data = examDate,
           varredura = examScan)

  data_table
}

clean_spine <- function(raw) {
  patientName <- str_trim(str_extract_all(raw, "(?<=Nome: ).+(?=Sexo)"), side="both")
  examDate <- str_extract_all(raw, "(?<=Data da varredura: ).+(?= ID)")
  examScan <- str_trim(str_extract_all(raw, "(?<=Tipo de varredura:).+(?=\n)"))
  
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())
  raw <- reduce(raw, c)
  raw <- str_replace(raw, "Imagem não destinada a diagnóstico", "")
  
  table_start <- stringr::str_which(tolower(raw), "região")
  table_end <- stringr::str_which(tolower(raw), "total")
  table_end <- table_end[min(which(table_end > table_start))]
  
  table <- raw[(table_start):(table_end)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|")
  
  data_table <- data_table |>
    slice(-1) |>
    separate(CMO.DMO, c("CMO", "DMO"), " ") |>
    select(2:7)

  colnames(data_table) <- c("regiao", "area", "CMO", "DMO", "zscore", "AM")

  data_table <- data_table %>%
    mutate(paciente = patientName,
           data = examDate,
           varredura = examScan)
  
  data_table
}

clean_wb <- function(raw) {
  patientName <- str_trim(str_extract_all(raw, "(?<=Nome: ).+(?=Sexo)"), side="both")
  examDate <- str_extract_all(raw, "(?<=Data da varredura: ).+(?= ID)")
  examScan <- str_trim(str_extract_all(raw, "(?<=Tipo de varredura:).+(?=\n)"))
  
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())
  raw <- reduce(raw, c)
  raw <- str_replace(raw, "Imagem não destinada a diagnóstico", "")
  raw <- str_replace(raw, "318 x 150", "")
  raw <- str_replace(raw, "Resumo dos resultados de DXA:", "")
  
  table_start <- str_which(raw, "Região")
  table_end <- str_which(raw, "Total")
  table_end <- table_end[min(which(table_end > table_start))]
  
  table <- raw[(table_start):(table_end)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|")
  
  all <- data_table |> 
    slice(-1) |> 
    select(2:4)
  
  z = all[12,2]
  AM = all[12,3]
  
  all[12,2] = ""
  all[12,3] = ""
  
  data_table = as.data.frame(str_split_fixed(
    str_trim(apply(all, 1, paste, collapse=" ")), "\\s+(?=\\d)", 4)) |>
    mutate(zscore = ifelse(V1 == "Total", z, NA_real_),
           AM = ifelse(V1 == "Total", AM, NA_real_)) |> 
    `colnames<-`(c("Região", "Área", "CMO", "DMO", "zscore", "AM")) |> 
    mutate(paciente = patientName,
           data = examDate,
           varredura = examScan)
  
  data_table
}

clean_comp <- function(raw) {
  patientName <- str_trim(str_extract_all(raw, "(?<=Nome: ).+(?=Sexo)"), side="both")
  examDate <- str_extract_all(raw, "(?<=Data da varredura: ).+(?= ID)")
  examScan <- "Composição_Corporal_1"
  
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())
  raw <- reduce(raw, c)
  raw <- str_replace(raw, "Imagem não destinada a diagnóstico", "")
  raw <- str_replace(raw, "318 x 150", "")
  
  table_start <- str_which(raw, "Região")
  table_end <- str_which(raw, "Total")
  table_end <- table_end[min(which(table_end > table_start))]
  
  table <- raw[(table_start):(table_end)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|")

  data_table <- data_table |>
    slice(-1) |> 
    `colnames<-`(c("Região", "CMO", "Gordura", "Massa_magra", 
                   "Massa_magra_e_CMO", "Massa_total", "Porcentagem_gordura")) |> 
    mutate(paciente = patientName,
           data = examDate,
           varredura = examScan)
  
  data_table
}

clean_comp_2_t1 <- function(raw) {
  patientName <- str_trim(str_extract_all(raw, "(?<=Nome: ).+(?=Sexo)"), side="both")
  examDate <- str_trim(str_extract_all(raw, "(?<=Data da varredura: ).+(?= ID)"))
  examScan <- "Composição_Corporal_2"
  
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())
  raw <- reduce(raw, c)
  raw <- str_replace(raw, "Imagem não destinada a diagnóstico", "")
  raw <- str_replace(raw, "318 x 150", "")
  
  table_start <- str_which(raw, "Região")
  table_end <- str_which(raw, "Apêndice Massa magra/altura²")
  table_end <- table_end[min(which(table_end > table_start))]
  
  table <- raw[(table_start):(table_end)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  
  table_1 <- matrix()
  table_1[1] <- "|Região|Gordura|MassaMagra|MassaTotal|Gordura_Porcentagem|Percentil|AM"
  table_1[2] <- paste(sep="", "|", str_extract(table[3], "(?<= ).+(?=% de gordura corporal total)")) 
  table_1[3] <- paste(sep="", "|", str_extract(table[4], "(?<= ).+(?=Massa adiposa)"))
  table_1[4] <- paste(sep="", "|", str_extract(table[5], "(?<= ).+(?=Taxa androide)"))
  table_1[5] <- paste(sep="", "|", str_extract(table[6], "(?<= ).+(?=% gord. tronco)")) 
  table_1[6] <- paste(sep="", "|", str_extract(table[7], "(?<= ).+(?=Prop. massa gord.)")) 
  table_1[7] <- paste(sep="", "|", str_extract(table[8], "(?<= ).+(?=Massa de TAV estim)")) 
  table_1[8] <- paste(sep="", "|", str_extract(table[9], "(?<= ).+(?=Volume de TAV estim)")) 
  table_1[9] <- paste(sep="", "|", str_extract(table[10], "(?<= ).+(?=Área de TAV estim)"))
  table_1[10] <- paste("|", str_trim(table[11]), "|", sep="") 
  table_1[11] <- paste(sep="", "|",str_extract(table[12], "(?<= ).+(?=Índices de massa magra)")) 
  
  data_table_1 <- str_replace_all(table_1, "\\s{2,}", "|")
  data_table_1 <- read.csv(textConnection(table_1), sep = "|", row.names=NULL) |> 
    select(2:8) |> 
    `colnames<-`(c("Região", "Gordura", "Massa_magra", "Massa_total", 
                   "Porcentagem_gordura", "Percentil_YN", "Percentil_AM")) |> 
    mutate(paciente = patientName,
           data = examDate,
           varredura = examScan)
  
  data_table_1
}

clean_comp_2_t2 <- function(raw) {
  patientName <- str_trim(str_extract_all(raw, "(?<=Nome: ).+(?=Sexo)"), side="both")
  examDate <- str_trim(str_extract_all(raw, "(?<=Data da varredura: ).+(?= ID)"))
  examScan <- "Composição_Corporal_2_Indices"
  
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())
  raw <- reduce(raw, c)
  raw <- str_replace(raw, "Imagem não destinada a diagnóstico", "")
  raw <- str_replace(raw, "318 x 150", "")
  
  table_start <- str_which(raw, "Região")
  table_end <- str_which(raw, "Apêndice Massa magra/altura²")
  table_end <- table_end[min(which(table_end > table_start))]
  
  table <- raw[(table_start):(table_end)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  
  table_2 <- matrix()
  table_2[1] <- "Indice|Resultado|Percentil_YN|Percentil_AM"
  table_2[2] <- paste("% de", str_split_fixed(table[3], "% de ", n=2)[,2])
  table_2[3] <- paste("Massa adiposa/", str_split_fixed(table[4], "Massa adiposa/", n=2)[,2])
  table_2[4] <- paste("Taxa", str_split_fixed(table[5], "Taxa ", n=2)[,2])
  table_2[5] <- paste("% gord", str_split_fixed(table[6], "% gord. ", n=2)[,2])
  table_2[6] <- paste("prop", str_split_fixed(table[7], "Prop. ", n=2)[,2])
  table_2[7] <- paste("Massa de", str_split_fixed(table[8], "Massa de ", n=2)[,2])
  table_2[8] <- paste("Volume de", str_split_fixed(table[9], "Volume de ", n=2)[,2])
  table_2[9] <- paste("Área de", str_split_fixed(table[10], "Área de ", n=2)[,2])
  table_2[10] <- paste("Massa magra/altura²", str_split_fixed(table[15], "Massa magra/altura² ", n=2)[,2])
  table_2[11] <- paste("Apêndice Massa magra/altura²", str_split_fixed(table[16], "Apêndice Massa magra/altura² ", n=2)[,2])
  table_2 <- str_replace_all(table_2, "\\s{2,}", "|")
  
  data_table_2 <- read.csv(textConnection(table_2), sep = "|", row.names=NULL) |> 
    mutate(paciente = patientName,
           data = examDate,
           varredura = examScan)
  
  data_table_2
}

QA_tab <- tabPanel("Dúvidas frequentes",
         strong("Esse app parece muito legal, como ele funciona/foi feito?"),
         h5("Esse app utiliza código de R para receber o PDF, transformá-lo num arquivo de texto (string),
            e então usa palavras-chave para identificar o começo e final da tabela.
            São feitas mais algumas correções para que a tabela saia da forma correta!
            Esse app foi feito utilizando R Shiny, uma plataforma para criar programas facilmente
            utilizando R. Caso você tenha interesse em aprender mais, existem uma série de tutoriais online.
            Eu comecei com https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/."),
         br(),
         strong("Posso enviar um PDF inteiro com todas as diferentes varreduras (i.e., corpo inteiro, coluna, femur, etc.)?"),
         h5("Por enquanto, o app não tem a função de receber um PDF com todos as varreduras do DEXA e extrair
            cada tabela individualmente. Por enquanto, é necessário submeter apenas um tipo de exame por vez.
            Por isso, é recomendado que você salve cada varredura do seu exame de DXA separadamente (e.g., paciente_A_femur.pdf, paciente_A_coluna.pdf, etc.),
            e depois submeta aqui (você pode fazer isso com vários pacientes de uma vez, desde seja o mesmo tipo de exame!)."),
         br(),
         strong("Posso tirar uma foto ou escanear um exame impresso (e.g., Adobe ou Microsoft Lens) e então submeter o PDF aqui? 
                Ou então, PDFs convertidos para conter texto podem funcionar?"),
         h5("O app depende de exames importados diretamente pelos programas de DEXA, pois convertem o texto presente no PDF em string.
            A conversão de um exame impresso muitas vezes não faz uma conversão perfeita, e dessa forma, é muito provável que o app não consiga extrair corretamente a tabela.
            Você pode tentar, mas muito provavelmente não dará certo! Prefira emitir o PDF diretamente pelo software do computador que realiza o DEXA."),
         br(),
         strong("Parece que tem algo de errado com a minha tabela! O que eu faço?"),
         h5("É muito provavel que surjam alguns erros conforme mais exames diferentes sejam submetidos!
            Caso você tenha encontrado um erro que eu não encontrei, por favor me envie os detalhes para que eu possa arrumar (se eu conseguir!)."),
         br(),
         helpText("Caso tenha outras dúvidas, fique a vontade para me contatar: gabriel.perri.esteves@usp.br"))

####

