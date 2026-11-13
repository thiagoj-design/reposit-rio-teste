# ==============================================================================
# atualizar_wcvp.R
#
# Prepara a base local de nomes do Kew (WCVP — World Checklist of Vascular
# Plants, a mesma base por trás do POWO) para o app do herbário conferir os
# nomes científicos sem depender de internet.
#
# Rode este script uma vez antes de usar o app, e de novo sempre que quiser
# atualizar a taxonomia (o Kew publica versões novas com frequência):
#
#   source("atualizar_wcvp.R")
#
# Para forçar uma versão nova, apague o arquivo wcvp.zip da pasta antes de rodar.
# ==============================================================================

library(readr)
library(dplyr)

# Mesma pasta do repositório configurada no app.R. Se existir uma subpasta
# "wcvp" dentro dela, os arquivos ficam lá; senão, na raiz do repositório.
repo_path <- "C:/Users/PC/Documents/GitHub/reposit-rio-teste"
wcvp_dir  <- if (dir.exists(file.path(repo_path, "wcvp"))) {
  file.path(repo_path, "wcvp")
} else {
  repo_path
}
wcvp_url <- "http://sftp.kew.org/pub/data-repositories/WCVP/wcvp.zip"

dir.create(wcvp_dir, showWarnings = FALSE, recursive = TRUE)
zip_path    <- file.path(wcvp_dir, "wcvp.zip")
lookup_path <- file.path(wcvp_dir, "wcvp_lookup.rds")

# --- 1. Baixar o arquivo do Kew (se ainda não estiver na pasta) ---------------
if (!file.exists(zip_path)) {
  message("Baixando a WCVP do Kew (arquivo grande, pode demorar alguns minutos)...")
  options(timeout = 3600)
  ok <- tryCatch({
    download.file(wcvp_url, zip_path, mode = "wb")
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  
  if (!ok || !file.exists(zip_path)) {
    unlink(zip_path)
    stop(
      "Não foi possível baixar automaticamente.\n",
      "Baixe pelo navegador: ", wcvp_url, "\n",
      "Salve o arquivo como: ", path.expand(zip_path), "\n",
      "e rode este script de novo.",
      call. = FALSE
    )
  }
}

# --- 2. Extrair e ler só as colunas necessárias --------------------------------
message("Lendo wcvp_names.csv...")
unzip(zip_path, files = "wcvp_names.csv", exdir = wcvp_dir)
csv_wcvp <- file.path(wcvp_dir, "wcvp_names.csv")

# O arquivo do Kew usa "|" como separador; confere no cabeçalho por garantia
cabecalho   <- readLines(csv_wcvp, n = 1, warn = FALSE)
delimitador <- if (grepl("|", cabecalho, fixed = TRUE)) "|" else ","

nomes <- read_delim(
  csv_wcvp,
  delim      = delimitador,
  quote      = "",
  col_types  = cols(.default = "c"),
  col_select = c(plant_name_id, taxon_status, family, taxon_name, taxon_authors,
                 accepted_plant_name_id),
  na         = c("", "NA")
)

# --- 3. Montar a tabela de consulta ---------------------------------------------
normalizar_nome <- function(x) {
  x <- tolower(gsub("\\s+", " ", trimws(x)))
  gsub("(^| )x ", "\\1× ", x)   # aceita "x" no lugar de "×" em híbridos
}

aceitos <- nomes %>%
  select(accepted_plant_name_id = plant_name_id,
         nome_aceito  = taxon_name,
         autor_aceito = taxon_authors)

lookup <- nomes %>%
  filter(!is.na(taxon_name)) %>%
  left_join(aceitos, by = "accepted_plant_name_id") %>%
  transmute(
    chave  = normalizar_nome(taxon_name),
    genero = sub(" .*$", "", sub("^× ", "", chave)),
    nome   = taxon_name,
    autor  = taxon_authors,
    status = taxon_status,
    familia = family,
    nome_aceito,
    autor_aceito
  )

attr(lookup, "data_download") <- as.Date(file.mtime(zip_path))

saveRDS(lookup, lookup_path)
unlink(csv_wcvp)   # o CSV extraído é grande e não é mais necessário

message("Pronto: ", format(nrow(lookup), big.mark = "."), " nomes salvos em ",
        path.expand(lookup_path))
print(count(lookup, status, sort = TRUE))


library(shiny)
library(gert)
library(readr)
library(dplyr)
library(pointblank)


# ------------------------------------------------------------------------------
# Configurações
# ------------------------------------------------------------------------------

repo_path        <- "C:/Users/PC/Documents/GitHub/reposit-rio-teste"
csv_nome         <- "herbario.csv"   # nome do arquivo, na raiz do repositório
csv_path         <- file.path(repo_path, csv_nome)
branch_principal <- "main"   # troque para "master" se for o caso do seu repositório

# Formato do número de tombo (coluna "barcode" do BRAHMS): SPSC + 6 dígitos
tombo_regex   <- "^SPSC[0-9]{6}$"
tombo_exemplo <- "SPSC059131"

# Base local de nomes do Kew (WCVP), gerada pelo script atualizar_wcvp.R.
# Fica dentro da pasta do repositório: na raiz ou numa subpasta "wcvp".
wcvp_candidatos <- c(file.path(repo_path, "wcvp_lookup.rds"),
                     file.path(repo_path, "wcvp", "wcvp_lookup.rds"))
wcvp_path <- c(wcvp_candidatos[file.exists(wcvp_candidatos)], wcvp_candidatos[1])[1]

# ------------------------------------------------------------------------------
# Leitura e gravação do banco (CSV no formato do BRAHMS: separador ";", UTF-8)
# ------------------------------------------------------------------------------

ler_banco <- function() {
  read_delim(csv_path, delim = ";", col_types = cols(.default = "c"),
             na = "", trim_ws = FALSE, progress = FALSE)
}

# Acrescenta UMA linha no fim do arquivo, sem reescrever as linhas existentes.
# Assim o pull request mostra só o registro novo, e os registros antigos ficam
# exatamente como estavam (acentos, aspas, quebras de linha).
# `registro` é um vetor nomeado; `colunas` é o cabeçalho do CSV, na ordem.
acrescentar_registro <- function(registro, colunas) {
  desconhecidas <- setdiff(names(registro), colunas)
  if (length(desconhecidas) > 0) {
    stop("o CSV não tem a(s) coluna(s): ", paste(desconhecidas, collapse = ", "))
  }
  
  valores <- setNames(rep("", length(colunas)), colunas)
  valores[names(registro)] <- ifelse(is.na(registro), "", registro)
  
  # Aspas só onde o CSV exige (campo com ; " ou quebra de linha)
  precisa_aspas <- grepl("[;\"\r\n]", valores)
  valores[precisa_aspas] <- paste0("\"", gsub("\"", "\"\"", valores[precisa_aspas]), "\"")
  linha <- paste(valores, collapse = ";")
  
  # Usa o mesmo fim de linha que o arquivo já tem (no Windows pode ser CRLF)
  bytes   <- readBin(csv_path, "raw", n = file.size(csv_path))
  fim     <- if (length(grepRaw("\r\n", bytes, fixed = TRUE)) > 0) "\r\n" else "\n"
  prefixo <- if (length(bytes) > 0 && bytes[length(bytes)] != as.raw(10)) fim else ""
  
  con <- file(csv_path, open = "ab")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(prefixo, linha, fim))), con)
}

# ------------------------------------------------------------------------------
# Conferência do nome científico na base local do Kew (WCVP)
# ------------------------------------------------------------------------------

if (!file.exists(wcvp_path)) {
  stop("Base de nomes do Kew (wcvp_lookup.rds) não encontrada em ", repo_path,
       ".\nRode o script atualizar_wcvp.R antes de abrir o app.", call. = FALSE)
}

wcvp        <- readRDS(wcvp_path)   # carregada uma vez, ao iniciar o app
wcvp_versao <- format(attr(wcvp, "data_download"), "%d/%m/%Y")

normalizar_nome <- function(x) {
  x <- tolower(gsub("\\s+", " ", trimws(x)))
  gsub("(^| )x ", "\\1× ", x)   # aceita "x" no lugar de "×" em híbridos
}

vazio_se_na <- function(x) ifelse(is.na(x), "", x)

juntar_autor <- function(nome, autor) trimws(paste(nome, vazio_se_na(autor)))

# Nomes parecidos, para ajudar em erro de digitação
sugerir_nomes <- function(chave, n = 5) {
  genero     <- sub(" .*$", "", sub("^× ", "", chave))
  candidatos <- wcvp[wcvp$genero == genero, ]
  
  # Gênero inexistente: procura gêneros de grafia parecida
  if (nrow(candidatos) == 0) {
    generos  <- unique(wcvp$genero)
    proximos <- generos[adist(genero, generos)[1, ] <= 2]
    candidatos <- wcvp[wcvp$genero %in% proximos, ]
    if (nrow(candidatos) == 0) return(character(0))
  }
  
  distancia  <- adist(chave, candidatos$chave)[1, ]
  candidatos <- candidatos[distancia <= 3, ]
  distancia  <- distancia[distancia <= 3]
  if (nrow(candidatos) == 0) return(character(0))
  
  # Mais parecidos primeiro; em empate, nomes aceitos na frente
  ordem <- order(distancia, candidatos$status != "Accepted")
  head(unique(candidatos$nome[ordem]), n)
}

# Devolve uma lista com:
#   status = "aceito"         -> nome aceito no WCVP
#   status = "sinonimo"       -> nome existe, mas não é o aceito; `aceitos` traz o(s) atual(is)
#   status = "sem_aceito"     -> nome existe, mas sem nome aceito associado (ex.: Unplaced)
#   status = "nao_encontrado" -> nome não existe no WCVP; `sugestoes` traz nomes parecidos
consultar_wcvp <- function(nome) {
  chave       <- normalizar_nome(nome)
  encontrados <- wcvp[wcvp$chave == chave, ]
  
  if (nrow(encontrados) == 0) {
    return(list(status = "nao_encontrado", sugestoes = sugerir_nomes(chave)))
  }
  
  aceito <- encontrados[encontrados$status == "Accepted", ]
  if (nrow(aceito) > 0) {
    return(list(
      status  = "aceito",
      nome    = aceito$nome[1],
      autor   = vazio_se_na(aceito$autor[1]),
      # a coluna "familia" só existe em bases geradas pela versão nova do script
      familia = if ("familia" %in% names(aceito)) vazio_se_na(aceito$familia[1]) else ""
    ))
  }
  
  com_aceito <- encontrados[!is.na(encontrados$nome_aceito), ]
  if (nrow(com_aceito) > 0) {
    return(list(
      status   = "sinonimo",
      situacao = unique(com_aceito$status),
      aceitos  = unique(juntar_autor(com_aceito$nome_aceito, com_aceito$autor_aceito))
    ))
  }
  
  list(status = "sem_aceito", situacao = unique(encontrados$status))
}

# Autor de um nome aceito (usado para o autor da espécie em nomes infraespecíficos)
autor_no_wcvp <- function(nome) {
  linhas <- wcvp[wcvp$chave == normalizar_nome(nome), ]
  if (nrow(linhas) == 0) return("")
  linhas <- linhas[order(linhas$status != "Accepted"), ]
  vazio_se_na(linhas$autor[1])
}

# Separa o nome conferido nas colunas do BRAHMS: genus, sp1, author1, rank1, sp2, author2
separar_nome <- function(taxon) {
  p <- strsplit(taxon$nome, " ", fixed = TRUE)[[1]]
  partes <- c(genus = p[1], sp1 = "", author1 = "", rank1 = "", sp2 = "", author2 = "")
  
  if (length(p) == 2) {
    # Gênero + espécie
    partes[["sp1"]]     <- p[2]
    partes[["author1"]] <- taxon$autor
  } else if (length(p) == 4 && grepl("\\.$", p[3])) {
    # Gênero + espécie + categoria (var., subsp., f.) + epíteto infraespecífico
    partes[["sp1"]]     <- p[2]
    partes[["author1"]] <- autor_no_wcvp(paste(p[1], p[2]))
    partes[["rank1"]]   <- p[3]
    partes[["sp2"]]     <- p[4]
    partes[["author2"]] <- taxon$autor
  } else if (length(p) > 1) {
    # Outros casos (ex.: híbridos com ×): tudo depois do gênero vai em sp1
    partes[["sp1"]]     <- paste(p[-1], collapse = " ")
    partes[["author1"]] <- taxon$autor
  }
  # Só o gênero (material identificado até gênero): sp1 e autores ficam vazios
  partes
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Cadastro de espécime — Herbário"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("barcode", "Número de tombo*", placeholder = tombo_exemplo),
      textInput("scientificName", "Nome científico*", placeholder = "Cattleya walkeriana"),
      textInput("collector", "Coletor(a)*"),
      dateInput("eventDate", "Data de coleta*", value = Sys.Date(), format = "dd/mm/yyyy",
                language = "pt-BR"),
      textInput("country", "País", value = "Brasil"),
      textInput("majorarea", "Estado"),
      textInput("minorarea", "Município"),
      textInput("gazetteer", "Localidade"),
      numericInput("lat", "Latitude (decimal, negativa no hemisfério sul)", value = NA),
      numericInput("long", "Longitude (decimal, negativa a oeste)", value = NA),
      tags$hr(),
      actionButton("submit", "Cadastrar registro", class = "btn-primary"),
      tags$p(tags$small("* campos obrigatórios")),
      tags$p(tags$small(paste("Nomes conferidos na WCVP (Kew), versão baixada em",
                              wcvp_versao)))
    ),
    
    mainPanel(
      h4("Status"),
      verbatimTextOutput("status"),
      tags$hr(),
      h4("Últimos registros no banco"),
      tableOutput("preview")
    )
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  status_msg <- reactiveVal("Aguardando cadastro...")
  
  # Mostra os últimos registros já na branch principal, para conferência visual
  output$preview <- renderTable({
    input$submit  # recalcula depois de cada submissão
    if (!file.exists(csv_path)) return(NULL)
    dados <- ler_banco()
    mostrar <- intersect(c("barcode", "family", "genus", "sp1", "collector",
                           "collyy", "minorarea"), names(dados))
    tail(dados[, mostrar], 5)
  })
  
  output$status <- renderText(status_msg())
  
  observeEvent(input$submit, {
    
    # --- 1. Validação básica dos campos ----------------------------------
    rotulos <- c(barcode = "Número de tombo", scientificName = "Nome científico",
                 collector = "Coletor(a)")
    faltando <- names(rotulos)[sapply(names(rotulos), function(x) trimws(input[[x]]) == "")]
    
    if (length(faltando) > 0) {
      status_msg(paste("Erro: preencha os campos obrigatórios:",
                       paste(rotulos[faltando], collapse = ", ")))
      return(invisible(NULL))
    }
    
    if (length(input$eventDate) == 0 || is.na(input$eventDate)) {
      status_msg("Erro: preencha a data de coleta.")
      return(invisible(NULL))
    }
    
    tombo <- toupper(trimws(input$barcode))
    if (!grepl(tombo_regex, tombo)) {
      status_msg(paste0("Erro: número de tombo fora do padrão. Use SPSC seguido de ",
                        "6 dígitos, por exemplo ", tombo_exemplo, "."))
      return(invisible(NULL))
    }
    
    lat  <- input$lat
    long <- input$long
    if (is.na(lat) != is.na(long)) {
      status_msg("Erro: preencha latitude e longitude juntas, ou deixe as duas em branco.")
      return(invisible(NULL))
    }
    if (!is.na(lat) && (lat < -90 || lat > 90)) {
      status_msg("Erro: latitude fora do intervalo válido (-90 a 90).")
      return(invisible(NULL))
    }
    if (!is.na(long) && (long < -180 || long > 180)) {
      status_msg("Erro: longitude fora do intervalo válido (-180 a 180).")
      return(invisible(NULL))
    }
    
    # --- 2. Conferir o nome científico na base do Kew (WCVP) --------------
    # Feito antes de mexer no repositório: se o nome não passar, nada é alterado.
    taxon <- consultar_wcvp(input$scientificName)
    
    if (taxon$status == "nao_encontrado") {
      msg <- paste0("Erro: o nome \"", input$scientificName,
                    "\" não foi encontrado na base do Kew (WCVP). Confira a grafia.")
      if (length(taxon$sugestoes) > 0) {
        msg <- paste0(msg, "\nNomes parecidos: ",
                      paste(taxon$sugestoes, collapse = "; "))
      }
      status_msg(msg)
      return(invisible(NULL))
    }
    
    if (taxon$status == "sinonimo") {
      status_msg(paste0(
        "Erro: \"", input$scientificName, "\" não é o nome aceito atualmente ",
        "(situação no WCVP: ", paste(taxon$situacao, collapse = ", "), ").\n",
        "Nome aceito: ", paste(taxon$aceitos, collapse = "; "), "\n",
        "Corrija o campo e cadastre novamente."
      ))
      return(invisible(NULL))
    }
    
    if (taxon$status == "sem_aceito") {
      status_msg(paste0(
        "Erro: \"", input$scientificName, "\" consta no WCVP como ",
        paste(taxon$situacao, collapse = ", "),
        ", sem nome aceito correspondente. Registro não salvo."
      ))
      return(invisible(NULL))
    }
    
    nome  <- separar_nome(taxon)
    etapa <- "início"
    
    tryCatch({
      
      etapa <- "sincronizar com o GitHub (checkout + pull)"
      # --- 3. Voltar para a branch principal e sincronizar ----------------
      # Garante que cada cadastro parte do estado atual da principal, e não
      # da branch do cadastro anterior.
      git_branch_checkout(branch_principal, repo = repo_path)
      git_pull(repo = repo_path)
      
      etapa <- "ler o CSV"
      dados <- ler_banco()
      
      # --- 4. Checar duplicidade do número de tombo ------------------------
      if (tombo %in% dados$barcode) {
        status_msg(paste("Erro: já existe um registro com o número de tombo", tombo))
        return(invisible(NULL))
      }
      
      etapa <- "montar o novo registro"
      # --- 5. Montar o novo registro (colunas do BRAHMS) -------------------
      # Família: a do WCVP; se a base local ainda não tiver essa coluna, usa a
      # família mais frequente desse gênero no próprio banco.
      familia <- taxon$familia
      if (familia == "") {
        do_banco <- dados$family[!is.na(dados$genus) & dados$genus == nome[["genus"]] &
                                   !is.na(dados$family)]
        if (length(do_banco) > 0) familia <- names(which.max(table(do_banco)))
      }
      
      data_coleta <- as.Date(input$eventDate)
      
      registro <- c(
        barcode   = tombo,
        collector = trimws(input$collector),
        colldd    = as.character(as.integer(format(data_coleta, "%d"))),
        collmm    = as.character(as.integer(format(data_coleta, "%m"))),
        collyy    = format(data_coleta, "%Y"),
        family    = familia,
        genus     = nome[["genus"]],
        sp1       = nome[["sp1"]],
        author1   = nome[["author1"]],
        rank1     = nome[["rank1"]],
        sp2       = nome[["sp2"]],
        author2   = nome[["author2"]],
        country   = trimws(input$country),
        majorarea = trimws(input$majorarea),
        minorarea = trimws(input$minorarea),
        gazetteer = trimws(input$gazetteer),
        lat       = if (is.na(lat))  "" else as.character(round(lat, 6)),
        ns        = if (is.na(lat))  "" else if (lat < 0)  "S" else "N",
        long      = if (is.na(long)) "" else as.character(round(long, 6)),
        ew        = if (is.na(long)) "" else if (long < 0) "W" else "E"
      )
      
      etapa <- "validar com pointblank"
      # --- 6. Validação formal do registro novo com pointblank -------------
      # Só o registro novo é validado: o banco antigo tem tombos repetidos e
      # em branco, que fariam qualquer cadastro falhar.
      novo_tbl <- tibble::as_tibble(as.list(registro)) %>%
        mutate(across(everything(), ~ na_if(.x, "")))
      
      agente <- create_agent(tbl = novo_tbl) %>%
        col_vals_not_null(vars(barcode, genus, collector, collyy)) %>%
        col_vals_regex(vars(barcode), regex = tombo_regex) %>%
        interrogate()
      
      # all_passed() devolve NA quando alguma checagem não pôde ser avaliada,
      # por isso o isTRUE(): só segue se todas passaram de fato.
      if (!isTRUE(all_passed(agente))) {
        vs <- agente$validation_set
        detalhe <- tryCatch(
          paste(sprintf(
            "- %s (%s): %s",
            vs$assertion_type,
            vapply(vs$column, function(x) paste(unlist(x), collapse = ", "), character(1)),
            ifelse(is.na(vs$all_passed), "não pôde ser avaliada",
                   ifelse(vs$all_passed, "ok", "com problema"))
          ), collapse = "\n"),
          error = function(e) "(não foi possível detalhar as checagens)"
        )
        status_msg(paste0(
          "Erro: validação de qualidade dos dados falhou. Registro não salvo.\n",
          detalhe
        ))
        return(invisible(NULL))
      }
      
      etapa <- "criar a branch e salvar o CSV"
      # --- 7. Criar a branch do cadastro e acrescentar a linha no CSV ------
      branch_nome <- paste0("cadastro-", tombo, "-", format(Sys.time(), "%Y%m%d%H%M%S"))
      git_branch_create(branch_nome, ref = branch_principal, repo = repo_path)
      acrescentar_registro(registro, names(dados))
      
      etapa <- "conferir o CSV gravado"
      # Relê o arquivo: tem que ter exatamente uma linha a mais, e ela tem que
      # ser o registro novo. Se não for, nada é enviado.
      conferido <- ler_banco()
      ultimo    <- conferido[nrow(conferido), ]
      if (nrow(conferido) != nrow(dados) + 1 ||
          !identical(names(conferido), names(dados)) ||
          !identical(ultimo$barcode, tombo) ||
          !identical(ultimo$genus, nome[["genus"]])) {
        stop("o arquivo gravado não bate com o esperado; registro descartado.")
      }
      
      # --- 8. Commit + push (branch própria, para revisão via PR) ---------
      etapa <- "commit"
      nome_completo <- trimws(paste(taxon$nome, taxon$autor))
      git_add(files = csv_nome, repo = repo_path)
      git_commit(
        message = paste("Adiciona registro", tombo, "-", taxon$nome),
        repo = repo_path
      )
      etapa <- "push para o GitHub"
      git_push(repo = repo_path, set_upstream = TRUE)
      
      status_msg(paste0(
        "Registro ", tombo, " salvo com sucesso!\n",
        "Nome conferido no WCVP: ", nome_completo, " (aceito)\n",
        "Família: ", if (familia == "") "(não preenchida)" else familia, "\n",
        "Branch enviada: ", branch_nome, "\n",
        "Abra um pull request no GitHub para revisão e merge."
      ))
      
      # Limpa o formulário (país fica, porque quase sempre se repete)
      updateTextInput(session, "barcode", value = "")
      updateTextInput(session, "scientificName", value = "")
      updateTextInput(session, "collector", value = "")
      updateTextInput(session, "majorarea", value = "")
      updateTextInput(session, "minorarea", value = "")
      updateTextInput(session, "gazetteer", value = "")
      updateNumericInput(session, "lat", value = NA)
      updateNumericInput(session, "long", value = NA)
      
    }, error = function(e) {
      status_msg(paste0("Erro inesperado na etapa \"", etapa, "\": ",
                        conditionMessage(e)))
    }, finally = {
      # Sempre volta para a branch principal, dando certo ou errado.
      # force = TRUE descarta um CSV gravado mas não commitado (ex.: falha no
      # meio do caminho), para o próximo cadastro começar de um estado limpo.
      try(git_branch_checkout(branch_principal, force = TRUE, repo = repo_path),
          silent = TRUE)
    })
  })
}

shinyApp(ui, server)

# ==============================================================================
# Observações:
# - O banco está no formato do BRAHMS (separador ";", UTF-8). O app preenche as
#   colunas barcode, collector, colldd/collmm/collyy, family, genus, sp1,
#   author1, rank1, sp2, author2, country, majorarea, minorarea, gazetteer,
#   lat, ns, long, ew. As demais ficam em branco no registro novo.
# - Cada cadastro acrescenta uma linha no fim do arquivo; as linhas antigas não
#   são reescritas.
# - A conferência de nomes usa uma cópia local da WCVP (World Checklist of
#   Vascular Plants, Kew), gerada pelo script atualizar_wcvp.R. Só passa nome
#   idêntico a um nome aceito (pode ser só o gênero); sinônimos e nomes não
#   encontrados bloqueiam o cadastro e mostram a sugestão no Status.
# - A WCVP cobre só plantas vasculares. Briófitas, fungos, líquens e algas não
#   estão na base e seriam recusados.
# - Cada submissão cria uma branch e um PR separados, sempre a partir da branch
#   principal. Enquanto um PR não for mergeado, o registro dele não aparece na
#   principal: a checagem de tombo duplicado e a tabela de "últimos registros"
#   só enxergam o que já foi mergeado.
# - Ao final de cada cadastro o app descarta alterações não commitadas em
#   arquivos do repositório. Faça commit das suas edições (inclusive deste
#   script e da base do Kew) antes de usar o formulário.
# ==============================================================================