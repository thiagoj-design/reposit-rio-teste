#COLOCANDO TUDO NO GIT HUB
repo <- "~/herbario-db"


git_config_global_set("user.name",  "thiago-design")
git_config_global_set("user.email", "thiagojcsgyn@gmail.com")


# 2. Colocar o CSV na pasta que o app espera
dir.create(file.path(repo, "dados"), showWarnings = FALSE)
file.copy("C:/Users/herbario-db/herbario.csv",
          file.path(repo, "dados", "herbario.csv"))

# 3. Primeiro commit
git_add("dados/herbario.csv", repo = repo)
git_commit("Adiciona o banco de dados do herbário", repo = repo)

# 4. Garantir que a branch se chama "main" e enviar para o GitHub
if (git_branch(repo = repo) != "main") {
  git_branch_move(git_branch(repo = repo), "main", repo = repo)
}
git_push(remote = "origin", set_upstream = TRUE, repo = repo)
































library(shiny)
library(gert)
library(readr)
library(dplyr)
library(pointblank)
library(uuid)


# ------------------------------------------------------------------------------
# Configurações
# ------------------------------------------------------------------------------

repo_path        <- "~/herbario-db"
csv_path         <- file.path(repo_path, "dados", "herbario.csv")
branch_principal <- "main"   # troque para "master" se for o caso do seu repositório

# Base local de nomes do Kew (WCVP), gerada pelo script atualizar_wcvp.R
wcvp_path <- "~/herbario-db/wcvp_lookup.rds"

# ------------------------------------------------------------------------------
# Conferência do nome científico na base local do Kew (WCVP)
# ------------------------------------------------------------------------------

if (!file.exists(wcvp_path)) {
  stop("Base de nomes do Kew não encontrada em ", path.expand(wcvp_path),
       ".\nRode o script atualizar_wcvp.R antes de abrir o app.", call. = FALSE)
}

wcvp        <- readRDS(wcvp_path)   # carregada uma vez, ao iniciar o app
wcvp_versao <- format(attr(wcvp, "data_download"), "%d/%m/%Y")

normalizar_nome <- function(x) {
  x <- tolower(gsub("\\s+", " ", trimws(x)))
  gsub("(^| )x ", "\\1× ", x)   # aceita "x" no lugar de "×" em híbridos
}

juntar_autor <- function(nome, autor) {
  trimws(paste(nome, ifelse(is.na(autor), "", autor)))
}

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
  chave      <- normalizar_nome(nome)
  encontrados <- wcvp[wcvp$chave == chave, ]
  
  if (nrow(encontrados) == 0) {
    return(list(status = "nao_encontrado", sugestoes = sugerir_nomes(chave)))
  }
  
  aceito <- encontrados[encontrados$status == "Accepted", ]
  if (nrow(aceito) > 0) {
    return(list(status = "aceito",
                nome   = aceito$nome[1],
                autor  = ifelse(is.na(aceito$autor[1]), "", aceito$autor[1])))
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

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Cadastro de espécime — Herbário"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("catalogNumber", "Número de tombo*", placeholder = "HERB-00231"),
      textInput("scientificName", "Nome científico*", placeholder = "Cattleya walkeriana"),
      textInput("recordedBy", "Coletor(a)*"),
      dateInput("eventDate", "Data de coleta*", value = Sys.Date(), format = "yyyy-mm-dd"),
      textInput("locality", "Localidade"),
      numericInput("decimalLatitude", "Latitude (decimal)", value = NA),
      numericInput("decimalLongitude", "Longitude (decimal)", value = NA),
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
  
  # Mostra os últimos registros já commitados, para conferência visual
  output$preview <- renderTable({
    input$submit  # recalcula depois de cada submissão
    if (!file.exists(csv_path)) return(NULL)
    dados <- read_csv(csv_path, col_types = cols(.default = "c"))
    tail(dados, 5)
  })
  
  output$status <- renderText(status_msg())
  
  observeEvent(input$submit, {
    
    # --- 1. Validação básica dos campos obrigatórios ---------------------
    obrigatorios <- c("catalogNumber", "scientificName", "recordedBy")
    faltando <- obrigatorios[sapply(obrigatorios, function(x) trimws(input[[x]]) == "")]
    
    if (length(faltando) > 0) {
      status_msg(paste("Erro: preencha os campos obrigatórios:",
                       paste(faltando, collapse = ", ")))
      return(invisible(NULL))
    }
    
    if (!is.na(input$decimalLatitude) &&
        (input$decimalLatitude < -90 || input$decimalLatitude > 90)) {
      status_msg("Erro: latitude fora do intervalo válido (-90 a 90).")
      return(invisible(NULL))
    }
    if (!is.na(input$decimalLongitude) &&
        (input$decimalLongitude < -180 || input$decimalLongitude > 180)) {
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
    
    # Usa a grafia exatamente como está no WCVP (maiúsculas, espaços, ×)
    nome_cientifico <- taxon$nome
    catalog_number  <- trimws(input$catalogNumber)
    
    etapa <- "início"
    
    tryCatch({
      
      etapa <- "sincronizar com o GitHub (checkout + pull)"
      # --- 3. Voltar para a branch principal e sincronizar ----------------
      # Garante que cada cadastro parte do estado atual da principal, e não
      # da branch do cadastro anterior.
      git_branch_checkout(branch_principal, repo = repo_path)
      git_pull(repo = repo_path)
      etapa <- "ler o CSV"
      dados <- read_csv(csv_path, col_types = cols(.default = "c"), na = c("", "NA"))
      
      # --- 4. Checar duplicidade de catalogNumber ------------------------
      if (catalog_number %in% dados$catalogNumber) {
        status_msg(paste("Erro: já existe um registro com o número de tombo",
                         catalog_number))
        return(invisible(NULL))
      }
      
      etapa <- "montar o novo registro"
      # --- 5. Montar o novo registro --------------------------------------
      novo_registro <- tibble::tibble(
        occurrenceID       = UUIDgenerate(),
        catalogNumber      = catalog_number,
        scientificName     = nome_cientifico,
        recordedBy         = input$recordedBy,
        eventDate          = format(input$eventDate, "%Y-%m-%d"),
        locality           = input$locality,
        decimalLatitude    = as.character(input$decimalLatitude),
        decimalLongitude   = as.character(input$decimalLongitude),
        basisOfRecord      = "PreservedSpecimen"
      )
      
      dados_atualizados <- bind_rows(dados, novo_registro) %>%
        arrange(catalogNumber)
      
      etapa <- "validar com pointblank"
      # --- 6. Validação formal com pointblank -----------------------------
      agente <- create_agent(tbl = dados_atualizados) %>%
        col_vals_not_null(vars(catalogNumber, scientificName)) %>%
        rows_distinct(vars(catalogNumber)) %>%
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
                   ifelse(vs$all_passed, "ok",
                          paste(vs$n_failed, "linha(s) com problema")))
          ), collapse = "\n"),
          error = function(e) "(não foi possível detalhar as checagens)"
        )
        status_msg(paste0(
          "Erro: validação de qualidade dos dados falhou. Registro não salvo.\n",
          detalhe, "\n",
          "Colunas do CSV: ", paste(names(dados), collapse = ", ")
        ))
        return(invisible(NULL))
      }
      
      etapa <- "criar a branch e salvar o CSV"
      # --- 7. Criar a branch do cadastro e salvar o CSV --------------------
      branch_nome <- paste0("cadastro-", catalog_number, "-",
                            format(Sys.time(), "%Y%m%d%H%M%S"))
      git_branch_create(branch_nome, ref = branch_principal, repo = repo_path)
      write_csv(dados_atualizados, csv_path, na = "")
      
      # --- 8. Commit + push (branch própria, para revisão via PR) ---------
      etapa <- "commit"
      git_add(files = file.path("dados", "herbario.csv"), repo = repo_path)
      git_commit(
        message = paste("Adiciona registro", catalog_number, "-", nome_cientifico),
        repo = repo_path
      )
      etapa <- "push para o GitHub"
      git_push(repo = repo_path, set_upstream = TRUE)
      
      status_msg(paste0(
        "Registro ", catalog_number, " salvo com sucesso!\n",
        "Nome conferido no WCVP: ", trimws(paste(taxon$nome, taxon$autor)), " (aceito)\n",
        "Branch enviada: ", branch_nome, "\n",
        "Abra um pull request no GitHub/GitLab para revisão e merge."
      ))
      
      # Limpa o formulário
      updateTextInput(session, "catalogNumber", value = "")
      updateTextInput(session, "scientificName", value = "")
      updateTextInput(session, "recordedBy", value = "")
      updateTextInput(session, "locality", value = "")
      updateNumericInput(session, "decimalLatitude", value = NA)
      updateNumericInput(session, "decimalLongitude", value = NA)
      
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
# - A conferência de nomes usa uma cópia local da WCVP (World Checklist of
#   Vascular Plants, Kew), gerada pelo script atualizar_wcvp.R. Só passa nome
#   idêntico a um nome aceito; sinônimos e nomes não encontrados bloqueiam o
#   cadastro e mostram a sugestão no Status. Rode o script de novo de tempos em
#   tempos para atualizar a taxonomia, e reinicie o app depois.
# - A WCVP cobre só plantas vasculares. Briófitas, fungos, líquens e algas não
#   estão na base e seriam recusados.
# - Cada submissão cria uma branch e um PR separados, sempre a partir da branch
#   principal. Enquanto um PR não for mergeado, o registro dele não aparece na
#   principal: a checagem de tombo duplicado e a tabela de "últimos registros"
#   só enxergam o que já foi mergeado.
# - Para publicar o app (shinyapps.io, Posit Connect, ou um servidor próprio),
#   as credenciais git (chave SSH ou token) precisam estar configuradas no
#   ambiente onde o app roda — não deixe token/senha hardcoded no código.
# - Se vários curadores usarem o app ao mesmo tempo no mesmo servidor, eles
#   compartilham o mesmo clone do repositório; duas submissões simultâneas
#   podem se atrapalhar. Nesse caso, oriente o usuário a tentar novamente.
# ==============================================================================