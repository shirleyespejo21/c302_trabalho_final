# =====================================================================
# PÁGINA ALIMENTOS —  heatmap, guerras e ranking
# =====================================================================
# Convertido de: codigo_principal_alimentos.R + shiny_alimentos.R
# Molde seguido: R/20_alimentos.R
#
# Para usar: salve como R/21_alimentos.R
# Precisa em R/00_pacotes.R:  library(DT)  e  library(plotly)
#
# REGRA: tudo termina em _alimentos (objetos, inputs e outputs),
# para NÃO colidir com o que já existe em R/20_alimentos.R.
# Este arquivo é autônomo: não depende de nada do 20, só de
#   food_beverages (R/01_dados.R), cores, tema_mix e pagina() do projeto.
#
# O QUE FOI TIRADO do código original:
#   - library(...)                 -> vão para R/00_pacotes.R
#   - read_csv() do GitHub         -> food_beverages já vem de R/01_dados.R
#   - options(DT.options ...)      -> já definido em R/20_alimentos.R (é global)
#   - dashboardPage / menuItem     -> o menu do projeto já existe (R/03_layout.R)
#   - box() / valueBox()           -> viraram card() / value_box() (bslib)
#   - shinyApp(ui, server)         -> o app.R já faz isso
# =====================================================================

setor_alimentos <- "Alimentos"
cor_alimentos   <- unname(cores[setor_alimentos])


# =====================================================================
# PARTE 1 — DADOS E GRÁFICOS   (veio de codigo_principal + server)
# =====================================================================

variaveis_alimentos <- c(
  "Açúcar", "Glicose", "Substituto_café", "Oleo_de_sementes",
  "Etanol_1", "Etanol_2", "Cerveja"
)

paleta_alimentos <- c(
  "Açúcar"           = "#1b9e77",
  "Glicose"          = "#d95f02",
  "Substituto_café"  = "#7570b3",
  "Oleo_de_sementes" = "#e7298a",
  "Etanol_1"         = "#66a61e",
  "Etanol_2"         = "#e6ab02",
  "Cerveja"          = "#1f78b4"
)

# tradução das colunas + conversão: quintal -> toneladas (x / 10)
comidas_bebidas_alimentos <- food_beverages |>
  rename(
    Ano = Year, Açúcar = Sugar, Glicose = Glucose,
    Substituto_café = Coffee_substitute, Oleo_de_sementes = Seed_oil,
    Etanol_1 = Ethyl_alcohol_1, Etanol_2 = Ethyl_alcohol_2, Cerveja = Beer
  ) |>
  mutate(across(c(Glicose, Substituto_café, Oleo_de_sementes), ~ .x / 10))

# formato longo + década + unidade (hL para cerveja/etanol, t para o resto)
longo_alimentos <- comidas_bebidas_alimentos |>
  pivot_longer(all_of(variaveis_alimentos), names_to = "Variavel", values_to = "Valor") |>
  mutate(
    Decada  = paste0(floor(Ano / 10) * 10, "s"),
    unidade = ifelse(str_detect(str_to_lower(Variavel), "cerveja|etanol"), "hL", "t")
  )

# estatística descritiva por década e produto (com soma, como no original)
cols_estat_alimentos <- c("soma", "media", "dp", "min", "q1", "mediana", "q3", "max")

descritiva_alimentos <- longo_alimentos |>
  group_by(Decada, Variavel) |>
  summarise(
    n_obs   = sum(!is.na(Valor)),
    n_na    = sum(is.na(Valor)),
    soma    = sum(Valor, na.rm = TRUE),
    media   = mean(Valor, na.rm = TRUE),
    dp      = sd(Valor, na.rm = TRUE),
    min     = suppressWarnings(min(Valor, na.rm = TRUE)),
    q1      = quantile(Valor, 0.25, na.rm = TRUE, names = FALSE),
    mediana = median(Valor, na.rm = TRUE),
    q3      = quantile(Valor, 0.75, na.rm = TRUE, names = FALSE),
    max     = suppressWarnings(max(Valor, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  mutate(across(all_of(cols_estat_alimentos),
                ~ if_else(is.nan(.x) | is.infinite(.x) | is.na(.x), NA_real_, .x))) |>
  arrange(Decada, Variavel)

anos_min_alimentos <- min(longo_alimentos$Ano, na.rm = TRUE)
anos_max_alimentos <- max(longo_alimentos$Ano, na.rm = TRUE)
decadas_alimentos  <- sort(unique(na.omit(longo_alimentos$Decada)))

fmt_br_alimentos <- function(x) {
  ifelse(is.na(x) | !is.finite(x), "NA",
         formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
}

# média ± DP por década (para o gráfico das guerras)
dados_decada_alimentos <- longo_alimentos |>
  filter(!is.na(Decada), !is.na(Variavel), !is.na(Valor)) |>
  summarise(
    valor_decada = mean(Valor, na.rm = TRUE),
    dp_decada    = sd(Valor, na.rm = TRUE),
    n_obs        = n(),
    .by = c(Decada, Variavel)
  ) |>
  mutate(
    decada_num = as.numeric(sub("s$", "", Decada)),
    dp_decada  = coalesce(dp_decada, 0),   # década com 1 observação: DP = 0
    inferior   = valor_decada - dp_decada,
    superior   = valor_decada + dp_decada
  ) |>
  arrange(Variavel, decada_num)

# limites do eixo Y por variável + faixas das guerras
# (eram reactive() no original, mas não dependem de input: viram objetos)
limites_y_alimentos <- dados_decada_alimentos |>
  summarise(ymin = min(valor_decada, na.rm = TRUE),
            ymax = max(valor_decada, na.rm = TRUE),
            .by = Variavel) |>
  mutate(folga = (ymax - ymin) * 0.05,
         ymin  = ymin - folga,
         ymax  = ymax + folga) |>
  select(Variavel, ymin, ymax)

periodos_guerra_alimentos <- tibble(
  xmin   = c(1910, 1940) - 5,
  xmax   = c(1910, 1940) + 5,
  guerra = c("1910s (1914–1918)", "1940s (1939–1945)")
) |>
  crossing(limites_y_alimentos)

# totais por produto no período todo (usados nos KPIs)
totais_alimentos <- longo_alimentos |>
  filter(!is.na(Valor)) |>
  group_by(Variavel, unidade) |>
  summarise(total_periodo = sum(Valor, na.rm = TRUE), .groups = "drop")


# ---------------------------------------------------------------------
# GRÁFICO 1: heatmap década x produto   (veio de output$grafico_heat)
# ---------------------------------------------------------------------
grafico_heat_alimentos <- function() {
  f <- function(x) ifelse(is.na(x), "NA", fmt_br_alimentos(x))

  d <- descritiva_alimentos |>
    filter(!is.na(Decada)) |>
    mutate(
      Decada   = factor(Decada, levels = decadas_alimentos),
      Variavel = factor(Variavel, levels = variaveis_alimentos),
      z        = log1p(media),
      hover = paste0(
        "<b>Década:</b> ", Decada, "<br>",
        "<b>Variável:</b> ", Variavel, "<br>",
        "<b>N:</b> ", n_obs, " | <b>NAs:</b> ", n_na, "<br>",
        "<b>Soma:</b> ", f(soma), "<br>",
        "<b>Média:</b> ", f(media), "<br>",
        "<b>DP:</b> ", f(dp), "<br>",
        "<b>Mín:</b> ", f(min), "<br>",
        "<b>Q1:</b> ", f(q1), "<br>",
        "<b>Mediana (Q2):</b> ", f(mediana), "<br>",
        "<b>Q3:</b> ", f(q3), "<br>",
        "<b>Máx:</b> ", f(max)
      ),
      rotulo = ifelse(is.na(media), "", paste0(f(media), "<br>±", f(dp)))
    )

  z_meio <- mean(range(d$z, na.rm = TRUE))
  d <- d |> mutate(cor_txt = ifelse(!is.na(z) & z > z_meio, "white", "black"))

  anotacoes <- lapply(seq_len(nrow(d)), function(i) {
    list(
      x = as.character(d$Decada[i]),
      y = as.character(d$Variavel[i]),
      text = d$rotulo[i],
      showarrow = FALSE,
      font = list(size = 10, color = d$cor_txt[i])
    )
  })

  plot_ly(
    data = d, x = ~Decada, y = ~Variavel, z = ~z,
    type = "heatmap", text = ~hover, hoverinfo = "text",
    colorscale = list(c(0, "#ffffcc"), c(0.5, "#fd8d3c"), c(1, "#800026")),
    colorbar = list(title = "log1p(média)")
  ) |>
    layout(
      xaxis = list(title = "Década", categoryorder = "array",
                   categoryarray = decadas_alimentos),
      yaxis = list(title = "Variável", autorange = "reversed"),
      annotations = anotacoes,
      hoverlabel = list(align = "left")
    )
}


# ---------------------------------------------------------------------
# GRÁFICO 2: ranking por década   (veio de output$grafico_ranking_prod)
# ---------------------------------------------------------------------
grafico_ranking_alimentos <- function(periodo) {
  d <- longo_alimentos |> filter(!is.na(Valor), !is.na(Decada))

  if (periodo == "ww1") {
    d <- d |> filter(!is.na(Ano), Ano >= 1914, Ano <= 1918)
  } else if (periodo == "ww2") {
    d <- d |> filter(!is.na(Ano), Ano >= 1939, Ano <= 1945)
  } else if (periodo == "pos_ww2") {
    d <- d |> filter(!is.na(Ano), Ano >= 1946)
  }

  validate(need(nrow(d) > 0, "Sem dados disponíveis para o período selecionado."))

  rk <- d |>
    group_by(Decada, Variavel, unidade) |>
    summarise(total_decada = sum(Valor, na.rm = TRUE), .groups = "drop") |>
    group_by(Decada) |>
    mutate(posicao = rank(desc(total_decada), ties.method = "min")) |>
    ungroup()

  niveis_decada <- intersect(decadas_alimentos, unique(rk$Decada))
  n_prod <- length(variaveis_alimentos)

  rk <- rk |>
    mutate(
      Decada   = factor(Decada, levels = niveis_decada),
      Variavel = factor(Variavel, levels = variaveis_alimentos),
      texto_hover = paste0(
        "<b>", Variavel, "</b> (", Decada, ")<br>",
        "<b>Posição:</b> #", posicao, "<br>",
        "<b>Total na década:</b> ", fmt_br_alimentos(total_decada), " ", unidade
      )
    )

  p <- ggplot(rk, aes(x = Decada, y = posicao, color = Variavel, group = Variavel)) +
    geom_line(linewidth = 1.1, alpha = 0.9) +
    geom_point(aes(text = texto_hover), size = 3.2, alpha = 0.95) +
    scale_y_reverse(breaks = seq_len(n_prod), labels = paste0("#", seq_len(n_prod))) +
    scale_color_manual(values = paleta_alimentos, name = "Produto", drop = FALSE) +
    labs(x = "Década", y = "Posição no ranking") +
    tema_mix +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")

  ggplotly(p, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
}


# ---------------------------------------------------------------------
# GRÁFICO 3: média ± DP por década, com guerras   (output$grafico_mm_guerras)
# ---------------------------------------------------------------------
grafico_guerras_alimentos <- function() {
  eixo_x <- sort(unique(dados_decada_alimentos$decada_num))

  dd <- dados_decada_alimentos |>
    mutate(
      Variavel = factor(Variavel, levels = variaveis_alimentos),
      texto_hover = paste0(
        "<b>Década:</b> ", Decada, "<br>",
        "<b>Média:</b> ", fmt_br_alimentos(valor_decada), "<br>",
        "<b>DP:</b> ", fmt_br_alimentos(dp_decada), "<br>",
        "<b>Média ± DP:</b> ", fmt_br_alimentos(inferior), " – ",
        fmt_br_alimentos(superior), "<br>",
        "<b>N:</b> ", n_obs
      )
    )

  p <- ggplot() +
    geom_rect(
      data = periodos_guerra_alimentos,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = guerra),
      alpha = 0.35, inherit.aes = FALSE
    ) +
    geom_errorbar(
      data = dd,
      aes(x = decada_num, ymin = inferior, ymax = superior, color = Variavel),
      width = 2, linewidth = 0.6, alpha = 0.85
    ) +
    geom_line(
      data = dd,
      aes(x = decada_num, y = valor_decada, color = Variavel, group = Variavel),
      linewidth = 1.0, alpha = 0.95
    ) +
    geom_point(
      data = dd,
      aes(x = decada_num, y = valor_decada, color = Variavel, text = texto_hover),
      size = 2.2, alpha = 0.95
    ) +
    facet_wrap(~ Variavel, scales = "free_y", ncol = 2) +
    scale_color_manual(values = paleta_alimentos, guide = "none", drop = FALSE) +
    scale_fill_manual(
      values = c("1910s (1914–1918)" = "#f39c12", "1940s (1939–1945)" = "#e74c3c"),
      name = "Período destacado"
    ) +
    scale_x_continuous(breaks = eixo_x, labels = paste0(eixo_x, "s")) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    labs(x = "Década", y = "Média na década") +
    tema_mix +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")

  ggplotly(p, tooltip = "text") |> layout(hoverlabel = list(align = "left"))
}


# =====================================================================
# PARTE 2 — TELA (UI)
#   tabItem -> nav_panel     box -> card     valueBox -> value_box
# =====================================================================
ui_alimentos <- function() {
  pagina("alimentos", "Alimentos (2)", "Visão geral, guerras e ranking da produção italiana",

    # cor do cabeçalho das tabelas
    tags$style(HTML(paste0(
      "#tabela_brutos_alimentos thead th, #tabela_descritiva_alimentos thead th {",
      " background-color: ", cor_alimentos, " !important; color: #fff !important; }"
    ))),

    navset_card_tab(

      # ------------ ABA 1 (era aba1_visao_geral) ------------
      nav_panel("Visão Geral e KPIs",

        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box("Período",
                    textOutput("kpi_periodo_alimentos"),
                    textOutput("kpi_periodo_sub_alimentos"),
                    showcase = icon("calendar")),
          value_box("Toneladas totais",
                    textOutput("kpi_ton_alimentos"),
                    textOutput("kpi_ton_sub_alimentos"),
                    showcase = icon("weight-hanging")),
          value_box("Volume total",
                    textOutput("kpi_hl_alimentos"),
                    textOutput("kpi_hl_sub_alimentos"),
                    showcase = icon("bottle-droplet")),
          value_box("Maior produção total",
                    textOutput("kpi_maior_alimentos"),
                    textOutput("kpi_maior_sub_alimentos"),
                    showcase = icon("arrow-trend-up"))
        ),

        card(card_header("Comparativo entre produtos: como cada produto evoluiu de década em década?"),
             plotlyOutput("heat_alimentos", height = "520px"),
             helpText("Cor = log1p da média. Em cada célula: média e desvio padrão (±).",
                      "Passe o mouse para ver N, NAs, soma, média, DP, mín, Q1, mediana, Q3 e máx.",
                      "Células em branco = década sem dados para a variável.")),

        card(card_header("Base traduzida e convertida (toneladas e hectolitros)"),
             downloadButton("baixar_base_alimentos", "Baixar base (.csv)"),
             DTOutput("tabela_brutos_alimentos"))
      ),

      # ------------ ABA 2 (era aba2_his_war_beer) ------------
      nav_panel("História, Guerras e Ranking",

        card(card_header("Série por década: o que mais oscilou nas guerras? (1914–1918 e 1939–1945)"),
             plotlyOutput("guerras_alimentos", height = "900px")),

        card(card_header("Ranking por década: qual produto liderou cada década?"),
          layout_sidebar(
            sidebar = sidebar(
              selectInput("periodo_ranking_alimentos", "Selecione o período:",
                          choices = c("Período todo"          = "todo",
                                      "1ª Guerra (1914–1918)" = "ww1",
                                      "2ª Guerra (1939–1945)" = "ww2",
                                      "Pós-2ª Guerra (1946+)" = "pos_ww2"),
                          selected = "todo")
            ),
            plotlyOutput("ranking_alimentos", height = "460px")
          )),

        card(card_header("Estatística descritiva por década e produto"),
             downloadButton("baixar_descritiva_alimentos", "Baixar estatística (.csv)"),
             DTOutput("tabela_descritiva_alimentos"))
      )
    )
  )
}


# =====================================================================
# PARTE 3 — SERVER
# =====================================================================
server_alimentos <- function(input, output, session) {

  # ---------------- KPIs ----------------
  df_ton <- totais_alimentos |> filter(unidade == "t")  |> arrange(desc(total_periodo))
  df_hl  <- totais_alimentos |> filter(unidade == "hL") |> arrange(desc(total_periodo))
  df_top <- totais_alimentos |> arrange(desc(total_periodo))

  output$kpi_periodo_alimentos     <- renderText(paste0(anos_min_alimentos, "-", anos_max_alimentos))
  output$kpi_periodo_sub_alimentos <- renderText(paste0("Faixa de anos (", length(decadas_alimentos), " décadas)"))

  output$kpi_ton_alimentos <- renderText(
    if (nrow(df_ton) == 0) "0,00 t" else paste0(fmt_br_alimentos(sum(df_ton$total_periodo)), " t")
  )
  output$kpi_ton_sub_alimentos <- renderText(
    if (nrow(df_ton) == 0) "Sem produtos em t" else paste0("Líder: ", df_ton$Variavel[1])
  )

  output$kpi_hl_alimentos <- renderText(
    if (nrow(df_hl) == 0) "0,00 hL" else paste0(fmt_br_alimentos(sum(df_hl$total_periodo)), " hL")
  )
  output$kpi_hl_sub_alimentos <- renderText(
    if (nrow(df_hl) == 0) "Sem produtos em hL" else paste0("Líder: ", df_hl$Variavel[1])
  )

  output$kpi_maior_alimentos <- renderText({
    req(nrow(df_top) > 0)
    as.character(df_top$Variavel[1])
  })
  output$kpi_maior_sub_alimentos <- renderText({
    req(nrow(df_top) > 0)
    paste0(fmt_br_alimentos(df_top$total_periodo[1]), " ", df_top$unidade[1])
  })

  # ---------------- tabelas ----------------
  output$tabela_brutos_alimentos <- renderDT({
    js_na <- JS(
      "function(data, type, row, meta) {",
      "  if (type !== 'display') return data;",
      "  if (data === null || data === undefined || data === '') return 'NA';",
      "  if (typeof data === 'number' && Number.isNaN(data)) return 'NA';",
      "  return data;",
      "}"
    )
    alvo <- which(names(comidas_bebidas_alimentos) %in% c("Ano", variaveis_alimentos)) - 1
    datatable(comidas_bebidas_alimentos, rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE,
                             columnDefs = list(list(targets = alvo, render = js_na))))
  })

  output$tabela_descritiva_alimentos <- renderDT({
    js_2dec <- JS(
      "function(data, type, row, meta) {",
      "  if (type !== 'display') {",
      "    return (data === null || data === undefined || data === '' || Number.isNaN(Number(data))) ? -Infinity : Number(data);",
      "  }",
      "  if (data === null || data === undefined || data === '') return 'NA';",
      "  var num = Number(data);",
      "  if (Number.isNaN(num) || !Number.isFinite(num)) return 'NA';",
      "  return num.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });",
      "}"
    )
    alvo <- which(names(descritiva_alimentos) %in% cols_estat_alimentos) - 1
    datatable(descritiva_alimentos, rownames = FALSE,
              colnames = c("Década", "Variável", "N", "NAs", "Soma", "Média", "DP",
                           "Mín", "Q1", "Mediana", "Q3", "Máx"),
              options = list(pageLength = 10, scrollX = TRUE,
                             columnDefs = list(list(targets = alvo, render = js_2dec))))
  })

  # ---------------- downloads ----------------
  output$baixar_base_alimentos <- downloadHandler(
    filename = function() paste0("comidas_bebidas_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(comidas_bebidas_alimentos, file)
  )
  output$baixar_descritiva_alimentos <- downloadHandler(
    filename = function() paste0("estatistica_descritiva_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(descritiva_alimentos, file)
  )

  # ---------------- gráficos (1 linha cada) ----------------
  output$heat_alimentos    <- renderPlotly(grafico_heat_alimentos())
  output$guerras_alimentos <- renderPlotly(grafico_guerras_alimentos())
  output$ranking_alimentos <- renderPlotly(
    grafico_ranking_alimentos(input$periodo_ranking_alimentos)
  )
}

