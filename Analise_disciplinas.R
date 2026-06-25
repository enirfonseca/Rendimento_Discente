# Instale os pacotes caso não os tenha: install.packages(c("tidyverse", "scales"))
library(tidyverse)
library(scales)

# Força o RStudio a reconhecer acentuação em sistemas Windows/Unificados
Sys.setlocale("LC_ALL", "pt_BR.UTF-8") 

# ==============================================================================
# 1. CARREGAMENTO DOS DADOS (CORRIGIDO PARA LATIN1/WINDOWS)
# ==============================================================================
dados_brutos <- read_delim(
  "Dados_TCC_grafico.csv", 
  delim = ";", 
  locale = locale(encoding = "Latin1"), 
  show_col_types = FALSE
)

# Selecionando, renomeando as colunas e padronizando os nomes das disciplinas
dados_disciplinas <- dados_brutos %>%
  select(
    Disciplina = Disciplinas,
    Ingresso,
    Iniciante,
    Aprovado,
    Reprovado,
    Taxa_Aprovacao_Texto = `Taxa de Aprovacao`,
    Taxa_Reprovacao_Texto = `Taxa de Reprovacao`
  ) %>%
  filter(!is.na(Disciplina)) %>%
  mutate(Disciplina = str_trim(Disciplina)) %>%
  mutate(Disciplina = str_replace(Disciplina, "Programação em Computadores", "Programação de Computadores"))

# ==============================================================================
# 2. LIMPEZA, TRATAMENTO E TIPAGEM DE DADOS
# ==============================================================================
dados_limpos <- dados_disciplinas %>%
  mutate(
    Taxa_Aprovacao = as.numeric(str_replace(str_remove(Taxa_Aprovacao_Texto, "%"), ",", ".")) / 100,
    Taxa_Reprovacao = as.numeric(str_replace(str_remove(Taxa_Reprovacao_Texto, "%"), ",", ".")) / 100
  )

# Resumo da Mediana E Média (para o texto do Boxplot)
dados_resumo_boxplot <- dados_limpos %>%
  group_by(Disciplina) %>%
  summarise(
    Media = mean(Taxa_Aprovacao, na.rm = TRUE),
    Mediana = median(Taxa_Aprovacao, na.rm = TRUE)
  )

# ==============================================================================
# 3. GERAÇÃO DOS GRÁFICOS
# ==============================================================================

# --- GRÁFICO 1: Boxplot ---
grafico_boxplot <- ggplot(dados_limpos, aes(x = Disciplina, y = Taxa_Aprovacao, fill = Disciplina)) +
  
  geom_boxplot(width = 0.375, alpha = 0.7, outlier.colour = "red", outlier.size = 2) +
  
  # ALTERADO: Tamanho da Média igualado ao da Mediana (de 1.75 para 2.45)
  geom_text(data = dados_resumo_boxplot, 
            aes(x = Disciplina, y = Media, label = paste0("Média: ", percent(Media, accuracy = 0.1))), 
            color = "blue", size = 2.45, vjust = -1.5, fontface = "bold", inherit.aes = FALSE) +
  
  geom_text(data = dados_resumo_boxplot, 
            aes(x = Disciplina, y = Mediana, label = paste0("Med.: ", percent(Mediana, accuracy = 0.1))), 
            color = "black", size = 2.45, vjust = 2.0, fontface = "bold", inherit.aes = FALSE) +
  
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15), expand = c(0, 0.5)) + 
  theme_minimal() +
  
  labs(
    title = "Dispersão das Taxas de Aprovação por Disciplina",
    subtitle = NULL, 
    y = "Taxa de Aprovação\n\n", 
    x = ""
  ) +
  
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5, size = rel(1.1), face = "bold"),
    axis.title.y = element_text(size = rel(0.9), face = "bold"), 
    
    # ALTERADO: Textos do eixo Y aumentados em 50% (de rel(0.75) para rel(1.125))
    axis.text.y = element_text(size = rel(1.125)),                 
    
    # ALTERADO: Textos do eixo X aumentados em 50% (de 8.5 para 12.75)
    axis.text.x = element_text(size = 12.75, angle = 0, hjust = 0.5, face = "bold") 
  )

print(grafico_boxplot)


# --- GRÁFICO 2: Barras Agrupadas Unificadas ---
dados_barras <- dados_limpos %>%
  select(Disciplina, Ingresso, Aprovado, Reprovado) %>%
  pivot_longer(cols = c("Aprovado", "Reprovado"), names_to = "Status", values_to = "Quantidade") %>%
  mutate(
    Disciplina_Rotulo = case_when(
      str_detect(Disciplina, "L.gica|gica") ~ "Lógica de Programação",
      str_detect(Disciplina, "Computadores I$") ~ "\n\nProgramação de Computadores I",
      str_detect(Disciplina, "Computadores II$") ~ "\n\nProgramação de Computadores II",
      TRUE ~ "Programação"
    )
  ) %>%
  mutate(Disciplina_Rotulo = factor(Disciplina_Rotulo, levels = c(
    "Programação", "Lógica de Programação", 
    "\n\nProgramação de Computadores I", "\n\nProgramação de Computadores II"
  )))

grafico_barras <- ggplot(mapping = aes(x = Ingresso, y = Quantidade, fill = Status)) +
  
  geom_bar(
    data = dados_barras %>% filter(!str_detect(Disciplina, "Computadores")),
    stat = "identity", position = position_dodge(width = 0.9), width = 0.9
  ) +
  geom_text(
    data = dados_barras %>% filter(!str_detect(Disciplina, "Computadores")),
    aes(label = Quantidade), 
    position = position_dodge(width = 0.9), 
    vjust = -0.5, 
    size = 2.5, fontface = "bold", color = "black" 
  ) +
  
  geom_bar(
    data = dados_barras %>% filter(str_detect(Disciplina, "Computadores")),
    stat = "identity", position = position_dodge(width = 0.45), width = 0.45
  ) +
  geom_text(
    data = dados_barras %>% filter(str_detect(Disciplina, "Computadores")),
    aes(label = Quantidade), 
    position = position_dodge(width = 0.45), 
    vjust = -0.5, 
    size = 2.5, fontface = "bold", color = "black" 
  ) +
  
  facet_wrap(~Disciplina_Rotulo, scales = "free_x") +
  scale_fill_manual(values = c("Aprovado" = "#2E7D32", "Reprovado" = "#C62828")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  theme_minimal() +
  
  labs(
    title = "Aprovados X Reprovados\n\n", 
    y = "Número de Alunos",
    x = "",
    fill = NULL 
  ) +
  
  theme(
    legend.position = "bottom",
    legend.key.width = unit(3, "mm"),
    legend.key.height = unit(3, "mm"),
    
    strip.text = element_text(size = 12.58, face = "plain"),                      
    plot.title = element_text(hjust = 0.5, size = rel(1.5), face = "bold"),       
    axis.text.x = element_text(size = 8.5, angle = 0, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = rel(1.0)),                                  
    axis.title.y = element_text(size = rel(1.4), face = "bold"),                  
    legend.text = element_text(size = rel(1.0))                                   
  )

print(grafico_barras)