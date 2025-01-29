########################## Analise dos Índices ###############################

# Carregando Pacotes

if(!require(pacman))
  install.packages("pacman")
library(pacman)

pacman::p_load("ggplot2", 'dplyr', "raster", "rstatix", "multcompView",
               'gridExtra')

################################# Funções ######################################

# Banco de Dados
db = function(files, g, a, b) {
  data_list = list()
  
  # Banco de dados
  for(i in 1:length(files)) {
    
    # Matriz de Dados
    raster_drone = paste(files[[i]], '.tif', sep = '')
    data_drone = raster(raster_drone)
    x = values(data_drone)
    
    # Padroniação dos IVs
    x[is.na(x)] = 0
    x = x[which(x != 0)]
    x = ((x / 255) - (min(x) / 255)) / ((max(x) / 255) - (min(x) / 255))
    mat = data.frame(x = x)
    glimpse(mat)
    
    # Classificando
    mat$Classe[mat$x >  g[[i]][1] & mat$x <= g[[i]][2]] = 'Classe 2'
    mat$Classe[mat$x > g[[i]][4] & mat$x <= g[[i]][5]] = 'Classe 5'
    
    glimpse(mat)
    
    # Eliminando as classes fora do estudo
    mat = na.omit(mat)
    
    glimpse(mat)
    
    # Selecionado 25% do banco de dados
    classe2 = mat[mat$Classe == 'Classe 2', ]
    classe2 = classe2[sample(nrow(classe2), nrow(classe2) * 0.25), ]
    
    classe5 = mat[mat$Classe == 'Classe 5', ]
    classe5 = classe5[sample(nrow(classe5), nrow(classe5) * 0.25), ]
    
    # Adicionando listageam
    data_list[[files[[i]]]] = c(classe2$x, classe5$x)
    
    glimpse(data_list[[files[[i]]]])
  }
  
  ## Variavel Dependente
  value = c(data_list[[1]], data_list[[2]], data_list[[3]],
            data_list[[4]], data_list[[5]], data_list[[6]])
  
  ## Variavel Independente (VI) - kruskal Wallis
  kw = c(rep(b[1], length(data_list[[1]])), rep(b[2], length(data_list[[2]])),
         rep(b[3], length(data_list[[3]])), rep(b[4], length(data_list[[4]])),
         rep(b[5], length(data_list[[5]])), rep(b[6], length(data_list[[6]])))
  
  print(kw)
  
  ## Variavel Independente (VI) - Manny Whitney
  iv1 = length(data_list[[1]]) + length(data_list[[2]]) + length(data_list[[3]])
  
  iv2 = length(data_list[[4]]) + length(data_list[[5]]) + length(data_list[[6]])
  
  mw = c(rep(a[1], iv1), rep(a[2], iv2))
  
  print(mw)
  
  data = data.frame(IVs = mw, Alt = kw, Valores = value)
  
  glimpse(data)
  
  return(data)
}

# Tabela com Estatstistica Descrtiva - Kruskall Wallis
table_letter = function(table, data, a) {
  
  post_hoc_order = table %>% arrange(-statistic) # Ordenando as comparação
  
  comp = post_hoc_order$p.adj <= 0.05 # Avaliando p < 0.05
  
  # Adicionado as letras
  names(comp) = paste0(post_hoc_order$group2, "-", post_hoc_order$group1)
  comp = multcompView::multcompLetters(comp)
  comp = as.data.frame(comp$Letters)
  colnames(comp) = 'letras'
  comp$Alt = rownames(comp)
  median = data %>% group_by(Alt) %>%
    summarise(median = median(Valores), IQR = IQR(Valores), Amostras = n(),
              Erro_Padrao = sd(Valores) / sqrt(n()), max = max(Valores))
  comp = dplyr::left_join(comp, median, by = 'Alt')
  comp$Alt = factor(comp$Alt, levels = c(a))
  comp$static = 
  return(comp)
}

# Box plot com letras
box_plot = function(data, table) {
  
  gf = ggplot(data, aes(x = Alt, y = Valores)) +
    geom_boxplot(outlier.size = 3, fill = "lightgray", color = "black", size = 1.5) +  
    geom_text(data = table, aes(x = Alt, y = max, label = letras),
              position = position_dodge(width = 0.4), vjust = -1, size = 5,
              fontface = "bold") +
    xlab("IVs") +
    ylab("Valores") +
    theme_classic() +
    theme(plot.title = element_text(size = 20, face = "bold"),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 14, face = "bold"))  
  return(gf)
}

######################### Teste de comparação ###############################

# Estresse Hídrico

## Parâmetros de Classificação - ExR
alt5_exr = c(0.267,	0.311, 0.369, 0.515, 0.746)
alt10_exr = c(0.255, 0.301, 0.360, 0.495, 0.696)
alt15_exr = c(0.282, 0.330,  0.390, 0.527,	0.796)

## Parâmetros de Classificação - VDVI
alt5_vdvi = c(0.479, 0.532, 0.638, 0.688, 0.741)
alt10_vdvi = c(0.476, 0.559, 0.645, 0.690, 0.744)
alt15_vdvi = c(0.487, 0.558, 0.643, 0.691, 0.742)

## Labels
labels_iv = c('ExR', 'VDVI')
labels_alt_iv = c('ExR5', 'ExR10', 'ExR15', 'VDVI5', 'VDVI10', 'VDVI15')

DB = db(c("exr5", "exr10", "exr15", "vdvi5", "vdvi10", "vdvi15"),
        list(alt5_exr, alt10_exr, alt15_exr, alt5_vdvi, alt10_vdvi, alt15_vdvi),
        labels_iv, labels_alt_iv) # Banco de Dados

## Teste de Kruskall Wallis (Melhor Altura de Voo)
kruskal.test(Valores ~ Alt, data = DB)

post_hoc = dunn_test(Valores ~ Alt, data = DB, p.adjust.method = 'bonferroni')
write.csv(post_hoc, file = 'post_hoc_eh.csv')

## Estatistica Descritva e Box Plot
ed_kw = table_letter(post_hoc, DB, labels_alt_iv)
write.csv(ed_kw, file = 'ed_kw_eh.csv')

windows()
box_plot(DB, ed_kw)
dev.off()

## Teste de Manny Whitney (Melhor IV)
wilcox.test(Valores ~ IVs, data = DB)

## Estatística Descritiva
ED = DB %>% group_by(IVs) %>% get_summary_stats(Valores, type = 'median_iqr')
write.csv(ED, file = 'ed_mw_eh.csv') # Salvando Arquivo

rm(DB, alt5_exr, alt10_exr, alt15_exr, alt5_vdvi, alt10_vdvi, alt15_vdvi,
   labels_alt_iv, post_hoc, ed_kw, ED, labels_iv)

# Teor de Clorofila

## Parâmetros de Classificação - NGRDI
alt5_ngrdi = c(0.424, 0.475, 0.584, 0.617, 0.650)
alt10_ngrdi = c(0.420, 0.510, 0.603, 0.639, 0.675)
alt15_ngrdi = c(0.432, 0.491, 0.581, 0.613, 0.647)

## Parâmetros de Classificação - TGI
alt5_tgi = c(0.232, 0.308, 0.498, 0.614, 0.696)
alt10_tgi = c(0.210, 0.327, 0.511, 0.623, 0.710)
alt15_tgi = c(0.248, 0.363, 0.552, 0.655, 0.734)

## Labels
labels_iv = c('NGRDI', 'TGI')
labels_alt_iv = c('NGRDI5', 'NGRDI10', 'NGRDI15', 'TGI5', 'TGI10', 'TGI15')

DB = db(c('ngrdi5', 'ngrdi10', 'ngrdi15', 'tgi5', 'tgi10', 'tgi15'),
        list(alt5_ngrdi, alt10_ngrdi, alt15_ngrdi, alt5_tgi, alt10_tgi,
             alt15_tgi), labels_iv, labels_alt_iv) 

## Teste de Kruskall Wallis (Melhor Altura de Voo)
kruskal.test(Valores ~ Alt, data = DB)

post_hoc = dunn_test(Valores ~ Alt, data = DB, p.adjust.method = 'bonferroni')
write.csv(post_hoc, file = 'post_hoc_tc.csv')

## Estatistica Descritva e Box Plot
ed_kw = table_letter(post_hoc, DB, labels_alt_iv) 
write.csv(ed_kw, file = 'ed_kw_tc.csv')

windows()
box_plot(DB, ed_kw)
dev.off()

## Teste de Manny Whitney (Melhor IV)
wilcox.test(Valores ~ IVs, data = DB)

## Estatística Descritiva
ED = DB %>% group_by(IVs) %>% get_summary_stats(Valores, type = 'median_iqr')
write.csv(ED, file = 'ed_mw_tc.csv') # Salvando Arquivo

rm(DB, alt5_ngrdi, alt10_ngrdi, alt15_ngrdi, alt5_tgi, alt10_tgi, alt15_tgi, 
   labels_alt_iv, post_hoc, ed_kw, ED, labels_iv)

# Cobertura do Solo

## Parâmetros de Classificação - ExG
alt5_exg = c(0.231, 0.321, 0.524, 0.634, 0.707)
alt10_exg = c(0.210, 0.351, 0.546, 0.656, 0.737)
alt15_exg = c(0.238, 0.377, 0.577, 0.679, 0.752)

## Parâmetros de Classificação - GLI
alt5_gli = c(0.479, 0.532, 0.638, 0.688, 0.741)
alt10_gli = c(0.476, 0.559, 0.645, 0.690,	0.744)
alt15_gli = c(0.487, 0.558, 0.643, 0.691, 0.742)

## Labels
labels_iv = c('ExG', 'GLI')
labels_alt_iv = c('ExG5', 'ExG10', 'ExG15', 'GLI5','GLI10', 'GLI15')

DB = db(c('exg5', 'exg10', 'exg15', 'gli5', 'gli10', 'gli15'),
        list(alt5_exg, alt10_exg, alt15_exg, alt5_gli, alt10_gli, alt15_gli),
        labels_iv, labels_alt_iv) # BD

## Teste de Kruskall Wallis (Melhor Altura de Voo)
kruskal.test(Valores ~ Alt, data = DB)

post_hoc = dunn_test(Valores ~ Alt, data = DB, p.adjust.method = 'bonferroni')
write.csv(post_hoc, file = 'post_hoc_cs.csv')

## Estatistica Descritva e Box Plot
ed_kw = table_letter(post_hoc, DB, labels_alt_iv)
write.csv(ed_kw, file = 'ed_kw_cs.csv')

windows()
box_plot(DB, ed_kw)
dev.off()

## Teste de Manny Whitney (Melhor IV)
wilcox.test(Valores ~ IVs, data = DB)

## Estatística Descritiva
ED = DB %>% group_by(IVs) %>% get_summary_stats(Valores, type = 'median_iqr')
write.csv(ED, file = 'ed_mw_cs.csv') # Salvando Arquivo

rm(DB, alt5_exg, alt10_exg, alt15_exg, alt5_gli, alt10_gli, alt15_gli,
   labels_alt_iv, post_hoc, ed_kw, ED, labels_iv)

# Biomassa Verde

## Parâmetros de Classificação - VARI
alt5_vari = c(0.465, 0.468, 0.474, 0.475, 0.477)
alt10_vari = c(0.433, 0.440, 0.445, 0.448, 0.450)
alt15_vari = c(0.451, 0.454,	0.458, 0.459, 0.460)

## Parâmetros de Classificação - BGI
alt5_bgi = c(0.005, 0.010, 0.015, 0.022, 0.026)
alt10_bgi = c(0.007, 0.013, 0.017, 0.024, 0.030)
alt15_bgi = c(0.004, 0.008, 0.011, 0.017, 0.022)

## Labels
labels_iv = c('VARI', 'BGI')
labels_alt_iv = c('VARI5', 'VARI10', 'VARI15', 'BGI5', 'BGI10', 'BGI15')

DB = db(c('vari5', 'vari10', 'vari15', 'bgi5', 'bgi10', 'bgi15'),
        list(alt5_vari, alt10_vari, alt15_vari, alt5_bgi, alt10_bgi,
             alt15_bgi), labels_iv, labels_alt_iv) # BD

## Teste de Kruskall Wallis (Melhor Altura de Voo)
kruskal.test(Valores ~ Alt, data = DB)

post_hoc = dunn_test(Valores ~ Alt, data = DB, p.adjust.method = 'bonferroni')
write.csv(post_hoc, file = 'post_hoc_bv.csv')

## Estatistica Descritva e Box Plot
ed_kw = table_letter(post_hoc, DB, labels_alt_iv)
write.csv(ed_kw, file = 'ed_kw_bv.csv')

windows()
box_plot(DB, ed_kw)
dev.off()

## Teste de Manny Whitney (Melhor IV)
wilcox.test(Valores ~ IVs, data = DB)

## Estatística Descritiva
ED = DB %>% group_by(IVs) %>% get_summary_stats(Valores, type = 'median_iqr')
write.csv(ED, file = 'ed_mw_bv.csv') # Salvando Arquivo

rm(DB, alt5_vari, alt10_vari, alt15_vari, alt5_bgi, alt10_bgi, alt15_bgi,
   labels_alt_iv, post_hoc, ed_kw, ED, labels_iv)


