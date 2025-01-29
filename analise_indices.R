########################## Analise dos Índices ###############################

# Carregando Pacotes

if(!require(pacman))
  install.packages("pacman")
library(pacman)

pacman::p_load("nortest", "dplyr", "psych", "raster", "ggplot2")

################################# Funções ######################################

# Banco de Dados
db = function(files, g, c) {
  data = list()
  
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
    mat$Classe[mat$x <= g[[i]][1]] = c[1]
    mat$Classe[mat$x >  g[[i]][1] & mat$x <= g[[i]][2]] = c[2]
    mat$Classe[mat$x > g[[i]][2] & mat$x<= g[[i]][3]] = c[3]
    mat$Classe[mat$x > g[[i]][3] & mat$x <= g[[i]][4]] = c[4]
    mat$Classe[mat$x > g[[i]][4] & mat$x <= g[[i]][5]] = c[5]
    mat$Classe[mat$x > g[[i]][5]] = c[6]
    
    data[[files[[i]]]] = mat
    
    glimpse(data[[files[[i]]]])
  }
  
  return(data)
}

# Teste de normalidade e homogeneidade
ndh = function(datas) {
  test = list()
  
  for(i in 1:length(datas)) {
    data = datas[[i]]
    normalidade = ad.test(data$x)
    test[[i]] = normalidade
    print(normalidade)
    homogeneidade = bartlett.test(data$x ~ data$Classe)
    test[[i]] = homogeneidade
    print(homogeneidade)
  }
  
  return(test)
}

# Distribuição normal dos dados
gf = function(data, a) {
  
  hist(data$x, breaks = 50, col = "lightgray", border = 'black', freq = FALSE,
       main = "", xlab = "", ylab = "", axes = FALSE)
  
  curve(dnorm(x, mean = mean(data$x), sd = sd(data$x)), col = "red", lwd = 2,
        add = TRUE)
  
  axis(1, at = pretty(data$x, n = 10), cex.axis = 1.2, font.axis = 2)
  axis(2, las = 1, cex.axis = 1.2, font.axis = 2)
  
  title(xlab = a, ylab = "Frequência", line = 3, font.lab = 2, cex.lab = 1.5)
}

################################# Labels #######################################

labels = c("Classe 1", "Classe 2", "Classe 3", "Classe 4", "Classe 5",
           "Classe 6")

########################### Analise dos Dados ##################################

# Estresse Hídrico

## Parâmetros de Classificação - ExR
alt5_exr = c(0.267,	0.311, 0.369, 0.515, 0.746)
alt10_exr = c(0.255, 0.301, 0.360, 0.495, 0.696)
alt15_exr = c(0.282, 0.330,  0.390, 0.527,	0.796)

## Parâmetros de Classificação - VDVI
alt5_vdvi = c(0.479, 0.532, 0.638, 0.688, 0.741)
alt10_vdvi = c(0.476, 0.559, 0.645, 0.690, 0.744)
alt15_vdvi = c(0.487, 0.558, 0.643, 0.691, 0.742)

EH = db(c('exr5', 'exr10', 'exr15', 'vdvi5', 'vdvi10', 'vdvi15'),
            list(alt5_exr, alt10_exr,alt15_exr, alt5_vdvi, alt10_vdvi,
                 alt15_vdvi), labels) # Banco de Dados

## Teste de Normalidade e Homogeneidade
NDH = ndh(EH)

# Distribuição Normal dos Dados
titles = c('ExR - 5m', 'ExR - 10m', 'ExR - 15m', 'VDVI - 5m', 'VDVI - 10m',
             'VDVI - 15m')

windows()
layout(matrix(c(1:6), nrow = 2, byrow = TRUE))
for (i in 1:length(titles)) {
  gf(EH[[i]], titles[i])
}
dev.off()

rm(EH, NDH, alt5_exr, alt10_exr, alt15_exr, alt5_vdvi, alt10_vdvi,
   alt15_vdvi, titles) # Limpando memória

# Teor de Clorofila

## Parâmetros de Classificação - NGRDI
alt5_ngrdi = c(0.424, 0.475, 0.584, 0.617, 0.650)
alt10_ngrdi = c(0.420, 0.510, 0.603, 0.639, 0.675)
alt15_ngrdi = c(0.432, 0.491, 0.581, 0.613, 0.647)

## Parâmetros de Classificação - TGI
alt5_tgi = c(0.232, 0.308, 0.498, 0.614, 0.696)
alt10_tgi = c(0.210, 0.327, 0.511, 0.623, 0.710)
alt15_tgi = c(0.248, 0.363, 0.552, 0.655, 0.734)

TC = db(c('ngrdi5', 'ngrdi10', 'ngrdi15', 'tgi5', 'tgi10', 'tgi15'),
        list(alt5_ngrdi, alt10_ngrdi, alt15_ngrdi, alt5_tgi, alt10_tgi,
             alt15_tgi), labels) 

## Teste de Normalidade e Homogeneidade
NDH = ndh(TC)

## Distribuição Normal dos Dados
titles = c('NGRDI - 5m', 'NGRDI - 10m', 'NGRDI - 15m', 'TGI - 5m', 'TGI - 10m',
           'TGI - 15m')

windows()
layout(matrix(c(1:6), nrow = 2, byrow = TRUE))
for (i in 1:length(titles)) {
  gf(TC[[i]], titles[i])
}
dev.off()

rm(TC, NDH, alt5_ngrdi, alt10_ngrdi, alt15_ngrdi, alt5_tgi, alt10_tgi,
   alt15_tgi, titles) # Limpando memória

# Cobertura do Solo

## Parâmetros de Classificação - ExG
alt5_exg = c(0.231, 0.321, 0.524, 0.634, 0.707)
alt10_exg = c(0.210, 0.351, 0.546, 0.656, 0.737)
alt15_exg = c(0.238, 0.377, 0.577, 0.679, 0.752)

## Parâmetros de Classificação - GLI
alt5_gli = c(0.479, 0.532, 0.638, 0.688, 0.741)
alt10_gli = c(0.476, 0.559, 0.645, 0.690,	0.744)
alt15_gli = c(0.487, 0.558, 0.643, 0.691, 0.742)

CS = db(c('exg5', 'exg10', 'exg15', 'gli5', 'gli10', 'gli15'),
        list(alt5_exg, alt10_exg, alt15_exg, alt5_gli, alt10_gli, alt15_gli),
        labels) # BD

## Teste de Normalidade e Homogeneidade
NDH = ndh(CS)

## Distribuição Normal dos Dados
titles = c('ExG - 5m', 'ExG - 10m', 'ExG - 15m', 'GLI - 5m', 'GLI - 10m',
           'GLI - 15m')

windows()
layout(matrix(c(1:6), nrow = 2, byrow = TRUE))
for (i in 1:length(titles)) {
  gf(CS[[i]], titles[i])
}
dev.off()

rm(CS, NDH, alt5_exg, alt10_exg, alt15_exg, alt5_gli, alt10_gli,
   alt15_gli, titles) # Limpando memória

# Biomassa Verde

## Parâmetros de Classificação - VARI
alt5_vari = c(0.465, 0.468, 0.474, 0.475, 0.477)
alt10_vari = c(0.433, 0.440, 0.445, 0.448, 0.450)
alt15_vari = c(0.451, 0.454,	0.458, 0.459, 0.460)

## Parâmetros de Classificação - BGI
alt5_bgi = c(0.005, 0.010, 0.015, 0.022, 0.026)
alt10_bgi = c(0.007, 0.013, 0.017, 0.024, 0.030)
alt15_bgi = c(0.004, 0.008, 0.011, 0.017, 0.022)

BV = db(c('vari5', 'vari10', 'vari15', 'bgi5', 'bgi10', 'bgi15'),
        list(alt5_vari, alt10_vari, alt15_vari, alt5_bgi, alt10_bgi,
        alt15_bgi), labels) # BD

### Teste de Normalidade e Homogeneidade
NDH = ndh(BV)

### Distribuição Normal dos Dados
titles = c('VARI - 5m', 'VARI - 10m', 'VARI - 15m', 'BGI - 5m', 'BGI - 10m',
           'BGI - 15m')

windows()
layout(matrix(c(1:6), nrow = 2, byrow = TRUE))
for (i in 1:length(titles)) {
  gf(BV[[i]], titles[i])
}
dev.off()

rm(BV, NDH, alt5_vari, alt10_vari, alt15_vari, alt5_bgi, alt10_bgi,
   alt15_bgi) # Limpando memória
