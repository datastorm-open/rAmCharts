library("magrittr")
library("rAmCharts")
data(data_bar)

amBarplot(x = "country", y = "visits", data = data_bar) %>%
  resetProperties("valueAxes") %>%  # supprime tous les axes des ordonnées. Tel quel, on laisse amcharts instancier lui même l'axe avec les propriétés par défaut
  addValueAxis(maximum = 2000, minimum = 200, strictMinMax = TRUE,
               autoGridCount = FALSE, gridCount = 10) # On ajoute un axe, amcharts va instancie l'axe avec les propriétés par défaut sauf celles renseignées
