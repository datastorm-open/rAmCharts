library(rAmCharts)
library(magrittr)

# Pour une légende, peut-on (et si oui comment) afficher la légende avec
# les couleurs + labels mais pas les valeurs de chaque classe (par exemple pour un "amPie") ?

data("data_pie")
amPie(data = data_pie) %>%
  setLegend(valueText = "")



# ---



# Dans le cas d'un graphique de type "ligne" (fait avec "amSerialChart"),
# est-il possible de spécifier l'épaisseur souhaitée pour la courbe tracée ?

amPlot(x = rnorm(100), lwd = 3, type = 'l')

# ou ...

dataProvider <- data.frame(x = paste0(1:100, ""), y = rnorm(100))
amSerialChart(dataProvider = dataProvider, categoryField = "x") %>%
  addGraph(valueField = "y", lineThickness = 3)



# ---



# Pour un "amBarplot", est-ce qu'il y a la possibilité de modifier le "libellé au survol"
# (du style avec une option "balloonText"), et si oui comment ?

# > malheureusement nous n'avons pas intégré cette fonctionnalité dans la fonction, elle se ferait comme ça...
data("data_bar")
amSerialChart(dataProvider = data_bar, categoryField = "country") %>%
  addGraph(valueField = "visits", type = "column", fillAlphas = 1,
           lineColorField = "color", fillColorsField = "color",
           balloonText = "The value for [[category]] is [[value]]")
# plus de détails avec la commande api("AmGraph")
# i.e. sur la page http://docs.amcharts.com/3/javascriptstockchart/AmGraph



# ---



# Pour un graph en "pyramide" (type "amFunnel"), est-il possible de spécifier
# la largeur minimale de la base  En effet, lorsque j'intègre un tel graphique
# dans un élément "box" de shiny, celui-ci se retrouve pas défaut "tout écrasé/tout fin".
# En gros, plus mon écran est grand, plus ma pyramide est écrasée,
# et en taille "téléphone mobile", c'est là qu'elle est la mieux... Étrange.


data(data_funnel)
amFunnel(data = data_funnel, inverse = TRUE) %>%
  setProperties(baseWidth = 400)
