#' @include AmBalloon.R
setClassUnion(name = "AmBalloonOrMissing", members = c("AmBalloon", "missing"))

#' @include AmGraph.R
setClassUnion(name = "AmGraphOrMissing", members = c("AmGraph", "missing"))

#' @include AmGraph.R
setClassUnion(name = "AmGraphOrCharacterOrMissing", members = c("AmGraph", "character", "missing"))

#' @include AmLegend.R
setClassUnion(name = "AmLegendOrMissing", members = c("AmLegend", "missing"))

#' @include ChartCursor.R
setClassUnion("ChartCursorOrMissing", c("ChartCursor", "missing"))

#' @include ChartScrollbar.R
setClassUnion("ChartScrollbarOrMissing", c("ChartScrollbar", "missing"))

#' @include DataSet.R
setClassUnion("DataSetOrMissing", c("DataSet", "missing"))

#' @include GaugeArrow.R
setClassUnion(name = "GaugeArrowOrMissing", members = c("GaugeArrow", "missing"))

#' @include GaugeAxis.R
setClassUnion(name = "GaugeAxisOrMissing", members = c("GaugeAxis", "missing"))

#' @include GaugeAxis.R
setClassUnion(name = "GaugeAxisOrCharacterOrMissing", members = c("GaugeAxis", "character", "missing"))

#' @include GaugeBand.R
setClassUnion(name = "GaugeBandOrMissing", members = c("GaugeBand", "missing"))

#' @include Guide.R
setClassUnion(name = "GuideOrMissing", members = c("Guide", "missing"))

#' @include Label.R
setClassUnion(name = "LabelOrMissing", members = c("Label", "missing"))

#' @include PeriodSelector.R
setClassUnion(name = "PeriodSelectorOrMissing", members = c("PeriodSelector", "missing"))

#' @include StockEvent.R
setClassUnion(name = "StockEventOrMissing", members = c("StockEvent", "missing"))

#' @include StockPanel.R
setClassUnion(name = "StockPanelOrMissing", members = c("StockPanel", "missing"))

#' @include Title.R
setClassUnion(name = "TitleOrMissing", members = c("Title", "missing"))

#' @include TrendLine.R
setClassUnion("TrendLineOrMissing", c("TrendLine", "missing"))

#' @include ValueAxis.R
setClassUnion("ValueAxisOrMissing", c("ValueAxis", "missing"))

#' @include ValueAxis.R
setClassUnion("ValueAxisOrCharacterOrMissing", c("ValueAxis", "character", "missing"))