setOldClass("tbl_df")
setOldClass("tbl_dt")
setClassUnion(name = "DataFrame", members = c("data.table", "data.frame", "tbl_df", "tbl_dt"))

#' @include class_AmBalloon.R
setClassUnion(name = "AmBalloonOrMissing", members = c("AmBalloon", "missing"))

#' @include class_AmGraph.R
setClassUnion(name = "AmGraphOrMissing", members = c("AmGraph", "missing"))

#' @include class_AmGraph.R
setClassUnion(name = "AmGraphOrCharacterOrMissing", members = c("AmGraph", "character", "missing"))

#' @include class_AmLegend.R
setClassUnion(name = "AmLegendOrMissing", members = c("AmLegend", "missing"))

#' @include class_ChartCursor.R
setClassUnion("ChartCursorOrMissing", c("ChartCursor", "missing"))

#' @include class_ChartScrollbar.R
setClassUnion("ChartScrollbarOrMissing", c("ChartScrollbar", "missing"))

#' @include class_DataSet.R
setClassUnion("DataSetOrMissing", c("DataSet", "missing"))

#' @include class_GaugeArrow.R
setClassUnion(name = "GaugeArrowOrMissing", members = c("GaugeArrow", "missing"))

#' @include class_GaugeAxis.R
setClassUnion(name = "GaugeAxisOrMissing", members = c("GaugeAxis", "missing"))

#' @include class_GaugeAxis.R
setClassUnion(name = "GaugeAxisOrCharacterOrMissing", members = c("GaugeAxis", "character", "missing"))

#' @include class_GaugeBand.R
setClassUnion(name = "GaugeBandOrMissing", members = c("GaugeBand", "missing"))

#' @include class_Guide.R
setClassUnion(name = "GuideOrMissing", members = c("Guide", "missing"))

#' @include class_Label.R
setClassUnion(name = "LabelOrMissing", members = c("Label", "missing"))

#' @include class_PeriodSelector.R
setClassUnion(name = "PeriodSelectorOrMissing", members = c("PeriodSelector", "missing"))

#' @include class_StockEvent.R
setClassUnion(name = "StockEventOrMissing", members = c("StockEvent", "missing"))

#' @include class_StockPanel.R
setClassUnion(name = "StockPanelOrMissing", members = c("StockPanel", "missing"))

#' @include class_Title.R
setClassUnion(name = "TitleOrMissing", members = c("Title", "missing"))

#' @include class_TrendLine.R
setClassUnion("TrendLineOrMissing", c("TrendLine", "missing"))

#' @include class_ValueAxis.R
setClassUnion("ValueAxisOrMissing", c("ValueAxis", "missing"))

#' @include class_ValueAxis.R
setClassUnion("ValueAxisOrCharacterOrMissing", c("ValueAxis", "character", "missing"))