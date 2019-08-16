# Copyright (c) 2019 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.

library(shiny)
library(data.table)
library(dplyr)

ui <- fluidPage(
  # App title ----
  titlePanel("複数CSVファイルの左結合"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
  # Sidebar panel for inputs ----
  sidebarPanel(
    # Input: Select a file ----
    fileInput(
      "files",
      "CSVファイルを選択",
      multiple = TRUE,
      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
    ),

    selectInput("fileenc", "テキストエンコーディング", choices = c("UTF-8", "UTF-8-BOM", "CP932")),

    htmlOutput("leftcsv"), # left joinの再左端となるcsv

    htmlOutput("keycolname"), # キー項目を含む列

    # Horizontal line ----
    tags$hr(),

    # Input: Checkbox if file has header ----
    checkboxInput("header", "ヘッダー行", TRUE),

    # Input: Select separator ----
    radioButtons(
      "sep",
      "区切り文字",
      choices = c(
        Comma = ",",
        Semicolon = ";",
        Tab = "\t"
      ),
      selected = ","
    ),

    # Input: Select quotes ----
    radioButtons(
      "quote",
      "テキスト修飾子",
      choices = c(
        None = "",
        "Double Quote" = '"',
        "Single Quote" = "'"
      ),
      selected = '"'
    ),

    # Horizontal line ----
    tags$hr(),

    # Input: Select number of rows to display ----
    radioButtons(
      "disp",
      "表示する行",
      choices = c(Head = "head", All = "all"),
      selected = "head"
    )

  ),

  # Main panel for displaying outputs ----
  mainPanel(
    # Output: Data file ----
    tableOutput("contents"))
  )
)

getColumnsWhichhasUniqueItems = function(df) {
  result = c()
  cols = colnames(df)
  for (col in cols) {
    vec = as.vector(df[, col])
    if (length(unique(vec)) == length(vec)) {
      # if(is.character(vec) && length(unique(vec))==length(vec)){
      # 要素の型が文字列型
      # かつ
      # そのラベル列の要素に重複するものがない
      result = c(result, col)
    }
  }

  return(as.character(result))
}

server <- function(input, output) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$files)

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch({

      if (length(input$leftcsv) == 0) {
        output$leftcsv = renderUI({
          selectInput("leftcsv", "Leftmost", input$files$name)
        })
      } else {

        fileenc = 'UTF-8'
        if (length(input$fileenc) > 0) {
          fileenc = input$fileenc
        }

        # 選択されたファイル名を持つファイルのパスを取得する
        path = input$files[input$files$name == input$leftcsv, 'datapath']

        df_left <- read.csv(
          path,
          fileEncoding = fileenc,
          header = input$header,
          sep = input$sep,
          quote = input$quote
        )

        col_by = 'username'
        if (length(input$keycolname) == 0) {
          output$keycolname = renderUI({
            selectInput("keycolname", "Key", getColumnsWhichhasUniqueItems(df_left), selected = getColumnsWhichhasUniqueItems(df_left)[1])
          })
        } else {
          col_by = input$keycolname
        }

        paths = input$files[input$files$name != input$leftcsv, 'datapath']
        for (p in paths) {
          # 一番左のCSV以外を読み込んでjoinする
          df <- read.csv(
              p,
              fileEncoding = fileenc,
              header = input$header,
              sep = input$sep,
              quote = input$quote
          )
          df <- inner_join(df_left, df, by = col_by)
        }

        # 列の順序を入れ替え
        s = as.character(colnames(df))
        s = sort(s)
        s = s[-which(s %in% col_by)]
        df = df[, c(col_by, s)]
      }

    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })

    if (input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }

  })

}

shinyApp(ui = ui, server = server)