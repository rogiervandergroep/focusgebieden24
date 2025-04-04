library(DiagrammeR)
library(DiagrammeRsvg)
library(tidyverse)
library(rsvg)


grDevices::windowsFonts("Amsterdam Sans" = grDevices::windowsFont("Amsterdam Sans"))
font <- "Amsterdam Sans"

grViz("digraph flowchart {

      # node definitions with substituted label text
      
      node [fontname = font, shape = rectangle] 
      tab1  [label = '@@1']
      tab5  [label = '@@5']
      tab7  [label = '@@7']
      tab8  [label = '@@8']

      node [fontname = font, shape = cylinder] 
      tab2  [label = '@@2']
      tab3  [label = '@@3']
      tab4  [label = '@@4']
      tab6  [label = '@@6']
      
      
      # edge definitions with the node IDs
      tab1 -> tab4;
      tab2 -> tab4;
      tab3 -> tab4;
      tab4 -> tab5;
      tab4 -> tab6;
      tab6 -> tab7;
      tab6 -> tab8;

      }

      [1]: 'stuurbestand input xlsx'
      [2]: 'BBGA csv'
      [3]: 'extra datasets 1.csv; 2.csv; ... ; n.csv'
      [4]: 'BASISBESTAND FOCUSGEBIEDEN'
      [5]: 'definitieve indicatorenlijsten xlsx'
      [6]: 'ruwe datasets Zuidoost, Noord en Nieuw-West'
      [7]: 'publicatietabellen Zuidoost, Noord en Nieuw-West'
      [8]: 'figuren Zuidoost, Noord en Nieuw-West'
      ") |>
  export_svg() |> 
  charToRaw() |> 
  rsvg_svg("assets/images/graph.svg")
