

################################################################################################
###### PACKAGES USED  ---------
################################################################################################



#for shiny related packages
library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)

#for graphs/plots/data manipulation
library(ggplot2)
library(DT)
library(gapminder)
library(tidyverse)
library(dplyr)
library(heatmaply)
library(plotly)
library(factoextra)
library(NbClust)

#for installing bio analysis-related packages available only in the Bioconductor Repository
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# library(BiocManager)

#these commands are required to install the 3 packages below from Bioc Repo
# BiocManager::install("topGO")
# BiocManager::install("org.Sc.sgd.db")
# BiocManager::install("Rgraphviz")
# BiocManager::install("clusterProfiler")

#for enrichment analysis
library(topGO)
library(org.Sc.sgd.db)
library(Rgraphviz)
#library(clusterProfiler)


# for GSEA plot
library(igraph)
library(visNetwork)
library(networkD3)
library(ggraph)


#for interactive bar plot
library(highcharter)

#for animation
library(gganimate)
theme_set(theme_bw())
library(gifski)
library(png)

#for reading in data
library(readr)

#for maps
library(maps)
library(mapdata)

#for interactive questions
library(shinysurveys)

