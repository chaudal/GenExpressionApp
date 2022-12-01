
#___________________________________________________________
#
#-----------------------------------------------------------
#                          SERVER 
#-----------------------------------------------------------
#___________________________________________________________


# Title: Server Gene Expression Shiny App
# Authors: Alisha Chaudhry, Logain Elnimeiry, Salem Bajjali


#___________________________________________________________
#                      SOURCE DATA                          
#___________________________________________________________

source("~/Desktop/App_files/Packages_file.R")
source("~/Desktop/App_files/processing_data.R")
library(knitr)



server <- function(input, output, session) {
  
  #___________________________________________________________
  #                   HOME PAGE - MAIN TAB 1
  #___________________________________________________________
  
  
  # Action button to go to 'About the Data' tab
  observeEvent(input$startbutton, {
    
    updateTabsetPanel(session, "inTabset", selected = "About the Data")
  })
  
  
  #___________________________________________________________
  #               ABOUT THE DATA - MAIN TAB 2                 
  #___________________________________________________________

  
  #------------------------------------------------------------
  #                GENE EXPRESSION DATA - SUBTAB 1                                        
  #------------------------------------------------------------
  
  
  # Reactive value for selected data
  datasetInput <- reactive({
    switch(input$dataset,
           "Gasch Yeast Expression Data" = expr_data_df,
           "Gasch Yeast Description" = gene_info_table)
  })
  
  # Table of user-selected data
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of user-selected data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  # Creating a table containing user-selected data
  output$mytable = DT::renderDataTable({
    expr_data_df},
    extensions = "FixedColumns",
    options = list(scrollX = TRUE,fixedColumns = list(leftColumns = 2)))
  
  
  # Action button to go to 'Gene Information' tab
  observeEvent(input$switch_to_geneinfo_subtab, {
    updateTabsetPanel(session, "inDataTabset", selected = "Gene Information")
  })
  
  
  
  #------------------------------------------------------------
  #                GENE INFORMATION - SUBTAB 2                                            
  #------------------------------------------------------------
  

  # Reactive value for user-selected data
  datasetInput2 <- reactive({
    switch(input$dataset2,
           "Gasch Yeast Expression Data" = expr_data_df,
           "Gasch Yeast Description" = gene_info_table)
  })
  
  # Table of user-selected data
  output$table <- renderTable({
    datasetInput2()
  })
  
  # Downloadable csv of user-selected data
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste(input$dataset2, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput2(), file, row.names = FALSE)
    }
  )
  
  # Creating a table containing user-selected data
  output$mytable2 = DT::renderDataTable({
    gene_info_table
  }, extensions = "FixedColumns",
  options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2) ))
  
  
  # Action button to go to 'Major Clustering Analysis' tab
  observeEvent(input$switch_to_clustering_analysis_tab, {
    updateTabsetPanel(session, "inTabset",
                     selected = "Clustering Analysis"
    )
  })
  
  
  #___________________________________________________________
  #             CLUSTERING ANALYSIS - MAIN TAB 3              
  #___________________________________________________________
  
  #------------------------------------------------------------
  #                 ABOUT CLUSTERING - SUBTAB 1                                          
  #------------------------------------------------------------
  
  
  # Action button to go to 'Major Clustering Patterns' tab
  observeEvent(input$switch_to_step1_subtab, {
    updateTabsetPanel(session, "inKmeansTabset", selected = "Major Clustering Patterns")
  })
  
  #------------------------------------------------------------
  #            MAJOR CLUSTERING PATTERNS - SUBTAB 2                                           
  #------------------------------------------------------------
  
  # Tutorial question:
  output$valueHeat <- renderPrint({
    if(input$radioHeat == 2){return('Correct! Great job!')}
    else if(input$radioHeat == 4){return('Select an answer choice.')}
    else{return('Not quite. Try again. Hint: Look for clusters in the heat map where the gene expression is similar.')}})
  
  
  # 'Choosing an Optimal Cluster Number' plots
  output$methodClusterPlot <- renderPlot({
    
    fviz_nbclust(cluster_data, kmeans, method = input$methodCluster,k.max = 20,iter.max=15)
    
  })
  
  
  # 'Visualizing Patterns with Heat Maps' plot
  output$HeatMapPlot <- renderPlotly({
    
    condition = cluster_data[rownames(cluster_data[1:100,]),]
    
    gene_desc <- gene_info[rownames(condition), 1:3]
    genedescmat <- t(gene_desc[3])
    conditions.text <- glue::glue("Gene Name: {gene_desc$gene_name} \nGene Description: {gene_desc$description}")
    n=173
    genedescmatnew <- do.call("rbind", replicate(n, conditions.text, simplify = FALSE)) %>% as.matrix()
    
    matrix_data = t(as.matrix(condition))
    
    heatmaply(matrix_data,
              colorscale = "Viridis",
              dendrogram = "both",
              row_dend_left = TRUE,
              scale = "column",
              margins = c(60,100,40,20),
              #grid_color = "black",
              grid_gap = .7,
              grid_width =  0.01,
              hide_colorbar = FALSE,
              branches_lwd = 0.1,
              label_names = c("Treatment", "Gene ORF", "Expression Level"),
              fontsize_row = 6, fontsize_col = 8,
              showticklabels = FALSE,
              custom_hovertext = genedescmatnew,

              heatmap_layers = theme(axis.line=element_blank())

    )
  })
  
  # Action button to go to 'Explore Cluster Sizes' tab
  observeEvent(input$switch_to_exploreclustersizes_tab, {
    updateTabsetPanel(session, "inKmeansTabset", selected = "Explore Cluster Sizes")
  })
  
  
  #------------------------------------------------------------
  #              EXPLORE CLUSTER SIZES SUBTAB 3                                              
  #------------------------------------------------------------

  # Tutorial question
  output$valueClustSize <- renderPrint({
    if(input$radioClustSize == 1){return('Correct! Great job!')}
    else if(input$radioClustSize == 3){return('Select an answer choice.')}
    else{return('Not quite. Try again.')}})
  
  # Plot input
  allclusters <- reactive({
    set.seed(300)
    kmeans(cluster_data, centers = input$centroids)
  })
  
  # Reactive: making the plot react with user input
  df.allclusters <-
    reactive({
      data.frame(clusternumber = 1:length(allclusters()$size),
                 freq = allclusters()$size)
    })
  
  # Data point labels when hovering over
  point.format = paste("{point.cluster}</span><br/><span>",
                       
                       "Number of Genes: {point.y}</span>",
                       sep = "")
  
  # Bar plot of cluster sizes
  output$barplotclustsizes <- renderHighchart({
    hc =  highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(
        categories = df.allclusters()$clusternumber,
        title = list(text = "Cluster Number"),
        labels = list(format = "Cluster {value}"),
        tickInterval = 1,
        step = 1
      ) %>%
      hc_colors("#39568CFF") %>% 
      hc_add_series(data = df.allclusters()$freq) %>%
      hc_yAxis(title = list(text = "Number of Genes in Cluster")) %>%
      
      # Data point labels when hovering over
      hc_tooltip(headerFormat = "<span><b>Cluster {point.key}</b>",
                 pointFormat = point.format) %>%
      hc_legend(enabled = F)
    
    
  })
  
  # Action button to go to 'Gene Expression Analysis' tab
  observeEvent(input$switch_to_step3_subtab, {
    updateTabsetPanel(session, "inTabset", selected = "Gene Expression Analysis")
  })
  
  
  #___________________________________________________________
  #         GENE EXPRESSION ANALYSIS - MAIN TAB 4             
  #___________________________________________________________

  #------------------------------------------------------------
  #              VISUALIZE CLUSTER PROFILE - SUBTAB 1                                              
  #------------------------------------------------------------
  
  # User-selected cluster number
  selectedData2 <- reactive({
    
    cluster <- which(allclusters()$cluster == input$clustnum)
    
  })
  
  # Render the line plot based on the above inputs
  output$plot2 <- renderPlotly({
    
    cluster_matrix <-
      matrix(
        cluster_data[selectedData2(), ],
        nrow = length(selectedData2()),
        ncol = ncol(cluster_data)
      )
    cl_data <- as.data.frame(t(cluster_matrix))
    rownames(cl_data) <- colnames(cluster_data)
    colnames(cl_data) <-  names(selectedData2())
    
    cl_data <- rownames_to_column(cl_data, var = 'Conditions')
    cl_data_long <- cl_data %>%
      pivot_longer(!Conditions, names_to = 'genes', values_to = 'expression')

    
    temp_cl_data_long<- merge(cl_data_long,gene_info,by.x = "genes" , by.y = "ORF",all.x = TRUE)
    
    fig <- ggplot(temp_cl_data_long, 
                  aes(x = Conditions, y = expression,group = genes,
                      color = genes,
                      text=paste0("Gene: ", genes,"\nExpression: ", expression,"\nCondition: ",Conditions,
                                  "\nDescription: ",description, "\nGene Function: ",gene_function)))+
      geom_line(alpha = 0.8) +
      geom_line(stat = 'summary', fun = 'mean', colour = 'black', size = 1.5,
                aes(group = 1)) +
      theme_classic() +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) + 
      xlab("Stress Conditions") +
      ylab("Gene Expression") + labs(color = "Genes")
    
    test1 <- ggplotly(fig,tooltip = 'text') 
    
  })
  
 
  # Action button to go to 'View Cluster Heat Map' tab
  observeEvent(input$switch_to_ViewClusterHeatmap_tab, {
    updateTabsetPanel(session, "ingeneExpTabset", selected = "View Cluster Heat Map")
  })
  
  # Tutorial Question:
  output$value <- renderPrint({
    if(input$radio == 3){return('Correct! Great job! Notice that the most of the gene functions in cluster 6 under nitrogen depletion conditions are related to biosynthesis of amino acids like arginine and lysine.')}
    else if(input$radio == 5){return('Select an answer choice.')}
    else{return('Not quite. Try again. Hint: Look at the first highest peak in cluster 6.')}})

 
  #------------------------------------------------------------
  #              VIEW CLUSTER HEAT MAP - SUBTAB 2                                              
  #------------------------------------------------------------

  # Pop-up text on 'Enrichment Analysis' tab 
  # Pops up when previous tab's action button is clicked to go to enrichment analysis tab
  observeEvent(input$switch_to_enrichmentAnalysis_tab, {
    showModal(
      modalDialog(
        title = "About this tab",
        HTML("The analysis in this tab is Gene Ontology Term Enrichment. This analysis takes the yeast DNA microarray data from the yeast we have been utilizing and gives us the gene products and functions associated with the genes that were found to have the highest expression in our data.
        
        <br><br>You'll want to play around with the inputs on the left side of this page to check out different gene clusters and different pieces of information about those clusters based on their expression that will help you make biological connections.
        
         <br><br>This information is valuable when we are trying to compare expression in different samples. In this case, we can use this information to compare the genes that have higher expression based on the different environmental conditions the yeast samples were subjected to.
         
         <br><br>Happy exploring!"),
        size = "l",
        easyClose = T,
        footer = NULL
      )
    )
  })
  
  # User-selected cluster number
  selectedData_3 <- reactive({
    
    cluster <- which(allclusters()$cluster == input$Clusternumber_2)
    
  })
  
  
  # Heat map plot
  output$HeatMapPlot_2 <- renderPlotly({
    
    cluster_gene_names = rownames(cluster_data)[selectedData_3()]
    
    condition = cluster_data[cluster_gene_names,]
    
    gene_desc <- gene_info[rownames(condition), 1:3]
    genedescmat <- t(gene_desc[3])
    conditions.text <- glue::glue("Gene Name: {gene_desc$gene_name} \nGene Description: {gene_desc$description}")
    n=173
    genedescmatnew <- do.call("rbind", replicate(n, conditions.text, simplify = FALSE)) %>% as.matrix()
    
    matrix_data = t(as.matrix(condition))

    cluster_heatmap <- heatmaply(matrix_data,
                                 distfun= 'pearson',
                                 hclust_method = 'average',
                                 colorscale = "Viridis",
                                 dendrogram = "both",
                                 row_dend_left = TRUE,
                                 scale = "column",
                                 margins = c(60,100,40,20),
                                 grid_gap = .7,
                                 grid_width =  0.1,
                                 hide_colorbar = FALSE,
                                 branches_lwd = 0.1,
                                 label_names = c("Treatment", "Gene ORF", "Expression Level"),
                                 fontsize_row = 6, fontsize_col = 8,
                                 showticklabels = FALSE,
                                 custom_hovertext = genedescmatnew,
                                 heatmap_layers = theme(axis.line=element_blank())) 
    
    cluster_heatmap 

  })
  
  

  
  # Action button to go to 'Enrichment Analysis' tab
  observeEvent(input$switch_to_enrichmentAnalysis_tab, {
    updateTabsetPanel(session, "inTabset", selected = "Enrichment Analysis")
  })
  
  
  
  #___________________________________________________________
  #           ENRICHMENT ANALYSIS - MAIN TAB 5              
  #___________________________________________________________  
 
  # Clickable display button to retrieve information popup
  observeEvent(input$show, {
    showModal(
      modalDialog(
        title = "About this tab",
        HTML("The analysis in this tab is Gene Ontology Term Enrichment. This analysis takes the yeast DNA microarray data from the yeast we have been utilizing and gives us the gene products and functions associated with the genes that were found to have the higest expression in our data.
        
        <br><br>You'll want to play around with the inputs on the left side of this page to check out different gene clusters and different pieces of information about those clusters based on their expression that will help you make biological connections.
        
         <br><br>This information is valuable when we are trying to compare expression in different samples. In this case, we can use this information to compare the genes that have higher expression based on the different environmental conditions the yeast samples were subjected to.
         
         <br><br>Happy exploring!"),
        size = "l",
        easyClose = T,
        footer = NULL
      )
    )
  })
  

  #------------------------------------------------------------
  #             ENRICHMENT FUNCTION - SUBTAB 1                                               
  #------------------------------------------------------------

  output$valueEnf <- renderPrint({
    if(input$radioEnf == 3){return('Correct! Great job!')}
    else if(input$radioEnf == 4){return('Select an answer choice.')}
    else{return('Not quite. Try again. Hint: Look back at the gene functions in under nitrogen depletion conditions for cluster 6 under the Gene Expression Analysis tab in Visualize Cluster Profile section.' )}})
  gene_choice <-
    reactive({
      rownames(cluster_data[which(allclusters()$cluster == input$clustnum2),]) 
      })

  output$table2 <- renderTable({
    all_ids = mappedkeys(org.Sc.sgdGO)
    ORFs = rownames(cluster_data)
    gene_universe = all_ids[all_ids %in% ORFs]
    
    
    interesting_genes <- gene_choice()
    gene_list = factor(as.integer(gene_universe %in% interesting_genes))
    names(gene_list) = gene_universe
    
    
    
    orf_2_go_list = as.list(org.Sc.sgdGO[intersect(gene_universe, mappedkeys(org.Sc.sgdGO))])
    orf_2_go_final = lapply(orf_2_go_list, function(x) {
      unique(unlist(lapply(x, `[[`, 'GOID')))
    })
    
    GO_bp_tester = new(
      'topGOdata',
      description = 'Yeast Genes Enriched Functions',
      ontology = input$GOtype,
      allGenes = gene_list,
      annotationFun = annFUN.gene2GO,
      gene2GO = orf_2_go_final
    )
    
    result_fisher = runTest(GO_bp_tester, algorithm = 'classic', statistic = 'fisher')
    GenTable(
      GO_bp_tester,
      fisher_p = result_fisher,
      orderBy = 'fisher_p',
      topNodes = input$GOnodes,
      numChar=1000
    )
    
  },
  
  striped = T,
  hover = T,
  bordered = FALSE,
  width = "100%")
  
  
  # Action button to go to 'Visualize Network Plot' tab 
  observeEvent(input$switch_to_networkplot_tab, {
    updateTabsetPanel(session, "inEATabset", selected = "Visualize Network Plot")
  })
  
  
  #------------------------------------------------------------
  #             VISUALIZE NETWORK PLOT - SUBTAB 2                                            
  #------------------------------------------------------------

   output$valueNet <- renderPrint({
    if(input$radioNet == 2){return('Correct! Great job!')}
    else if(input$radioNet == 5){return('Select an answer choice.')}
    else{return('Not quite. Try again. Hint: Look at the yellow-colored shape. Also, make sure the number of centroids is set to 50 on the Explore Cluster Sizes section under Clustering Analysis.')}})

  output$img1 <- renderVisNetwork({
    all_ids = mappedkeys(org.Sc.sgdGO)
    ORFs = rownames(cluster_data)
    gene_universe = all_ids[all_ids %in% ORFs]
    
    interesting_genes <- gene_choice()
    gene_list = factor(as.integer(gene_universe %in% interesting_genes))
    names(gene_list) = gene_universe
    
    
    
    orf_2_go_list = as.list(org.Sc.sgdGO[intersect(gene_universe, mappedkeys(org.Sc.sgdGO))])
    orf_2_go_final = lapply(orf_2_go_list, function(x) {
      unique(unlist(lapply(x, `[[`, 'GOID')))
    })
    
    GO_bp_tester = new(
      'topGOdata',
      description = 'Yeast Genes Enriched Functions',
      ontology = input$GOtype,
      allGenes = gene_list,
      annotationFun = annFUN.gene2GO,
      gene2GO = orf_2_go_final
    )
    
    result_fisher = runTest(GO_bp_tester, algorithm = 'classic', statistic = 'fisher')
    GenTable(
      GO_bp_tester,
      fisher_p = result_fisher,
      orderBy = 'fisher_p',
      topNodes = input$GOnodes
    )
    
    showSigOfNodes(
      GO_bp_tester,
      score(result_fisher),
      useInfo = 'all',
      useFullNames = T
    )
    
    # Pulling out the nodes and edges
    mynodes <- showSigOfNodes(GO_bp_tester, score(result_fisher))[["dag"]]@nodes
    myedges <- names(showSigOfNodes(GO_bp_tester, score(result_fisher))[["dag"]]@edgeL)
    
    # Pulling out all the go terms scores
    len_fisher_pval <- length(result_fisher@score)
    
    # Creating a new results table that provides us with all the go term information
    all_results = GenTable(GO_bp_tester, fisher_p = result_fisher, orderBy = 'fisher_p', numChar=1000, topNodes = len_fisher_pval)
    
    # Extracting the information needed to generate the plot & tooltip
    index = all_results$GO.ID %in% mynodes
    
    tooltip_table = all_results[index,]
    
    tooltip_table$title =glue::glue("GO.ID: {tooltip_table$GO.ID} <br> p-value: {tooltip_table$fisher_p} <br> Info: {tooltip_table$Term} <br> Genes Annotated: {tooltip_table$Annotated}")
    
    
    # Creating an igraph object
    lo <- graph_from_graphnel(showSigOfNodes(GO_bp_tester, score(result_fisher),useInfo = 'all',
                                             useFullNames = T)[["dag"]])
    
    
    # Pulling out the shapes and colors
    description <- showSigOfNodes(GO_bp_tester, score(result_fisher))[["complete.dag"]]@AgNode
    
    topgo_shape <- unlist(lapply(description,"shape"))
    topgo_color <- unlist(lapply(description,"fillcolor"))
    
    
    
    # Pulling out the x and y location
    x_points <- c()
    y_points <- c()
    for (i in 1:length(mynodes)){
      x_points[i] = description[[i]]@center@x
      y_points[i] = description[[i]]@center@y
    }
    
    # Picking out specific color and shape
    V(lo)$pval <- round( result_fisher@score[mynodes],4)
    V(lo)$color <- topgo_color
    V(lo)$color  <- ifelse(V(lo)$color == "#FF8000", " #95D840FF ",ifelse(V(lo)$color == "#FF0000"," #FDE725FF"," #29AF7FFF "))
    V(lo)$shape  <- topgo_shape
    
    
    # Pulling out the significant values
    fisher_pval <- result_fisher@score[mynodes]
    mydata <- toVisNetworkData(lo)
    
    
    
    # Combining the tooltip table with the mydata$table in order to show the tooltip on the visnetwork graph
    mydata$nodes = tooltip_table %>% 
      rename("GO.ID" ="id" ) %>% 
      dplyr::select(id ,title) %>% 
      inner_join(mydata$nodes, . , by = "id")
    
    
    
    # Creating a coordinates matrix
    coords <- as.matrix(data.frame(x = x_points, y = y_points,stringsAsFactors = FALSE))
    
    # Creating a visNetwork plot
    visNetwork(nodes = mydata$nodes, edges = mydata$edges) %>%
      visIgraphLayout(layout = "layout.norm",layoutMatrix = coords) %>%
      visHierarchicalLayout(sortMethod = "directed",blockShifting = TRUE,nodeSpacing = 190,levelSeparation = 200,edgeMinimization = TRUE) %>%
      visEdges( arrows = "to", color = "black", smooth = FALSE, width = 1) %>%
      visInteraction(navigationButtons = TRUE) %>% 
      visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T),
                 nodesIdSelection = list(enabled = TRUE,useLabels = FALSE)) 
  })
  
}



#___________________________________________________________
#                    *** END OF SCRIPT ***
#___________________________________________________________

