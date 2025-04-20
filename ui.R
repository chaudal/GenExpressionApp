
#___________________________________________________________
#
#-----------------------------------------------------------
#                          UI 
#-----------------------------------------------------------
#___________________________________________________________


# Title: UI Gene Expression Shiny App
# Authors: Alisha Chaudhry, Logain Elnimeiry, Salem Bajjali


#___________________________________________________________
#                      SOURCE DATA                          
#___________________________________________________________


source("~/Desktop/App_files/Packages_file.R")
source("~/Desktop/App_files/processing_data.R")


ui <- shinyUI(fluidPage(
  
  # Selecting theme:
  theme = shinytheme("yeti"),
  
  # Title of the panel
  # mainPanel(img(src='G.png', align = "left")),#titlePanel() #"Gene Expression Analysis"
  titlePanel(div(
    img(
      src = "logo.png",
      height = 100,
      width = 400 #,
       # style = "margin:10px 40px",
    ))),
  
  navbarPage(
    
    "",
    
    id = "inTabset",
    
    
    #___________________________________________________________
    #                  HOME PAGE - MAIN TAB 1                 
    #___________________________________________________________
    
    tabPanel(
      icon("house"),
      
      #'What you'll find here' Section
      fluidRow(
        column(3),
        column(6,
               shiny::HTML("<br><br><center> <h1>What you'll find here</h1> </center><br>"),
               shiny::HTML("<h4><center>This app is an interactive tool to help you explore gene expression analysis. It doesn't matter how much you know about gene expression analysis, this app is here to help you understand the basics. It will take you step-by-step through a few different tools computational biologists use to investigate the meaning behind biological data. </center></h4>")),
        column(3)
      ),
      
      #----------------------------
      
     
      fluidRow(
        
        style = "height:50px;"),
      
      # PAGE BREAK
      tags$hr(),
      
      #----------------------------
      
      
      #'Gene expression analysis made easy' Section  
      fluidRow(
        shiny::HTML("<br><br><center> <h1>Gene expression analysis made easy</h1> </center>
                                            <br>")
      ),
      
      fluidRow(
        column(3),
        # column(2),
        
        column(2,
               div(class="panel panel-default", #height = "800px", 
                   div(class="panel-body",  width = "600px", 
                       align = "center",
                       div(
                         tags$img(src = "one.png", 
                                  width = "50px", height = "50px")
                       ),
                       div(
                         h4(
                           "Explore the gene expression data"
                         )
                       )
                   )
               )
        ),
        column(2,
               div(class="panel panel-default", #height = "800px",
                   div(class="panel-body",  width = "600px", height = "800px",
                       align = "center",
                       div(
                         tags$img(src = "two.png", 
                                  width = "50px", height = "50px")
                       ),
                       div(
                         h4(
                           "Find patterns using visual tools"
                         )
                       )
                   )
               )
        ),
        column(2,
               div(class="panel panel-default", #height = "800px",
                   div(class="panel-body", width = "600px", height = "800px",
                       align = "center",
                       div(
                         tags$img(src = "three.png", 
                                  width = "50px", height = "50px")),
                       div(
                         h4(
                           "Connect the underlying biology"
                         )
                       )
                   )
               )
        ),
       
        column(3)

      ),
      
      #----------------------------
      
      
      fluidRow(
        
        style = "height:50px;"),
      
      # PAGE BREAK
      tags$hr(),
      
      #----------------------------
      
      # 'How this app can help you' Section
      fluidRow(
        column(3),
        column(6,
               shiny::HTML("<br><br><center> <h1>How this app can help you</h1> </center><br>"),
               shiny::HTML("<h4><center>We created this app as students for students. We break down various topics such as clustering analysis and enrichment analysis to help you get familiar with commonly used bioinformatics tools and methods to make biological interpretations.</center></h4>")
        ), 
        
        column(3)
      ),
      
  
      fluidRow(
        
        style = "height:50px;"),
      
      #----------------------------
      
      fluidRow(
        
        style = "height:50px;"),
      
      # PAGE BREAK
      tags$hr(),
      
      #----------------------------
      
      #'A special thanks to those...' Section
       fluidRow(
         column(3),
         column(6,
                shiny::HTML("<br><br><center> <h1>A special thanks to those who have supported us along the way</h1> </center><br>"),
                shiny::HTML("<h4><center>Thank you to our mentor, Dr. Chad Myers, for his fervent support and for providing a creative space for us to collaborate and learn together during our time in his class. Thank you also to Professor Marta Shore for her encouragement and for giving us the opportunity to start this project together as a team. Shout out to the wonderful TAs that supported our growth and gave us their time. </center></h4>")
         ),
         column(3)
       ),
      
      #----------------------------
      
      fluidRow(
        
        style = "height:50px;"),
      
      
      # PAGE BREAK
      tags$hr(),
      
      #----------------------------
      
      #'Ready to get started?' Section 
      fluidRow(shiny::HTML("<br><br><center> <h1>Ready to get started?</h1> </center>
                                                 <br>")
      ),
      fluidRow(
        column(3),
        column(6,
               div(shiny::actionButton("startbutton" ,"Start", class = "btn btn-primary btn-lg"), align = "center")
               # style="color: #fff; background-color: #337ab7
        ),
        column(3)
      ),
      
      fluidRow(style = "height:25px;"
      )
    ),
    
    #___________________________________________________________
    #               ABOUT THE DATA - MAIN TAB 2                 
    #___________________________________________________________
    

    tabPanel(
      "About the Data",
      
      tabsetPanel(
        id = "inDataTabset",
        
        #------------------------------------------------------------
        #                GENE EXPRESSION DATA - SUBTAB 1                                        
        #------------------------------------------------------------

        tabPanel(
          "Gene Expression Data",
          
          br(),
          
          "This app involves an exploration of gene expression signatures through cluster analysis. We will use this to visualize certain gene expression patterns. For example, we might look for whether genes that are co-expressed in response to several different environmental conditions perform similar functions in a given cell. In other words, the expression profiles of genes under certain conditions can give us some information about the functions that these genes perform.",
         
          HTML("The dataset used here is from an experimental study conducted by <a href = 'https://www.molbiolcell.org/doi/10.1091/mbc.11.12.4241' > Gasch et al. (2000)</a> using yeast cells. In their study, they sought to use yeast gene expression to understand how yeast cells adapt to different environmental stressors. Yeast is a useful model for studying gene function and may be helpful for understanding gene function in humans."),
          
          br(),
          br(),
          
          "The data table below displays the resulting data from their experiments. The columns are named after the
              condition labels used in the experiments performed by Gasch et al (2000) and rows are named
              after the yeast gene names. Each cell across each row is the observed expression for the named gene.",
          "The expression profiles can be visualized via heatmaps using certain visualization tools such as Java TreeView3. But for this app, we will explore them via plots generated using R. Additionally, the yeast gene names seen below can be individually explored using the Yeast Genome Database linked below. You may read more about each experimental condition in the Gasch et al. paper linked below.",
          
          br(),
          br(),
          em("Note: The Gasch et al (2000) dataset contains 6,152 genes. However, for the purpose of our analysis, we only looked at the top 2000 most variant genes."),
          
          br(),
          br(),
          h5(strong("Resources")),
          
          tags$a(
            href = "https://www.yeastgenome.org/reference/S000059106#overview",
            "Link to Yeast Genome Database.",
            target = "_blank"
          ),
          br(),
          # Creating a link to a web page
          tags$a(
            href = "https://www.molbiolcell.org/doi/10.1091/mbc.11.12.4241",
            "Reference: Gasch AP, et al. (2000) Genomic expression programs in the response of yeast cells to environmental changes. Mol Biol Cell 11(12):4241-57.",
            
            # Open on a new page
            target = "_blank"
          ),
          
          br(),
          br(),
          actionButton(
            "switch_to_geneinfo_subtab",
            "Proceed to Gene Information"
          ),
          br(),
          br(),
          
          h2("Yeast Expression Data"),
          DT::dataTableOutput("mytable"),
          

          selectInput("dataset", "Choose a dataset:",
                      choices = c("Gasch Yeast Expression Data","Gasch Yeast Description")),
          
          # Button
          downloadButton("downloadData", "Download")
          
        ),
        
        
        #------------------------------------------------------------
        #                GENE INFORMATION - SUBTAB 2                                            
        #------------------------------------------------------------

          tabPanel(
          "Gene Information",
          br(),
          p(
            "This is a supplemental data table that has one row of information for each gene used in the Gasch et al. experiments. This information can be found on the previously linked Yeast Genome Database (also linked below!)."
          ),
          br(),
        
          p(h5("Overview of Dataset Columns:")),
          
          tags$ul( 
            
            tags$li(
              "Within this database, you can search using the ORF, gene name, or the SGD ID.") ,
            br(),
            tags$li(
            strong("ORF"),
            "refers to the specific 'Open Reading Frame' or region that contains the nuclear genome of interest. Each ORF is given a systematic name based on a 7-character alphanumeric formula. This unique identifier does not provide functional information, but is useful for bioinformatics purposes."
          ),
          
          #Citation: https://onlinelibrary.wiley.com/doi/pdf/10.1002/9783527636778.app1
          br(),
          
          tags$li(
            strong("Gene name"),
            "is another identifier used and typically has 3 letters followed by a number and is generally an abbreviation of some biological activity it has as determined by studies."
          ),
          br(),
          #Citation: https://onlinelibrary.wiley.com/doi/pdf/10.1002/9783527636778.app1
          
          tags$li(
            strong("Description"),
            "and",
            strong("gene function"),
            " is the longer name of the abbreviated",
            em("gene name"),
            "while the",
            strong("gene function"),
            "refers to the general function that may be known about this particular gene."
          ),
          br(),
          
          tags$li(
            strong("SGD ID"),
            "refers to the specific ID number assigned to this gene in the database it was pulled from. In this case, this gene info was pulled from the",
            em("Saccharomyces"),
            "Genome Database (SGD)."
          )
          
          ),
          
          br(),
          
          tags$a(
            href = "https://www.yeastgenome.org/",
            "Click here to learn more about the genes listed below!",
            target = "_blank"
          ),
          #Citation: https://onlinelibrary.wiley.com/doi/pdf/10.1002/9783527636778.app1
          
          br(),
          br(),
          
          actionButton("switch_to_clustering_analysis_tab", "Proceed to Clustering Analysis"),
          
          br(),
          br(),
          
          h2("Yeast Gene Information"),
          DT::dataTableOutput("mytable2"),
          
          selectInput("dataset2", "Choose a dataset:",
                      choices = c("Gasch Yeast Expression Data","Gasch Yeast Description")),
          
          # Button to download yeast dataset
          downloadButton("downloadData2", "Download")

          
        )
        
      )
      
    ),
    
    
    #___________________________________________________________
    #             CLUSTERING ANALYSIS - MAIN TAB 3              
    #___________________________________________________________

    tabPanel(
      "Clustering Analysis",
      
      tabsetPanel(
        id = "inKmeansTabset",
        
        #------------------------------------------------------------
        #                 ABOUT CLUSTERING - SUBTAB 1                                          
        #------------------------------------------------------------

        
        tabPanel(
          "About Clustering",
          
          br(),
          p(h3(strong("Clustering Analysis"))),
          p(
            "The gene expression data we saw on the previous tab is an example of the data output we can expect to have after sequencing occurs.
            In order to analyze this data, we can use clustering analysis in order to learn and identify patterns in our data. 
            Clustering is often used as a form of exploratory data analysis because it helps us get an initial sense of what major patterns exist in our data.
            The result is clusters of varying sizes that consist of data points that get grouped together based on similarities that exist between the data points.",
            strong("Two types of clustering analyses we will illustrate below are hierarchical clustering and k-means clustering.")
            ),
          
          br(),
          
          p(h4(strong("A. Hierarchical clustering") )),
          wellPanel(p(
            strong('What is it?'),
              "Hierarchical clustering is an extremely popular and simple type of clustering analysis." ,
              strong("What is it used for?"),
            "Hierarchical clustering allows us to easily visualize similarities in our data through
              the grouping of genes with similar expression levels, as can be seen in the hierarchical tree." )),
          fluidRow(
            column(width = 12, 
                   img(src='hctest1.png', align = "center",width="100%"),
                   wellPanel(
                     p(
                    h5(strong("How does it work?")),
                    tags$ol(
                    
                    tags$li("The first step is to treat each gene as an individual cluster."),
                    tags$li("Find the closest pair of genes with the highest similarity and merge them into a single cluster. From the figure above you can see that Gene 1 and Gene 3 have the most similarity, so these two genes will be merged into a cluster."),
                    tags$li("Now we compare Cluster 1 with Gene 2 and Gene 4 in order to compute the similarity between the newly formed cluster with the unmerged clusters."),
                    tags$li("We repeat steps 2 and 3 until all clusters are combined into a single cluster.")  ),
                    
                    strong("NOTE:"), "The hierarchical tree(dendrogram) in the figure illustrates which genes are the most similar. The length of each branch indicates relative similarity betweens each gene."
                    ))))
          ,
          p(strong(h4(strong("B. K-means Clustering")) )),
          wellPanel(p(
            strong("What is K-means Clustering"),
                 "K-means is one of the simplest types of clustering analyses, which makes it extremely popular.",
            strong("Why is it useful:"),
              "It can be used in a variety of situations and on various types of data, anything from genetic analysis to customer segmentation. 
              It is an extremely powerful type of analysis, because there are very few requirements needed to perform it. "
          )),
          
          fluidRow(
            column(width = 12, 
                   img(src='kmeantest1.png', align = "center",width="100%"),
            
            wellPanel(
                p(h5(strong("How is K-means Performed?")),
                  
                  tags$ol(
                    
                    tags$li("Pick the number of clusters, represented as k."),
                    tags$li("Randomly assign a centroid to each cluster."),
                    
                    tags$li("Compute the distance from each data point to the centroids and assign those data points to the centroid closest to them."),
                    tags$li("The algorithm will then compute the centroid's location within each newly formed cluster."),
                    tags$li("We then repeat steps 3 and 4 until we see that the data points remain assigned to the same cluster and centroids of the new clusters remain the same and stop changing after multiple iterations.")  ),
                  
                strong("NOTE:"), "For the clusters that form, we expect that within the cluster the data points are very similar to each other, known as intracluster similarity. 
                  Between clusters, we expect that the data points in different clusters are as different to each other as possible(intercluster similarity)."
                )))),
          
          
                #citation: https://bigdata-madesimple.com/possibly-the-simplest-way-to-explain-k-means-algorithm/
          
          fluidRow(
            column(width = 12,height = 12, 
                   wellPanel( 
                     p(h5(strong("Additional Resources:")),"You're practically a clustering expert now!",
                       tags$a(
                         href = "https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/kmeans",
                         "For more info about Kmeans.",
                         target = "_blank"
                       )
                       
                       
                       
                     ) ))),
      
          
          actionButton("switch_to_step1_subtab", "Proceed to Major Clustering Patterns")
        ),
        
        
        
        #------------------------------------------------------------
        #            MAJOR CLUSTERING PATTERNS - SUBTAB 2                                           
        #------------------------------------------------------------

          tabPanel(
          "Major Clustering Patterns",
          
          br(),
          
          fluidRow(
            column(width = 12,height = 12, 
                   wellPanel( 
                     h5("Now
that we have the basics of how clustering analysis is performed, let's talk a little bit more about what clustering analysis can tell us about our genetic data. In this page, we will go through a method we can use to identify the optimal cluster number (k) to use in our clustering analysis and what that tells us about major patterns in our data. We will also go through how to use heat maps as visualization tools to view the results of our clustering analysis and identify patterns of gene expression.") ) )),

          
            fluidRow(
              
          
            column(width = 5,height = 12, wellPanel( 
              p(h4("Choosing an Optimal Cluster Number"),
                p("Here, we talk about two different statistical methods for choosing an optimal cluster number."),
                h5("Within-Cluster Sum of Square (WCSS)"),
                tags$ul(tags$li("The elbow method, also known as within-cluster sum of square (WCSS), is a method that finds an optimal number of clusters (k). When plotting WCSS vs. number of clusters, we see an elbow-shaped curve. Notice that there's a sharp decrease at point 1 leveling off around point 2 along the x-axis. The x-axis value of 2 represents the optimal number of cluster.")),
                
                
                h5("Silhouette"),
                tags$ul(tags$li("The silhouette method is another way to find the optimal number of clusters (k). When plotting average silhouette width vs. number of clusters, we can once again see a sharp increase at point 1 and a leveling off beginning at point 2 along the x-axis. Thus, the optimal number of clusters is once again 2.")),
                
                h5("Select a method in the drop-down menu:") ),
              
              
              
              selectInput("methodCluster", "Methods of Choosing Optimal Number of Clusters:",
                          choices = list("Within-Cluster Sum of Square" = "wss", 
                                         "Silhouette" = "silhouette"),selected = "silhouette" )
              
              )),
            
            
            
            column(width = 7,height = 12, wellPanel( withSpinner(plotOutput("methodClusterPlot"))))
            ),
          
          fluidRow(
            
            
            column(width = 5,height = 12, wellPanel(
              p( h4("Visualizing Patterns with Heat Maps"),
                 
                 tags$ul( 
                 tags$li("Heat maps are a great visual tool for exploring patterns in a dataset. They give a bird's eye view of the data we are visualizing. Here, in the example heat map shown to the right, we can observe the expression patterns of many yeast genes across multiple experimental conditions all in one plot.", em("Note: This heat map only displays 100 genes out of a total of 2000 genes.")),
                 br(),

                 tags$li("From the dendrogram in the heat map, we see two major clusters. But further exploring the dendrogram, we observe many clusters. The heat map shows clustered areas where expression across certain genes across certain conditions are lowly expressed whereas some others are either highly expressed or not expressed."),
                 br(),
                 tags$li("One problem of analyzing the heat map in R is the difficulty in visualizing all the patterns that are present in such a large dataset. One way of solving this problem is to use TreeView 3.0, which is demonstrated in the video below. TreeView is used in the video to visualize the same dataset. TreeView is more computationally efficient and is able to better handle large datasets."),
                 )
                 )
              
            ), 
            # Question:
            wellPanel(radioButtons("radioHeat",
                                   label = HTML('<FONT size = "3pt"><b>How many patterns do you see in the heat map?</b></FONT>'),
                                   choiceNames = list(
                                     HTML('<FONT size = "2.7pt">2 patterns only.</FONT>'),
                                     HTML('<FONT size = "2.7pt">50+ patterns.</FONT>'),
                                     HTML('<FONT size = "2.7pt">Between 5 and 10 patterns.</FONT>'),
                                     HTML('<FONT size = "2.7pt">None selected.</FONT>')),
                                   
                                   choiceValues = list(1,2,3,4),
                                   
                                   selected = 4,
                                   inline = FALSE,
                                   width = "100%"),
                      fluidRow(column(10, textOutput("valueHeat")))),
            
            
            ),
            
            column(width = 7,height = 12, 
                   withSpinner(plotlyOutput("HeatMapPlot", width = "800px",height  = "900px"),hide.ui = FALSE )  )
            
          ),
          
          fluidRow(
            
            column(width = 5,height = 12, wellPanel( 
              p(
                
                h4("TreeView Explained"),
                "TreeView 3 is an application that allows for clustering analysis of large genomic data matrices and interactive visualization of the data." ,
                "For more information, please browse the",tags$a(href = "http://bonsai.hgc.jp/~mdehoon/software/cluster/manual/index.html#Top","TreeView3 manual.",target = "_blank"),
                "To download TreeView3, go to",tags$a(href = "http://jtreeview.sourceforge.net/","this link ",target = "_blank"),
            
                  
                  h5("Step 1: Importing file .cdt file"),
                  tags$ul(tags$li("File —> open —> select .cdt file"),
                  tags$li("Input files must be in .pcl or .cdt format.")),

                  h5("Step 2: Correcting labels"),
                  tags$ul(tags$li("View —> Labels")),

                  tags$ul(em("In the 'Show Label Type(s)' section: "),
                  tags$li("Select:  rows = UID & columns = UID")),
               
                  tags$ul(em("In the 'When labels don’t fit, show' section: "),
                  tags$li(" Select: 'As many as possible' "),
                  tags$li(" Finally: Click 'OK' ")),
                
                  h5("Step 3: Edit Colors"),
                  tags$ul(tags$li("View —> Colors"),
                  tags$li("Heat map can be changed to user preference.")),

                  h5("Step 4: Cluster analysis"),
                  tags$ul(tags$li("Cluster —> Hierarchical")),
               
                  tags$ul(em("Settings: "),
                  tags$li("Cluster Type: 'Hierarchical'"),
                  tags$li("Rows: 'Pearson Correlation (uncentered)'"),
                  tags$li("Columns: 'Pearson Correlation (uncentered)' "),
                  tags$li("Linkage Method: 'Average Linkage'"),
                  tags$li("Finally: Click 'Cluster' (NOTE: This step should take a few minutes to run.)" )),
                
                  h5("Step 5: Data Exploration "),
                  tags$ul(tags$li("You can drag down the columns and rows to view the dendrograms."))

              
                ) ) ),

              column( width = 7,height = 12,
              shiny::tags$iframe(src = "TreeViewShort.mp4",
                                type = "video/mp4", controls = "controls",allow = "accelerometer",width = "800px",height = '600px') ,
              
              br(),
              
              
              
              actionButton("switch_to_exploreclustersizes_tab", "Proceed to Explore Cluster Sizes") )
          )
          
          
        ),
        
        #------------------------------------------------------------
        #              EXPLORE CLUSTER SIZES SUBTAB 3                                              
        #------------------------------------------------------------

          tabPanel(
          "Explore Cluster Sizes",
          
          br(),
          
          
          fluidRow(
            
            
            column( width = 4,height = 12, wellPanel(
              p(
                "Before choosing a cluster in the next step, you may want to explore how 
              changing the number of centroids (k) affects cluster size. Pick the number 
              of centroids you would like. Then, hover over the bars to see the number of 
              genes in that cluster.") ,
              
              br(),
              
              p(
                "For this simulation, we recommend you try entering numbers between",
                strong("10"),
                "and",
                strong("100"),
                "for the number of centroids."
              ),
              
              br(),
              numericInput(
                'centroids',
                'Number of Centroids (k)',
                50,
                min = 10,
                max = 100,
                step = 5
              ),
              
              
              br(),
              br(),
              p(strong("Explanation of Plot:")),
              "The bar graph to the right represents the number of clusters the user has chosen and the size of each cluster. The optimal number of centroids is 50. This is the number of centroids we will use for the analyses to follow."),
              
              wellPanel(
              radioButtons("radioClustSize", 
                           label = HTML('<FONT size = "3pt"><b>What happens to the cluster sizes overall as the number of centroids increases from 10 to 50?</b></FONT>'),
                           choiceNames = list(
                             HTML('<FONT size = "2.7pt">As the centroids increase, the cluster sizes become smaller.</FONT>'), 
                             HTML('<FONT size = "2.7pt">As the centroids increase, the cluster sizes become larger.</FONT>'),
                             HTML('<FONT size = "2.7pt">None selected.</FONT>')), 
                           
                           choiceValues = list(1,2,3),
                           
                           selected = 3,
                           inline = FALSE,
                           width = "100%"),      
              fluidRow(column(8, textOutput("valueClustSize"))),
              
              br(),
              br(),
              actionButton("switch_to_step3_subtab", "Proceed to Gene Expression Analysis")
            ) )  ,
            
            column(width = 6,height = 12, 
                   withSpinner(highchartOutput('barplotclustsizes', 
                                               height = "1000px"),hide.ui = FALSE) # width = "700px", 
                   
            )
            
            
          )
          
          
        ),
        
        
      )
    ),
    
    
    #___________________________________________________________
    #         GENE EXPRESSION ANALYSIS - MAIN TAB 4             
    #___________________________________________________________
    

    tabPanel(
      "Gene Expression Analysis",
      
      tabsetPanel(
        id = "ingeneExpTabset",
        
  
        #------------------------------------------------------------
        #              VISUALIZE CLUSTER PROFILE - SUBTAB 1                                              
        #------------------------------------------------------------
        
        tabPanel(
          "Visualize Cluster Profile",
          
          br(),
          fluidRow(column(width = 12, wellPanel(h5("We've explored cluster sizes. We are now ready to look at some gene clusters and look for unique expression patterns within each cluster."),
                                                br(),
                                                p(strong("Explanation of Plot:")),
                                                p(
                                                  "This plot shows the gene expression (y-axis) of each gene in the cluster chosen in response to the stress conditions (x-axis). The" ,
                                                  strong("bold black line"),
                                                  "represents the mean expression for the chosen cluster. The",
                                                  strong("colored lines"),
                                                  "plotted around the bold black line are the gene expression profiles for 
                            all the genes that are assigned to the cluster number shown. To futher explore how these genes are similarly expressed under different conditions, hover over the colored lines to see more information including the condition and the corresponding gene name and expression value. Moving your cursor vertically along the same point in the x-axis shows the expression for the other genes in the cluster for that condition. Genes can be toggled on and off in the legend to the right of the plot. Investigate conditions where gene expression profiles",
                                                  em("dip"),
                                                  "or",
                                                  em("spike"),
                                                  "towards the two extremes of the y-axis or where the lines flatten."
                                                ),
                                                
                                                p(em(strong("For example, go to cluster number 32 (make sure centroids are set to 50 in the Explore Cluster Sizes under the Clustering Analysis tab).")), "If you hover over the lowest average peak (look at the bold black line), you'll see that it corresponds with brief exposure to heat shock (\"Heat.Shock.000.minutes..hs.2\"). Notice that for this cluster the lowest average expression is about", em("-1.5"), "for this heat shock condition, while each gene (colored lines) ranges in expression from", em("-0.32"), "(Gene YGR174C aka Ubiquinol-cytochrome-c reductase assembly; Gene function: Respiration) to", em("-3.18"), "(Gene YIL099W aka Glucan 1,4-alpha-glucosidase; Gene function: Sporulation). This indicates that in response to heat shock initially, these genes (and their intended function) are repressed."),
                                                p('We see that the info box indicates that these genes seem to have important survival functions such as glucose repression, transcription, and ATP synthesis. There are also some genes which we don\'t yet know their functions. But since these genes cluster together and have similar expression patterns, these unknown genes might give rise to products that carry out similar functions. Look at other variations of the heat shock experimental condition we just looked at, such as heat shock carried out for different durations of time (e.g. "heat.shock.10.minutes.hs.1").', em("Why is this cluster of genes highly expressed after 10 minutes of heat shock exposure vs. 0 minutes?'")),
                                                br(),
                                                br()
                                                )),
                             ),

          fluidRow(
            column( width =4, wellPanel(
              p("Now, give this a try yourself below. When you're ready, check your understanding by answering the question below."),
              
              numericInput(
                'clustnum',
                'Cluster Number',
                5,
                min = 1,
                max = 100,
                step = 1
              ),
              
              br(),
              br(),
              actionButton("switch_to_ViewClusterHeatmap_tab", "Proceed to View Cluster Heatmap")
              
            )),
            
            column(width = 8, withSpinner(plotlyOutput('plot2'), hide.ui = FALSE),
                   uiOutput("tutorial"),
                   
            br(),
            radioButtons("radio", 
                                   label = HTML('<FONT size = "3pt"><b>On average, how do the gene expression levels differ between clusters 5 and 6 for the nitrogen depletion stress conditions?</b></FONT>'),
                                   
                                   choiceNames = list(
                         HTML('<FONT size = "2.7pt">There is no difference in expression levels between the two clusters.</FONT>'), 
                         HTML('<FONT size = "2.7pt">Genes in cluster 5 are on average more highly expressed than those in cluster 6.</FONT>'),
                         HTML('<FONT size = "2.7pt">Genes in cluster 6 are on average more highly expressed than those in cluster 5.</FONT>'),
                         HTML('<FONT size = "2.7pt">Both clusters are lowly expressed under the nitrogen depletion conditions.</FONT>'),
                         HTML('<FONT size = "2.7pt">None selected.</FONT>')), 
                                   
                                   choiceValues = list(1,2,3,4,5),
                                   
                                   selected = 5,
                                   inline = FALSE,
                                   width = "100%"),      
                      fluidRow(column(8, textOutput("value"))),
             )
            
          ),
          br()

        ),
        
        #------------------------------------------------------------
        #              VIEW CLUSTER HEATMAP - SUBTAB 2                                              
        #------------------------------------------------------------
        
        tabPanel(
          "View Cluster Heat Map",
          
          br(),
          
          fluidRow(
            column(width = 4, 
                   wellPanel( 
                     p("In the last exercise, we looked at how gene expression levels differed between two clusters for a certain set of conditions. Here, we'll try to identify major patterns in expression levels for a given cluster of genes using a different visual tool. But first, let\'s go over some basics of gene expression."),
                     p(h4("Gene Expression"),
                     "Gene expression describes the process of making a product, usually a protein, using information stored in a gene. Gene expression is measured by quantifying the amount of RNA or protein made from RNA." ,
                     br(),
                     h5("To simply things, let's think of gene expression as an on and off switch!"),
                     tags$ul( 
                     tags$li("This switch controls when proteins are made and the amount of proteins being made."),
                     tags$li("When genes are",strong("turned on"),", this means they have a gene expression level," ,strong("greater than zero.")),
                     tags$li("When genes are" ,strong("turned off")," that means they have a gene expression level", strong('lower than zero.')) ) ),
                     
                    p("On the right, we've annotated what the colors in the heat map below mean in terms of reading expression levels. If the dendrograms above each axis in the heat map are confusing, try looking back at the", em("Clustering Analysis"), "tab in the", em("About Clustering"),  "section for some clarification.")

                                         )),
            
            column(width = 8 , img(src='saf.png', align = "center",width="100%"))
          ),
          
          fluidRow( 
            
            column(width =4,height = 12, wellPanel(
              
              br(),
              p(
                em(strong("Give it a try!")), "Choose a gene cluster to view. The heatmap to the right will display the genes in the cluster chosen (x-axis) and their expression in response to the stress conditions (y-axis)."
              ),
              
              numericInput(
                'Clusternumber_2',
                'Select The Cluster ',
                5,
                min = 1,
                max = 100,
                step = 1 ),
              
              br(),
              p(em("What major patterns do you see? Does the cluster you picked have similar expression patterns for certain stress conditions?")),
              br(),
              br(),
              
              actionButton("switch_to_enrichmentAnalysis_tab", "Proceed to Enrichment Analysis")
              
            )),
            
            
            column(width =8, mainPanel(withSpinner(plotlyOutput("HeatMapPlot_2",width = "900px" ,height  = "1100px"), 
                                                               hide.ui = FALSE )))
            
          ), 
        ) 
        
        
      )
      
    ),
    
    
    #___________________________________________________________
    #           ENRICHMENT ANALYSIS - MAIN TAB 5              
    #___________________________________________________________
    

    tabPanel(
      "Enrichment Analysis",
      
      actionButton("show", "Information About This Tab"),
      br(),
      br(),
      
      sidebarPanel(
        h5("Now that we have explored gene expression patterns in the data in the previous tab, we can now try to connect this to some of the underlying biological functions."),
        p("You can choose from three different inputs:"),
        
        "1. Cluster Number",
        br(),
        "2. Gene Ontology Terms",
        br(),
        "3. The Number of Top GO Nodes",
        br(),
        br(),
        p(
          "The cluster number input let\'s you conduct gene ontology analysis on each of the different clusters.
          The gene ontology (GO) input lets you specify certain GO terms associated with different functions 
          to see if these functions are present in the clusters. By changing the number of top GO nodes, you 
          can specify the number of top GO functions you want to look at)."
        ),
        br(),
        selectInput(
          'clustnum2',
          'Cluster Number:',
          df.allclusters$clusternumber,
          selected = df.allclusters$clusternumber[5]
        ),
        
        selectInput('GOtype', 'Gene Ontology:', choices = list("Biological Process" = "BP", "Cellular Component" = "CC", "Molecular Function" = "MF"),
                    selected = "BP"),
        
        selectInput(
          'GOnodes',
          'Number of Top GO Nodes to Display:',
          c("5", "10", "15", "20", "25", "30", "35", "40", "45", "50"),
          selected = "5"
        ),
        
        width = 3
      ),
      
      #------------------------------------------------------------
      #             ENRICHMENT FUNCTION - SUBTAB 1                                               
      #------------------------------------------------------------

        mainPanel(tabsetPanel(
        id = "inEATabset",
        
        tabPanel(
          "Enriched Functions",
          br(),
          p(
            "The table below is a list of the resulting top enriched functions as defined 
            by the GO term selected for the cluster of interest including a fisher test 
            for significance. The 'Annotated' column refers to the number of genes that 
            have this function associated with it, while 'Significant' column refers to 
            how many of those genes who have this function annotated are actually significant. 
            'Expected' is the proportion of genes predicted to have this function. 'Term' simply 
            refers to the function or GO term being used. The 'GO ID' is the assigned identifier 
            for this GO term. See the next tab for a network analysis plot of the top GO terms defined here."
          ),
          
          br(),

          wellPanel(radioButtons("radioEnf",
                                 label = HTML('<FONT size = "3pt"><b>Look at the gene functions listed for cluster 6, do you see any similarities? Take some time to think about why genes with this function might show increased expression when subjected to nitrogen depletion conditions.<br><br> Which of the answer choices below gives a plausible explanation? Hint: Go to <a href="http://geneontology.org/"> Gene Ontology</a> to learn more about these functions.</b></FONT>'),
                                 choiceNames = list(
                                   HTML('<FONT size = "2.7pt">There is no logical association between why we would see higher expression levels of genes related to DNA repair under nitrogen depletion conditions.
</FONT>'),
                                   HTML('<FONT size = "2.7pt">Nitrogen is essential to yeast protein localization processes. Many of the gene functions in cluster 6 are related to protein localization. Yeast proteins utilize nitrogen to translocate essential nutrients throughout the cell and neighboring yeast cells, which is important for yeast cell survival.</FONT>'),
                                   HTML('<FONT size = "2.7pt">Nitrogen is essential for yeast growth. A common source of nitrogen is the breakdown of amino acids. Many of the gene functions in cluster 6 are related to amino acid production and metabolism. It would make sense that under nitrogen depletion conditions that yeast cells will activate different pathways to adapt to an environmental stressor.


</FONT>'),                                   HTML('<FONT size = "2.7pt">None selected.</FONT>')),
                                 
                                 choiceValues = list(1,2,3,4),
                                 
                                 selected = 4,
                                 inline = FALSE,
                                 width = "100%"),
                    fluidRow(column(12, textOutput("valueEnf")))),
          
          actionButton("switch_to_networkplot_tab", "Visualize Network Plot"),
          
          br(),
          br(),
          
          withSpinner(tableOutput('table2'), hide.ui = FALSE)
          
        ),
        
        #------------------------------------------------------------
        #             VISUALIZE NETWORK PLOT - SUBTAB 2                                            
        #------------------------------------------------------------
        
        tabPanel(
          "Visualize Network Plot",
          br(),
          p(
            "The plot below is a network plot that shows how the top enriched gene functions are associated with each other. Rectangles/boxes indicate the most significant terms (based on number of nodes selected). Rectangle color represents the relative significance, ranging from bright yellow (most significant) to dark green (least significant). Black arrows indicate relationships. For each node, some basic information is displayed such as the GO identifier (GO ID), raw pvalue, and number of genes annotated to the respective GO term. Specific GO ID can be selected to highlight the connections related to that GO ID."
          ),
          # Citation: https://www.bioconductor.org/packages/devel/bioc/vignettes/topGO/inst/doc/topGO.pdf


          wellPanel(radioButtons("radioNet",
                       label = HTML('<FONT size = "3pt"><b>Go to cluster number 6. Which GO ID has the most significant p-value?</b></FONT>'),
                        choiceNames = list(
                        HTML('<FONT size = "2.7pt">GO:1901607</FONT>'),
                        HTML('<FONT size = "2.7pt">GO:0008652</FONT>'),
                        HTML('<FONT size = "2.7pt">GO:0048460</FONT>'),
                        HTML('<FONT size = "2.7pt">GO:0051704</FONT>'),
                        HTML('<FONT size = "2.7pt">None selected.</FONT>')),

                        choiceValues = list(1,2,3,4,5),

                       selected = 5,
                       inline = FALSE,
                       width = "100%"),
                      fluidRow(column(10, textOutput("valueNet")))),
          
          
          withSpinner(visNetworkOutput(
            "img1", height = "1300px" 
          ), hide.ui = FALSE)
        )
        
      )),
      width = 9
    )
    
     
  ) # Closing parenthesis of Navigation bar
  
) # Closing parenthesis of fluidPage
) # Closing parenthesis of shinyUI


#___________________________________________________________
#                    *** END OF SCRIPT ***
#___________________________________________________________

