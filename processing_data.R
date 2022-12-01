# loading in expression data

load("gasch_expression_data.RData")



gene_info$gene_name <- as.character(gene_info$gene_name)

gene_info$gene_name[which(gene_info$gene_name == "")] <- "UNKNOWN"

# Capturing top 2000 highest variable genes 
var_data <- apply(expr_data, 1, var)
sort_var <- sort(var_data, decreasing = T)
cluster_data <- expr_data[names(sort_var[1:2000]),]

# look at were these lines are used in the server and see if we can make the code
# more efficient 

# jimmy work on this: place the description in expr_data

# creating two sets and setting them equal to each other 
#set1 <- row.names(expr_data)
#set2 <- row.names(gene_info)
#setequal(set1,set2)

temp_expr_data_df <- as.data.frame(expr_data)
expr_data_df <- temp_expr_data_df %>% add_column(Description = gene_info$description, .before = colnames(expr_data)[1])
gene_info_table <- as.data.frame(gene_info)






# for reproducibility of kmeans algorithm
set.seed(300)
allclusters <- kmeans(cluster_data, centers = 50)

# creating a dataframe of cluster sizes 
df.allclusters <-
  data.frame(clusternumber = 1:length(allclusters$size),
             freq = allclusters$size)


# creating a list of all 173 conditions 
# see if we can make the code more efficient  

condlist <- list(
  "heatshock" = c(colnames(expr_data)[1:15]),
  
  "X37C.to.25C.shock" = c(colnames(expr_data)[16:20]),
  
  "X.heat.shock.17.to.37..20.minutes." = c(colnames(expr_data)[21:25]),
  
  "X29C.to.33C...5.minutes" = c(colnames(expr_data)[26:29]),
  
  "X29C..1M.sorbitol.to.33C...1M.sorbitol...5.minutes" = c(colnames(expr_data)[30:35]),
  
  "constant.0.32.mM.H2O2..10.min..redo" = c(colnames(expr_data)[36:45]),
  
  "X1.mM.Menadione..10.min.redo" = c(colnames(expr_data)[46:54]),
  
  "X2.5mM.DTT.005.min.dtt.1" = c(colnames(expr_data)[55:62]),
  
  "dtt.000.min..dtt.2" = c(colnames(expr_data)[63:69]),
  
  "X1.5.mM.diamide..5.min." = c(colnames(expr_data)[70:77]),
  
  "X1M.sorbitol...5.min" = c(colnames(expr_data)[78:84]),
  
  "Hypo.osmotic.shock...5.min" = c(colnames(expr_data)[85:89]),
  
  "aa.starv.0.5.h" =   c(colnames(expr_data)[91:95]),
  
  "Nitrogen.Depletion.30.min." = c(colnames(expr_data)[96:105]),
  
  "Diauxic.Shift.Timecourse...0.h" = c(colnames(expr_data)[106:112]),
  
  "YPD.2.h.ypd.2" = c(colnames(expr_data)[113:122]),
  
  "YPD.stationary.phase.2.h.ypd.1" = c(colnames(expr_data)[123:134]),
  
  "DBY7286.37degree.heat...20.min" = c(colnames(expr_data)[135:139]),
  
  # COMEBACK TO THIS: POSSIBLE NOT INCLUDE
  "DBY7286...0.3.mM.H2O2..20.min." = c(colnames(expr_data)[140:144]),
  
  # COMEBACK TO THIS: POSSIBLE NOT INCLUDE
  "Msn2.overexpression..repeat." = c(colnames(expr_data)[145:147]),
  
  # COMEBACK TO THIS: POSSIBLE NOT INCLUDE
  "Steady-state growth on alternative carbon sources" = c(colnames(expr_data)[148:153]),
  
  "YP.ethanol.vs.reference.pool.car.2" = c(colnames(expr_data)[154:160]),
  
  "X17.deg.growth.ct.1" = c(colnames(expr_data)[161:165]),
  
  "steady.state.15.dec.C.ct.2" = c(colnames(expr_data)[166:173])
  
)


# Loading question files
q1_clustertab <-read.csv("q1_clustertab.csv")

df.questions <- data.frame(question = "What is your favorite food?",
                 option = c("A","B","C"),
                 input_type = "mc",
                 input_id = "favorite_food",
                 dependence = NA,
                 dependence_value = NA,
                 required = F)

