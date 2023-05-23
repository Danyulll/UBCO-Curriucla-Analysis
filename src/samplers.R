###########################
### Libraries and Functions
###########################

# Load libraries
library(stringr)
library(igraph)
library(docstring)
library(dplyr)

# Load data
ubco <- read.csv(".\\data\\UBCO\\UBCO_Course_Calendar.csv")


# Delay Factor

delay_factor <- function(edge_list, node_list) {
  #' Delay factor
  #'
  #' Calculates the delay factor for each node and the total delay factor of the curriculum graph. Delay factor is defined as the number of vertices in the longest path in G that pass through v. The function returns a list where bynode holds the delay factor for each node and total holds the delay factor for the graph.
  #'
  #' @param edge_list data frame containing the edge list of the graph.
  #' @param node_list data frame containing the node list of the graph.
  
  bynode <- data.frame(id = NA, df = NA)
  
  network <- graph_from_data_frame(d = edge_list,
                                   vertices = node_list,
                                   directed = TRUE)
  paths <- list()
  
  
  for (v in as.numeric(node_list$id)) {
    paths <- c(paths, all_simple_paths(network, from = v, mode = "out"))
  }
  
  for (v in as.numeric(node_list$id)) {
    max_length <- 0
    for (path in paths) {
      if (v %in% as.vector(path) &&
          max_length < length(as.vector(path))) {
        max_length <- length(as.vector(path))
      }
    }
    
    if (max_length == 0) {
      max_length <- 1
    }
    bynode <-
      rbind(bynode, data.frame(id = as.character(v), df = max_length))
    
  }
  
  bynode <- na.omit(bynode)
  total <- sum(bynode$df)
  
  list(bynode = bynode, total = total)
}

# Blocking Factor

blocking_factor <- function(edge_list, node_list) {
  #' Blocking factor
  #'
  #' Calculates the blocking factor for each node and the total blocking factor of the curriculum graph. The blocking factor of a node v is the number of nodes reachable from v. The function returns a list where bynode holds the blocking factor for each node and total holds the blocking factor for the graph.
  #'
  #' @param edge_list data frame containing the edge list of the graph.
  #' @param node_list data frame containing the node list of the graph.
  
  bynode <- data.frame(id = NA, bf = NA)
  
  network <- graph_from_data_frame(d = edge_list,
                                   vertices = node_list,
                                   directed = TRUE)
  paths <- list()
  
  for (v in as.numeric(node_list$id)) {
    paths <- c(paths, all_simple_paths(network, from = v, mode = "out"))
  }
  
  
  for (v in as.numeric(node_list$id)) {
    nodes_reachable <- c()
    for (path in paths) {
      curr_path <- as.vector(path)
      if (v %in% curr_path[1]) {
        nodes_reachable <- c(nodes_reachable, curr_path)
      }
    }
    
    nodes_reachable <- unique(nodes_reachable)
    nodes_reachable <- nodes_reachable[nodes_reachable != v]
    
    bynode <-
      rbind(bynode, data.frame(id = as.character(v), bf = length(nodes_reachable)))
  }
  
  bynode <- na.omit(bynode)
  
  list(bynode = bynode, total = sum(bynode$bf))
  
}

# Centrality Factor

centrality_factor <- function(edge_list, node_list) {
  #' Centrality factor
  #'
  #' Calculates the centrality factor for each node. The centrality factor of a node v is the number of long paths containing v where a long path is one of length 3 or greater. The function returns a dataframe that holds the centrality factor for each node.
  #'
  #' @param edge_list data frame containing the edge list of the graph.
  #' @param node_list data frame containing the node list of the graph.
  
  bynode <-
    data.frame(id = as.numeric(node_list$id), cf = rep(0, length(node_list$id)))
  
  network <- graph_from_data_frame(d = edge_list,
                                   vertices = node_list,
                                   directed = TRUE)
  paths <- list()
  
  for (v in as.numeric(node_list$id)) {
    paths <- c(paths, all_simple_paths(network, from = v, mode = "out"))
  }
  
  for (path in paths) {
    curr_path <- as.vector(path)
    if (length(curr_path) >= 3) {
      long_path_nodes <- curr_path[c(-1, -length(curr_path))]
      
      for (node in long_path_nodes) {
        bynode[node, c("cf")] <- bynode[node, c("cf")] + length(curr_path)
      }
    }
  }
  
  bynode$id <- as.character(bynode$id)
  bynode
}

# Structural Complexity and Cruciality

structural_complexity <- function(edge_list, node_list) {
  #' Structural Complexity
  #'
  #' Calculates the cruciality for each node and the structural complexity for the entire curriculum graph. The structural complexity is the sum of all node crucialities.The function returns a list where bynode holds the cruciality for each node and total holds the structural complexity for the graph.
  #'
  #' @param edge_list data frame containing the edge list of the graph.
  #' @param node_list data frame containing the node list of the graph.
  
  
  bf_df <- blocking_factor(edge_list, node_list)
  bf <- bf_df$bynode$bf
  
  df_df <- delay_factor(edge_list, node_list)
  df <- df_df$bynode$df
  
  bynode <- data.frame(id = node_list$id, sc = (bf + df))
  
  list(bynode = bynode, total = sum(bynode$sc))
  
}

###########################
### DS Major Sampler
###########################

# Set seed
set.seed(87460945)

# Create list for storing pathways
Gc <- list()

for (i in 1:1000) {
  # Generate degree pathway
  pathway <-
    data.frame(
      Course.Code = NA,
      Course.Name = NA,
      Course.Description = NA,
      Prerequisite = NA,
      Corequisite = NA,
      Equivalents = NA
    )
  
  # First Year
  pathway <- rbind(pathway, subset(ubco, Course.Code == "DATA 101"))
  samp <- sample(c("CHEM 111", "CHEM 121"), 1)
  pathway <- rbind(pathway, subset(ubco, Course.Code == samp))
  pathway <-
    rbind(pathway, subset(ubco, Course.Code %in% c("MATH 100", "MATH 101")))
  samp <- sample(c(TRUE, FALSE), 1)
  
  if (samp) {
    pathway <- rbind(pathway, subset(ubco, Course.Code == "ENGL 109"))
  } else {
    samp <- sample(
      c(
        "ENGL 112",
        "ENGL 113",
        "ENGL 114",
        "ENGL 150",
        "ENGL 151",
        "ENGL 153",
        "ENGL 154",
        "ENGL 155",
        "ENGL 156"
      ),
      2
    )
    pathway <- rbind(pathway, subset(ubco, Course.Code %in% samp))
  }
  
  
  samp <- sample(c("PHYS 111", "PHYS 112"), 1)
  pathway <- rbind(pathway, subset(ubco, Course.Code %in% samp))
  samp <- sample(c("PHYS 121", "PHYS 122"), 1)
  pathway <- rbind(pathway, subset(ubco, Course.Code %in% samp))
  pathway <-
    rbind(pathway, subset(ubco, Course.Code %in% c("COSC 111", "COSC 121")))
  
  
  # Second Year
  pathway <-
    rbind(pathway, subset(
      ubco,
      Course.Code %in% c("MATH 200",
                         "MATH 221",
                         "STAT 230",
                         "COSC 221",
                         "COSC 222")
    ))
  
  
  # Third and Fourth Year
  pathway <-
    rbind(pathway, subset(
      ubco,
      Course.Code %in% c("DATA 301",
                         "DATA 311",
                         "COSC 304",
                         "STAT 303",
                         "PHIL 331")
    ))
  
  
  
  upper_year_data <-
    c("DATA 310", "DATA 315", "DATA 405", "DATA 407", "DATA 410")
  max_2_stat <- c("STAT 400", "STAT 401", "STAT 403", "STAT 406")
  
  # TODO I don't include MATH 409 or PHYS 420
  max_2_cosc_math_phys <-
    c(
      "COSC 303",
      "COSC 322",
      "COSC 329",
      "COSC 344",
      "COSC 407",
      "COSC 421",
      "MATH 303",
      "MATH 307"
    )
  course <- c()
  while (length(unique(course)) < 9) {
    var <- sample(c("1", "2", "3"), 1)
    
    if (var == 1) {
      course <- c(course, sample(max_2_stat, 1))
    } else if (var == 2) {
      course <- c(course, sample(max_2_cosc_math_phys, 1))
    } else if (var == 3) {
      course <- c(course, sample(upper_year_data, 1))
    }
  }
  
  pathway <-
    rbind(pathway, subset(ubco, Course.Code %in% unique(course)))
  
  pathway <- na.omit(pathway)
  rownames(pathway) <- 1:nrow(pathway)
  
  # Construct node and edge list
  
  node_list <-
    data.frame(id = rownames(pathway), label = pathway$Course.Code)
  
  edge_list <- data.frame(from = NA, to = NA)
  
  for (node in node_list$label) {
    str <- subset(pathway, Course.Code == node)$Prerequisite
    course_code <- ""
    if (str != "") {
      course_code <- str_extract_all(str, "[A-Z]{4} [0-9]{3}")[[1]]
    }
    
    from <- rownames(subset(pathway, Course.Code == node))
    to <- rownames(subset(pathway, Course.Code %in% course_code))
    if (length(to) > 1) {
      for (id in to) {
        edge_list <- rbind(edge_list, data.frame(from = id, to = from))
      }
    } else {
      edge_list <- rbind(edge_list, data.frame(from = to[1], to = from))
    }
  }
  edge_list <- na.omit(edge_list)
  
  # Get structural complexity
  
  sc <- structural_complexity(edge_list, node_list)
  
  # Store in list
  results <-
    list(node_list = node_list,
         edge_list = edge_list,
         sc = sc)
  
  Gc <- c(Gc, list(results))
}

# Initialize variables for maximum and minimum values
max_total <- -Inf
min_total <- Inf
max_index <- NULL
min_index <- NULL

# Iterate through Gc to find max and min indices
for (i in seq_along(Gc)) {
  total <- Gc[[i]]$sc$total
  if (total > max_total) {
    max_total <- total
    max_index <- i
  }
  if (total < min_total) {
    min_total <- total
    min_index <- i
  }
}

# Create Max Graph
node_list <- Gc[[max_index]]$node_list
edge_list <- Gc[[max_index]]$edge_list

sc_df <- structural_complexity(edge_list,node_list)
cf_df <- centrality_factor(edge_list,node_list)
bf_df <- blocking_factor(edge_list,node_list)
df_df <- delay_factor(edge_list,node_list)

node_list <- left_join(node_list, sc_df$bynode, by = c("id" = "id"))
node_list <- left_join(node_list, cf_df, by = c("id" = "id"))
node_list <- left_join(node_list, bf_df$bynode, by = c("id" = "id"))
node_list <- left_join(node_list, df_df$bynode, by = c("id" = "id"))
max_graph <- list(edge_list = edge_list, node_list = node_list, sc_total = sc_df$total, bf_total = bf_df$total, df_total = df_df$total)

save(max_graph, file = ".\\data\\rdata\\maxGraph.RData")

# Create Min Graph

node_list <- Gc[[min_index]]$node_list
edge_list <- Gc[[min_index]]$edge_list

sc_df <- structural_complexity(edge_list,node_list)
cf_df <- centrality_factor(edge_list,node_list)
bf_df <- blocking_factor(edge_list,node_list)
df_df <- delay_factor(edge_list,node_list)

node_list <- left_join(node_list, sc_df$bynode, by = c("id" = "id"))
node_list <- left_join(node_list, cf_df, by = c("id" = "id"))
node_list <- left_join(node_list, bf_df$bynode, by = c("id" = "id"))
node_list <- left_join(node_list, df_df$bynode, by = c("id" = "id"))

min_graph <- list(edge_list = edge_list, node_list = node_list, sc_total = sc_df$total, bf_total = bf_df$total, df_total = df_df$total)

save(min_graph, file = ".\\data\\rdata\\minGraph.RData")


###########################
### DS Minor Sampler
###########################

# Create list for storing pathways
Gc <- list()

for (i in 1:1000) {
  pathway <-
    data.frame(
      Course.Code = NA,
      Course.Name = NA,
      Course.Description = NA,
      Prerequisite = NA,
      Corequisite = NA,
      Equivalents = NA,
      Minor_Prereq = NA
    )
  # 30 Credits from:
  
  # Required courses
  pathway <- rbind(pathway, cbind(subset(ubco, Course.Code %in% c("DATA 101","STAT 230", "DATA 301","DATA 311")),data.frame(Minor_Prereq = rep(FALSE,4))))
  
  up_to_6_creds <- c("MATH 100", "MATH 101", "MATH 200", "MATH 221", "COSC 111", "COSC 121", "COSC 221", "COSC 222", "ECON 102", "APSC", "BIOL 202", "PSYO 373", "APSC 254")
  courses <- sample(up_to_6_creds,2)
  pathway <- rbind(pathway, cbind(subset(ubco, Course.Code %in% courses),data.frame(Minor_Prereq = rep(FALSE,length(courses)))))
  
  upper_year_data <-
    c("DATA 310", "DATA 315", "DATA 405", "DATA 407", "DATA 410")
  max_3_cosc <- c("COSC 304","COSC 322","COSC 329","COSC 344", "COSC 421")
  max_6_stat <- c("STAT 303","STAT 401")
  cosc_cred <- 0
  stat_cred <- 0
  total_cred <- 0
  
  while (total_cred  != 12) {
    x <- sample(1:3, 1)
    
    if (x == 1) {
      courses <- sample(upper_year_data, 1)
      if (sum(courses %in% pathway$Course.Code)==0) {
        pathway <-
          rbind(pathway, cbind(subset(ubco, Course.Code %in% courses),data.frame(Minor_Prereq = FALSE)))
        total_cred <- total_cred + 3
      }
      
    } else if (x == 2 & cosc_cred != 3) {
      courses <- sample(max_3_cosc, 1)
      if (sum(courses %in% pathway$Course.Code)==0) {
        pathway <-
          rbind(pathway, cbind(subset(ubco, Course.Code %in% courses),data.frame(Minor_Prereq = FALSE)))
        cosc_cred <- 3
        total_cred <- total_cred + 3
      }
    } else if (x == 3 & stat_cred < 7) {
      courses <- sample(max_6_stat,1)
      if (sum(courses %in% pathway$Course.Code)==0) {
        pathway <-
          rbind(pathway, cbind(subset(ubco, Course.Code %in% courses),data.frame(Minor_Prereq = FALSE)))
        stat_cred <- stat_cred + 3
        total_cred <- total_cred + 3
      }
    }
  }
  
  pathway <- na.omit(pathway)
  
  
  prereqs <- unlist(str_extract_all(subset(ubco,Course.Code %in% pathway$Course.Code)$Prerequisite, "[A-Z]{4} [0-9]{3}"))
  # prereqs <- prereqs[!(prereqs %in% pathway$Course.Code)]
  
  # TODO while this works, its an ugly solution
  for (i in 1:50) {
    prereqs <- c(prereqs,unlist(str_extract_all(subset(ubco,Course.Code %in% prereqs)$Prerequisite, "[A-Z]{4} [0-9]{3}")))
  }
  
  pathway <- rbind(pathway, cbind(subset(ubco, Course.Code %in% prereqs),data.frame(Minor_Prereq = rep(TRUE,nrow(subset(ubco, Course.Code %in% prereqs))))))
  pathway <- pathway[!duplicated(pathway$Course.Code),]
  rownames(pathway) <- 1:nrow(pathway)
  
  # Construct node and edge list
  
  node_list <-
    data.frame(id = rownames(pathway), label = pathway$Course.Code)
  node_list$shape <- ifelse(pathway$Minor_Prereq,"triangle","circle")
  node_list$group <- ifelse(pathway$Minor_Prereq,"TRUE","FALSE")
  
  edge_list <- data.frame(from = NA, to = NA)
  
  for (node in node_list$label) {
    str <- subset(pathway, Course.Code == node)$Prerequisite
    course_code <- ""
    if (str != "") {
      course_code <- str_extract_all(str, "[A-Z]{4} [0-9]{3}")[[1]]
    }
    
    from <- rownames(subset(pathway, Course.Code == node))
    to <- rownames(subset(pathway, Course.Code %in% course_code))
    if (length(to) > 1) {
      for (id in to) {
        edge_list <- rbind(edge_list, data.frame(from = id, to = from))
      }
    } else {
      edge_list <- rbind(edge_list, data.frame(from = to[1], to = from))
    }
  }
  edge_list <- na.omit(edge_list)
  
  # Get structural complexity
  
  sc <- structural_complexity(edge_list, node_list)
  
  # Store in list
  results <-
    list(node_list = node_list,
         edge_list = edge_list,
         sc = sc)
  
  Gc <- c(Gc, list(results))
}

# Initialize variables for maximum and minimum values
max_total <- -Inf
min_total <- Inf
max_index <- NULL
min_index <- NULL

# Iterate through Gc to find max and min indices
for (i in seq_along(Gc)) {
  total <- Gc[[i]]$sc$total
  if (total > max_total) {
    max_total <- total
    max_index <- i
  }
  if (total < min_total) {
    min_total <- total
    min_index <- i
  }
}

node_list <- Gc[[max_index]]$node_list
edge_list <- Gc[[max_index]]$edge_list

sc_df <- structural_complexity(edge_list,node_list)
cf_df <- centrality_factor(edge_list,node_list)
bf_df <- blocking_factor(edge_list,node_list)
df_df <- delay_factor(edge_list,node_list)

node_list <- left_join(node_list, sc_df$bynode, by = c("id" = "id"))
node_list <- left_join(node_list, cf_df, by = c("id" = "id"))
node_list <- left_join(node_list, bf_df$bynode, by = c("id" = "id"))
node_list <- left_join(node_list, df_df$bynode, by = c("id" = "id"))
max_graph <- list(edge_list = edge_list, node_list = node_list, sc_total = sc_df$total, bf_total = bf_df$total, df_total = df_df$total)

save(max_graph, file = ".\\data\\rdata\\maxGraph_minor.RData")

node_list <- Gc[[min_index]]$node_list
edge_list <- Gc[[min_index]]$edge_list

sc_df <- structural_complexity(edge_list,node_list)
cf_df <- centrality_factor(edge_list,node_list)
bf_df <- blocking_factor(edge_list,node_list)
df_df <- delay_factor(edge_list,node_list)
# 
node_list <- left_join(node_list, sc_df$bynode, by = c("id" = "id"))
node_list <- left_join(node_list, cf_df, by = c("id" = "id"))
node_list <- left_join(node_list, bf_df$bynode, by = c("id" = "id"))
node_list <- left_join(node_list, df_df$bynode, by = c("id" = "id"))
min_graph <- list(edge_list = edge_list, node_list = node_list, sc_total = sc_df$total, bf_total = bf_df$total, df_total = df_df$total)

save(min_graph, file = ".\\data\\rdata\\mingraph_minor.RData")

###########################
### DS Math Stream Sampler
###########################

# Create list for storing pathways
Gc <- list()

for (i in 1:1000) {
  # Generate degree pathway
  pathway <-
    data.frame(
      Course.Code = NA,
      Course.Name = NA,
      Course.Description = NA,
      Prerequisite = NA,
      Corequisite = NA,
      Equivalents = NA
    )
  
  # First Year
  pathway <- rbind(pathway, subset(ubco, Course.Code == "DATA 101"))
  samp <- sample(c("CHEM 111", "CHEM 121"), 1)
  pathway <- rbind(pathway, subset(ubco, Course.Code == samp))
  pathway <-
    rbind(pathway, subset(ubco, Course.Code %in% c("MATH 100", "MATH 101")))
  samp <- sample(c(TRUE, FALSE), 1)
  
  if (samp) {
    pathway <- rbind(pathway, subset(ubco, Course.Code == "ENGL 109"))
  } else {
    samp <- sample(
      c(
        "ENGL 112",
        "ENGL 113",
        "ENGL 114",
        "ENGL 150",
        "ENGL 151",
        "ENGL 153",
        "ENGL 154",
        "ENGL 155",
        "ENGL 156"
      ),
      2
    )
    pathway <- rbind(pathway, subset(ubco, Course.Code %in% "ENGL 109"))
  }
  
  
  samp <- sample(c("PHYS 111", "PHYS 112"), 1)
  pathway <- rbind(pathway, subset(ubco, Course.Code %in% samp))
  samp <- sample(c("PHYS 121", "PHYS 122"), 1)
  pathway <- rbind(pathway, subset(ubco, Course.Code %in% samp))
  pathway <-
    rbind(pathway, subset(ubco, Course.Code %in% c("COSC 111", "COSC 121")))
  
  
  # Second Year
  pathway <-
    rbind(pathway, subset(
      ubco,
      Course.Code %in% c("MATH 200",
                         "MATH 221",
                         "STAT 230",
                         "COSC 221",
                         "COSC 222")
    ))
  
  
  # Third and Fourth Year
  pathway <-
    rbind(pathway, subset(
      ubco,
      Course.Code %in% c("DATA 301",
                         "DATA 311",
                         "COSC 304",
                         "STAT 303",
                         "PHIL 331")
    ))
  
  
  
  upper_year_data <-
    c("DATA 310", "DATA 315", "DATA 405", "DATA 407", "DATA 410")
  max_2_stat <- c("STAT 400", "STAT 401", "STAT 403", "STAT 406")
  
  upper_year_math <-
    c(
      "MATH 409",
      "MATH 303",
      "MATH 307",
      "MATH 319",
      "MATH 225"
    )
  course <- c()
  while (length(unique(course)) < 4) {
    var <- sample(1:2, 1)
    
    if (var == 1) {
      course <- c(course, sample(max_2_stat, 1))
    } else if (var == 2) {
      course <- c(course, sample(upper_year_data, 1))
    }
  }
  
  pathway <-
    rbind(pathway, subset(ubco, Course.Code %in% c(unique(course),upper_year_math)))
  
  pathway <- na.omit(pathway)
  # pathway <- pathway[!duplicated(pathway),]
  rownames(pathway) <- 1:nrow(pathway)
  
  # Construct node and edge list
  
  node_list <-
    data.frame(id = rownames(pathway), label = pathway$Course.Code)
  
  edge_list <- data.frame(from = NA, to = NA)
  
  for (node in node_list$label) {
    str <- subset(pathway, Course.Code == node)$Prerequisite
    course_code <- ""
    if (str != "") {
      course_code <- str_extract_all(str, "[A-Z]{4} [0-9]{3}")[[1]]
    }
    
    from <- rownames(subset(pathway, Course.Code == node))
    to <- rownames(subset(pathway, Course.Code %in% course_code))
    if (length(to) > 1) {
      for (id in to) {
        edge_list <- rbind(edge_list, data.frame(from = id, to = from))
      }
    } else {
      edge_list <- rbind(edge_list, data.frame(from = to[1], to = from))
    }
  }
  edge_list <- na.omit(edge_list)
  
  # Get structural complexity
  
  sc <- structural_complexity(edge_list, node_list)
  
  # Store in list
  results <-
    list(node_list = node_list,
         edge_list = edge_list,
         sc = sc)
  
  Gc <- c(Gc, list(results))
}

# Initialize variables for maximum and minimum values
max_total <- -Inf
min_total <- Inf
max_index <- NULL
min_index <- NULL

# Iterate through Gc to find max and min indices
for (i in seq_along(Gc)) {
  total <- Gc[[i]]$sc$total
  if (total > max_total) {
    max_total <- total
    max_index <- i
  }
  if (total < min_total) {
    min_total <- total
    min_index <- i
  }
}

node_list <- Gc[[max_index]]$node_list
edge_list <- Gc[[max_index]]$edge_list

sc_df <- structural_complexity(edge_list,node_list)
cf_df <- centrality_factor(edge_list,node_list)
bf_df <- blocking_factor(edge_list,node_list)
df_df <- delay_factor(edge_list,node_list)

node_list <- left_join(node_list, sc_df$bynode, by = c("id" = "id"))
node_list <- left_join(node_list, cf_df, by = c("id" = "id"))
node_list <- left_join(node_list, bf_df$bynode, by = c("id" = "id"))
node_list <- left_join(node_list, df_df$bynode, by = c("id" = "id"))
max_graph <- list(edge_list = edge_list, node_list = node_list, sc_total = sc_df$total, bf_total = bf_df$total, df_total = df_df$total)

save(max_graph, file = ".\\data\\rdata\\maxGraph_math.RData")

node_list <- Gc[[min_index]]$node_list
edge_list <- Gc[[min_index]]$edge_list

sc_df <- structural_complexity(edge_list,node_list)
cf_df <- centrality_factor(edge_list,node_list)
bf_df <- blocking_factor(edge_list,node_list)
df_df <- delay_factor(edge_list,node_list)

node_list <- left_join(node_list, sc_df$bynode, by = c("id" = "id"))
node_list <- left_join(node_list, cf_df, by = c("id" = "id"))
node_list <- left_join(node_list, bf_df$bynode, by = c("id" = "id"))
node_list <- left_join(node_list, df_df$bynode, by = c("id" = "id"))

min_graph <- list(edge_list = edge_list, node_list = node_list, sc_total = sc_df$total, bf_total = bf_df$total, df_total = df_df$total)

save(min_graph, file = ".\\data\\rdata\\minGraph_math.RData")