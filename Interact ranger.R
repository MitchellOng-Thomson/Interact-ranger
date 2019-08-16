


# Interact function - adapting to ranger package - use treeInfo()

interact <- function(forest, depth){
  
  ### Pass the function a random forest that has been grown through the ranger package ###
  ### Count the number of total occurences, average depth and number of occurences at levels 1 and 2 for each variable pair ###
  
  # Create tibble to store the splits in each tree
  Total_splits <- tibble(splits = as.character(), count = as.integer())
  
  # Create tibble to store the first two splits in each tree
  Total_first_two_splits <- tibble(splits = as.character(), count = as.integer())
  
  
  
  # loop through all the trees using seq_len(forest$num.trees)
  for(i in seq_len(forest$num.trees)){
    
    # Extract a dataframe that captures tree i and slice it at the pre-determined depth
    rf.tree <- treeInfo(forest, i) %>% 
      slice(1:depth)
    
    # Create tibble to join with the rf.tree to get the variables split on the left and right
    extra_var <- tibble(lc = rep(as.character(1), nrow(rf.tree)), rc = rep(as.character(1), nrow(rf.tree)))
    
    # Loop through each split level and get variable names of the left and right splits - fill out extra_var 
    for(x in seq_len(depth)){    
      lc <- rf.tree[x, ] %>% pull(leftChild)
      rc <- rf.tree[x, ] %>% pull(rightChild)
      
      lc_var <- rf.tree %>% filter(nodeID == lc) %>% pull(splitvarName) %>% as.character()
      rc_var <- rf.tree %>% filter(nodeID == rc) %>% pull(splitvarName) %>% as.character()
      
      extra_var[x, 1] <- ifelse(identical(lc_var, character(0)), "Terminal node", lc_var)
      extra_var[x, 2] <- ifelse(identical(rc_var, character(0)), "Terminal node", rc_var)
    }
    
    # Adding the variable names to the original tree data frame
    rf.tree <- rf.tree %>% 
      bind_cols(extra_var)
    
    # Collapsing the columns to count the number of variable pairs, sorting is done to avoid having both variable pair combinations
    all_splits <- rf.tree %>% 
      slice(1:depth) %>%
      arrange(splitvarName, lc, rc) %>%
      unite("lc_split", splitvarName, lc, sep = "-", remove = FALSE) %>% 
      unite("rc_split", splitvarName, rc, sep = "-", remove = FALSE)
    
    lc_splits <- all_splits %>% 
      select(splits = lc_split) 
    
    all_splits <- all_splits %>% 
      select(splits = rc_split) %>% 
      bind_rows(lc_splits) %>% 
      count(splits) 
    
    Total_splits <- Total_splits %>% 
      right_join(all_splits) %>% 
      mutate(count = case_when(
        is.na(count) ~ n,   
        TRUE ~ count + n
      )) %>% 
      select(splits, count)
    
    
    # Count the variable pairs that occur in either of the first two splits
    first_two_splits <- rf.tree %>% 
      slice(1:2) %>% 
      unite("lc_split", splitvarName, lc, sep = "-", remove = FALSE) %>% 
      unite("rc_split", splitvarName, rc, sep = "-", remove = FALSE)
    
    first_two_lc <- first_two_splits %>% 
      select(splits = lc_split)
    
    first_two_splits <- first_two_splits %>% 
      select(splits = rc_split) %>% 
      bind_rows(first_two_lc) %>% 
      count(splits)
    
    Total_first_two_splits <- Total_first_two_splits %>% 
      right_join(first_two_splits) %>% 
      mutate(count = case_when(
        is.na(count) ~ n,   
        TRUE ~ count + n
      )) %>% 
      select(splits, count)
    
    
  }
  
  list(Total_splits = Total_splits, First_two_splits = Total_first_two_splits)
}






































