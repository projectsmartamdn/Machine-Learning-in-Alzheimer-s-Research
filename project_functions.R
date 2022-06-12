get_num_frec_total = function(data_describe)
{
  num_frec = 0
  data_describe_unlist = unlist(data_describe[[1]])
  for (i in 1:length(data_describe_unlist)) { 
    if(names(data_describe_unlist[i])== "counts.n")
    {
      num_frec = (data_describe_unlist[[i]])
      return(as.numeric(num_frec))
    }
  }
}

get_list_grouped = function(data, percentage_limit)
{
  data_describe = describe(data)
  
  num_frec_total = get_num_frec_total(data_describe)
  data_percentage = 100 * (table(data)/num_frec_total)
  
  
  sum_perc = 0
  label = ""
  vector_label = c()
  vector_percent = c()
  
  for (i in 1:length(data_percentage)) {
    if(data_percentage[[i]] > 0) {
      if (sum_perc < percentage_limit)
      {
        sum_perc = sum_perc + data_percentage[[i]]
        if (label == "")
        {
          label = names(data_percentage[i])
        }
        else
        {
          label = paste(label, names(data_percentage[i]),sep="-")
        }
        
      }
      else
      {
        if (label == "")
        {
          label = names(data_percentage[i])
        }
        vector_label = append(vector_label,label)
        vector_percent = append(vector_percent,sum_perc)
        label = names(data_percentage[i])
        sum_perc = data_percentage[[i]]
      }
    }
  }
  vector_label = append(vector_label,label)
  vector_percent = append(vector_percent,sum_perc)
  df_group = data.frame(vector_label,vector_percent)
  last_two_rows = tail(df_group, n=2)
  if (nrow(last_two_rows) == 2) {
    if (tail(last_two_rows["vector_percent"], n=1) <percentage_limit) {
      new_vector_label = paste(head(last_two_rows["vector_label"], n=1)[[1]],
                               tail(last_two_rows["vector_label"], n=1)[[1]],sep="-")
      new_vector_percent = head(last_two_rows["vector_percent"], n=1)[[1]] + 
        tail(last_two_rows["vector_percent"], n=1)[[1]]
      df_head_except_2 = head(df_group,-2)
      df_last_two = data.frame(new_vector_label,new_vector_percent)
      colnames(df_last_two) <- c("vector_label", "vector_percent")
      df_group = rbind(df_head_except_2, df_last_two)
    }
  }
  
  
  return(df_group)
}




#-------------------
remove_columns_percentage_higher_limit = function(data, limit_percentage)
{

  columns_to_eliminate = c()
  for (column in colnames(data)){
    data_describe = describe(data[column])
    
    num_frec_total = get_num_frec_total(data_describe)
    data_percentage = 100 * (table(data[column])/num_frec_total)
    for (column_percentage in data_percentage)
    {
      if (column_percentage >= limit_percentage)
      {
        columns_to_eliminate = append(columns_to_eliminate,column)
        break
      }
    }
  }
  
  print(paste("Removed column:", columns_to_eliminate))
  data[ , columns_to_eliminate] = NULL
  return (data)
}







#-------------------
replace_levels_grouped_in_data = function(list_grouped, data_column) {
  for (row in 1:nrow(list_grouped)) {
    if(!is.na(str_locate(as.character(list_grouped[row, "vector_label"]), "-")[[1]])) {
      element = list_grouped[row, "vector_label"]
      list_grouped_split = strsplit(as.character(element), "-")[[1]]
      print(element)
      for (number in list_grouped_split) {
        data_column = as.character(data_column)
        data_column[data_column == number] = as.character(element)
        data_column = as.factor(data_column)
      }
    }
  }
  return(data_column)
}



#-------------------

















