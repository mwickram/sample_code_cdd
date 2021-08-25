# Mahesh Imaduwa

# given key and level, pull fields of a nested dataframe in a json object
get_field_names <-  function(dataframe, row, level = 4, key = "contentId", fun = length, ...){
  
  field_names <- NULL
  df <- dataframe
  Pipe(df[row,])$
    list.ungroup(level = level)$
    .( ~ s)$
    list.names()$
    list.any(cond = x ~ x == key)$
    .(if(.) {
    unlist(s[list.names(s) == key], use.names = F) %>>%
      fun %>>%
      (~ field_names)
  })
  field_names
} 


# find data frames with 'Chemicals' materialType
any_chemical <- funtion(dataframe,...){

  df <- dataframe
  dfout <- list(is_chem_index = NULL)
  
  LEVEL <- 4
  
  for (row_index in 1:nrow(df)){
    get_field_names(df, 
                    row=row_index,
                    level = LEVEL,
                    field = 'materialTypeName',
                    fun=function(x){
                      x == 'Chemicals'
                    })%>>%
       (if(!is.null(.) & sum(., na.rm = T) > 0){
        vec_len <- length(dfout$is_chem_index)
        dfout$is_chem_index <- append(dfout$is_chem_index,
                                      row_index,
                                      after = vec_len)
       })
  }
  dfout
}
