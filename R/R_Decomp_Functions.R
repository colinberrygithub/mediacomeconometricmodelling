clean_eviews_vars = function(vector){
  
  require(magrittr)
  
  out = vector %>%
    gsub("\\/[[:graph:]]+","", ., ignore.case = TRUE) %>%
    gsub("\\*[[:graph:]]+","", ., ignore.case = TRUE) %>%
    gsub("\\+[[:graph:]]+","", ., ignore.case = TRUE) %>%
    gsub("\\-[[:graph:]]+","", ., ignore.case = TRUE) %>% 
    gsub("\\/[[:graph:]]+","", ., ignore.case = TRUE) %>%
    gsub("\\,[[:graph:]]+","", ., ignore.case = TRUE) %>%
    gsub("\\^[[:graph:]]+","", ., ignore.case = TRUE) %>%
    gsub("^LOG","", ., ignore.case = TRUE) %>%
    gsub("^@atan","", ., ignore.case = TRUE) %>%
    gsub("^@movavc","", ., ignore.case = TRUE) %>% 
    gsub("\\([[:alnum:]]\\)","", ., ignore.case = TRUE) %>%
    gsub("\\(-[[:alnum:]]\\)","", ., ignore.case = TRUE) %>%
    gsub("^\\(+|$\\)","", ., ignore.case = TRUE) %>%
    gsub("\\)$","", ., ignore.case = TRUE) %>%
    gsub("\\($","", ., ignore.case = TRUE) %>%
    gsub("_sp[[:alnum:]][[:alnum:]]","_sp", ., ignore.case = TRUE) %>%
    gsub("_w[[:alnum:]][[:alnum:]]","_w", ., ignore.case = TRUE) %>% 
    gsub("_ots[[:alnum:]][[:alnum:]]","_ots", ., ignore.case = TRUE) %>%
    gsub("_vol[[:alnum:]][[:alnum:]]","_vol", ., ignore.case = TRUE) %>% 
    gsub("_vo[[:alnum:]][[:alnum:]]","_vo", ., ignore.case = TRUE)  %>%
    gsub("_im[[:alnum:]][[:alnum:]]","_im", ., ignore.case = TRUE) %>%
    gsub("_et[[:alnum:]][[:alnum:]]","_et", ., ignore.case = TRUE) %>%
    gsub("_ag[[:alnum:]][[:alnum:]]","_at", ., ignore.case = TRUE) %>%
    gsub("_et[[:alnum:]][[:alnum:]]","_eg", ., ignore.case = TRUE) %>%
    gsub("_ag[[:alnum:]][[:alnum:]]","_ag", ., ignore.case = TRUE) %>%
    gsub("_vi[[:alnum:]][[:alnum:]]","_vi", ., ignore.case = TRUE) %>%
    gsub("_cl[[:alnum:]][[:alnum:]]","_cl", ., ignore.case = TRUE) %>%
    gsub("_va[[:alnum:]][[:alnum:]]","_va", ., ignore.case = TRUE)
  
  return(out)
  
}


create_unique_map = function(dataframe, column_to_unique){
  
  require(magrittr)
  
  master <- dataframe[1,]
  
  for(x in 2:length((dataframe %>%pull(get(column_to_unique))))){
    
    count <- sum((dataframe %>%  pull(get(column_to_unique)))[1:x] == (dataframe %>%  pull(get(column_to_unique)))[x])
    
    if(count==1){
      master <- bind_rows(master,dataframe[x,])
    }else{}
  }
  
  return(master)
}