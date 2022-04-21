#' Decomp my model names mapping
#'
#' This function loads the mapping files and processes them - it binds the 7 mappings files
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export

decomp_my_model_names_mapping <- function(a,b,c,d,e,f,g,date_mapping){
  start.date = as.Date(dates[1], format = '%d/%m/%Y')
  end.date = as.Date(dates[2], format = '%d/%m/%Y')
  
  tbl_map <- cbind(a,b,c,d,e,f,g)
  tbl_map <- as.data.frame(tbl_map, stringsAsFactors =  FALSE)
  
  colnames(tbl_map) <- tbl_map[1,]
  colnames(tbl_map) <- tolower(colnames(tbl_map))
  
  tbl_map <- tbl_map[-1,]
  tbl_map <-as.data.frame(lapply(tbl_map,tolower), stringsAsFactors = FALSE)
  
  tbl_map_master <- tbl_map[1,]
  
  my_model_names_mapping <- toupper(my_model_names_mapped)
  
  if(tolower(frequency) == 'weekly'){
    
    my_model_variables <- my_vars %>%
      rename_all(toupper) %>%
      replace(is.na(.),0)  %>%
      select(my_model_names_mapping) %>%
      mutate(date = seq(from = start.date, to = end.date, by = 'week')) %>%
      select(date, everything())
  }
  
  if(tolower(frequency) == 'daily'){
    
    my_model_variables <- my_vars %>%
      rename_all(toupper) %>%
      replace(is.na(.),0) %>%
      select(my_model_names_mapping) %>%
      mutate(date = seq(from = start.date, to = end.date, by = 'day')) %>%
      select(date, everything())
    
  }
  return(my_model_names_mapping)
}

decomp_my_model_kpi_data <- function(a,b,c,d,e,f,g,date_mapping){start.date = as.Date(dates[1], format = '%d/%m/%Y')
end.date = as.Date(dates[2], format = '%d/%m/%Y')

tbl_map <- cbind(tbl_map_1,
                 tbl_map_2,
                 tbl_map_3,
                 tbl_map_4,
                 tbl_map_5,
                 tbl_map_6,
                 tbl_map_7)


tbl_map <- as.data.frame(tbl_map, stringsAsFactors =  FALSE)

colnames(tbl_map) <- tbl_map[1,]
colnames(tbl_map) <- tolower(colnames(tbl_map))

tbl_map <- tbl_map[-1,]
tbl_map <-as.data.frame(lapply(tbl_map,tolower), stringsAsFactors = FALSE)

tbl_map_master <- tbl_map[1,]

my_model_names_mapping <- toupper(my_model_names_mapped)

if(tolower(frequency) == 'weekly'){
  
  my_model_variables <- my_vars %>%
    rename_all(toupper) %>%
    replace(is.na(.),0)  %>%
    select(my_model_names_mapping) %>%
    mutate(date = seq(from = start.date, to = end.date, by = 'week')) %>%
    select(date, everything())
}

if(tolower(frequency) == 'daily'){
  
  my_model_variables <- my_vars %>%
    rename_all(toupper) %>%
    replace(is.na(.),0) %>%
    select(my_model_names_mapping) %>%
    mutate(date = seq(from = start.date, to = end.date, by = 'day')) %>%
    select(date, everything())
  
}


# Making sure all variables in tbl_map are unique
tbl_map_master <- create_unique_map(tbl_map, "variable")

# Create date mapping
calendar_mapping <- read.csv(paste0(output_path[1], '/date_mapping_P9-10_2021.csv')) %>%
  mutate(start_date = as.Date(start_date, format = '%d/%m/%Y'),
         end_date = as.Date(end_date, format = '%d/%m/%Y')) %>%
  rename(period = 1) %>%
  select(period, start_date, end_date) %>%
  arrange(start_date)


# Create KPI data with date column and dependent variable column
my_model_kpi_data <- my_model_variables %>% select(date, my_model_names_mapping[1]) %>% rename(y = 2)
return(my_model_kpi_data)}

decomp_format_table_one <- function(a,b,c,d,e,f,g,date_mapping){
  start.date = as.Date(dates[1], format = '%d/%m/%Y')
  end.date = as.Date(dates[2], format = '%d/%m/%Y')
  
  tbl_map <- cbind(a,b,c,d,e,f,g)
  tbl_map <- as.data.frame(tbl_map, stringsAsFactors =  FALSE)
  
  colnames(tbl_map) <- tbl_map[1,]
  colnames(tbl_map) <- tolower(colnames(tbl_map))
  
  tbl_map <- tbl_map[-1,]
  tbl_map <-as.data.frame(lapply(tbl_map,tolower), stringsAsFactors = FALSE)
  
  tbl_map_master <- tbl_map[1,]
  
  my_model_names_mapping <- toupper(my_model_names_mapped)
  
  if(tolower(frequency) == 'weekly'){
    
    my_model_variables <- my_vars %>%
      rename_all(toupper) %>%
      replace(is.na(.),0)  %>%
      select(my_model_names_mapping) %>%
      mutate(date = seq(from = start.date, to = end.date, by = 'week')) %>%
      select(date, everything())
  }
  
  if(tolower(frequency) == 'daily'){
    
    my_model_variables <- my_vars %>%
      rename_all(toupper) %>%
      replace(is.na(.),0) %>%
      select(my_model_names_mapping) %>%
      mutate(date = seq(from = start.date, to = end.date, by = 'day')) %>%
      select(date, everything())
    
  }
  
  # Making sure all variables in tbl_map are unique
  tbl_map_master <- create_unique_map(tbl_map, "variable")
  
  # Create date mapping
  calendar_mapping <- read.csv(paste0(output_path[1], date_mapping)) %>%
    mutate(start_date = as.Date(start_date, format = '%d/%m/%Y'),
           end_date = as.Date(end_date, format = '%d/%m/%Y')) %>%
    rename(period = 1) %>%
    select(period, start_date, end_date) %>%
    arrange(start_date)
  
  
  # Create KPI data with date column and dependent variable column
  my_model_kpi_data <- my_model_variables %>% select(date, my_model_names_mapping[1]) %>% rename(y = 2)
  
  
  # Create model data wide table with date, then intercept and then model variables
  
  my_model_data_wide <- my_model_variables %>%
    select(date, my_model_names_mapping[2:length(my_model_names_mapping)]) %>%
    mutate(Intercept = 1) %>%
    select(date, Intercept, everything())
  
  
  # Create coefficent data
  
  my_coefs_data <- as.data.frame(my_coefficients)
  my_coefs_data[,1] <- as.character(my_coefs_data[,1])
  
  ######## Formatting tbl_map ###########
  
  # Create mapping table
  index <- seq(from = 1, to = nrow(my_model_names_mapped), by = 1)
  my_mapper <- as.data.frame(index)
  
  my_mapper$actual_vars <- tolower(my_model_names_mapped)
  my_mapper$modelled_vars <- tolower(my_model_names_mapped)
  my_mapper$decomp_group <- "BASE"
  my_mapper$alpha <- 0
  my_mapper$orig_vars <- tolower(my_model_names)
  my_mapper$clean_vars <- tolower(my_model_names)
  my_mapper$spend_vars <- tolower(my_model_names)
  
  return(my_mapper)
}

decomp_my_media_variables <- function(){
  my_mapper$clean_vars <-  my_mapper$clean_vars %>%
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
    gsub("^\\(|$\\)","", ., ignore.case = TRUE) %>%
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
  
  
  my_mapper <- my_mapper %>%
    left_join(., select(tbl_map_master, variable, maxmin), by = c("clean_vars" = "variable")) %>%
    left_join(., select(tbl_map_master, variable, group), by = c("clean_vars" = "variable"))
  
  
  colnames(my_mapper)[which(colnames(my_mapper) == "maxmin")]<- "ref_points"
  my_mapper$ref_points[is.na(my_mapper$ref_points)] <- ""
  my_mapper$ref_points[which(my_mapper$ref_points == "")] <- "none"
  
  ### Work out what variables to call Non-Base
  # The default is only price, promotions and media
  my_mapper$decomp_group[grep("^pri",my_mapper$clean_vars)] <- my_mapper$modelled_vars[grep("^pri",my_mapper$clean_vars)]
  my_mapper$decomp_group[grep("^m_",my_mapper$clean_vars)] <- my_mapper$modelled_vars[grep("^m_",my_mapper$clean_vars)]
  my_mapper$decomp_group[grep("^pro",my_mapper$clean_vars)] <- my_mapper$modelled_vars[grep("^pro",my_mapper$clean_vars)]
  my_mapper$decomp_group[grep("^pro",my_mapper$clean_vars)] <- my_mapper$modelled_vars[grep("^pro",my_mapper$clean_vars)]
  
  # incorporate the constant into the mapping table
  my_mapper$actual_vars[1] <- "intercept"
  my_mapper$modelled_vars[1] <- "intercept"
  my_mapper$alpha[1] <- 1
  my_mapper$`ref points`[1] <- "none"
  my_mapper$actual_vars[1] <- "intercept"
  my_mapper$group[1] <- "base"
  my_mapper$orig_vars[1] <- "intercept"
  my_mapper$clean_vars[1] <- "intercept"
  my_mapper$spend_vars[1] <- "intercept"
  
  
  my_media_vars_cols <- my_mapper$actual_vars[grep("^m_", my_mapper$clean_vars)]
  
  my_media_vars <- select(my_model_variables, "date", toupper(my_media_vars_cols))
  
  if(tolower(frequency) == 'weekly'){
    
    my_media_variables <- my_media_vars %>%
      replace(is.na(.),0) %>%
      mutate(date = seq(from = start.date, to = end.date, by = 'week')) %>%
      select(date, everything())
    
    
    calendar <- bind_rows(lapply(1:nrow(calendar_mapping), function(x){
      data.frame(date = seq(from = calendar_mapping$start_date[x], to = calendar_mapping$end_date[x], by = 'week'),
                 period = calendar_mapping$period[x])
      
      
      
    }))
    
    
  }
  
  
  if(tolower(frequency) == 'daily'){
    
    my_media_variables <- my_media_vars %>%
      replace(is.na(.),0) %>%
      mutate(date = seq(from = start.date, to = end.date, by = 'day')) %>%
      select(date, everything())
    
    
    calendar <- bind_rows(lapply(1:nrow(calendar_mapping), function(x){
      data.frame(date = seq(from = calendar_mapping$start_date[x], to = calendar_mapping$end_date[x], by = 'day'),
                 period = calendar_mapping$period[x])
    }))
    
  }
  
  colnames(my_media_variables) <- gsub('_CLEAN', '', colnames(my_media_variables))
  return(my_media_variables)
}

decomp_my_coefs_data <- function(){
  media_spends <- my_media_variables %>%
    left_join(calendar, by = c('date')) %>%
    mutate(period = as.character(period)) %>%
    filter(!is.na(period)) %>%
    select(date, period, everything())
  
  colnames(media_spends) <- tolower(colnames(media_spends))
  
  ## Formatting the spend varuables
  spend_vars <- cbind(my_mapper$clean_vars,my_mapper$clean_vars)
  colnames(spend_vars) <- c("x","y")
  spend_vars <- as.data.frame(spend_vars, stringsAsFactors = FALSE)
  my_spend_vars <- as.data.frame(spend_vars$x)
  
  my_mapper <- my_mapper %>%
    select(actual_vars, modelled_vars, decomp_group, alpha, group, ref_points, orig_vars, clean_vars, spend_vars) %>%
    rename(actual_vars = 1, modelled_vars = 2, decomp_group = 3, alpha = 4, group = 5, ref_points = 6) %>%
    mutate(index = 1:nrow(.)) %>%
    select(index, everything())%>%
    mutate(orig_vars = gsub('@', '', as.character(orig_vars)))
  
  # format coefficients data
  my_coefs_data <- bind_cols(as.data.frame(toupper(my_mapper$actual_vars[])), as.data.frame(my_coefficients)) %>%
    rename(variable = 1, coeffs = 2)
  
  # Format model data wide
  colnames(my_model_data_wide) <- tolower(colnames(my_model_data_wide))
  return(my_coefs_data)
}

decomp_my_model_data_wide <- function(){
  media_spends <- my_media_variables %>%
    left_join(calendar, by = c('date')) %>%
    mutate(period = as.character(period)) %>%
    filter(!is.na(period)) %>%
    select(date, period, everything())
  
  colnames(media_spends) <- tolower(colnames(media_spends))
  
  ## Formatting the spend varuables
  spend_vars <- cbind(my_mapper$clean_vars,my_mapper$clean_vars)
  colnames(spend_vars) <- c("x","y")
  spend_vars <- as.data.frame(spend_vars, stringsAsFactors = FALSE)
  my_spend_vars <- as.data.frame(spend_vars$x)
  
  my_mapper <- my_mapper %>%
    select(actual_vars, modelled_vars, decomp_group, alpha, group, ref_points, orig_vars, clean_vars, spend_vars) %>%
    rename(actual_vars = 1, modelled_vars = 2, decomp_group = 3, alpha = 4, group = 5, ref_points = 6) %>%
    mutate(index = 1:nrow(.)) %>%
    select(index, everything())%>%
    mutate(orig_vars = gsub('@', '', as.character(orig_vars)))
  
  # format coefficients data
  my_coefs_data <- bind_cols(as.data.frame(toupper(my_mapper$actual_vars[])), as.data.frame(my_coefficients)) %>%
    rename(variable = 1, coeffs = 2)
  
  # Format model data wide
  colnames(my_model_data_wide) <- tolower(colnames(my_model_data_wide))
  return(my_model_data_wide)
}

decomp_my_mapper <- function(){
  media_spends <- my_media_variables %>%
    left_join(calendar, by = c('date')) %>%
    mutate(period = as.character(period)) %>%
    filter(!is.na(period)) %>%
    select(date, period, everything())
  
  colnames(media_spends) <- tolower(colnames(media_spends))
  
  ## Formatting the spend varuables
  spend_vars <- cbind(my_mapper$clean_vars,my_mapper$clean_vars)
  colnames(spend_vars) <- c("x","y")
  spend_vars <- as.data.frame(spend_vars, stringsAsFactors = FALSE)
  my_spend_vars <- as.data.frame(spend_vars$x)
  
  my_mapper <- my_mapper %>%
    select(actual_vars, modelled_vars, decomp_group, alpha, group, ref_points, orig_vars, clean_vars, spend_vars) %>%
    rename(actual_vars = 1, modelled_vars = 2, decomp_group = 3, alpha = 4, group = 5, ref_points = 6) %>%
    mutate(index = 1:nrow(.)) %>%
    select(index, everything())%>%
    mutate(orig_vars = gsub('@', '', as.character(orig_vars)))
  
  return(my_mapper)
}

decomp_decomp <- function(){add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]


decomp.obj <- LogLinearDecomper(CoeffsData = my_coefs_data,
                                ModelData.wide = my_model_data_wide,
                                VarSetup = my_mapper,
                                kpiData = my_model_kpi_data)


decomp <- decomp.obj@DecompedData %>%
  left_join(my_mapper %>% select(modelled_vars, orig_vars, clean_vars), by = c('modelled_vars')) %>%
  mutate(modelled_vars = orig_vars,
         clean.vars = clean_vars,
         decomp_group = case_when(decomp_group != 'BASE' ~ clean_vars,
                                  decomp_group == 'BASE' ~ decomp_group)) %>%
  select(-clean_vars, -orig_vars) %>%
  mutate(contributions = replace_na(contributions, 0)) %>%
  select(date, modelled_vars, clean.vars, group, decomp_group, contributions)
return(decomp)}

decomp_decomp_mapper <- function(){add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]


decomp.obj <- LogLinearDecomper(CoeffsData = my_coefs_data,
                                ModelData.wide = my_model_data_wide,
                                VarSetup = my_mapper,
                                kpiData = my_model_kpi_data)


decomp <- decomp.obj@DecompedData %>%
  left_join(my_mapper %>% select(modelled_vars, orig_vars, clean_vars), by = c('modelled_vars')) %>%
  mutate(modelled_vars = orig_vars,
         clean.vars = clean_vars,
         decomp_group = case_when(decomp_group != 'BASE' ~ clean_vars,
                                  decomp_group == 'BASE' ~ decomp_group)) %>%
  select(-clean_vars, -orig_vars) %>%
  mutate(contributions = replace_na(contributions, 0)) %>%
  select(date, modelled_vars, clean.vars, group, decomp_group, contributions)

decomp_group <- decomp %>%
  group_by(date, group) %>%
  summarise(contributions = sum(contributions)) %>%
  ungroup() %>%
  spread(group, contributions)

colnames(decomp_group) <- gsub(' ', '_', colnames(decomp_group))


decomp_group_props <- decomp.obj@DecompGroupProps

# decomp_graph <- decomp.obj@DecompGraph

decomp_data <- decomp.obj@ModelDataWide
colnames(decomp_data)[2:ncol(decomp_data)] <- my_mapper$orig_vars

decomp_mapper <- decomp.obj@SetupData
return(decomp_mapper)}

decomp_CiCo <- function(){dates.start <- min(decomp$date)
dates.end <- max(decomp$date)
Ci.Co.start.date <- as.Date(dates.start) + 1 #let the CiCo start a period after model start date


dat.CiCo <- decomp %>%
  filter(startsWith(clean.vars, 'm_')) %>%
  select(date, modelled_vars, contributions) %>%
  spread(modelled_vars, contributions) %>%
  rename(Date = date) %>%
  mutate(Date = as.Date(Date))

if(tolower(frequency) == 'weekly'){
  
  asp <- asp_series %>%
    rename(ASP = 1) %>%
    mutate(Date = seq(from = start.date, to = end.date, by = 'week')) %>%
    replace(is.na(.),0) %>%
    select(Date, ASP)
  
  margin <- mrg_series %>%
    rename(margin = 1) %>%
    mutate(Date = seq(from = start.date, to = end.date, by = 'week')) %>%
    replace(is.na(.),0) %>%
    select(Date, margin)
}

if(tolower(frequency) == 'daily'){
  
  asp <- asp_series %>%
    rename(ASP = 1) %>%
    mutate(Date = seq(from = start.date, to = end.date, by = 'day')) %>%
    replace(is.na(.),0) %>%
    select(Date, ASP)
  
  margin <- mrg_series %>%
    rename(margin = 1) %>%
    mutate(Date = seq(from = start.date, to = end.date, by = 'day')) %>%
    replace(is.na(.),0) %>%
    select(Date, margin)
}

revenue <- dat.CiCo %>% mutate_at(vars(-1), funs(. * asp$ASP))

profit <- revenue %>% mutate_at(vars(-1), funs(. * margin$margin))


adstocks.dataframe <- suppressWarnings(data.frame(variable_name = names(revenue %>% select(-Date))) %>%
                                         mutate(value = parse_number(word(variable_name, 1,sep = fixed('/'))),
                                                value = replace_na(value, 0),
                                                value = as.numeric(value),
                                                value = value/100,
                                                variable_name = as.character(variable_name)))


date_mapping_table <- read.csv(paste0(output_path[1], '/date_mapping_P9-10_2021.csv')) %>%
  mutate(start_date = as.Date(start_date, format = '%d/%m/%Y'),
         end_date = as.Date(end_date, format = '%d/%m/%Y')) %>%
  filter(start_date >= as.Date(Ci.Co.start.date) & end_date <= as.Date(dates.end)) %>%
  rename(period = 1) %>%
  select(period, start_date, end_date) %>%
  arrange(start_date)


Ci.Co.out_vol <- calculate_media_timeperiod(dat.CiCo,
                                            adstock_rates =  adstocks.dataframe,
                                            date_mapping = date_mapping_table) %>%
  rename_at(vars(3:6), funs(paste0('volume_', .)))


Ci.Co.out_rev <- calculate_media_timeperiod(revenue,
                                            adstock_rates =  adstocks.dataframe,
                                            date_mapping = date_mapping_table) %>%
  rename_at(vars(3:6), funs(paste0('revenue_', .)))


Ci.Co.out_profit <- calculate_media_timeperiod(profit,
                                               adstock_rates =  adstocks.dataframe,
                                               date_mapping = date_mapping_table) %>%
  rename_at(vars(3:6), funs(paste0('margin_', .)))

result.spend <- media_spends %>%
  gather(Variable, cost, 3:ncol(.)) %>%
  group_by(period, Variable) %>%
  summarise(cost = sum(cost)) %>%
  ungroup() %>%
  rename(Variable.Spend = Variable)

result.final <- Ci.Co.out_vol %>%
  left_join(Ci.Co.out_rev, by = c("Variable", "period")) %>%
  left_join(Ci.Co.out_profit, by = c("Variable", "period")) %>%
  mutate(Campaign = plyr::mapvalues(Variable, decomp_mapper$orig_vars, decomp_mapper$group),
         Campaign = gsub('m_', '', Campaign),
         Campaign = toupper(Campaign),
         Variable.Clean = plyr::mapvalues(Variable, decomp_mapper$orig_vars, decomp_mapper$clean_vars),
         Variable.Spend = plyr::mapvalues(Variable, decomp_mapper$orig_vars, decomp_mapper$modelled_vars)) %>%
  select(period,Variable, Variable.Clean, Campaign, everything()) %>%
  left_join(result.spend, by = c('period', 'Variable.Spend'), all = T) %>%
  select(period, Variable, Variable.Clean, Variable.Spend, Campaign, cost, everything()) %>%
  mutate(cpa = cost/volume_final_total,
         revenue_roi = revenue_final_total/cost,
         margin_roi = margin_final_total/cost)

return(result.final)}