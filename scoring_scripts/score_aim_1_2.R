
# How about MI instead of covariance ?
# how about weighting stats stronger if it is further from zero?

require(tidyverse)

# this function computes the mean and covariance matrices from the single cell data
data_to_stats <- function(single_cell_data){
	# first we compute the mean and the covariance of the reporters
	single_cell_stats <-  single_cell_data %>% 
		group_by(cell_line,treatment,time) %>%
		nest(.key = "data") %>%
		mutate(mean_values = map(data,colMeans)) %>%
		mutate(cov_values = map(data,cov))
	
	# flatten the upper triangular of a symmetric matrix with diagonal to a table (from Stackoverflow)
	flattenCovMatrix <- function(covmat) {
		ut <- upper.tri(covmat,diag = TRUE)
		tibble(
			stat_variable = paste0("cov_", rownames(covmat)[row(covmat)[ut]],"_",rownames(covmat)[col(covmat)[ut]]),
			stat_value  =(covmat)[ut]
		)
	}
	
	# we reshape the statistics to a column
	# first the mean, then the cov matrix, finally we bind them 
	single_cell_stats_long <- single_cell_stats %>% 
		mutate(vec_mean = map(mean_values,function(x){
			# reshape the row vector to a column vector
			df = enframe(x,name = "stat_variable", value = "stat_value")
			df %>% mutate(stat_variable=paste0("mean_",stat_variable))
		}
		)) %>%
		mutate(vec_cov = map(cov_values,flattenCovMatrix)) %>% 
		mutate(all_stats = map2(vec_mean,vec_cov,function(mean,cov){
			rbind(mean,cov)
		})) %>% unnest(all_stats)
	
	
	return(single_cell_stats_long)
	
}



# scores the subchallenge aim 1.2.1 and 1.2.2
#' @param prediction_data_file path to prediction data file (.csv)
#' @param validation_data_file path to validation data file (.csv)
#' @description checks input for missing columns
#' check input for missing conditions (missing predicted cells)
#' computes root-mean square error by conditions, then averages these


score_aim_1_2 <- function(prediction_data_file,validation_data_file){
	
	# load validation data
	validation_data <- read_csv (validation_data_file) %>% select(-fileID,-cellID)
	prediction_data <- read_csv(prediction_data_file)
	
	### Checking inputs -------------------
	# checking columns of input data table
	# HER2, PLCg2, cellID and fileID not included on purpose ! 
	required_columns <- c('cell_line','treatment', 'time',
						  'b.CATENIN', 'cleavedCas', 'CyclinB', 'GAPDH', 'IdU',
						  'Ki.67', 'p.4EBP1', 'p.Akt.Ser473.', 'p.AKT.Thr308.',
						  'p.AMPK', 'p.BTK', 'p.CREB', 'p.ERK', 'p.FAK', 'p.GSK3b',
						  'p.H3', 'p.JNK', 'p.MAP2K3', 'p.MAPKAPK2',
						  'p.MEK', 'p.MKK3.MKK6', 'p.MKK4', 'p.NFkB', 'p.p38',
						  'p.p53', 'p.p90RSK', 'p.PDPK1', 'p.RB', 
						  'p.S6', 'p.S6K', 'p.SMAD23', 'p.SRC', 'p.STAT1',
						  'p.STAT3', 'p.STAT5') 
	
	if(!all(required_columns %in% names(prediction_data))) {
		stop(paste0("missing columns detected. Required columns: ", paste(required_columns,collapse = ", ")))
	}
	prediction_data = prediction_data %>% select(required_columns)
	# as we agreed, we remove plcg and her2 from the validation data:
	validation_data = validation_data %>% select(required_columns)
	
	# checking for any missing conditions
	required_conditions <- validation_data %>% select(cell_line,treatment,time) %>% unique()
	predicted_conditions <- prediction_data %>% select(cell_line,treatment,time)
	
	missing_conditions = anti_join(required_conditions,predicted_conditions,by = c("cell_line", "treatment", "time"))
	
	if(nrow(missing_conditions)>0){
		print(missing_conditions %>% select(c("cell_line", "treatment", "time")))
		stop("missing predictions detected for above conditions")	
	} 
	
	## Calculate statistics from single cell-data -----------------
	validation_stats <- data_to_stats(validation_data) %>% rename(test_stat_value = stat_value)
	prediction_stats <- data_to_stats(prediction_data) %>% rename(predicted_stat_value = stat_value)
	
	### Formating -------------------------
	# join the test and validation data
	
	combined_data = validation_stats %>%
		full_join(prediction_stats, by=c("cell_line", "treatment", "time",  "stat_variable"))
	
	### Calculate score --------------------
	# calculate the  distance over all stats
	final_score = dist(rbind(test = combined_data$test_stat_value,
							 prediction =combined_data$predicted_stat_value), method = "euclidean")
}





