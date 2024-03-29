
library(dplyr)
library(tidyr)
library(readr)


# scores the subchallenge aim 2
#' @param prediction_data_file path to prediction data file (.csv)
#' @param validation_data_file path to validation data file (.csv)
#' @description checks input for missing columns
#' check input for missing conditions (missing predicted cells)
#' computes root-mean square error by conditions, then averages these


score_sc4 <- function(prediction_data_file,validation_data_file){
	
	# load validation data
	validation_data <- read_csv (validation_data_file) 
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

		### Formating -------------------------
	# join the test and validation data
	
	combined_data = full_join(prediction_data %>% gather(marker,prediction,-cell_line, -treatment, -time ),
							  validation_data %>% gather(marker,test,-cell_line, -treatment, -time ),
							  by=c("cell_line", "treatment", "time","marker"))
	
	### Calculate score --------------------
	# calculate the  distance over all stats
	RMSE_cond = combined_data %>% group_by(cell_line,treatment,marker) %>% 
		summarise(RMSE = sqrt(sum((test - prediction)^2)/n())) 
	
	final_score = mean(RMSE_cond$RMSE)
}





