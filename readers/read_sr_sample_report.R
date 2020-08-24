assert_one_value = function(vector, name){
	unique_vals = unique(vector)	
	if (length(unique_vals)	> 1){
		stop(paste('Multiple values for variable: ', name))
	} else {
		return(unique_vals)
	}
}

read_sr_sample_report = function(path) {
	# returns: subject_id | x | y | t | aoi	
	sample_df = read.table(path, sep ='\t', header=T)
	subject_id = assert_one_value(sample_df$Session_Name_, 'Session_Name_')
	trial_id = assert_one_value(sample_df$TRIAL_INDEX, 'TRIAL_INDEX')
	
	#use EYE_TRACKED to determine which eye is being tracked
	which_eye = toupper(assert_one_value(sample_df$EYE_TRACKED, 'EYE_TRACKED'))	
	x_colname = paste(which_eye,'_GAZE_X', sep='')
	y_colname = paste(which_eye,'_GAZE_Y', sep='')
	label = paste(which_eye,'_INTEREST_AREA_LABEL', sep='')
	
	rdf = data.frame(sample_df[,c('TIMESTAMP', x_colname, y_colname, label)])
	names(rdf) = c('timestamp','x','y', 'roi')
	rdf$subject_id = subject_id
	rdf$trial_id = trial_id
	return(rdf)
}
	
read_sr_sample_report(path = 'sample_data/sr_research/example_sr_research_output.txt')

#[ ] what is `event`
#[ ] Trial-level vars: target and distractor
