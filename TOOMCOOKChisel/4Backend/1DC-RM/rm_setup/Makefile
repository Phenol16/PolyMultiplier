##DC RM Makefile
##See README.* file for RM details


DC_EXEC 	= dc_shell -64bit
LOGS_DIR    = logs
REPORTS_DIR = reports
RESULTS_DIR = results

FM_EXEC 	= fm_shell -64bit

####Source Options
OPTIONS 	= 

##Optional: Specify design library if you want backup step and clean step to use it
##CAUTION: if added, the clean step will delete it
DESIGN_LIB 	= DGR_DUT


##########################################################################################################################
# Flat floorplanning script execution section
# Execute "make dp" to run ICC Design Planning RM based on your common_setup.tcl, icc_setup.tcl
##########################################################################################################################


dc: 
	mkdir -p $(REPORTS_DIR) $(RESULTS_DIR) $(LOGS_DIR)
	$(DC_EXEC) $(OPTIONS) -f ../rm_dc_scripts/dc.tcl | tee -i $(LOGS_DIR)/dc.log
	date > dc

fm: dc
	$(FM_EXEC) -f ../rm_dc_scripts/fm.tcl | tee -i $(LOGS_DIR)/fm.log
	date > fm

#############################################################################################################################
# Utilities Section
#
#############################################################################################################################

##Backup
BACKUP  	= BACKUP.`date "+%m_%d_%H_%M"`
backup:
	rm -rf $(BACKUP)
	mkdir -p $(BACKUP)
	cp -rf ../rm_dc_scripts ../src ../rm_setup  $(DESIGN_LIB) $(REPORTS_DIR) $(RESULTS_DIR) $(LOGS_DIR) $(BACKUP)


##Clean
clean_all:
	rm -f dc
	rm -f fm
	rm -rf $(DESIGN_LIB) $(LOGS_DIR) command.log ./work $(REPORTS_DIR) $(RESULTS_DIR)  

##ICC_RM-Info: "make clean" does not remove the design library unless you have specified it with the DESIGN_LIB variable in Makefile"

clean: clean_all
