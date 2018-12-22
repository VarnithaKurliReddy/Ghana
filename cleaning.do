* ******************************************************************** *
   * ******************************************************************** *
   *                                                                      *
   *                      Ghana Builtup                                   *
   *                       Cleaning                                       *
   *                                                                      *
   * ******************************************************************** *
   * ******************************************************************** *

       /*
       ** PURPOSE:      The purpose of this do file is to use the original builtup data set and clean it.  
       ** OUTLINE:      PART 0: Import the original dataset.
	                    PART 1: Merging different datasets.
                        PART 2: Renaming Variables
						PART 3: Labeling Variables
						PART 4: Value labels for categorical variables
						PART 5: Save the datasets
						
						 
						
       ** REQUIRES:	    "$datasets/builtup_sp12lags_database.dta"  
	                     "$datasets/ghs_data1975.dta"
						 "$datasets/ghs_data1990.dta"
						 "$datasets/ghs_data2000.dta"
						 "$datasets/ghs_data2014.dta"
						 
                     
	

	   ** CREATES:		""$datasets/cleaned_builtup.dta""          

       ** IDS VAR:         
       ** NOTES:

       ** WRITEN BY:    Varnitha Kurli

       ** Last date modified:  December 20th, 2018
       */

*******************************
*PART 0: Import the original dataset
******************************** 
*Importing the original dataset
 
 use "$datasets/builtup_sp12lags_database.dta", clear
 
 
 *******************************
*PART 1: Merge dataset
******************************** 

merge 1:1 x y using "$datasets/ghs_data1975.dta"
              drop  _merge
			  
merge 1:1 x y using "$datasets/ghs_data1990.dta"
              drop  _merge		
			  
merge 1:1 x y using "$datasets/ghs_data2000.dta"
              drop  _merge
			  
merge 1:1 x y using "$datasets/ghs_data2004.dta"
              drop  _merge			  
 
*******************************
*PART 2: Renaming Variables
******************************** 

rename ///
		(x             y             Rd_Track_1976   Rd_Class2_1976     Rd_Class3_1976    Rd_Class1_1976    Rd_Unknown_1976) ///
		(x_coordinates y_coordinates Track_road_1976 Class2_road_1976   Class3_road_1976  Class1_road_1976  Unknown_road_1976)                                           

rename ///
       (Mean3x3_Rd_Track_1976               Sum3x3rd_Rd_Track_1976                Mean5x5_Rd_Track_1976             Sum5x5_Rd_Track_1976 ) ///
	   (Track_road_spatial_lag_1_mean       Track_road_spatial_lag_1_sum          Track_road_spatial_lag_2_mean     Track_road_spatial_lag_2_sum)
	   
rename ///
        (Mean3x3_Rd_Class2_1976             Sum3x3rd_Rd_Class2_1976               Mean5x5_Rd_Class2_1976             Sum5x5_Rd_Class2_1976) ///
		(Class2_road_spatial_lag_1_mean     Class2_road_spatial_lag_1_sum         Class2_road_spatial_lag_2_mean     Class2_road_spatial_lag_2_sum)
		
rename ///
        (Mean3x3_Rd_Class3_1976             Sum3x3rd_Rd_Class3_1976               Mean5x5_Rd_Class3_1976             Sum5x5_Rd_Class3_1976) ///
		(Class3_road_spatial_lag_1_mean     Class3_road_spatial_lag_1_sum         Class3_road_spatial_lag_2_mean     Class3_road_spatial_lag_2_sum)
		
rename ///
        (Mean3x3_Rd_Class1_1976             Sum3x3rd_Rd_Class1_1976              Mean5x5_Rd_Class1_1976             Sum5x5_Rd_Class1_1976) ///
		(Class1_road_spatial_lag_1_mean     Class1_road_spatial_lag_1_sum        Class1_road_spatial_lag_2_mean     Class1_road_spatial_lag_2_sum)
		
rename ///
        (Mean3x3_Rd_Unknown_1976             Sum3x3rd_Rd_Unknown_1976              Mean5x5_Rd_Unknown_1976             Sum5x5_Rd_Unknown_1976) ///
		(Unknown_road_spatial_lag_1_mean     Unknown_road_spatial_lag_1_sum        Unknown_road_spatial_lag_2_mean     Unknown_road_spatial_lag_2_sum)
		
rename ///
        (Water        Mean3x3_water                  Sum3x3_water                  Mean5x5_water                    Sum5x5_water) ///
		(Water_body   Water_body_spatial_lag_1_mean  Water_body_spatial_lag_1_sum  Water_body_spatial_lag_2_mean    Water_body_spatial_lag_2_sum)
rename ///
        (Land_border    Mean3x3splag1_land              Sum3x3splag1_land              Mean5x5splag2_land                Sum5x5splag2_land) ///
		 (Land_border   Land_border_spatial_lag_1_mean  Land_border_spatial_lag_1_sum  Land_border_spatial_lag_2_mean    Land_border_spatial_lag_2_sum)
rename ///
		(Sea_border   Mean3x3splag1_sea              Sum3x3splag1_sea              Mean5x5splag2_sea                Sum5x5splag2_sea) ///
		(Sea_border   Sea_border_spatial_lag_1_mean  Sea_border_spatial_lag_1_sum  Sea_border_spatial_lag_2_mean    Sea_border_spatial_lag_2_sum)
		
rename ///
       (POPGHS1975          mean3x3_POPGHS1975             sum3x3_POPGHS1975           mean5x5_POPGHS1975            sum5x5_POPGHS1975) ///
	   (Population_1975      Population_1975_spatial_lag1_mean   Population_1975_spatial_lag1_sum Population_1975_spatial_lag2_mean  Population_1975_spatial_lag2_sum)
	   
rename ///
       (POPGHS1990          mean3x3_POPGHS1990           sum3x3_POPGHS1990         mean5x5_POPGHS1990          sum5x5_POPGHS1990) ///
	   (Population_1990     Population_1990_spatial_lag1_mean   Population_1990_spatial_lag1_sum Population_1990_spatial_lag2_mean  Population_1990_spatial_lag2_sum)
	   
rename ///
       (POPGHS2000          mean3x3_POPGHS2000           sum3x3_POPGHS2000                       mean5x5_POPGHS2000                 sum5x5_POPGHS2000) ///
	   (Population_2000     Population_2000_spatial_lag1_mean   Population_2000_spatial_lag1_sum Population_2000_spatial_lag2_mean  Population_2000_spatial_lag2_sum)
	   
rename ///
       (POPGHS2014          mean3x3_POPGHS2014                  sum3x3_POPGHS2014                mean5x5_POPGHS2014                 sum5x5_POPGHS2014) ///
	   (Population_2014     Population_2014_spatial_lag1_mean   Population_2014_spatial_lag1_sum Population_2014_spatial_lag2_mean  Population_2014_spatial_lag2_sum)
	   
rename  ///
        (BUILTGHS1975      mean3x3_BUILTGHS1975              sum3x3_BUILTGHS1975               mean5x5_BUILTGHS1975            sum5x5_BUILTGHS1975) ///
		(Builtup_1975      Builtup_1975_spatial_lag1_mean    Builtup_1975_spatial_lag1_sum     Builtup_1975_spatial_lag2_mean  Builtup_1975_spatial_lag2_sum)
		
rename  ///
        (BUILTGHS1990      mean3x3_BUILTGHS1990              sum3x3_BUILTGHS1990               mean5x5_BUILTGHS1990            sum5x5_BUILTGHS1990) ///
		(Builtup_1990      Builtup_1990_spatial_lag1_mean    Builtup_1990_spatial_lag1_sum     Builtup_1990_spatial_lag2_mean  Builtup_1990_spatial_lag2_sum)
		
rename  ///
        (BUILTGHS2000      mean3x3_BUILTGHS2000              sum3x3_BUILTGHS2000               mean5x5_BUILTGHS2000            sum5x5_BUILTGHS2000) ///
		(Builtup_2000      Builtup_2000_spatial_lag1_mean    Builtup_2000_spatial_lag1_sum     Builtup_2000_spatial_lag2_mean  Builtup_2000_spatial_lag2_sum)
		
rename  ///
        (BUILTGHS2014      mean3x3_BUILTGHS2014              sum3x3_BUILTGHS2014               mean5x5_BUILTGHS2014            sum5x5_BUILTGHS2014) ///
		(Builtup_2014      Builtup_2014_spatial_lag1_mean    Builtup_2014_spatial_lag1_sum     Builtup_2014_spatial_lag2_mean  Builtup_2014_spatial_lag2_sum)
 
*******************************
*PART 3: Labeling variables
******************************** 


		lab          var      x_coordinates                               "x coordinate in Ghana (mollweide projection) : GHS database"
        lab          var      y_coordinates                               "y coordinate in Ghana (mollweide projection) : GHS database"
		lab          var      Track_road_1976                               "Presence of track road in 1976"
		lab          var      Class2_road_1976                              "Presence of class2 road in 1976"
		lab          var      Class3_road_1976                              "Presence of class3 road in 1976"
		lab          var      Class1_road_1976                              "Presence of  class1 road in 1976"
		lab          var      Unknown_road_1976                             "Presence of unknown road in 1976"
		lab          var      Track_road_spatial_lag_1_mean                 "Mean of 1st spatial lag of track road in 1976"
		lab          var      Track_road_spatial_lag_1_sum                  "Sum of  1st spatial lag of track road in 1976"
		lab          var      Track_road_spatial_lag_2_mean                 "Mean of 2nd spatial lag of track road in 1976"
		lab          var      Track_road_spatial_lag_2_sum                  "Sum of  2nd spatial lag of track road in 1976"
		lab          var      Class2_road_spatial_lag_1_mean                "Mean of 1st spatial lag of class2 road in 1976"
		lab          var      Class2_road_spatial_lag_1_sum                 "Sum of  1st spatial lag of class2 road in 1976"
		lab          var      Class2_road_spatial_lag_2_mean                "Mean of 2nd spatial lag of class2 road in 1976"
		lab          var      Class2_road_spatial_lag_2_sum                 "Sum of  2nd spatial lag of class2 road in 1976"
		lab          var      Class3_road_spatial_lag_1_mean                "Mean of 1st spatial lag of class3 road in 1976"
		lab          var      Class3_road_spatial_lag_1_sum                 "Sum of  1st spatial lag of class3 road in 1976"
		lab          var      Class3_road_spatial_lag_2_mean                "Mean of 2nd spatial lag of class3 road in 1976"
		lab          var      Class3_road_spatial_lag_2_sum                 "Sum of 2nd spatial lag of class3 road in 1976"
		lab          var      Class1_road_spatial_lag_1_mean                "Mean of 1st spatial lag of class1 road in 1976"
		lab          var      Class1_road_spatial_lag_1_sum                 "Sum of  1st spatial lag of class1 road in 1976"
		lab          var      Class1_road_spatial_lag_2_mean                "Mean of 2nd spatial lag of class1 road in 1976"
		lab          var      Class1_road_spatial_lag_2_sum                 "Sum of 2nd spatial lag of class1 road in 1976"
		lab          var      Unknown_road_spatial_lag_1_mean               "Mean of 1st spatial lag of unknown road in 1976"
		lab          var      Unknown_road_spatial_lag_1_sum                "Sum of  1st spatial lag of unknown road in 1976"
		lab          var      Unknown_road_spatial_lag_2_mean               "Mean of 2nd spatial lag of unknown road in 1976"
		lab          var      Unknown_road_spatial_lag_2_sum                "Sum of 2nd spatial lag of  unknown road in 1976"
		lab          var      Water_body                                    "Presence of water body"
		lab          var      Water_body_spatial_lag_1_mean                 "Mean of 1st spatial lag of water body"
		lab          var      Water_body_spatial_lag_1_sum                  "Sum of 1st spatial lag of water body"
		lab          var      Water_body_spatial_lag_2_mean                 "Mean of 2nd spatial lag of water body"
		lab          var      Water_body_spatial_lag_2_sum                  "Sum of 2nd spatial lag of water body"
		lab          var      Land_border                                   "Presence of land border"
		lab          var      Land_border_spatial_lag_1_mean                "Mean of 1st spatial lag of land border"
		lab          var      Land_border_spatial_lag_1_sum                 "Sum of 1st spatial lag of  land border"
		lab          var      Land_border_spatial_lag_2_mean                "Mean of 2nd spatial lag of land border"
		lab          var      Land_border_spatial_lag_2_sum                 "Sum of  2nd spatial lag of land border" 
		lab          var      Sea_border                                    "Presence of sea border"
		lab          var      Sea_border_spatial_lag_1_mean                 "Mean of 1st spatial lag of sea border"
		lab          var      Sea_border_spatial_lag_1_sum                  "Sum of 1st  spatial lag of sea border"
		lab          var      Sea_border_spatial_lag_2_mean                 "Mean of 2nd spatial lag of sea border"
		lab          var      Sea_border_spatial_lag_2_sum                  "Sum of  2nd spatial lag of sea border"
		lab          var      Population_1975                               "Population in 1975"
		lab          var      Population_1990                               "Population in 1990"
		lab          var      Population_2000                               "Population in 2000"
		lab          var      Population_2014                               "Population in 2014"
		lab          var      Population_1975_spatial_lag1_mean             "Mean of 1st spatial lag of population in 1975"
		lab          var      Population_1975_spatial_lag1_sum              "Sum of 1st  spatial lag of population in 1975"
		lab          var      Population_1975_spatial_lag2_mean             "Mean of 2nd spatial lag of population in 1975"
		lab          var      Population_1975_spatial_lag2_sum              "Sum of  2nd spatial lag of population in 1975"
		lab          var      Population_1990_spatial_lag1_mean             "Mean of 1st spatial lag of population in 1990"
		lab          var      Population_1990_spatial_lag1_sum              "Sum of 1st  spatial lag of population in 1990"
		lab          var      Population_1990_spatial_lag2_mean             "Mean of 2nd spatial lag of population in 1990"
		lab          var      Population_1990_spatial_lag2_sum              "Sum of  2nd spatial lag of population in 1990"
		lab          var      Population_2000_spatial_lag1_mean             "Mean of 1st spatial lag of population in 2000"
		lab          var      Population_2000_spatial_lag1_sum              "Sum of 1st  spatial lag of population in 2000"
		lab          var      Population_2000_spatial_lag2_mean             "Mean of 2nd spatial lag of population in 2000"
		lab          var      Population_2000_spatial_lag2_sum              "Sum of  2nd spatial lag of population in 2000"
		lab          var      Population_2014_spatial_lag1_mean             "Mean of 1st spatial lag of population in 2014"
		lab          var      Population_2014_spatial_lag1_sum              "Sum of 1st  spatial lag of population in 2014"
		lab          var      Population_2014_spatial_lag2_mean             "Mean of 2nd spatial lag of population in 2014"
        lab          var      Population_2014_spatial_lag2_sum              "Sum of 1st  spatial lag of population in 2014"
		lab          var      Builtup_1975                                  "Builtup in 1975"
		lab          var      Builtup_1990                                  "Builtup in 1990"
		lab          var      Builtup_2000                                  "Builtup in 2000"
		lab          var      Builtup_2014                                  "Builtup in 2014"
		lab          var      Builtup_1975_spatial_lag1_mean                "Mean of 1st spatial lag of builtup in 1975"
		lab          var      Builtup_1975_spatial_lag1_sum                 "Sum of 1st spatial lag of  builtup in 1975"
		lab          var      Builtup_1975_spatial_lag2_mean                "Mean of 2nd spatial lag of builtup in 1975"
		lab          var      Builtup_1975_spatial_lag2_sum                  "Sum of 2nd spatial lag of builtup in 1975"
		lab          var      Builtup_1990_spatial_lag1_mean                "Mean of 1st spatial lag of builtup in 1990" 
		lab          var      Builtup_1990_spatial_lag1_sum                 "Sum of 1st spatial lag of  builtup in 1990"
		lab          var      Builtup_1990_spatial_lag2_mean                "Mean of 2nd spatial lag of builtup in 1990"
		lab          var      Builtup_1990_spatial_lag2_sum                 "Sum of 2nd spatial lag of  builtup in 1990"
		lab          var      Builtup_2000_spatial_lag1_mean                "Mean of 1st spatial lag of builtup in 2000"
		lab          var      Builtup_2000_spatial_lag1_sum                 "Sum of 1st spatial lag of  builtup in 2000"
		lab          var      Builtup_2000_spatial_lag2_mean                "Mean of 2nd spatial lag of builtup in 2000"
		lab          var      Builtup_2000_spatial_lag2_sum                 "Sum of 2nd spatial lag of  builtup in 2000"
		lab          var      Builtup_2014_spatial_lag1_mean                "Mean of 1st spatial lag of builtup in 2014"
		lab          var      Builtup_2014_spatial_lag1_sum                 "Sum of 1st spatial lag of  builtup in 2014"
		lab          var      Builtup_2014_spatial_lag2_mean                "Mean of 2nd spatial lag of builtup in 2014"
        lab          var      Builtup_2014_spatial_lag2_sum                 "Sum of 2nd spatial lag of  builtup in 2014"
		
*******************************
*PART 4: Value labels for categorical variables
******************************** 		

 lab def cat 0 "No"  1 "Yes", replace
		foreach var of varlist  Track_road_1976 Class2_road_1976 Class3_road_1976 Class1_road_1976 Unknown_road_1976 Water_body Land_border Sea_border  ///
		                        {
								  replace `var'=0 if `var'==.
		                          lab val `var' cat
								 }
        foreach var of varlist  Track_road_spatial_lag_1_mean Track_road_spatial_lag_1_sum Track_road_spatial_lag_2_mean Track_road_spatial_lag_2_sum /// 
		                        Class2_road_spatial_lag_1_mean Class2_road_spatial_lag_1_sum Class2_road_spatial_lag_2_mean Class2_road_spatial_lag_2_sum  ///
		                        Class3_road_spatial_lag_1_mean Class3_road_spatial_lag_1_sum Class3_road_spatial_lag_2_mean Class3_road_spatial_lag_2_sum  ///
		                        Class1_road_spatial_lag_1_mean Class1_road_spatial_lag_1_sum Class1_road_spatial_lag_2_mean Class1_road_spatial_lag_2_sum  ///
								Unknown_road_spatial_lag_1_mean Unknown_road_spatial_lag_1_sum Unknown_road_spatial_lag_2_mean Unknown_road_spatial_lag_2_sum ///
		                        Water_body_spatial_lag_1_mean  Water_body_spatial_lag_1_sum Water_body_spatial_lag_2_mean Water_body_spatial_lag_2_sum  ///
		                        Land_border_spatial_lag_1_mean Land_border_spatial_lag_1_sum Land_border_spatial_lag_2_mean Land_border_spatial_lag_2_sum ///
		                        Sea_border_spatial_lag_1_mean Sea_border_spatial_lag_1_sum Sea_border_spatial_lag_2_mean Sea_border_spatial_lag_2_sum ///
								{
								replace `var'=0 if `var'==.
								}
								
	lab def missing  . "Missing"
	
	        foreach var of varlist  Population_1975 Population_1990 Population_2000 Population_2014 Builtup_1975 Builtup_1990 Builtup_2000 Builtup_2014 ///
			                        Population_1975_spatial_lag1_mean Population_1975_spatial_lag1_sum Population_1975_spatial_lag2_mean Population_1975_spatial_lag2_sum ///
									Population_1990_spatial_lag1_mean Population_1990_spatial_lag1_sum Population_1990_spatial_lag2_mean Population_1990_spatial_lag2_sum ///
									Population_2000_spatial_lag1_mean Population_2000_spatial_lag1_sum Population_2000_spatial_lag2_mean Population_2000_spatial_lag2_sum ///
									Population_2014_spatial_lag1_mean Population_2014_spatial_lag1_sum Population_2014_spatial_lag2_mean Population_2014_spatial_lag2_sum ///
			                        Builtup_1975_spatial_lag1_mean Builtup_1975_spatial_lag1_sum Builtup_1975_spatial_lag2_mean Builtup_1975_spatial_lag2_sum ///
									Builtup_1990_spatial_lag1_mean Builtup_1990_spatial_lag1_sum Builtup_1990_spatial_lag2_mean Builtup_1990_spatial_lag2_sum ///
									Builtup_2000_spatial_lag1_mean Builtup_2000_spatial_lag1_sum Builtup_2000_spatial_lag2_mean Builtup_2000_spatial_lag2_sum ///
									Builtup_2014_spatial_lag1_mean Builtup_2014_spatial_lag1_sum Builtup_2014_spatial_lag2_mean Builtup_2014_spatial_lag2_sum ///
			                        {
									  lab val `var' missing
									 }
		            
	lab def  built_up    0 "no" 1 "yes"
	
	     foreach var of varlist  Builtup_1975 Builtup_1990 Builtup_2000 Builtup_2014 ///
		                         {
		                         lab    val  `var' built_up
								 }
*******************************
*PART 5: Save the data
******************************** 
	 save           "$datasets/cleaned_builtup.dta",replace

// Have a lovely day!	
