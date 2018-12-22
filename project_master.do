* ******************************************************************** *

       /*
       ** PURPOSE:      The purpose of the project is to 

       ** OUTLINE:      
                        PART 1: Set globals for dynamic file paths
                        
                        PART 2: Call the task specific master do-files 
                               that call all do-files needed for that 
                               tas. Do not include Part 0-2 in a task
                               specific master do-file


       ** IDS VAR:      list_ID_var_here         

       ** NOTES:

       ** WRITEN BY:    Varnitha Kurli
	   
	   

       ** Last date modified:   20th December, 2018
       */

*
   * Users
   * -----------

   *User Number:
   * You                     1    //Replace "You" with your name
   * Next User               2    //Assign a user number to each additional collaborator of this code

   *Set this value to the user currently using this file
   global user  2

   * Root folder globals
   * ---------------------

   if $user == 1 {
       global projectfolder  ""
   }

   if $user == 2 {
       global projectfolder "C:/Users/WB538005/WBG/Brian Blankespoor - Ghana_/"
   }
  
	
	
	 * Project folder globals
   * ---------------------
   
   global  datasets         "$projectfolder/Statistics Tables"
