TUNINGS             		equ     11
pTuningsIndex				dd	TUNINGS		dup(0)

strTuningStdGuitar          db  "Guitar - Standard",0
                            dd  6
                            db  "E",0,"A",0,"D",0,"G",0,"B",0,"E",0
                            dd  0007F2890h	; Output the E note
	                        dd  0007F2D90h	; Output the A note
	                        dd  0007F3290h	; Output the D note
	                        dd  0007F3790h	; Output the G note
    	                    dd  0007F3B90h	; Output the B note
                            dd  0007F4090h	; Output the E note
                            dd  OFFSET strTuningDropd
                                         
strTuningDropd              db  "Guitar - Dropped D",0
                            dd  6
                            db  "D",0,"A",0,"D",0,"G",0,"B",0,"E",0 
                            dd  0007F2690h	; Output the D note
	                        dd  0007F2D90h	; Output the A note
	                        dd  0007F3290h	; Output the D note
	                        dd  0007F3790h	; Output the G note
    	                    dd  0007F3B90h	; Output the B note
                            dd  0007F4090h	; Output the E note
                            dd  OFFSET strTuningDaddad                                 

strTuningDaddad             db  "Guitar - DADDAD",0
                            dd  6
                            db  "D",0,"A",0,"D",0,"D",0,"A",0,"D",0
                            dd  0007F2690h	; Output the D note
	                        dd  0007F2D90h	; Output the A note
	                        dd  0007F3290h	; Output the D note
	                        dd  0007F3290h	; Output the D note
    	                    dd  0007F3990h	; Output the A note
                            dd  0007F3E90h	; Output the D note
                            dd  OFFSET strTuningDadgad                            
                            
strTuningDadgad             db  "Guitar - DADGAD",0
                            dd  6
                            db  "D",0,"A",0,"D",0,"G",0,"A",0,"D",0
                            dd  0007F2690h	; Output the D note
	                        dd  0007F2D90h	; Output the A note
	                        dd  0007F3290h	; Output the D note
	                        dd  0007F3790h	; Output the G note
    	                    dd  0007F3990h	; Output the A note
                            dd  0007F3E90h	; Output the D note
                            dd  OFFSET strTuningOpenG                            
                            
strTuningOpenG             	db  "Guitar - Open G",0
       	                    dd  6
	                        db  "D",0,"G",0,"D",0,"G",0,"B",0,"D",0
                            dd  0007F2690h	; Output the D note
	                        dd  0007F2B90h	; Output the G note
	                        dd  0007F3290h	; Output the D note
	                        dd  0007F3790h	; Output the G note
    	                    dd  0007F3B90h	; Output the B note
                            dd  0007F3E90h	; Output the D note
                            dd  OFFSET strTuningStdGuitar7                            
                            
strTuningStdGuitar7         db  "Guitar - 7 String",0
                            dd  7
                            db  "B",0,"E",0,"A",0,"D",0,"G",0,"B",0,"E",0
                            dd  0007F2390h	; Output the B note  
                            dd  0007F2890h	; Output the E note
	                        dd  0007F2D90h	; Output the A note
	                        dd  0007F3290h	; Output the D note
	                        dd  0007F3790h	; Output the G note
    	                    dd  0007F3B90h	; Output the B note
                            dd  0007F4090h	; Output the E note
                            dd  OFFSET strTuningStdBanjo5                                                                                    

strTuningStdBanjo5          db  "Banjo - Standard 5-String",0
                            dd  5
                            db  "G",0,"D",0,"G",0,"B",0,"D",0
                            dd  0007F3790h	; Output the G note
	                        dd  0007F3290h	; Output the D note
	                        dd  0007F3790h	; Output the G note
	                        dd  0007F3B90h	; Output the B note
    	                    dd  0007F3290h	; Output the D note
                            dd  OFFSET strTuningStdBanjo4    	                    
    	                    
strTuningStdBanjo4			db	"Banjo - Standard 4-String",0
	                        dd	4
		                    db	"C",0,"G",0,"D",0,"A",0
                            dd  0007F3090h	; Output the C note
	                        dd  0007F3790h	; Output the G note
	                        dd  0007F3E90h	; Output the D note
	                        dd  0007F3A90h	; Output the A note
                            dd  OFFSET strTuningElectricBass4	                          
	                        
strTuningElectricBass4			db	"Electric Bass - 4-String",0
	                            dd	4
		                        db	"E",0,"A",0,"D",0,"G",0
                                dd  0007F1C90h	; Output the E note
	                        	dd  0007F2190h	; Output the A note
	                        	dd  0007F2690h	; Output the D note
	                        	dd  0007F2B90h	; Output the G note
                                dd  OFFSET strTuningElectricBass5	                        	

strTuningElectricBass5			db	"Electric Bass - 5-String",0
	                            dd	5
		                        db	"B",0,"E",0,"A",0,"D",0,"G",0
                                dd  0007F1790h	; Output the B note
								dd  0007F1C90h	; Output the E note
	                        	dd  0007F2190h	; Output the A note
	                        	dd  0007F2690h	; Output the D note
	                        	dd  0007F2B90h	; Output the G note
                                dd  OFFSET strTuningElectricBass6	                        	
	                        	
strTuningElectricBass6			db	"Electric Bass - 6-String",0
                            	dd	6
		                        db	"B",0,"E",0,"A",0,"D",0,"G",0,"C",0
                                dd  0007F1790h	; Output the B note
								dd  0007F1C90h	; Output the E note
	                        	dd  0007F2190h	; Output the A note
	                        	dd  0007F2690h	; Output the D note
	                        	dd  0007F2B90h	; Output the G note
	                        	dd  0007F3090h	; Output the C note
                                dd  OFFSET strTuningStdGuitar