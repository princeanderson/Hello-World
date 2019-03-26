doubleMe x = x + x 
doubleUs x y = x*2 + y*2
--doubleMe 4+doubleUs 4 5

doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2 