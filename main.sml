(* geting all characters from file to list from filIO.sml *)
fun getclist (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (c, f') => loop (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loop ([], f))
    end

(* function for appending string in any file *)
fun append (filename: string, s) = 
    let val f =  TextIO.openAppend filename
    in  (TextIO.output (f, s); TextIO.closeOut f) 
    end

(* function for writing something in file *)
fun write (filename: string, s) = 
    let val f =  TextIO.openOut filename
    in  (TextIO.output (f, s); TextIO.closeOut f) 
    end

(* output fil name *)
val output = "out.html"
(* seprator for csv file format and can be easily changed from here *)
val seprator = #"|"

(* main local in end block start from here *)
local
        (* function which checks for charachter to be small or capital alphabet and number *)
    fun isalphabet(c) = if( (ord(c) < 48 andalso ord(c) > 38 ) orelse (ord(c) < 91 andalso ord(c) > 64 )
                     orelse (ord(c) < 123 andalso ord(c) > 96 ) ) then( true )
                        else (print(str(c)) ;false)

        (* function which handles bold and italic font style *)
    fun fonter(prev , ahead , list , starter) =
             if(starter <> []) then ( if( prev <> chr(92) andalso ahead = #"*" andalso hd(list) = #"*" andalso hd(starter) <> "</strong>" ) then ( append (output , "<strong>") ; ( "</strong>" , true) )
                        else if( prev <> chr(92) andalso ahead <> #"*" andalso hd(list) = #"*" andalso hd(starter) <> "</em>" ) then (append(output , "<em>") ; ( "</em>" , true) )
                        else ( " " , false)  
                        )
            else if( prev <> chr(92) andalso ahead = #"*" andalso hd(list) = #"*"  ) then ( append (output , "<strong>") ; ( "</strong>" , true) )
            else if( prev <> chr(92) andalso ahead <> #"*" andalso hd(list) = #"*" ) then ( append(output , "<em>") ; ( "</em>" , true) )
            else ( " " , false)


        (* function which checks is it a can be ordered list item and returns the true or false with number's digit length  *)
    fun islist(prev , charlist , t ,t2) =
        if(prev = #"\n" andalso (ord(hd(charlist)) < 58  andalso ord(hd(charlist)) > 47  )  ) then (
            islist(prev , tl(charlist) , t+1 , t2)
        )else if(prev = #"\n" andalso hd(charlist) = #"." ) then (
            (true , t+1 , t2) 
        )else if(( prev = #"\n") andalso (hd(charlist) = #"\t" ) ) then (
            islist(prev , tl(charlist), t+1 , t2 + 1)
        )else (false , 0 , 0)

        (* function which checks for the unordered list item and on the basis return true or false *)
    fun isunorderedlist( prev ,charlist,t) =
            if( ( prev = #"\n") andalso hd(charlist) = #"-" andalso hd(tl(charlist)) = #" " ) then (
                (true , t)
            )else if(( prev = #"\n") andalso (hd(charlist) = #"\t") ) then (
                isunorderedlist(prev , tl(charlist), t+1)
            )
            else (false , 0)
    
        (* function checks for blockquote and returns true ,if it is with it's depth, otherwise return false *)
    fun blkquote(prev,ahead , charlist, t) = 
            if( prev = #"\n" andalso hd(charlist) = #">" andalso ahead = #">" ) then (
                blkquote(prev, hd(tl(tl(charlist))) , tl(charlist) ,t+1 )
            )else if( prev = #"\n" andalso hd(charlist) = #">" andalso ahead <> #">" ) then (
                (true ,t+1 )
            )else (
                (false , 0)
            )

        (* checks for if it can be paragraph. paragraph starts and ends with one blank line  *)
    fun para(prev , list , ahead) = if(prev = #"\n" andalso hd(list) = #"\n" andalso (ahead <> #"#" )  
                                        andalso not (#1(islist(#"\n" , tl(list) , 1 , 0 ))) andalso not(#1(isunorderedlist(prev ,tl(list) , 0 ))) andalso not(#1(blkquote(prev, hd(tl(tl(list))) , tl(list) , 0 )))  ) then (append(output , "<p>" ); true )
                            else false

        (* checks for all 6 types of headers and returns size of header in number *)
    fun headers(prev ,list, t ) = 
        if( prev = chr(92) orelse hd(list) <> #"#" ) then (
                if( t > 0) then ( append(output , "<h"^Int.toString(t)^">" ); 
                        ("</h"^Int.toString(t)^">\n",t)  )
                else ("   " , 0) 
                )
        else headers(prev ,tl(list) , t+1)
    
        (* checks for if any fonter (bold or italic ) going to finish and returns true or falss *)
    fun isinline( [] , data ) = (false ,0)
        | isinline(ele , data) = if(hd(ele) = "</strong>" andalso List.take(data,2) = [#"*" , #"*" ] ) then (true , 2)
                else if(hd(ele) = "</em>" andalso hd(data) = #"*" ) then (true , 1)
                else (false , 0)

        (* this function is little special I made it for debugging purpose which can be put anywhere *)
    fun checker() = (append(output,"hey");false)

        (* checkspace function checks for number of indents in the form of tabs (number of tabs ) and returns that *)
    fun checkspace(charlist , t) = if(t = 0) then true 
                else if( hd(charlist) = #"\t") then checkspace(tl(charlist) , t - 1)
                else false 

        (* this function checks for character to be number and return true or false *)
    fun isnumber(n) = if((ord(n) < 58  andalso ord(n) > 47  )) then true
                else false

        (* nextchar function finds nect position of character c in charlist and returns its lcoation *)
    fun nextchar(c: char , [] , len) = ~1
        | nextchar(c: char ,charlist , len) = if( hd(charlist) = c ) then (len - length(charlist))
                                        else (nextchar(c ,tl(charlist) , len ))

        (* function for html entites like "<" and "&" *)
    fun entites(c: char) =
         case c of #"<" => append(output , "&lt;")
        | #">" => append(output , "&gt;")
        | #"&" => append(output , "&amp;")   
        | #"\"" => append(output , "&quot;")   
        | #"'" => append(output , "&apos;")   
        | _ => append(output , "<h3> error </h3>")
        
in
        (* function for csv file format input and make table of it in html  *)
    fun csv([c , c2]) = append(output , str(c)^"</td></tr> \n</TABLE></CENTER>")    (* ending table *)
        | csv(charlist) = if( hd(charlist) = seprator ) then(       (* creating new data element if find sperator of csv file *)
                            append(output , "</td> <td>");
                            csv(tl(charlist))
                        )else if(hd(charlist) = #"\n") then (       (* creating new row of table if finds a new line charater *)
                            append(output , "</td></tr> \n <tr><td>");
                            csv(tl(charlist))
                        )
                        else(       (* no newline and no seprartor means data . printing it asitis in html *)
                            append(output , str(hd(charlist)));
                            csv(tl(charlist))
                        )

        (* this function is which checks for all types of possiblites for lists, links , headers, font styles  .... etc  *)
        (* some important details related to function down here *)
        (* ahead --> char which have last character from charlist *)
        (* charlist --> list of all characters from input file *)
        (* ahead --> ahead is char which is next to 2nd element of charlist its just for ease *)
        (* starter is a stack which keeps memory of all opened elements which have not been closed till now. *)
    fun start(prev , [c1,c2] , ahead , starter ) = append(output , str(c1)^str(c2)^concat(rev(starter)) )     
        | start(prev , charlist , ahead  , starter ) = 
        let
            val (headstater , headgap ) = headers(prev ,charlist , 0);      (* can be heading at every starting of newline *)
            
            val (fontstarter , isfont) = fonter( prev, ahead , charlist, starter);  
        
            val (fontfinish , fontgap) = isinline(starter , charlist );

            val (islister , lisnumgap , tabnumol) = islist( prev , charlist , 1 , 0);

            val (isunlister , tabnum) = isunorderedlist(prev,charlist ,0);

            val (isblkquoter , numq ) = blkquote(prev, ahead , charlist , 0)

            (* overall upper functions which gets details at every char that if it can be syntax of markdown *)
        in
            if(hd(charlist) = #"\n" andalso length(starter) <> 0 andalso length(charlist) > 3 ) then (                                                                  (*all html elements closing happened in this if condition *)
                if( (List.take(explode(hd(starter)) , 3) = [#"<" , #"/" , #"h"] orelse hd(starter) = "</p>" ) andalso ahead = #"\n" ) then (    (*headers closing condition*)
                    append(output, hd(starter)^"\n" );
                    start(prev , charlist , ahead , tl(starter) )
                )else if( ahead <> #"\n"  andalso hd(starter) = "</li>" andalso 
                    ord(hd(explode(hd(tl(starter))))) > 47 andalso ord(hd(explode(hd(tl(starter))))) <58  ) then (                               (*list elements closing condition*)
                    if( not (checkspace( tl(charlist) , valOf(Int.fromString(hd(tl(starter))))+1 ))  ) then (
                        append(output , hd(starter)^"\n");
                        start(prev , charlist , ahead , tl(starter) )
                    )else(
                        append(output ,str(hd(charlist)) );
                        start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter )
                    )
                )else if( not islister andalso isnumber(hd(explode(hd(starter)))) andalso hd(tl(starter)) = "</ol>" ) then (                        (*<ol> tag closing condition *)
                    if( not (checkspace( tl(charlist) , valOf(Int.fromString(hd(starter))) )) orelse not(isnumber(ahead))  ) then (
                        append(output , hd(tl(starter))^"\n");
                        start(prev , charlist , ahead , tl(tl(starter)) )
                    )else(
                        append(output ,str(hd(charlist)) );
                        start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter )
                    )
                )else if( not isunlister andalso isnumber(hd(explode(hd(starter)))) andalso hd(tl(starter)) = "</ul>" ) then (                      (*<ul> tag closing condition *)
                    if( not (checkspace( tl(charlist) , valOf(Int.fromString(hd(starter))) )) orelse ahead <> #"-" ) then (
                        append(output , hd(tl(starter))^"\n");
                        start(prev , charlist , ahead , tl(tl(starter)) )
                    )else(
                        append(output ,str(hd(charlist)) );
                        start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter )
                    )
                )else if( isnumber(hd(explode(hd(starter)))) andalso hd(tl(starter)) = "</blockquote>" ) then (                                     (* blockquote closing condition *)
                    if(  not(#1(blkquote(hd(charlist) , hd(tl(tl(charlist))) ,tl(charlist) ,0  ))) orelse 
                        ( #2(blkquote(hd(charlist) , hd(tl(tl(charlist))) ,tl(charlist) ,0  )) < valOf(Int.fromString(hd(starter))) 
                        andalso #1(blkquote(hd(charlist) , hd(tl(tl(charlist))) ,tl(charlist) ,0  )) )  ) then (
                        append(output , hd(tl(starter))^"\n");
                        start(prev , charlist , ahead , tl(tl(starter)) )
                    )else(
                        append(output ,str(hd(charlist)) );
                        start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter )
                    )
                )else(                                                                                                                                  (*not matched any condition of different closing tags*)
                    append(output ,str(hd(charlist)) );
                    start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter )
                )
            )else if( fontfinish ) then (                                   (* font( bolds and italics) tags finishes in this if block *)
                append(output, hd(starter) );
                start(List.nth(charlist, fontgap -1 ) ,
                        List.drop(charlist , fontgap ) ,
                        List.nth(charlist , fontgap +1 ) , tl(starter)  )
            )else if( (headgap  > 0 ) ) then (                                  (* heading tag can be started from only this if block *)
                start(List.nth(charlist, headgap -1 ) ,
                        List.drop(charlist , headgap ) ,
                        List.nth(charlist , headgap +1 ) , [headstater]@starter  )
            )else if( islister ) then (                                               (* if block for ordered list (list or list item start from here) *)
                if( starter <> []) then(
                    if( not(isnumber(hd(explode(hd(starter))))) ) then(
                            append(output , "<ol><li>");
                            start(List.nth(charlist ,lisnumgap-1 ) , List.drop(charlist ,lisnumgap-1 ) , List.nth(charlist ,2 + lisnumgap ) , ["</li>"]@["00"^Int.toString(tabnumol)]@["</ol>"]@starter  )
                    )else(
                        append(output , "<li>");
                            start(List.nth(charlist ,lisnumgap-1 ) , List.drop(charlist ,lisnumgap-1 ) , List.nth(charlist ,2 + lisnumgap ) , ["</li>"]@starter  )
                    )
                )else (
                    append(output , "<ol><li>");
                    start(List.nth(charlist ,lisnumgap-1 ) , List.drop(charlist ,lisnumgap-1 ) , List.nth(charlist ,2 + lisnumgap ) , ["</li>"]@["00"^Int.toString(tabnumol)]@["</ol>"]@starter  )
                )
            )else  if( isunlister ) then (                                          (* if block for unordered list (list or list item start from here) *)
                if( starter <> [] ) then(
                    if( not(isnumber(hd(explode(hd(starter)))))  ) then(
                            append(output , "<ul><li>");
                            start(List.nth(charlist ,1 ) , List.drop(charlist ,2 ) , List.nth(charlist ,3 ) , ["</li>"]@["00"^Int.toString(tabnum)]@["</ul>"]@starter  )
                    )else(
                        append(output , "<li>");
                            start(List.nth(charlist ,1 ) , List.drop(charlist ,2 ) , List.nth(charlist ,3 ) , ["</li>"]@starter  )
                    )
                )else (
                    append(output , "<ul><li>");
                    start(List.nth(charlist ,0 ) , List.drop(charlist ,1 ) , List.nth(charlist ,2 ) , ["</li>"]@["00"^Int.toString(tabnum)]@["</ul>"]@starter  )
                )
            )else if(para(prev , charlist , ahead) ) then (                                 (* if block for paragraph *)
                start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , ["</p>"]@starter  )
            )else if(isfont ) then (                                                            (* fonts(bold and italics) elements start from this if block *)
                if(fontstarter = "</strong>") then
                    start(hd(tl(charlist)) , tl(tl(charlist)) , List.nth(charlist ,3) , [fontstarter]@starter  )
                else 
                    start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , [fontstarter]@starter  )    
            )else if( prev <> chr(92) (* " *) andalso hd(charlist) = #"_" ) then (                   (* underlined tag starts from this if block *)
                if( hd(starter) <> "</u>" ) then (
                    append(output ,"<u>");
                    start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , ["</u>"]@starter  )
                )else(
                    append(output , hd(starter));
                    start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , tl(starter)  )
                )
            )else if( isblkquoter ) then (                                              (* if block which handles or starts the blockquote elements*)
                if( starter <> [] ) then(
                    if( isnumber(hd(explode(hd(starter)))) ) then(
                        if( numq <> valOf(Int.fromString(hd(starter))) ) then (
                            append(output , "<blockquote>");
                            start(List.nth(charlist , numq - 1 ) , List.drop(charlist , numq ) , List.nth(charlist , numq + 1 ) , ["000"^Int.toString(numq)]@["</blockquote>"]@starter  )
                        )else(
                            start(List.nth(charlist ,1 ) , List.drop(charlist ,2 ) , List.nth(charlist ,3 ) , starter  )
                        )
                    )else(
                        start(List.nth(charlist ,1 ) , List.drop(charlist ,2 ) , List.nth(charlist ,3 ) , starter  )
                    )
                )else (
                    append(output , "<blockquote>");
                    start(List.nth(charlist ,numq-1 ) , List.drop(charlist , numq ) , List.nth(charlist , numq + 1 ) , ["000"^Int.toString(numq)]@["</blockquote>"]@starter  )
                )
            )else if( hd(charlist) = chr(92) andalso (ahead = #"*" orelse ahead = #"#" orelse ahead = #"-" orelse (ahead = #">" andalso prev = #"\n" ) ) ) then (               (* mentined characters which have backslash in back will not be treated for syntax of markdown *)
                start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter  ) 
            )else if(List.take(charlist , 3) = [#"-", #"-", #"-"] andalso prev <> chr(92) ) then (                              (* if block for handling (---)horizontal line which are not backslashed *)
                append(output , "<hr/>");
                start(List.nth(charlist , 2) , List.drop(charlist, 3) , List.nth(charlist , 4) , starter )
            )else if(List.take(charlist , 3) = [#"<", #"h", #"t"]  andalso prev <> chr(92) ) then (                              (* if block for handling automatic links( like <https://something/something>) *)
                append(output , "<a href =\""^implode(List.take(tl(charlist) , nextchar(#">" ,
                charlist, length(charlist))-1 ) )^"\">"^implode(List.take(tl(charlist) , nextchar(#">" , charlist, length(charlist)) -1 ))^"</a>" );
                start(List.nth(charlist , nextchar(#">" , charlist, length(charlist))) , List.drop(charlist, nextchar(#">" , charlist, length(charlist))+1) ,
                List.nth(charlist , nextchar(#">" , charlist, length(charlist)) + 2) , starter )
            )else if( hd(charlist) = chr(92) andalso (ahead = #"<" orelse ahead = #">" orelse ahead = #"&" orelse ahead = #"\"" orelse ahead = #"'" ) ) then (          (* html entities handling from here *)
                entites(ahead);
                start(hd(tl(charlist)) , tl(tl(charlist)) , List.nth(charlist ,3) , starter  ) 
            )else (                                                                 (* other all normal charcters just got printed normally *)
                append(output, str(hd(charlist)) ) ;
                start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter  ) 
            )
        end
end

            (*  main function starts just after getting input from user *)
fun main(filename) = ( write(output," ");                                                       (* clearing the file before creating it *)  
            if( List.take(rev(explode(filename)),3) = [#"v" , #"s" , #"c" ] ) then (            (* checking file for csv format (.csv) *)
                append(output , "<CENTER><TABLE border=\"1\">\n <tr><td>");                 (* starting table and calling csv function to do remaning work *)
                csv(getclist(filename))
            )else(
                (* start working on input file if it not csv format (not .csv file) *)
                start( #"\n" , [#"\n"]@getclist(filename), hd(getclist(filename)) , []  ) 
            )
        ) 