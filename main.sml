(* geting all characters from file to list from filIO.sml *)
fun getclist (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (c, f') => loop (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loop ([], f))
    end

(* appending string in any file *)
fun append (filename: string, s) = 
    let val f =  TextIO.openAppend filename
    in  (TextIO.output (f, s); TextIO.closeOut f) 
    end

(* writing something in file *)
fun write (filename: string, s) = 
    let val f =  TextIO.openOut filename
    in  (TextIO.output (f, s); TextIO.closeOut f) 
    end

val output = "out.html"

local
    fun isalphabet(c) = if( (ord(c) < 48 andalso ord(c) > 38 ) orelse (ord(c) < 91 andalso ord(c) > 64 ) orelse (ord(c) < 123 andalso ord(c) > 96 ) ) then( true )
                        else (print(str(c)) ;false)

    fun fonter(prev , ahead , list , starter) = if(starter <> []) then ( if( prev <> chr(92) andalso ahead = #"*" andalso hd(list) = #"*" andalso hd(starter) <> "</strong>" ) then ( append (output , "<strong>") ; ( "</strong>" , true) )
                                                        else if( prev <> chr(92) andalso ahead <> #"*" andalso hd(list) = #"*" andalso hd(starter) <> "</em>" ) then (append(output , "<em>") ; ( "</em>" , true) )
                                                        else ( " " , false)  
                                                        )
                                            else if( prev <> chr(92) andalso ahead = #"*" andalso hd(list) = #"*"  ) then ( append (output , "<strong>") ; ( "</strong>" , true) )
                                            else if( prev <> chr(92) andalso ahead <> #"*" andalso hd(list) = #"*" ) then ( append(output , "<em>") ; ( "</em>" , true) )
                                            else ( " " , false)


    fun islist(prev , charlist , t ,t2) = if(prev = #"\n" andalso (ord(hd(charlist)) < 58  andalso ord(hd(charlist)) > 47  )  ) then (
                                        islist(prev , tl(charlist) , t+1 , t2)
                                    )else if(prev = #"\n" andalso hd(charlist) = #"." ) then (
                                        (true , t+1 , t2) 
                                    )else if(( prev = #"\n") andalso (hd(charlist) = #"\t" ) ) then (
                                        islist(prev , tl(charlist), t+1 , t2 + 1)
                                    )else (false , 0 , 0)

    fun isunorderedlist( prev ,charlist,t) = if( ( prev = #"\n") andalso hd(charlist) = #"-" andalso hd(tl(charlist)) = #" " ) then (
                                        (true , t)
                                    )else if(( prev = #"\n") andalso (hd(charlist) = #"\t") ) then (
                                        isunorderedlist(prev , tl(charlist), t+1)
                                    )
                                    else (false , 0)
    
    fun blkquote(prev,ahead , charlist, t) = if( prev = #"\n" andalso hd(charlist) = #">" andalso ahead = #">" ) then (
                                        blkquote(prev, hd(tl(tl(charlist))) , tl(charlist) ,t+1 )
                                    )else if( prev = #"\n" andalso hd(charlist) = #">" andalso ahead <> #">" ) then (
                                        (true ,t+1 )
                                    )else (
                                        (false , 0)
                                    )

    fun para(prev , list , ahead) = if(prev = #"\n" andalso hd(list) = #"\n" andalso (ahead <> #"#" )  
                                        andalso not (#1(islist(#"\n" , tl(list) , 1 , 0 ))) andalso not(#1(isunorderedlist(prev ,tl(list) , 0 ))) andalso not(#1(blkquote(prev, hd(tl(tl(list))) , tl(list) , 0 )))  ) then (append(output , "<p>" ); true )
                            else false

    fun headers(prev ,list, t ) = if( prev = chr(92) orelse hd(list) <> #"#" ) then (
                if( t > 0) then ( append(output , "<h"^Int.toString(t)^">" ); 
                        ("</h"^Int.toString(t)^">\n",t)  )
                else ("   " , 0) 
                )
        else headers(prev ,tl(list) , t+1)
    
    fun isinline( [] , data ) = (false ,0)
        | isinline(ele , data) = if(hd(ele) = "</strong>" andalso List.take(data,2) = [#"*" , #"*" ] ) then (true , 2)
                else if(hd(ele) = "</em>" andalso hd(data) = #"*" ) then (true , 1)
                else (false , 0)

    fun checker() = (append(output,"hey");false)

    fun checkspace(charlist , t) = if(t = 0) then true 
                else if( hd(charlist) = #"\t") then checkspace(tl(charlist) , t - 1)
                else false 

    fun isnumber(n) = if((ord(n) < 58  andalso ord(n) > 47  )) then true
                else false

    fun nextchar(c: char , [] , len) = ~1
        | nextchar(c: char ,charlist , len) = if( hd(charlist) = c ) then (len - length(charlist))
                                        else (nextchar(c ,tl(charlist) , len ))

    fun entites(c: char) =
         case c of #"<" => append(output , "&lt;")
        | #">" => append(output , "&gt;")
        | #"&" => append(output , "&amp;")   
        | #"\"" => append(output , "&quot;")   
        | #"'" => append(output , "&apos;")   
        | _ => append(output , "<h1> error </h1>")
        
in

    fun csv([c , c2]) = append(output , str(c)^"</td></tr> \n</TABLE></CENTER>")
        | csv(charlist) = if( hd(charlist) = #"|" ) then(
                            append(output , "</td> <td>");
                            csv(tl(charlist))
                        )else if(hd(charlist) = #"\n") then (
                            append(output , "</td></tr> \n <tr><td>");
                            csv(tl(charlist))
                        )
                        else(
                            append(output , str(hd(charlist)));
                            csv(tl(charlist))
                        )

    fun start(prev , [c1,c2] , ahead , starter ) = append(output , str(c1)^str(c2)^concat(rev(starter)) )
        | start(prev , charlist , ahead  , starter ) = 
        let
            val (headstater , headgap ) = headers(prev ,charlist , 0);
            
            val (fontstarter , isfont) = fonter( prev, ahead , charlist, starter);
            
            val (fontfinish , fontgap) = isinline(starter , charlist );

            val (islister , lisnumgap , tabnumol) = islist( prev , charlist , 1 , 0);

            val (isunlister , tabnum) = isunorderedlist(prev,charlist ,0);

            val (isblkquoter , numq ) = blkquote(prev, ahead , charlist , 0);
        in
            if(hd(charlist) = #"\n" andalso length(starter) <> 0 andalso length(charlist) > 3 ) then (
                if( (List.take(explode(hd(starter)) , 3) = [#"<" , #"/" , #"h"] orelse hd(starter) = "</p>" ) andalso ahead = #"\n" ) then (
                    append(output, hd(starter)^"\n" );
                    start(prev , charlist , ahead , tl(starter) )
                )else if( ahead <> #"\n"  andalso hd(starter) = "</li>" andalso 
                    ord(hd(explode(hd(tl(starter))))) > 47 andalso ord(hd(explode(hd(tl(starter))))) <58  ) then (
                    if( not (checkspace( tl(charlist) , valOf(Int.fromString(hd(tl(starter))))+1 ))  ) then (
                        append(output , hd(starter)^"\n");
                        start(prev , charlist , ahead , tl(starter) )
                    )else(
                        append(output ,str(hd(charlist)) );
                        start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter )
                    )
                )else if( not islister andalso isnumber(hd(explode(hd(starter)))) andalso hd(tl(starter)) = "</ol>" ) then (
                    if( not (checkspace( tl(charlist) , valOf(Int.fromString(hd(starter))) )) orelse not(isnumber(ahead))  ) then (
                        append(output , hd(tl(starter))^"\n");
                        start(prev , charlist , ahead , tl(tl(starter)) )
                    )else(
                        append(output ,str(hd(charlist)) );
                        start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter )
                    )
                )else if( not isunlister andalso isnumber(hd(explode(hd(starter)))) andalso hd(tl(starter)) = "</ul>" ) then (
                    if( not (checkspace( tl(charlist) , valOf(Int.fromString(hd(starter))) )) orelse ahead <> #"-" ) then (
                        append(output , hd(tl(starter))^"\n");
                        start(prev , charlist , ahead , tl(tl(starter)) )
                    )else(
                        append(output ,str(hd(charlist)) );
                        start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter )
                    )
                )else if( isnumber(hd(explode(hd(starter)))) andalso hd(tl(starter)) = "</blockquote>" ) then (
                    if(  not(#1(blkquote(hd(charlist) , hd(tl(tl(charlist))) ,tl(charlist) ,0  ))) orelse 
                        ( #2(blkquote(hd(charlist) , hd(tl(tl(charlist))) ,tl(charlist) ,0  )) < valOf(Int.fromString(hd(starter))) 
                        andalso #1(blkquote(hd(charlist) , hd(tl(tl(charlist))) ,tl(charlist) ,0  )) )  ) then (
                        append(output , hd(tl(starter))^"\n");
                        start(prev , charlist , ahead , tl(tl(starter)) )
                    )else(
                        append(output ,str(hd(charlist)) );
                        start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter )
                    )
                )else(
                    append(output ,str(hd(charlist)) );
                    start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter )
                )
            )else if( fontfinish ) then (
                append(output, hd(starter) );
                start(List.nth(charlist, fontgap -1 ) ,
                        List.drop(charlist , fontgap ) ,
                        List.nth(charlist , fontgap +1 ) , tl(starter)  )
            )else if( (headgap  > 0 ) ) then (
                start(List.nth(charlist, headgap -1 ) ,
                        List.drop(charlist , headgap ) ,
                        List.nth(charlist , headgap +1 ) , [headstater]@starter  )
            )else if( islister ) then (
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
            )else  if( isunlister ) then (
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
            )else if(para(prev , charlist , ahead) ) then (
                start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , ["</p>"]@starter  )
            )else if(isfont ) then (
                if(fontstarter = "</strong>") then
                    start(hd(tl(charlist)) , tl(tl(charlist)) , List.nth(charlist ,3) , [fontstarter]@starter  )
                else 
                    start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , [fontstarter]@starter  )    
            )else if( prev <> chr(92) (* " *) andalso hd(charlist) = #"_" ) then ( 
                if( hd(starter) <> "</u>" ) then (
                    append(output ,"<u>");
                    start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , ["</u>"]@starter  )
                )else(
                    append(output , hd(starter));
                    start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , tl(starter)  )
                )
            )else if( isblkquoter ) then (
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
            )else if( hd(charlist) = chr(92) andalso (ahead = #"*" orelse ahead = #"#" orelse ahead = #"-" orelse (ahead = #">" andalso prev = #"\n" ) ) ) then (
                start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter  ) 
            )else if(List.take(charlist , 3) = [#"-", #"-", #"-"] andalso prev <> chr(92) ) then (
                append(output , "<hr/>");
                start(List.nth(charlist , 2) , List.drop(charlist, 3) , List.nth(charlist , 4) , starter )
            )else if(List.take(charlist , 3) = [#"<", #"h", #"t"]  andalso prev <> chr(92) ) then (
                append(output , "<a href =\""^implode(List.take(tl(charlist) , nextchar(#">" ,
                charlist, length(charlist))-1 ) )^"\">"^implode(List.take(tl(charlist) , nextchar(#">" , charlist, length(charlist)) -1 ))^"</a>" );
                start(List.nth(charlist , nextchar(#">" , charlist, length(charlist))) , List.drop(charlist, nextchar(#">" , charlist, length(charlist))+1) ,
                List.nth(charlist , nextchar(#">" , charlist, length(charlist)) + 2) , starter )
            )else if( hd(charlist) = chr(92) andalso (ahead = #"<" orelse ahead = #">" orelse ahead = #"&" orelse ahead = #"\"" orelse ahead = #"'" ) ) then (
                entites(ahead);
                start(hd(tl(charlist)) , tl(tl(charlist)) , List.nth(charlist ,3) , starter  ) 
            )else ( 
                append(output, str(hd(charlist)) ) ;
                start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter  ) 
            )
        end
end

(*  main function *)
fun main(filename) = ( write(output," ");                                                (* clearing the file before creating it *)  
            if( List.take(rev(explode(filename)),3) = [#"v" , #"s" , #"c" ] ) then (
                append(output , "<CENTER><TABLE border=\"1\">\n <tr><td>");
                csv(getclist(filename))
            )else(
                start( #"\n" , [#"\n"]@getclist(filename), hd(getclist(filename)) , []  ) 
            )
        ) 