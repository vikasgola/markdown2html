use "fileIO.sml";

open FileIO;

val output = "out.html";

 fun index([],pos,c1) = pos-1
    | index(s1,pos,c1) = if(hd(s1) <> c1 ) then index(tl(s1), pos+1 , c1 )
                        else pos-1

fun strongindex([],pos,c1,c2) = pos-1
    | strongindex(s1,pos,c1,c2) = if( List.take(s1,2) <> [c1 , c2] ) then strongindex(tl(s1), pos+1 ,c1, c2 )
                        else pos

fun checkaster(data) = if(hd(data) = #"*") then ( append(output,"<em>"^implode(List.take(List.drop(data,1),index(List.drop(data,1),1, #"*" ) ))^"</em>"); (true ,index(List.drop(data,1),1, #"*")+1 )  )
                            else (false , 0)

fun checkbold(data) = if(List.take(data,2) = [#"*",#"*"]) then ( append(output,"<strong>"^implode(List.take(List.drop(data,2),strongindex(List.drop(data,2),1, #"*", #"*" ) - 1))^"</strong>"); (true ,strongindex(List.drop(data,2),1, #"*", #"*")+2 )  )
                            else (let
                                val (bo , pos) = checkaster(data);
                            in
                                if(bo) then (true , pos)
                                else (false , pos)
                            end )

fun checkheadings(data) = if(length(data) <= 5 ) then ()
                            else if(List.take(data,6) = [#"#",#"#",#"#",#"#",#"#",#"#"]) then (append(output,"<h6>"^implode(List.take(List.drop(data,6),index(List.drop(data,6),1 , #"\n" )))^"</h6>\n");    checkheadings(List.drop(data,index(data,1, #"\n")))  )
                            else if(List.take(data,5) = [#"#",#"#",#"#",#"#",#"#"]) then (append(output,"<h5>"^implode(List.take(List.drop(data,5),index(List.drop(data,5),1, #"\n")))^"</h5>\n");     checkheadings(List.drop(data,index(data,1, #"\n")))    )
                            else if(List.take(data,4) = [#"#",#"#",#"#",#"#"]) then (append(output,"<h4>"^implode(List.take(List.drop(data,4),index(List.drop(data,4),1, #"\n")))^"</h4>\n");       checkheadings(List.drop(data,index(data,1, #"\n")))   )
                            else if(List.take(data,3) = [#"#",#"#",#"#"]) then (append(output,"<h3>"^implode(List.take(List.drop(data,3),index(List.drop(data,3),1, #"\n")))^"</h3>\n");     checkheadings(List.drop(data,index(data,1, #"\n")))  )
                            else if(List.take(data,2) = [#"#",#"#"]) then (append(output,"<h2>"^implode(List.take(List.drop(data,2),index(List.drop(data,2),1, #"\n")))^"</h2>\n");      checkheadings(List.drop(data,index(data,1, #"\n")))    )
                            else if(List.take(data,1) = [#"#"]) then (append(output,"<h1>"^implode(List.take(List.drop(data,1),index(List.drop(data,1),1, #"\n")))^"</h1>\n");      checkheadings(List.drop(data,index(data,1, #"\n")))   )
                            else ( let
                                    val (bo , pos) = checkbold(data);
                                in
                                    if(not bo) then (append(output , str(hd(data)) );checkheadings(List.drop(data,pos+1)) )  
                                    else checkheadings(List.drop(data,pos+1))
                                end
                                )
    fun startold(prev , [c1,c2] , ahead , starter) = append(output , str(c1)^str(c2)^starter )
        | startold(prev , charlist , ahead  , starter) = ( if(#1(isinline(starter , charlist )) ) then ( append(output , starter);
                                                                                            start(List.nth(charlist , #2(isinline(starter , charlist))-1 ),
                                                                                                tl(List.drop(charlist,(#2(isinline(starter , charlist))) ) ) ,
                                                                                                List.nth(charlist , (#2(isinline(starter , charlist))) + 1) , "    " )
                                                                                            )
                 (*inline items started *)  else if(prev = #"\n" andalso isalphabet(ahead ) andalso hd(charlist) = #"\n"  ) then ( append(output , "<p>");                  (* inline ended*)
                                                                                                         start(hd(charlist) , tl(charlist) , List.nth(charlist , 2) , "</p>\n" )  )
                                            else if( prev = #"\n" andalso hd(charlist) = #" " ) then start(prev , tl(charlist) , List.nth(charlist , 2) , starter)
                                            else if( (hd(charlist) = #"\n" orelse List.take(explode(starter) ,3 ) = [#"<" ,#"/" , #"h" ]  ) andalso ahead = #"\n" ) then ( append(output , str(hd(charlist))^starter);
                                                                                                                                                                     start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , "    " )    )
                                            else if(length(charlist) >= 5 andalso hd(charlist) = #"#" ) then ( let val(star , gap) = headers(tl(charlist) , 1);
                                                                                                                in start(List.nth(charlist , gap -1) , tl(List.drop(charlist,gap ) ) , List.nth(charlist ,gap+1) , star )
                                                                                                                end )
                                            else  if( ahead = #"*" andalso hd(charlist) = #"*" ) then ( append(output , "<strong>") ;                            (*inline items started *)
                                                                                                     start(prev , tl(tl(charlist)) , List.nth(charlist ,3) , "</strong>" )    )
                 (* inline ended*)          else ( append(output , str(hd(charlist)));
                                                    start(hd(charlist) , tl(charlist) , List.nth(charlist ,2) , starter ) 
                                                    )
                                        )

fun main(filename) = (write(output,"");  checkheadings(explode(slurp(filename))) )


else if( ( List.take(tl(charlist) , 4) <> [#" ", #" ", #" ", #" " ] andalso ahead <> #"\t" ) andalso ahead <> #"\n" andalso hd(starter) = "</li>" ) then (
                                append(output , hd(starter)^"\n");
                                start(prev , charlist , ahead , tl(starter) )
                            )else if( ( List.take(tl(charlist) , 4) <> [#" ", #" ", #" ", #" " ] andalso ahead <> #"\t" ) andalso ahead <> #"\n"  andalso (not (#1(islist(#"\n" , tl(charlist) , 1 ))) ) andalso ( hd(starter) = "</ol>") ) then (
                                append(output , hd(starter)^"\n");
                                start(prev , charlist , ahead , tl(starter) )
                            )