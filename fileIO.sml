signature  FILEIO =
sig
    val getclist : string -> TextIO.StreamIO.elem list
    val getclistNN : string -> char list
    val slurp : string -> TextIO.StreamIO.vector
    val readAll : string -> TextIO.StreamIO.vector
    val bite : string -> string list
    val lick : string -> string list 
    val readLines : string -> string list
    val write : string * TextIO.vector -> unit
    val writeLine : string * string -> unit
    val writeLines : string * string list -> unit
    val append : string * TextIO.vector -> unit
    val appendLine : string * string -> unit
    val appendLines : string * string list -> unit
end (* sig FILEIO *)

structure FileIO: FILEIO =
struct

(*=============== reading a text file ====================== *)

(* Partly  from the SML Basis LIbrary -- Gansner and Reppy *)

(* get a list of characters from a file *)
fun getclist (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (c, f') => loop (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loop ([], f))
    end

(* get list of chars except newline characters *)
fun getclistNoNewline  (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loopNoNewline (clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (#"\n", f') => loopNoNewline (clist, f')
	      | SOME (c, f') => loopNoNewline (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loopNoNewline ([], f))
    end

val getclistNN = getclistNoNewline

(* Get the contents of an entire text file as a string *)
fun slurp (filename:string) =
    let val f = TextIO.getInstream(TextIO.openIn filename)
	val (s, _) = TextIO.StreamIO.inputAll f
    in  TextIO.StreamIO.closeIn; s
    end

val readAll = slurp;

(* Reading a chunk of characters at a time and outputting a list of strings *)
fun bite (filename:string) =
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (accum: string list, f) =
	    case TextIO.StreamIO.input f
	     of ("", f')    => (TextIO.StreamIO.closeIn f'; accum)
	      | (chunk, f') => loop (chunk::accum, f')
            (* esac *)
    in  rev(loop ([], f))
    end



(* Reading by lines list of strings 
fun lick (filename:string) =
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (accum: string list, f) =
	    case (TextIO.StreamIO.inputLine f) of 
	        SOME(chunk, f') => loop (chunk::accum, f')
	      | NONE => (TextIO.StreamIO.closeIn f; accum)
            (* esac *)
    in  rev(loop ([], f))
    end


This function which is functional style yields the following error
	
/home/sak/sml/programs/fileio/fileIO.sml:74.6-76.52 Error: case object and rules don't agree [tycon mismatch]
  rule domain: (string * ?.TextIO.instream) option
  object: string * ?.TextIO.instream
  in expression:
    (case (TextIO.StreamIO.inputLine f)
      of SOME (chunk,f') => loop (<exp> :: <exp>,f')
       | NONE => (TextIO.StreamIO.closeIn f; accum))

So we do the imperative style
*)

fun lick (filename:string) =
    let val f = TextIO.openIn filename
        fun loop (accum: string list) =
            case (TextIO.inputLine f) of 
		NONE => accum
              | SOME line => loop (line::accum)
            (* esac *)
        val lines =   rev(loop [])
    in TextIO.closeIn f; lines
    end




(* The perl function chomp *)
fun chomp1 s = 
    let val charlist = rev (explode s)
	fun nibble [] = []
	  | nibble (#"\n"::l) = l
	  | nibble l = l
    in  implode (rev (nibble charlist))
    end

fun chomp (L: string list)  = map chomp1 L;

val readLines = chomp o lick

(*======================== writing a text file ========================= *)

(* writing a single string into a text file without adding a new line*)
fun write (filename: string, s) = 
    let val f =  TextIO.openOut filename
    in  (TextIO.output (f, s); TextIO.closeOut f) 
    end

(* writing a single string terminated by newline into a text file *)
fun writeLine (filename: string, s) = 
    let val f =  TextIO.openOut filename
    in  (TextIO.output (f, s^"\n"); TextIO.closeOut f) 
    end

(* the perl function join *)
fun join (glue, []) = ""
  | join (glue, [s]) = s
  | join (glue, (h::t)) = h^glue^(join (glue, t))

(* writing a string list into a file *)
fun writeLines (filename: string, sl) =
    let val ss =  join ("\n", sl)
    in  writeLine (filename: string, ss)
    end

(* appending a line to a file without a newline character *)
fun append (filename: string, s) = 
    let val f =  TextIO.openAppend filename
    in  (TextIO.output (f, s); TextIO.closeOut f) 
    end
(* same as append but adds a newline character *)
fun appendLine  (filename: string, s) = 
    let val f =  TextIO.openAppend filename
    in  (TextIO.output (f, s^"\n"); TextIO.closeOut f) 
    end

fun appendLines (filename: string, sl) =
    let val ss =  join ("\n", sl)
    in  appendLine (filename: string, ss)
    end
end (* struct Fileio *)
