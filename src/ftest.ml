open Gfile
open Tools
open Fordfulkerson
open Graph
open Money
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a money problem\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) outfile(2) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(2)
  in

  (* Open file *)
  let graph = get_exchange_graph infile in

(***** Modifications graphe *****)

(********************************)

  (* let graph = ford_fulkerson graph _source _sink in *)
  (* Rewrite the graph that has been read. *)
  let () = export graph ; write_file outfile graph in

  
  ()

