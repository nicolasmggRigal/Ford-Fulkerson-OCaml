open Gfile
open Tools
open Fordfulkerson
open Graph
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (* let f p =
    let rec ff = function
      | [] -> ()
      | head :: tail -> Printf.printf "%d\n" head ; ff tail
    in
    match p with
      | Some (p, flow) -> Printf.printf "\n==== Chemin trouve : flot %d ====\n" flow ; ff p ; Printf.printf "\n\n"
      | None -> Printf.printf "\n==== Aucun chemin trouve ====\n\n"
  in *)

(***** Modifications graphe *****)

(********************************)

  let graph = ford_fulkerson graph _source _sink in
  (* Rewrite the graph that has been read. *)
  let () = export graph ; write_file outfile graph in

  
  ()

