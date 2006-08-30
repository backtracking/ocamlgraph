(* $Id: md.mli,v 1.2 2004-06-28 13:48:25 signoles Exp $ *)

(** Minimum Degree algorithm
  
  Based on the article:
  The Minimum Degree Heuristic and the Minimal Triangulation Process
  by A. Berry, Pinar Heggernes & Geneviève Simonet.
  
  @author Matthieu Sozeau
  @author Pierre-Loic Garoche *)

module P(G : Sig.P) : sig

  type edgeset = (G.V.t * G.V.t) list

  val md : G.t -> G.t * edgeset * G.V.t list
    (** [md g] return a tuple [(g', e, o)] where [g'] is 
      a triangulated graph, [e] is the triangulation of [g] and
      [o] is a perfect elimination order of [g'] *)

  val triangulate : G.t -> G.t
    (** [triangulate g] return the graph [g'] produced by applying 
      miminum degree to [g]. *)

end

module I(G : Sig.I) : sig
  
  type edgeset = (G.V.t * G.V.t) list
	
  val md : G.t -> G.t * edgeset * G.V.t list
    (** [md g] return a tuple [(g', e, o)] where [g'] is 
      a triangulated graph, [e] is the triangulation of [g] and
      [o] is a perfect elimination order of [g'] *)

  val triangulate : G.t -> G.t
    (** [triangulate g] return the graph [g'] produced by applying 
      miminum degree to [g]. *)

end
