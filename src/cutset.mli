
module Make (G : Sig.G) : sig

  val min_cutset : G.t -> G.V.t -> G.V.t list option

end
