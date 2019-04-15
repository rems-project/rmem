module type S = sig
  type ui_trans
  val make_graph :
    Globals.ppmode ->
    Test.info ->
    CandidateExecution.cex_candidate ->
    ui_trans list -> unit
end
