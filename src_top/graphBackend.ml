module type S = sig
  type state
  type trans
  type ui_trans
  val make_graph :
    Globals.ppmode ->
    Test.info ->
    state ->
    CandidateExecution.cex_candidate ->
    ui_trans list -> unit
end
