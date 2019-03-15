module type S = sig
  type state
  type trans
  type ui_trans
  val make_graph :
    Globals.ppmode ->
    Test.info ->
    state ->
    MachineDefCandidateExecution.cex_candidate ->
    ui_trans list -> unit
end
