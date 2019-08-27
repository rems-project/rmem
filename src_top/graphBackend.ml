module type S = sig
  type ui_trans
  type instruction_ast
  val make_graph :
    Globals.ppmode ->
    Test.info ->
    instruction_ast CandidateExecution.cex_candidate ->
    ui_trans list -> unit
end
