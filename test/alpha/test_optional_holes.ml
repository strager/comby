open Core

open Matchers
open Rewriter

let configuration = Configuration.create ~match_kind:Fuzzy ()

let run ?(configuration = configuration) source match_template rewrite_template =
  Generic.all ~configuration ~template:match_template ~source
  |> function
  | [] -> print_string "No matches."
  | results ->
    Option.value_exn (Rewrite.all ~source ~rewrite_template results)
    |> (fun { rewritten_source; _ } -> rewritten_source)
    |> print_string

let%expect_test "optional_holes_basic_match" =
  let source = {||} in
  let match_template = {|:[[?x]]|} in
  let rewrite_template = {|/:[?x]/|} in
  run source match_template rewrite_template;
  [%expect_exact {|//|}];

  let source = {||} in
  let match_template = {|:[[?x]]:[[?y]]|} in
  let rewrite_template = {|/:[?x]/:[?y]/|} in
  run source match_template rewrite_template;
  [%expect_exact {|///|}];

  let source = {|a |} in
  let match_template = {|:[[x]] :[[?y]]|} in
  let rewrite_template = {|/:[x]/:[?y]/|} in
  run source match_template rewrite_template;
  [%expect_exact {|/a//|}];

  let source = {|a |} in
  let match_template = {|:[[x]] :[[?y]]|} in
  let rewrite_template = {|/:[x]/:[?y]/|} in
  run source match_template rewrite_template;
  [%expect_exact {|/a//|}];

  let source = {|(foo )|} in
  let match_template = {|(:[[?x]] :[[?y]])|} in
  let rewrite_template = {|/:[?x]/:[?y]/|} in
  run source match_template rewrite_template;
  [%expect_exact {|/foo//|}];

  let source = {|(foo)|} in
  let match_template = {|(:[[?x]]:[? w])|} in
  let rewrite_template = {|/:[?x]/:[?w]/|} in
  run source match_template rewrite_template;
  [%expect_exact {|/foo//|}];

  let source = {|()|} in
  let match_template = {|(:[[?x]]:[? w]:[?y]:[?z.])|} in
  let rewrite_template = {|/:[?x]/:[?w]/:[?y]|} in
  run source match_template rewrite_template;
  [%expect_exact {|///|}];

  let source = {|()|} in
  let match_template = {|(:[?s\n])|} in
  let rewrite_template = {|/:[?s]/|} in
  run source match_template rewrite_template;
  [%expect_exact {|//|}]

let%expect_test "optional_holes_match_over_coalesced_whitespace" =
  let source = {|a c|} in
  let match_template = {|:[[a]] :[[?b]] :[[c]]|} in
  let rewrite_template = {|/:[?a]/:[?b]/:[?c]|} in
  run source match_template rewrite_template;
  [%expect_exact {|No matches.|}];

  let source = {|a c|} in
  (* The optional matcher ?b does match c, and then there's nothing left to
     match c. This is correct behavior wrt the LL parsing. *)
  let match_template = {|:[[a]] :[[?b]]:[[c]]|} in
  let rewrite_template = {|/:[?a]/:[?b]/:[?c]|} in
  run source match_template rewrite_template;
  [%expect_exact {|No matches.|}];

  let source = {|a c|} in
  let match_template = {|:[[a]]:[[?b]]:[[c]]|} in
  let rewrite_template = {|/:[?a]/:[?b]/:[?c]|} in
  run source match_template rewrite_template;
  [%expect_exact {|No matches.|}];

  let source = {|a c|} in
  let match_template = {|:[[a]]:[[?b]] :[[?c]]|} in
  let rewrite_template = {|/:[?a]/:[?b]/:[?c]|} in
  run source match_template rewrite_template;
  [%expect_exact {|/a//c|}];

  let source = {|func foo(bar) {}|} in
  let match_template = {|func :[?receiver] foo(:[args])|} in
  let rewrite_template = {|/:[receiver]/:[args]/|} in
  run source match_template rewrite_template;
  [%expect_exact {|No matches.|}];

  let source = {|func (r *receiver) foo(bar) {}|} in
  let match_template = {|func :[?receiver] foo(:[args])|} in
  let rewrite_template = {|/:[receiver]/:[args]/|} in
  run source match_template rewrite_template;
  [%expect_exact {|/(r *receiver)/bar/ {}|}]

let%expect_test "optional_holes_substitute" =
  let source = {|()|} in
  let match_template = {|(:[[?x]]:[? w]:[?y]:[?z.])|} in
  let rewrite_template = {|/:[x]/:[w]/:[y]/:[z]|} in
  run source match_template rewrite_template;
  [%expect_exact {|////|}]
