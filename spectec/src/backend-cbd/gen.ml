open Cbd

open Al.Al_util
open Util.Source

let eq_list eq l1 l2 =
  List.length l1 = List.length l2
  && List.for_all2 eq l1 l2

let eq_pphint h1 h2 =
  match h1, h2 with
  | None, None -> true
  | Some h1, Some h2 -> h1 = h2
  | _, _ -> false

let eq_cmpop = (=)

let eq_expr = Al.Eq.eq_expr

let rec eq_stmt i1 i2 =
  match (i1, i2) with
  | LetS (e11, e12), LetS (e21, e22) ->
      eq_expr e11 e21
      && eq_expr e12 e22
  | CmpS (e11, cmp1, e12), CmpS (e21, cmp2, e22) ->
      eq_expr e11 e21
      && eq_cmpop cmp1 cmp2
      && eq_expr e12 e22
  | IsValidS (opt1, e11, l1, pphint1), IsValidS (opt2, e21, l2, pphint2) ->
      Option.equal eq_expr opt1 opt2
      && eq_expr e11 e21
      && eq_list eq_expr l1 l2
      && eq_pphint pphint1 pphint2
  | MatchesS (e11, e12), MatchesS (e21, e22) ->
      eq_expr e11 e21
      && eq_expr e12 e22
  | IsConstS (opt1, e11), IsConstS (opt2, e21) ->
      Option.equal eq_expr opt1 opt2
      && eq_expr e11 e21
  | IsDefinedS e1, IsDefinedS e2 ->
      eq_expr e1 e2
  | IsDefaultableS (e1, cmp1), IsDefaultableS (e2, cmp2) ->
      eq_expr e1 e2
      && eq_cmpop cmp1 cmp2
  | IfS (e11, il1), IfS (e21, il2) ->
      eq_expr e11 e21
      && List.for_all2 eq_stmt il1 il2
  | ForallS (l1, il1), ForallS (l2, il2) ->
      eq_list (fun (e11, e12) (e21, e22) -> eq_expr e11 e21 && eq_expr e12 e22) l1 l2
      && eq_list eq_stmt il1 il2
  | EitherS (il1), EitherS (il2) ->
      eq_list (fun l1 l2 -> eq_list eq_stmt l1 l2) il1 il2
  | RelS (s1, el1), RelS (s2, el2) ->
    s1 = s2
    && eq_list eq_expr el1 el2
  | YetS s1, YetS s2 ->
    s1 = s2
  | _, _ -> i1 = i2

let unify_either stmts =
  let f stmt =
    match stmt with
    | EitherS sss ->
      let unified, bodies = List.fold_left (fun (commons, stmtss) s ->
        let pairs = List.map (List.partition (eq_stmt s)) stmtss in
        let fsts = List.map fst pairs in
        let snds = List.map snd pairs in
        if List.for_all (fun l -> List.length l = 1) fsts then
          s :: commons, snds
        else
          commons, stmtss
      ) ([], sss) (List.hd sss) in
      let unified = List.rev unified in
      unified @ [ EitherS bodies ]
    | _ -> [stmt]
  in
  let rec walk stmts = List.concat_map walk' stmts
  and walk' stmt =
    f stmt
    |> List.map (function
      | IfS (e, sl) -> IfS (e, walk sl)
      | ForallS (vars, sl) -> ForallS (vars, walk sl)
      | EitherS sll -> EitherS (List.map walk sll)
      | s -> s
    )
  in
  walk stmts

let postprocess_cbd defs =
  List.map (fun def ->
    match def with
    | RuleD (anchor, i, il) ->
      let new_il = unify_either il in
      RuleD (anchor, i, new_il)
    | AlgoD _ -> def
  ) defs

let insert_state_binding algo =
  let open Al.Ast in
  let z_binding = ref 0 in
  let state_count = ref 0 in

  let count_state e =
    (match e.it with
    | VarE "z" -> state_count := !state_count + 1
    | _ -> ());
  in

  let check_z_binding i =
    (match i.it with
    | LetI (e, _) when e.it = VarE "z" -> z_binding := !z_binding + 1
    | _ -> ());
  in

  let walk_expr walker expr =
    if (!z_binding = 0 && !state_count = 0) then (
      count_state expr;
      Al.Walk.base_walker.walk_expr walker expr
    )
    else expr
  in
  let walk_instr walker instr =
    if (!z_binding = 0 && !state_count = 0) then (
      check_z_binding instr;
      Al.Walk.base_walker.walk_instr walker instr
    )
    else [instr]
  in
  let walker = { Al.Walk.base_walker with walk_expr; walk_instr } in
  let _ = walker.walk_algo walker algo in
  if !state_count > 0 then (
    match algo.it with
    | RuleA (name, anchor, params, body) ->
      let body = (letI (varE "z" ~note:stateT, getCurStateE () ~note:stateT)) :: body in
      { algo with it = RuleA (name, anchor, params, body) }
    | _ -> algo
  )
  else algo

(* idrk what this does *)
let gen_cbd _el _il al =
    List.map
        (fun algo ->
            let algo =
            algo
            |> insert_state_binding
            |> Il2al.Transpile.remove_exit
            |> Il2al.Transpile.remove_enter
            |> Il2al.Transpile.prosify_control_frame
          in
          AlgoD algo) al
