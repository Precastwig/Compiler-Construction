
module Basics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | TIMES
    | STRING of (string)
    | STR of (string)
    | SEMICOLON
    | RIGHTROUNDBRACKET
    | RIGHTBRACE
    | READINT
    | PRINTINT
    | PLUS
    | OR
    | NOT
    | NEW
    | MINUS
    | LET
    | LESS
    | LEFTROUNDBRACKET
    | LEFTBRACE
    | INT of (int)
    | IN
    | IF
    | GREATER
    | EQUAL
    | EOF
    | ELSE
    | DO
    | DIVIDE
    | COMMA
    | ASSIGN
    | AND
  
end

include Basics

let _eRR =
  Basics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState95
  | MenhirState85
  | MenhirState78
  | MenhirState75
  | MenhirState68
  | MenhirState66
  | MenhirState63
  | MenhirState60
  | MenhirState58
  | MenhirState54
  | MenhirState52
  | MenhirState50
  | MenhirState49
  | MenhirState47
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState26
  | MenhirState25
  | MenhirState22
  | MenhirState19
  | MenhirState18
  | MenhirState11
  | MenhirState10
  | MenhirState4
  | MenhirState2
  | MenhirState0
  
	open Types

let rec _menhir_goto_separated_nonempty_list_SEMICOLON_fundef_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Types.program)) = _v in
        let _v : (Types.program) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_fundef__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Types.program)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Types.fundef))) = _menhir_stack in
        let _2 = () in
        let _v : (Types.program) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMICOLON_fundef_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | AND | ASSIGN | COMMA | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (e : (Types.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Types.expression) =                             ( e ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState33 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | LEFTBRACE ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LEFTROUNDBRACKET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | NEW ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | NOT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | PRINTINT ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | READINT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | STR _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | STRING _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState46 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | LEFTBRACE ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LEFTROUNDBRACKET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NEW ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NOT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | PRINTINT ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | READINT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | STR _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | STRING _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run49 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState49 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | LEFTBRACE ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | LEFTROUNDBRACKET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | NEW ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | NOT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | PRINTINT ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | READINT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | STR _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | STRING _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | LEFTBRACE ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | LEFTROUNDBRACKET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | NEW ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | NOT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | PRINTINT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | READINT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                                ( Types.Operator (Times, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | EQUAL | IN | OR | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Types.expression))), _), _, (p : (Types.expression))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (Types.expression) =                                     ( Types.Operator (Noteq, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | DO | ELSE | EQUAL | GREATER | IN | LESS | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                               ( Types.Operator (Plus, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                                ( Types.Operator (Minus, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | RIGHTROUNDBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Types.expression) =                                                          ( Types.Application (e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | ELSE | IN | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                                   ( Types.Seq (e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | ELSE | IN | OR | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                             ( Types.Operator (Or, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | EQUAL | IN | OR | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Types.expression))), _), _, (p : (Types.expression))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (Types.expression) =                                      ( Types.Operator (Leq, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | EQUAL | IN | OR | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Types.expression))), _), _, (p : (Types.expression))) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _v : (Types.expression) =                                         ( Types.Operator (Geq, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                                 ( Types.Operator (Divide, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | DO | ELSE | EQUAL | GREATER | IN | LESS | OR | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                                 ( Types.Asg (e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | EQUAL | GREATER | IN | LESS | OR | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                                 ( Types.Operator (Greater, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | EQUAL | IN | LESS | OR | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                              ( Types.Operator (Less, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | EQUAL | IN | OR | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                                ( Types.Operator (Equal, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | IN | OR | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                              ( Types.Operator (And, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | ELSE | IN | RIGHTBRACE | RIGHTROUNDBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                                ( Types.Seq (e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | NOT | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                              ( Types.Operator (Not, e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | LEFTBRACE ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | LEFTROUNDBRACKET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | NEW ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | NOT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | PRINTINT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | READINT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState68
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | IN | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Types.expression) =                                  ( Types.If (e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | ELSE | IN | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e : (Types.expression))), _, (p : (Types.expression))), _, (f : (Types.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Types.expression) =                                             ( Types.Ifelse (e, p, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | RIGHTBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Types.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Types.expression) =                                     ( e ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | RIGHTROUNDBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Types.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Types.expression) =                                                 ( e ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | LEFTBRACE ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LEFTROUNDBRACKET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NEW ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NOT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | PRINTINT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | READINT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | ELSE | IN | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (s : (string))), _, (e : (Types.expression))), _, (f : (Types.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Types.expression) =                                              ( Types.Let (s, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | LEFTBRACE ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LEFTROUNDBRACKET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | NEW ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | NOT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | PRINTINT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | READINT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | DO | ELSE | IN | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), (s : (string))), _, (e : (Types.expression))), _, (f : (Types.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Types.expression) =                                              ( Types.New (s, e, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | COMMA | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | NOT | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Types.expression))) = _menhir_stack in
            let _1 = () in
            let _v : (Types.expression) =                        ( Types.Deref e ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | RIGHTROUNDBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Types.expression))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Types.expression) =                                                           ( Types.Printint (e) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFTBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IF ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | INT _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
                | LEFTBRACE ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | LEFTROUNDBRACKET ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | LET ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | NEW ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | NOT ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | PRINTINT ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | READINT ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | STR _v ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
                | STRING _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
                | WHILE ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | RIGHTBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Types.expression) =                                                       ( Types.While (e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | RIGHTBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (s : (string))), _, (a : (string list))), _, (f : (Types.expression))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _v : (Types.fundef) =                                                                                            ( (s, a, f) ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | STR _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
            | EOF ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (Types.fundef))) = _menhir_stack in
                let _v : (Types.program) =     ( [ x ] ) in
                _menhir_goto_separated_nonempty_list_SEMICOLON_fundef_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | SEMICOLON ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (string)) = _v in
    let _v : (Types.expression) =                      ( Types.String s ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (string)) = _v in
    let _v : (Types.expression) =                    ( Types.Identifier s ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFTROUNDBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHTROUNDBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Types.expression) =                                                 ( Types.Readint ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFTROUNDBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IF ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | INT _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | LEFTBRACE ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | LEFTROUNDBRACKET ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | LET ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | NEW ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | NOT ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | PRINTINT ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | READINT ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | STR _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | STRING _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | LEFTBRACE ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | LEFTROUNDBRACKET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | NEW ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | NOT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | PRINTINT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | READINT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | LEFTBRACE ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LEFTROUNDBRACKET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | NEW ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | NOT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | PRINTINT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | READINT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (Types.expression) =                    ( Types.Const i ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | INT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LEFTBRACE ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LEFTROUNDBRACKET ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LET ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NEW ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NOT ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | PRINTINT ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | READINT ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_goto_separated_nonempty_list_COMMA_STR_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (string list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _2 = () in
        let _v : (string list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_COMMA_STR_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (string list)) = _v in
        let _v : (string list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_COMMA_STR__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_STR__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (xs0 : (string list)) = _v in
    let _v : (string list) = let l =
      let xs = xs0 in
          ( xs )
    in
                                           ( l ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHTROUNDBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | INT _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
            | LEFTBRACE ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | LEFTROUNDBRACKET ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | LET ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | NEW ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | NOT ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | PRINTINT ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | READINT ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | STR _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
    | RIGHTROUNDBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _v : (string list) =     ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_COMMA_STR_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_fundef__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (xs0 : (Types.program))) = _menhir_stack in
        let _2 = () in
        let _v : (Types.program) = let el =
          let xs = xs0 in
              ( xs )
        in
                                                           ( el ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Types.program)) = _v in
        Obj.magic _1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFTROUNDBRACKET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | STR _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | RIGHTROUNDBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState2 in
            let _v : (string list) =     ( [] ) in
            _menhir_goto_loption_separated_nonempty_list_COMMA_STR__ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and top : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.program) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | EOF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        let _v : (Types.program) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_fundef__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
  

