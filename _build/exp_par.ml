
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
  | MenhirState77
  | MenhirState74
  | MenhirState67
  | MenhirState65
  | MenhirState62
  | MenhirState59
  | MenhirState57
  | MenhirState53
  | MenhirState51
  | MenhirState50
  | MenhirState48
  | MenhirState47
  | MenhirState45
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState37
  | MenhirState35
  | MenhirState34
  | MenhirState32
  | MenhirState30
  | MenhirState28
  | MenhirState27
  | MenhirState26
  | MenhirState23
  | MenhirState20
  | MenhirState19
  | MenhirState14
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

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | AND | COMMA | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (e : (Types.expression))) = _menhir_stack in
        let _2 = () in
        let _v : (Types.expression) =                             ( e ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState34 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | INT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | LEFTBRACE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LEFTROUNDBRACKET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | NEW ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | NOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | PRINTINT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | READINT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | STR _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | STRING _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState47 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | INT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
        | LEFTBRACE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | LEFTROUNDBRACKET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | NEW ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | NOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | PRINTINT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | READINT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | STR _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
        | STRING _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState50 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | INT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | LEFTBRACE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LEFTROUNDBRACKET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NEW ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PRINTINT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | READINT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | STR _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | STRING _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | INT _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | LEFTBRACE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LEFTROUNDBRACKET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | LET ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NEW ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NOT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | PRINTINT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | READINT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON | TIMES ->
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
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | EQUAL | GREATER | IN | LESS | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
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
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
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
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
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
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | NOT | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON | TIMES ->
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
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | INT _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | LEFTBRACE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LEFTROUNDBRACKET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | LET ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NEW ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | NOT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | PRINTINT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | READINT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | INT _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | LEFTBRACE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | LEFTROUNDBRACKET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | LET ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | NEW ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | NOT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | PRINTINT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | READINT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState74
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | INT _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | LEFTBRACE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LEFTROUNDBRACKET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | LET ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | NEW ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | NOT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | PRINTINT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | READINT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState77
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | NOT | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON | TIMES ->
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
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | AND | COMMA | DO | ELSE | EQUAL | GREATER | IN | LESS | OR | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (s : (string))), _, (p : (Types.expression))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.expression) =                                 ( Types.Asg (s, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | INT _v ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
                | LEFTBRACE ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | LEFTROUNDBRACKET ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | LET ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | NEW ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | NOT ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | PRINTINT ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | READINT ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState85
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
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
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
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11
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
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IF ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | INT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | LEFTBRACE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | LEFTROUNDBRACKET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | NEW ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | NOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | PRINTINT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | READINT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | STR _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | STRING _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
    | AND | COMMA | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LEFTROUNDBRACKET | LESS | MINUS | NOT | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (s : (string))) = _menhir_stack in
        let _v : (Types.expression) =                    ( Types.Identifier s ) in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | INT _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | LEFTBRACE ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | LEFTROUNDBRACKET ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | LET ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | NEW ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | NOT ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | PRINTINT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | READINT ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | STR _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | STRING _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | INT _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | LEFTBRACE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | LEFTROUNDBRACKET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | LET ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | NEW ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | NOT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | PRINTINT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | READINT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
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

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | INT _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | LEFTBRACE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | LEFTROUNDBRACKET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | LET ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | NEW ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | NOT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | PRINTINT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | READINT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | STR _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | STRING _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
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

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27
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

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (Types.expression) =                    ( Types.Const i ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INT _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LEFTBRACE ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LEFTROUNDBRACKET ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LET ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NEW ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NOT ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | PRINTINT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | READINT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | STR _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | STRING _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

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
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | INT _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
            | LEFTBRACE ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | LEFTROUNDBRACKET ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | LET ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | NEW ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | NOT ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | PRINTINT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | READINT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState10
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
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
  

