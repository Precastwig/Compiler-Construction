
module Basics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | TIMES
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
  | MenhirState73
  | MenhirState64
  | MenhirState58
  | MenhirState55
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState45
  | MenhirState31
  | MenhirState29
  | MenhirState26
  | MenhirState24
  | MenhirState21
  | MenhirState18
  | MenhirState17
  | MenhirState11
  | MenhirState10
  | MenhirState4
  | MenhirState2
  | MenhirState0
  
	open Types

let rec _menhir_goto_opcode : _menhir_env -> 'ttv_tail -> (Types.opcode) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | PRINTINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | STR _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_goto_separated_nonempty_list_SEMICOLON_fundef_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Types.program)) = _v in
        let _v : (Types.program) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_fundef__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Types.program)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Types.fundef))) = _menhir_stack in
        let _2 = () in
        let _v : (Types.program) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_SEMICOLON_fundef_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run28 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.opcode) =          ( Times ) in
    _menhir_goto_opcode _menhir_env _menhir_stack _v

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | PRINTINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | STR _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run34 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.opcode) =         ( Plus ) in
    _menhir_goto_opcode _menhir_env _menhir_stack _v

and _menhir_run35 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.opcode) =       ( Or ) in
    _menhir_goto_opcode _menhir_env _menhir_stack _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Types.opcode) =               ( Noteq ) in
        _menhir_goto_opcode _menhir_env _menhir_stack _v
    | IF | INT _ | LET | NEW | NOT | PRINTINT | READINT | STR _ | WHILE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (Types.opcode) =        ( Not ) in
        _menhir_goto_opcode _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run38 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.opcode) =          ( Minus ) in
    _menhir_goto_opcode _menhir_env _menhir_stack _v

and _menhir_run39 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Types.opcode) =                ( Leq ) in
        _menhir_goto_opcode _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | PRINTINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | STR _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run41 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Types.opcode) =                   ( Geq ) in
        _menhir_goto_opcode _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run43 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.opcode) =          ( Equal ) in
    _menhir_goto_opcode _menhir_env _menhir_stack _v

and _menhir_run44 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.opcode) =           ( Divide) in
    _menhir_goto_opcode _menhir_env _menhir_stack _v

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PRINTINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | STR _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run47 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.opcode) =        ( And ) in
    _menhir_goto_opcode _menhir_env _menhir_stack _v

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | PRINTINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | STR _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | NOT | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | SEMICOLON | TIMES ->
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
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | DO | ELSE | IN | RIGHTBRACE | RIGHTROUNDBRACKET ->
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
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | IN | RIGHTBRACE | RIGHTROUNDBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Types.expression))), (o : (Types.opcode))), _, (p : (Types.expression))) = _menhir_stack in
            let _v : (Types.expression) =                                     ( Types	.Operator (o, e, p) ) in
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
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | PRINTINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | STR _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState52
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e : (Types.expression))), _, (p : (Types.expression))), _, (f : (Types.expression))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Types.expression) =                                             ( Types.If (e, p, f) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | PRINTINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | STR _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | IN | RIGHTBRACE | RIGHTROUNDBRACKET ->
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
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | PRINTINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | STR _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
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
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | DO | ELSE | IN | RIGHTBRACE | RIGHTROUNDBRACKET ->
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
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | NOT | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | TIMES ->
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
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | PRINTINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | STR _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState64
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | DIVIDE | DO | ELSE | EQUAL | GREATER | IN | LESS | MINUS | OR | PLUS | RIGHTBRACE | RIGHTROUNDBRACKET | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Types.expression))), _, (p : (Types.expression))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Types.expression) =                                   ( Types.While (e, p) ) in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | LEFTROUNDBRACKET ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | NOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
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
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
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
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
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
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | PRINTINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | STR _v ->
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
    let _v : (Types.expression) =                     ( Types.Identifier s ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | INT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | LET ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | NEW ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | NOT ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | PRINTINT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | READINT ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | STR _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | WHILE ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
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
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | PRINTINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | STR _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | PRINTINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | STR _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
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

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | PRINTINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | STR _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
            | WHILE ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
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

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (int)) = _v in
    let _v : (Types.expression) =                    ( Types.Const i ) in
    _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | INT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LET ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NEW ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NOT ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | PRINTINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | READINT ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | STR _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | WHILE ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

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
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | INT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
            | LET ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | NEW ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | NOT ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | PRINTINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | READINT ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
            | STR _v ->
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
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
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
  

