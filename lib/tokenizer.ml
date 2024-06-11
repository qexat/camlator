class tokenizer (source : string) =
  object (self)
    val mutable start = 0
    val mutable current = 0
    val mutable tokens : Token.token list = []
    method is_at_end ?(n = 0) () = current + n >= String.length source

    method peek ?(n = 0) () =
      if self#is_at_end ?n:(Some n) () then '\x00' else source.[current]

    method advance ?(steps = 1) () = current <- current + steps
    method sync_head () = start <- current

    method consume () =
      let character = self#peek () in
      self#advance ();
      character

    method make_integer () =
      let chr = self#peek () in
      match chr with
      | '0' .. '9' ->
        self#advance ();
        self#make_integer ()
      | _ -> Token_type.INTEGER

    method scan_token () =
      match self#consume () with
      | ' ' | '\r' | '\t' ->
        self#sync_head ();
        self#scan_token ()
      | '-' -> Token_type.BINARY_OP Token_type.MINUS
      | '+' -> Token_type.BINARY_OP Token_type.PLUS
      | '/' -> Token_type.BINARY_OP Token_type.SLASH
      | '*' -> Token_type.BINARY_OP Token_type.STAR
      | '0' .. '9' -> self#make_integer ()
      | _ -> Token_type.INVALID

    method get_lexeme () = String.sub source start (current - start)

    method build_token typ =
      { Token.typ; lexeme = self#get_lexeme (); startpos = start; endpos = current }

    method run () =
      if self#is_at_end ()
      then (
        self#sync_head ();
        tokens <- self#build_token Token_type.EOF :: tokens;
        List.rev tokens)
      else (
        self#sync_head ();
        tokens <- self#build_token (self#scan_token ()) :: tokens;
        self#run ())
  end
