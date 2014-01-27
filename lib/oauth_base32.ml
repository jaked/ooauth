(* http://www.crockford.com/wrmg/base32.html *)

open Cryptokit

(* adapted from Cryptokit.Base64 *)

class buffered_output initial_buffer_size =
  object(self)
    val mutable obuf = String.create initial_buffer_size
    val mutable obeg = 0
    val mutable oend = 0

    method private ensure_capacity n =
      let len = String.length obuf in
      if oend + n > len then begin
        if oend - obeg + n < len then begin
          String.blit obuf obeg obuf 0 (oend - obeg);
          oend <- oend - obeg;
          obeg <- 0
        end else begin
          let newlen = min (2 * len) Sys.max_string_length in
          if oend - obeg + n > newlen then raise(Error Output_buffer_overflow);
          let newbuf = String.create newlen in
          String.blit obuf obeg newbuf 0 (oend - obeg);
          obuf <- newbuf;
          oend <- oend - obeg;
          obeg <- 0
        end
      end

    method available_output = oend - obeg

    method get_substring =
      let res = (obuf, obeg, oend - obeg) in obeg <- 0; oend <- 0; res

    method get_string =
      let res = String.sub obuf obeg (oend - obeg) in obeg <- 0; oend <- 0; res

    method get_char =
      if obeg >= oend then raise End_of_file;
      let r = obuf.[obeg] in
      obeg <- obeg + 1;
      r

    method get_byte =
      Char.code self#get_char

    method wipe =
      wipe_string obuf
  end

let base32_conv_table =
  "0123456789abcdefghjkmnpqrstvwxyz"

class encode =
  object (self)
    method input_block_size = 1
    method output_block_size = 1

    inherit buffered_output 256 as output_buffer

    val ibuf = String.create 5
    val mutable ipos = 0

    method put_char c =
      ibuf.[ipos] <- c;
      ipos <- ipos + 1;
      if ipos = 5 then begin
        let b0 = Char.code ibuf.[0]
        and b1 = Char.code ibuf.[1]
        and b2 = Char.code ibuf.[2]
        and b3 = Char.code ibuf.[3]
        and b4 = Char.code ibuf.[4] in
        self#ensure_capacity 8;
        obuf.[oend]   <- base32_conv_table.[b0 lsr 3];
        obuf.[oend+1] <- base32_conv_table.[(b0 land 7) lsl 2 + (b1 lsr 6)];
        obuf.[oend+2] <- base32_conv_table.[(b1 land 62) lsr 1];
        obuf.[oend+3] <- base32_conv_table.[(b1 land 1) lsl 4 + (b2 lsr 4)];
        obuf.[oend+4] <- base32_conv_table.[(b2 land 15) lsl 1 + (b3 lsr 7)];
        obuf.[oend+5] <- base32_conv_table.[(b3 land 124) lsr 2];
        obuf.[oend+6] <- base32_conv_table.[(b3 land 3) lsl 3 + (b4 lsr 5)];
        obuf.[oend+7] <- base32_conv_table.[b4 land 31];
        oend <- oend + 8;
        ipos <- 0;
      end

    method put_substring s ofs len =
      for i = ofs to ofs + len - 1 do self#put_char s.[i] done

    method put_string s =
      self#put_substring s 0 (String.length s)

    method put_byte b = self#put_char (Char.chr b)

    method flush : unit = raise (Error Wrong_data_length)

    method finish =
      (* I wonder how to do Duff's Device in OCaml... *)
      begin match ipos with
        | 1 ->
            let b0 = Char.code ibuf.[0] in
            self#ensure_capacity 2;
            obuf.[oend]   <- base32_conv_table.[b0 lsr 3];
            obuf.[oend+1] <- base32_conv_table.[(b0 land 7) lsl 2];
            oend <- oend + 2;
        | 2 ->
            let b0 = Char.code ibuf.[0]
            and b1 = Char.code ibuf.[1] in
            self#ensure_capacity 4;
            obuf.[oend]   <- base32_conv_table.[b0 lsr 3];
            obuf.[oend+1] <- base32_conv_table.[(b0 land 7) lsl 2 + (b1 lsr 6)];
            obuf.[oend+2] <- base32_conv_table.[(b1 land 62) lsr 1];
            obuf.[oend+3] <- base32_conv_table.[(b1 land 1) lsl 4];
            oend <- oend + 4;
        | 3 ->
            let b0 = Char.code ibuf.[0]
            and b1 = Char.code ibuf.[1]
            and b2 = Char.code ibuf.[2] in
            self#ensure_capacity 5;
            obuf.[oend]   <- base32_conv_table.[b0 lsr 3];
            obuf.[oend+1] <- base32_conv_table.[(b0 land 7) lsl 2 + (b1 lsr 6)];
            obuf.[oend+2] <- base32_conv_table.[(b1 land 62) lsr 1];
            obuf.[oend+3] <- base32_conv_table.[(b1 land 1) lsl 4 + (b2 lsr 4)];
            obuf.[oend+4] <- base32_conv_table.[(b2 land 15) lsl 1];
            oend <- oend + 5;
        | 4 ->
            let b0 = Char.code ibuf.[0]
            and b1 = Char.code ibuf.[1]
            and b2 = Char.code ibuf.[2]
            and b3 = Char.code ibuf.[3] in
            self#ensure_capacity 7;
            obuf.[oend]   <- base32_conv_table.[b0 lsr 3];
            obuf.[oend+1] <- base32_conv_table.[(b0 land 7) lsl 2 + (b1 lsr 6)];
            obuf.[oend+2] <- base32_conv_table.[(b1 land 62) lsr 1];
            obuf.[oend+3] <- base32_conv_table.[(b1 land 1) lsl 4 + (b2 lsr 4)];
            obuf.[oend+4] <- base32_conv_table.[(b2 land 15) lsl 1 + (b3 lsr 7)];
            obuf.[oend+5] <- base32_conv_table.[(b3 land 124) lsr 2];
            obuf.[oend+6] <- base32_conv_table.[(b3 land 3) lsl 3];
            oend <- oend + 7;
        | _ -> ()
      end

    method wipe =
      wipe_string ibuf; output_buffer#wipe
  end

let encode () = new encode

let base32_decode_char c =
  match c with
    | '0' | 'O' | 'o' -> 0
    | '1' | 'I' | 'i' | 'L' | 'l' -> 1
    | '2' .. '9' -> Char.code c - 48
    | 'A' .. 'H' -> Char.code c - 55 | 'a' .. 'h' -> Char.code c - 87
    | 'J' .. 'K' -> Char.code c - 56 | 'j' .. 'k' -> Char.code c - 88
    | 'M' .. 'N' -> Char.code c - 57 | 'm' .. 'n' -> Char.code c - 89
    | 'P' .. 'T' -> Char.code c - 58 | 'p' .. 't' -> Char.code c - 90
    | 'V' .. 'Z' -> Char.code c - 59 | 'v' .. 'z' -> Char.code c - 91
    | ' '|'\t'|'\n'|'\r' -> -1
    | _   -> raise (Error Bad_encoding)

class decode =
  object (self)
    inherit buffered_output 256 as output_buffer

    method input_block_size = 1
    method output_block_size = 1

    val ibuf = Array.create 8 0
    val mutable ipos = 0
    val mutable finished = false

    method put_char c =
      if c = '=' then finished <- true else begin
        let n = base32_decode_char c in
        if n >= 0 then begin
          if finished then raise(Error Bad_encoding);
          ibuf.(ipos) <- n;
          ipos <- ipos + 1;
          if ipos = 8 then begin
            self#ensure_capacity 5;
            obuf.[oend]   <- Char.chr(ibuf.(0) lsl 3 + ibuf.(1) lsr 2);
            obuf.[oend+1] <- Char.chr((ibuf.(1) land 3) lsl 6 + ibuf.(2) lsl 1 + ibuf.(3) lsr 4);
            obuf.[oend+2] <- Char.chr((ibuf.(3) land 15) lsl 4 + ibuf.(4) lsr 1);
            obuf.[oend+3] <- Char.chr((ibuf.(4) land 1) lsl 7 + ibuf.(5) lsl 2 + ibuf.(6) lsr 3);
            obuf.[oend+4] <- Char.chr((ibuf.(6) land 7) lsl 5 + ibuf.(7));
            oend <- oend + 5;
            ipos <- 0
          end
        end
      end

    method put_substring s ofs len =
      for i = ofs to ofs + len - 1 do self#put_char s.[i] done

    method put_string s =
      self#put_substring s 0 (String.length s)

    method put_byte b = self#put_char (Char.chr b)

    method flush : unit = raise (Error Wrong_data_length)

    method finish =
      finished <- true;
      match ipos with
        | 1 -> raise(Error Bad_encoding)
        | 2 ->
            self#ensure_capacity 1;
            obuf.[oend]   <- Char.chr(ibuf.(0) lsl 3 + ibuf.(1) lsr 2);
            oend <- oend + 1;
        | 3 -> raise(Error Bad_encoding)
        | 4 ->
            self#ensure_capacity 2;
            obuf.[oend]   <- Char.chr(ibuf.(0) lsl 3 + ibuf.(1) lsr 2);
            obuf.[oend+1] <- Char.chr((ibuf.(1) land 3) lsl 6 + ibuf.(2) lsl 1 + ibuf.(3) lsr 4);
            oend <- oend + 2;
        | 5 ->
            self#ensure_capacity 3;
            obuf.[oend]   <- Char.chr(ibuf.(0) lsl 3 + ibuf.(1) lsr 2);
            obuf.[oend+1] <- Char.chr((ibuf.(1) land 3) lsl 6 + ibuf.(2) lsl 1 + ibuf.(3) lsr 4);
            obuf.[oend+2] <- Char.chr((ibuf.(3) land 15) lsl 4 + ibuf.(4) lsr 1);
            oend <- oend + 3;
        | 6 -> raise(Error Bad_encoding)
        | 7 ->
            self#ensure_capacity 4;
            obuf.[oend]   <- Char.chr(ibuf.(0) lsl 3 + ibuf.(1) lsr 2);
            obuf.[oend+1] <- Char.chr((ibuf.(1) land 3) lsl 6 + ibuf.(2) lsl 1 + ibuf.(3) lsr 4);
            obuf.[oend+2] <- Char.chr((ibuf.(3) land 15) lsl 4 + ibuf.(4) lsr 1);
            obuf.[oend+3] <- Char.chr((ibuf.(4) land 1) lsl 7 + ibuf.(5) lsl 2 + ibuf.(6) lsr 3);
            oend <- oend + 4;
        | _ -> ()

    method wipe =
      Array.fill ibuf 0 8 0; output_buffer#wipe
  end

let decode () = new decode
