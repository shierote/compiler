(* ʸ�����ʲ��Υȡ��������ʬ�򤷤�ɽ������ץ����
   EQ:  =    LEQ: <=   LT: <   PLUS: +  IF: if THEN: then  
   INT: 0 |([1-9][0-9]* )   ID: [a-z]([a-z]|[0-9])*
*)
(* �ȡ��������� *)
type token = INVALID | EQ | LEQ | LT | PLUS | IF | THEN | INT | ID

(* ����ʸ������Ǽ���Ƥ������ *)
let input_buffer: string ref = ref ""

(* �����ɤ�Ǥ���lexeme(�����ǡˤ���Ƭ���� *)
let pos_start = ref 0

(* ���줫���ɤ�ʸ���ΰ��� *)
let pos_current = ref 0

(* �����ɤ�Ǥ���lexeme��ǺǸ��ǧ�����줿�ȡ����� *)
let last_token = ref INVALID

(* �����ɤ�Ǥ���lexeme��ǺǸ��ǧ�����줿�ȡ�����μ���ʸ���ΰ��� *)
let last_pos = ref 0

(* ���顼���� *)
let report_error pos =
  print_string ("invalid token at position "^(string_of_int pos)^"\n")

(* �Ǹ��ǧ�����줿�ȡ��������������ʸ�����lexeme�� *)
let get_lexeme() =
  let len = !last_pos - !pos_start in String.sub !input_buffer !pos_start len

(* �ȡ������ɽ�� *)
let output_token token =
  match token with
   EQ -> print_string "EQ "
 | LEQ -> print_string "LEQ "
 | LT -> print_string "LT "
 | PLUS -> print_string "PLUS "
 | IF -> print_string "IF "
 | THEN -> print_string "THEN "
 | INT -> (print_string ("INT("^(get_lexeme())^") "))
 | ID -> print_string ("ID("^(get_lexeme())^") ")
 | INVALID -> assert false

(* ��ʸ���ɤߤ��� *)
let readc() =
  let c = !input_buffer.[!pos_current] in (pos_current:= !pos_current+1; c)
(* �ޥå������ȡ�����Ȥ��ξ�����¸ *)
let save token = (last_token := token; last_pos := !pos_current)

(* �ᥤ��ʸ����input�������ꡢ�ȡ��������ʬ�򤷤�ɽ�� *)
let rec main (input: string) =
   input_buffer := (input^"\000"); (* input��ʸ����κǸ��ɽ��"\000"���ɲ� *)
   pos_start := 0; pos_current := 0; last_token := INVALID;
   q0()

and q0 () = (* ������� *)
  match readc() with
   ' ' -> (pos_start := !pos_start+1;
           q0())
  | '=' -> (save EQ; next())
  | '<' -> (save LT; q_lt())
  | '+' -> (save PLUS; next())
  | 'i' -> (save ID; q_i())
  | 't' -> (save ID; q_t())
  | '0' -> (save INT; next())
  | c -> if '1'<=c && c<='9' then (save INT; q_num())
         else if 'a'<= c && c<='z' then (save ID; q_sym())
         else if c='\000' then () (* ʸ����κǸ�ʤ齪λ *)
         else report_error(!pos_current)

and q_lt() =
  match readc() with
    '=' -> (save LEQ; next())
  | _ -> next()

and q_num() = 
  let c= readc() in
    if '0'<=c && c<='9' then (save INT; q_num()) else next()

and q_i() =
  match readc() with
    'f' -> (save IF; q_sym())
  | c -> if ('a'<= c && c<='z')||('0'<=c && c<='9') then (save ID; q_sym())
         else next()

and q_t() = 
  match readc() with
    'h' -> (save ID; q_th())
  | c -> if ('a'<= c && c<='z')||('0'<=c && c<='9') then (save ID; q_sym())
         else next()

and q_th() = 
  match readc() with
    'e' -> (save ID; q_the())
  | c -> if ('a'<= c && c<='z')||('0'<=c && c<='9') then (save ID; q_sym())
         else next()

and q_the() = 
  match readc() with
    'n' -> (save THEN; q_sym())
  | c -> if ('a'<= c && c<='z')||('0'<=c && c<='9') then (save ID; q_sym())
         else next()

and q_sym() =
  let c = readc() in
      if ('a'<= c && c<='z')||('0'<=c && c<='9') then (save ID; q_sym())
      else next()

and next() = 
     if !last_token=INVALID then (* ���顼 *)
        report_error(!pos_current)
     else (* �ȡ������ɽ�����Ƽ��Υȡ�����ν����� *)
       (output_token !last_token; pos_start := !last_pos;
        pos_current := !pos_start; last_token := INVALID;
        q0())
