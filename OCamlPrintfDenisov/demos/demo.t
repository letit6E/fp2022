  $ cat << EOF | ./demo.exe -
  > let id = fun x -> x
  > let ignore = printf
  > ;;
  > EOF
  val id : ('1 -> '1) = <fun>
  val ignore : ('3 format -> '3) = <fun>

  $ cat << EOF | ./demo.exe -
  > let ignore = printf "%b\n" true
  > ;;
  > EOF
  true
  val ignore : unit = ()

  $ cat << EOF | ./demo.exe -
  > let ignore = printf "%b\n" "text"
  > ;;
  > EOF
  Error: unification failed on bool and string

  $ cat << EOF | ./demo.exe -
  > let ignore = printf "abc\n"
  > ;;
  > EOF
  abc
  val ignore : unit = ()

  $ cat << EOF | ./demo.exe -
  > let ignore = printf "%d\n" 1 
  > ;;
  > EOF
  1
  val ignore : unit = ()

  $ cat << EOF | ./demo.exe -
  > let ignore = printf "%d %d\n" 42 
  > ;;
  > EOF
  val ignore : (int -> unit) = <fun>

  $ cat << EOF | ./demo.exe -
  > let myprintf = printf
  > let ignore = myprintf "%d\n" 2
  > ;;
  > EOF
  2
  val myprintf : ('1 format -> '1) = <fun>
  val ignore : unit = ()

  $ cat << EOF | ./demo.exe -
  > let foo = fun n -> printf (match n>0 with | true -> "positive %d" | false -> "negative %d") n;;
  > EOF
  val foo : (int -> unit) = <fun>

  $ cat << EOF | ./demo.exe -
  > let foo = fun n ->
  >   let fmt = (match n>0 with | true -> "positive %d" | false -> "negative %d") in
  >   printf fmt n
  > ;;
  > EOF
  val foo : (int -> unit) = <fun>

  $ cat << EOF | ./demo.exe -
  > let temp = fun eta ->
  >   printf "%s %d" eta
  > let debug = fun fmt -> match true with | true -> printf fmt | false -> printf fmt
  > ;;
  > EOF
  val temp : (string -> (int -> unit)) = <fun>
  val debug : ('10 format -> '10) = <fun>

  $ cat << EOF | ./demo.exe -
  > let s = fun  s -> fun f -> fun g -> fun x -> f x (g x)
  > let twice = fun x -> (x,x)  
  > ;;
  > EOF
  val s : ('7 -> (('8 -> ('9 -> '10)) -> (('8 -> '9) -> ('8 -> '10)))) = <fun>
  val twice : ('12 -> ('12 * '12)) = <fun>



  $ cat << EOF | ./demo.exe -
  > let rec print_list = fun print_elm -> fun l ->
  > match l with
  > | [] -> ()
  > | hd :: tl -> printf "%a; %a" print_elm hd (print_list print_elm) tl
  > let ignore = printf "[%a]\n" (print_list (printf "%d")) (1 :: 19 :: -4 :: [])
  > ;;
  > EOF
  [1; 19; -4; ]
  val print_list : (('4 -> unit) -> ('4 list -> unit)) = <fun>
  val ignore : unit = ()

  $ cat << EOF | ./demo.exe -
  > let rec print_list = fun print_elm -> fun l ->
  > match l with
  > | [] -> ()
  > | hd :: tl -> printf "%a; %a" print_elm hd (print_list print_elm) tl
  > let ignore = printf "[%a]\n" (print_list (printf "%d")) (false :: true :: [])
  > ;;
  > EOF
  Error: unification failed on int and bool

  $ cat << EOF | ./demo.exe -
  > let rec map = fun f -> fun l -> match l with 
  > | [] -> []
  > | a::l -> let r = f a in r :: map f l
  > let n = 5 :: 7 :: 9:: []
  > let sq = fun x -> x * x
  > let sqs = map sq n
  > ;;
  > EOF
  val map : (('4 -> '8) -> ('4 list -> '8 list)) = <fun>
  val n : int list = [5; 7; 9]
  val sq : (int -> int) = <fun>
  val sqs : int list = [25; 49; 81]

  $ cat << EOF | ./demo.exe -
  > let rec fix = fun f -> fun eta -> f (fix f) eta
  > let fact =
  > fix (fun fact -> fun n ->
  >  match n with
  >  | 0 -> 1
  >  | m -> let rez = m * fact (n - 1) in let rrr = printf "fact %d = %d\n" n rez in rez )
  > let fact5 = fact 5
  > ;;
  > EOF
  fact 1 = 1
  fact 2 = 2
  fact 3 = 6
  fact 4 = 24
  fact 5 = 120
  val fix : ((('2 -> '5) -> ('2 -> '5)) -> ('2 -> '5)) = <fun>
  val fact : (int -> int) = <fun>
  val fact5 : int = 120
