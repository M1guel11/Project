open Str
open List
open Printf
type simbolo = char option 
type fita = simbolo list 
type estado = int
type transicao = (estado * simbolo) * estado 
type maquina = transicao list * estado list * estado list 
type memoria = estado list * fita 
exception FIM of memoria      

let split_a s = s |> String.split_on_char ' ' 

let b =split_a (read_line())
let ()= iter (printf "%s ") b
