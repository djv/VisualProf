import Data.Sequence
 
insert_million 0 sequence = sequence
insert_million n sequence = insert_million (n - 1)(sequence |> n)
 
main = print (Data.Sequence.length (insert_million 1000000 empty))
