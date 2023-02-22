
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

import Common

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


-- Task 1

column_header_eight_hours = ["Name", "Average Number of Steps"]

{- 
  Transformam Stringurile in numere, apoi le adunam, iar la final facem
  media si cream numarul.
-}
average_steps :: Integer -> Row -> Value
average_steps a = (\x -> printf "%.2f" (compute_average x a)).
                  (foldr (+) 0).
                  (map (\x -> read x :: Integer))

{- 
  Impartim 2 numere si obtinem un Float pe care il folosim pentru printf. 
  Vom mai folosi si in continuare aceasta functie.
-}
compute_average :: Integer -> Integer -> Float
compute_average x y = (fromInteger x) / (fromInteger y)

compute_average_steps :: Table -> Table
compute_average_steps m = column_header_eight_hours:
                    (map (\x -> [(head x), (average_steps 8 (tail x))]) (tail m))


-- Task 2

-- Number of people who have achieved their goal:

{-
  Mai intai aplicam un map pe tabel. Astfel, pentru fiecare linie ignoram
  head-ul Row-ului intrucat este un nume. Apoi, mai aplicam un map pentru a
  parsa Stringurile in Int-uri, iar apoi le facem suma cu un foldr. Dupa ce
  am transformat tabela intr-o lista de Inturi, aplicam un filter ca sa gasim
  doar persoanele care si-au indeplinit goal-ul, iar apoi pur si simplu vedem
  lungimea acestui sir.
-}
get_passed_people_num :: Table -> Int
get_passed_people_num m = length (filter (>= 1000)
                                 (map ((foldr (+) 0).
                                 (\x -> row_to_int (tail x))) (tail m)))

-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = get_float_with_2_digits 
                                ((fromInteger (toInteger (get_passed_people_num m))) /
                                (fromInteger (toInteger (number_of_people m))))


--Numaram cate persoane sunt in tabela, ignorand header-ul.
number_of_people :: Table -> Int
number_of_people m = (length (tail m))

-- Transformam un Float cu oricate zecimale intr-unul cu exact 2.
get_float_with_2_digits :: Float -> Float
get_float_with_2_digits x = read (printf "%.2f" x)::Float

-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = get_float_with_2_digits
              ((fromInteger (toInteger (suma_table m))) /
              (fromInteger (toInteger (number_of_people m))))

{-
  Pentru a face suma facem un fold pentru a aduna toti pasii de pe fiecare rand.
  Pentru a aduna pasii de pe un rand folosim un alt fold dupa ce am transformat
  Row-ul in Int. Cand facem suma, ignoram prima linie din tabel deoarece este
  un header.
-}
suma_table :: Table -> Int
suma_table m = foldr (\x acc -> acc + (foldr (+) 0 (row_to_int (tail x)))) 0 (tail m)

-- Transforma un Row intr-un sir de Int-uri
row_to_int :: Row -> [Int]
row_to_int x = map (\y -> read y ::Int) x

-- Task 3

hour_header = ["H10","H11","H12","H13","H14","H15","H16","H17"]

transpose_table :: Table -> Table
transpose_table ([]:_) = []
transpose_table m = (map (head) m):(transpose_table (map (tail) m))

{-
  Aceasta functie va primi ca intrare tabela transpusa, astfel ca primul
  element de pe fiecare linie va fi ora pentru care se calculeaza media,
  deci o vom ignora cu tail atunci cand facem suma de pe fiecare Row.
-}
sum_on_lines :: Table -> [Int]
sum_on_lines m = map (\x -> foldr (+) 0 (tail (row_to_int  x))) m

-- Imparte fiecare Int la numarul de persoane din tabela.
average_per_line :: Int -> [Int] -> [Float]
average_per_line x = map (\y -> get_float_with_2_digits 
                    (compute_average (toInteger y) (toInteger x)))

{-
  Primeste map-ul si creaza randul cu mediile pe ore. Pentru aceasta vom
  lua matricea transpusa,fara primul rand care va deveni randul de nume, iar
  apoi pentru celelalte randuri se face media.
-}
get_average_per_h :: Table -> Row
get_average_per_h m = map (show) (average_per_line (number_of_people m)
                                  (sum_on_lines (tail (transpose_table m))))

get_avg_steps_per_h :: Table -> Table
get_avg_steps_per_h m = hour_header:((get_average_per_h m):[])


-- Task 4

header_active_minutes = ["column","range1","range2","range3"]                      

{- 
  Extragem din tabel doar ultimele 3 coloane care contin efectiv minutele.
  Pentru a folosi pe tabelul nostru trebuie sa ignoram prima linie care
  este de fapt header-ul tabelului.
-}
get_active_minutes_from_table :: Table -> Table
get_active_minutes_from_table m = map (\x -> tail (tail (tail x))) m

-- Numara numarul de elemente de pe un rand care sunt intr-un anumit interval.
number_in_interval :: Row -> Int -> Int -> Int
number_in_interval r lo hi = length (filter (\x -> lo <= x && x < hi) (row_to_int r))

-- Functia primeste un Row si intoarce numarul de persoane din fiecare range.
split_in_ranges :: Row -> Row
split_in_ranges x = (show (number_in_interval x 0 50)):
                    (show (number_in_interval x 50 100)):
                    (show (number_in_interval x 100 500)):[]

{-
  Urmatoarele 3 functii pur si simplu creaza randul specific fiecarui tip de
  minute active dupa care facem impartirea pe range-uri.
-}
get_very_active_minutes :: Table -> Row
get_very_active_minutes m = "VeryActiveMinutes":
                            (split_in_ranges (map (head) m))

get_fairly_active_minutes :: Table -> Row
get_fairly_active_minutes m = "FairlyActiveMinutes":
                              (split_in_ranges (map (\x -> head (tail x)) m))

get_lightly_active_minutes :: Table -> Row
get_lightly_active_minutes m = "LightlyActiveMinutes":
                        (split_in_ranges (map (\x -> head (tail (tail x))) m))

-- Punem cap la cap cele 3 tipuri de minute de activitati intr-un tabel.
create_active_table :: Table -> Table
create_active_table m = (get_very_active_minutes m):
                        (get_fairly_active_minutes m):
                        (get_lightly_active_minutes m):[]

get_activ_summary :: Table -> Table
get_activ_summary m = header_active_minutes:
                  create_active_table (get_active_minutes_from_table (tail m))


-- Task 5

-- Ne definim un tip nou de data dupa care sa sortam tabela
data Persoana = Persoana String Int deriving Eq

-- Extragem de pe un Row o Persoana
toPersoana :: Row -> Persoana
toPersoana x = Persoana (head x) (read (head (tail x))::Int)

fromPersoana :: Persoana -> Row
fromPersoana (Persoana a x) = a:(show x):[]

-- Comparam 2 persoane uitandu-ne la numarul de pasi si apoi la numele lor.
instance Ord Persoana where
  compare (Persoana a x) (Persoana b y)
    | x < y = LT
    | x > y = GT
    | x == y && a < b = LT
    | x == y && a > b = GT
    | otherwise = EQ

header_total_steps = ["Name","Total Steps"]
  
inserare :: Ord a => a -> [a] -> [a]
inserare x [] = [x]
inserare x (y:ys)
  | x <= y = x:y:ys
  | otherwise = y:(inserare x ys)

insertion_sort :: Ord a => [a] -> [a]
insertion_sort [] = []
insertion_sort (x:xs) = inserare x (insertion_sort xs)

{-
  Mai intai transformam toate elementele din tabel in Persoane, ignorand
  prima linie care este header-ul. Apoi sortam aceasta lista de persoane,
  iar apoi transformam toate persoanele inapoi in Row-uri.
-}
get_ranking :: Table -> Table
get_ranking m = header_total_steps:
                (map (fromPersoana) 
                (insertion_sort (map (toPersoana) (tail m))))

-- Task 6

header_average_4h = ["Name","Average first 4h","Average last 4h","Difference"]

{- 
  Functia extrage dintr-o linie elementele din intervalul celor 2 parametri.
  Spre exemplu, pentru parametrii 1 4 row, vom extrage din rand primele 4
  elemente.
-}
extract_certain_col :: Int -> Int -> Row -> Row
extract_certain_col x y r = op x y r 1 []
                        where op :: Int -> Int -> Row -> Int -> Row -> Row
                              op x y [] _ acc = acc
                              op x y (z:zs) acc1 acc2
                                | acc1 >= x &&  acc1 <= y = op x y zs (acc1 + 1) (acc2 ++ [z])
                                | acc1 >= y = acc2
                                | otherwise = op x y zs (acc1 + 1) acc2 

-- Functia face diferenta dintre media primelor 4 ore, si ultimelor 4 ore.
difference_average_4h :: Row -> Float
difference_average_4h r = abs 
                      ((read 
                      (average_steps 4 (extract_certain_col 2 5 r))::Float) - 
                      (read (average_steps 4 (extract_certain_col 6 9 r))::Float))

-- Primim un sir si construim randul final, cu nume si cu average-uri.
calculate_avg_columns :: Row -> Row
calculate_avg_columns r = (head r):
                          (average_steps 4 (extract_certain_col 2 5 r)):
                          (average_steps 4 (extract_certain_col 6 9 r)):
                          (printf "%.2f" (difference_average_4h r)):[]

sort_by_difference :: Table -> Table
sort_by_difference = sortBy f
                where f r1 r2
                        | difference_average_4h r1 > difference_average_4h r2 = GT
                        | difference_average_4h r1 < difference_average_4h r2 = LT
                        | difference_average_4h r1 == difference_average_4h r2 &&
                          ((head r1) > (head r2)) = GT
                        | difference_average_4h r1 == difference_average_4h r2 &&
                          ((head r1) < (head r2)) = LT
                        | otherwise = EQ

{-
  Punem la inceput header-ul tabelului, iar apoi, dupa ce am sortat 
  randurile, aplicam pe fiecare element functia calculate_avg_columns
  care primeste un Row si returneaza un Row cu nume si cele 3 average-uri.
-}
get_steps_diff_table :: Table -> Table
get_steps_diff_table m = header_average_4h:
                         (map (calculate_avg_columns) 
                         (sort_by_difference (tail m)))


-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (\x -> row_map f x) m

-- Este un map care se aplica fiecarui element de pe un rand.
row_map :: (Value -> Value) -> Row -> Row
row_map f [] = []
row_map f (x:xs) = (f x):(row_map f xs)


-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s:(map (f) m)

{-
  Mai intai, punem numele, iar apoi folosim un foldr pentru a aduna numerele
  pe care mai intai le transformam din String in Float, iar apoi transformam
  rezultatul inapoi in Value cu printf.
-}
get_sleep_total :: Row -> Row
get_sleep_total r = (head r):
                    (printf "%.2f"
                    (foldr (\x acc -> acc + (read x :: Float)) 0 (tail r))):[]

{-
    TASK SET 2
-}

-- Task 1

-- Primim un nume de coloana si obtinem numarul coloanei.
find_column_number :: ColumnName -> Table -> Int
find_column_number n t = f n (head t) 1
                            where 
                                f n row acc
                                    | row == [] = 0 -- Nu exista coloana
                                    | n == head row = acc
                                    | otherwise = f n (tail row) (acc + 1)

get_element_from_column :: ColumnName -> Table -> Row -> Value
get_element_from_column name table row = if (find_column_number name table) == 0
                                         then ""
                                         else head (drop ((find_column_number name table) - 1) row)

{-
    Vedem daca valorile sunt numere si daca da, le comparam, altfel le comparam
    lexicografic ca siruri de caractere. Numerele le vom compara ca Float-uri
    pentu a acoperi si acest caz.
-}
compare_values :: Value -> Value -> Ordering
compare_values x y
    | x == "" && y == "" = EQ
    | x == "" = LT
    | y == "" = GT
    | ((head x) >= '0' && (head x) <= '9') = compare (read x::Float) (read y::Float)
    | otherwise = compare x y

compare_row :: ColumnName -> Table -> Row -> Row -> Ordering
compare_row col t r1 r2 = if compare_values (get_element_from_column col t r1) (get_element_from_column col t r2) == EQ
                          then compare_values (head r1) (head r2)
                          else compare_values (get_element_from_column col t r1) (get_element_from_column col t r2)


tsort :: ColumnName -> Table -> Table
tsort column table = (head table):(sortBy f (tail table))
                                    where f r1 r2 = compare_row column table r1 r2

-- Task 2

equal_column_names :: [ColumnName] -> [ColumnName] -> Bool
equal_column_names [] [] = True
equal_column_names (x:xs) [] = False
equal_column_names [] (x:xs) = False
equal_column_names (x:xs) (y:ys)
    | x == y = (equal_column_names xs ys)
    | otherwise = False

{-
    Daca numele coloanelor este acelasi, atunci concatenam t2 la 
    finalul lui t1, ignoradnd header-ul.
-}
vunion :: Table -> Table -> Table
vunion t1 t2 = if equal_column_names (head t1) (head t2) == True
               then t1 ++ (tail t2)
               else t1

-- Task 3

empty_row :: Int -> Row
empty_row x = f x []
                where 
                    f 0 acc = acc
                    f x acc = f (x - 1) ("":acc)

{-
    Functia primeste 2 Row-uri si lungimea fiecaruia. Daca unul dintre ele
    este vid, atunci concatenam la celalalta un Row plin de valori nule, adica "".
-}
row_union :: Row -> Int -> Row -> Int -> Row
row_union [] x [] y = []
row_union r1 x [] y = r1 ++ (empty_row y)
row_union [] x r2 y = (empty_row x) ++ r2
row_union r1 x r2 y = r1 ++ r2

hunion :: Table -> Table -> Table
hunion t1 t2 = f t1 (length (head t1)) t2 (length (head t2))
                where
                    f [] _ [] _ = []
                    f (x:xs) a [] b = (row_union x a (empty_row b) b):(f xs a [] b)
                    f [] a (x:xs) b = (row_union (empty_row a) a x b):(f [] a xs b)
                    f (x:xs) a (y:ys) b = (row_union x a y b):(f xs a ys b)

-- Task 4

column_in_column_names :: [ColumnName] -> ColumnName -> Int
column_in_column_names [] col = 0
column_in_column_names l col = f l col 1
                                where 
                                    f [] col acc = 0
                                    f (x:xs) col acc
                                        | x == col = acc
                                        | otherwise = f xs col (acc + 1)


{- 
    Functia primeste un nume de coloana si un row, si inlocuieste valoarea de pe acea
    coloana cu una data ca parametru. Pentru acest lucru, luam primele elemente din
    rand, pana ajungem la coloana cautata, adaugam valoarea dorita, iar apoi adaugam
    finalul randului.
-}
replace_value_on_column :: [ColumnName] -> ColumnName -> Row -> Value -> Row
replace_value_on_column xs x r "" = r
replace_value_on_column xs x r val = if (column_in_column_names xs x) == 0
                                     then r
                                     else (take ((column_in_column_names xs x) - 1) r) ++
                                          (val:(drop (column_in_column_names xs x) r))

{- 
    Functia extrage dintr-un tabel toate randurile care contin pentru acelasi nume de coloana ca cel cautat,
    valoarea cautata.
-}
search_value_by_col_name :: ColumnName -> Value -> Table -> [Row]
search_value_by_col_name name x t = filter (\r -> (head (drop ((find_column_number name t) - 1) r)) == x) t

{-
    Functia primeste 2 headere de tabel si coloana dupa care se face mergeul. Mai primim si tabelul
    2 pentru folosi functia find_column_number. Mai avem nevoie de cele 2 randuri ce trebuie sa
    fie "merge-uite". Pentru aceasta functie, daca gasim un nume de coloana nou, atunci adaugam la
    finalul randului 1, valoarea din randul 2. Daca in schimb, coloana are acelasi nume ca o coloana
    din prima lista de nume, atunci inlocuim acel element cu elementul corespunzator din t2.
-}
combine_rows :: [ColumnName] -> [ColumnName] -> ColumnName -> Table -> Row -> Row -> Row
combine_rows n1 n2 name t2 r1 [] = []
combine_rows n1 n2 name t2 r1 r2 = foldl (\acc x -> f n1 x name t2 acc r2) r1 n2
                                where 
                                    f n1 x name t2 acc r2
                                        | x == name = acc
                                        | column_in_column_names n1 x == 0 =
                                             (acc ++ ((get_element_from_column x t2 r2):[]))
                                        | otherwise = (replace_value_on_column n1 x acc
                                                      (head (drop ((find_column_number x t2) - 1) r2)))
{- 
    Am dat drop la primele coloane pana am ajuns la cea corecta din t2, iar apoi aceasta
    suprascrie valoarea veche.
-}

-- Daca gasim un nume de coloana nou, il adaugam la final.
create_header_for_tjoin :: [ColumnName] -> [ColumnName] -> [ColumnName]
create_header_for_tjoin [] _ = []
create_header_for_tjoin col1 [] = col1
create_header_for_tjoin col1 col2 = foldl (\acc x -> f col1 x acc) col1 col2
                                    where f col1 x acc
                                            | (column_in_column_names col1 x) /= 0 = acc
                                            | otherwise = (acc ++ [x])

{-
    In primul rand avem header-ul. Apoi, luam fiecare rand din t1 si facem urmatorul lucru. Cautam
    dupa cheie coloana dorita si extragem de pe rand elementul care se afla pe acea coloana. Dupa
    aceea, cautam acest element in tabelul 2 si obtinem o lista de intrari in tabela 2 care au
    aceiasi valoare pentru cheie. Pentru toate acestea, facem un foldr, combinam randul
    initial cu cel gasit, si introducem apoi intr-un tabel final.
-}
tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = (create_header_for_tjoin (head t1) (head t2)):
                         (foldr (\x acc -> 
                            foldr (\y acc2 -> 
                            (combine_rows (head t1) (head t2) key_column t2 x y):acc2) acc
                         (search_value_by_col_name key_column 
                         (head (drop ((column_in_column_names (head t1) key_column) - 1) x)) t2))
                         [] (tail t1))

{-
    Pentru a gasi valoarea de pe o anumita coloana, cautam coloana prin numele de coloane din
    header, iar apoi eliminam primele coloane, pana la aceasta, iar elementul care se afla
    acum primul in lista este fix valoarea de pe coloana cautata.
-}

-- Task 5

apply_func_on_row :: (Row -> Row -> Row) -> Row -> Table -> Table
apply_func_on_row f row t = map (\x -> f row x) t

{-
    Pe fiecare Row din t1, exceptand header-ul, folosim functia apply_func_on_row care primeste
    o operatie intre 2 Row-uri un Row si aplica aceasta operatie pentru toate randurile
    din tabelul primit ca parametru. Astfel, obtinem un vector de tabele, fiecare
    reprezentand produsul cartezian dintre un singur rand din t1 si t2. Apoi,
    concatenam toate aceste tabele si adaugam header-ul.
-}
cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = (new_column_names):
                            (foldr (++) [] 
                            (map (\x -> apply_func_on_row new_row_function x (tail t2)) (tail t1)))

-- Task 6

-- Functia primeste un sir de numere si extrage elementele respective de pe rand.
extract_values_from_row :: [Int] -> Row -> Row
extract_values_from_row xs r = foldr (\x acc -> (head (drop (x - 1) r):acc)) [] xs

{-
    Functia primeste un sir de numere si extrage coloanele respective. Lista de numere
    trebuie sa fie sortata in prealabil, pentru a nu rearanja coloanele cautate.
-}
extract_columns_from_table :: [Int] -> Table -> Table
extract_columns_from_table xs t1 = foldr (\x acc -> (extract_values_from_row xs x):acc) [] t1

{-
    Ne folosim de functia extract_columns_from_table care ne extrage coloanele daca
    ii oferim o lista de cu numerele coloanelor cautate. Pentru a crea lista de numere
    ne folosim de functia find_column_number pentru a gasi numarul unei coloane dupa
    nume. Dupa aceea, sorta vectorul pentru a ne asigura ca nu vor aparea coloanele
    intr-o ordine aleatorie.
-}
projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = extract_columns_from_table 
                                (insertion_sort 
                                (map (\x -> find_column_number x t) columns_to_extract)) t

-- Task 7

-- Functia primeste un rand si un numar x, si pimim valoarea de pe acea coloana.
get_value_on_column :: Row -> Int -> Value
get_value_on_column [] x = "" 
get_value_on_column r x = if x > (length r) 
                          then ""
                          else head (drop (x - 1) r)

{-
    Functia cauta numarul coloanei cautate in tabel, iar apoi extrage valoarea de pe
    acea coloana din rand. Odata ce avem valoarea, verificam conditia.
-}
apply_condition_on_row :: (Value -> Bool) -> ColumnName -> Table -> Row -> Bool
apply_condition_on_row f col t row = f (get_value_on_column row (find_column_number col t))

{-
    Aplicam un filtru pentru a verifica conditia, ignorand header-ul tabelului, pe care il
    adaugam apoi la inceput.
-}
filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t):
                                     (filter 
                                     (\x -> apply_condition_on_row condition key_column t x)
                                     (tail t))

-- Task 8 TO_DO


{-
    TASK SET 3
-}


-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5

instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

class Eval a where
  eval :: a -> QResult

-- Functia intoarce o lista cu toate valorile de pe o anumita coloana
get_column_from_table :: ColumnName -> Table -> [Value]
get_column_from_table name t = if find_column_number name t == 0
                               then []
                               else map (\x -> get_element_from_column name t x) t

{-
  Pentru a inrola Query in Eval, unii constructori primesc ca parametru un alt
  Query. Cum in cerinta ni se specifica faptul ca in general este un tabel, putem
  aplica eval pe el si obtinem un QResult astfel. Ca sa folosim functiile definite
  anterior, avem nevoie de Table nu de QResult, asa ca aceasta functie face
  transformarea.
-}
qresult_to_table :: QResult -> Table
qresult_to_table (Table t) = t
qresult_to_table (List l) = []

-- Pentru RowMap am folosit tail de table deoarece rmap-ul aplica functia inclusiv pe header
instance Eval Query where
  eval (FromTable t) = (Table t)
  eval (AsList colname q) = List (tail (get_column_from_table colname (qresult_to_table (eval q))))
  eval (Sort colname q) = Table (tsort colname (qresult_to_table (eval q)))
  eval (ValueMap op q) = Table (vmap op (qresult_to_table (eval q)))
  eval (RowMap op colnames q) = Table (rmap op colnames (tail (qresult_to_table (eval q))))
  eval (VUnion query1 query2) = Table (vunion (qresult_to_table (eval query1))
                                       (qresult_to_table (eval query2)))
  eval (HUnion query1 query2) = Table (hunion (qresult_to_table (eval query1))
                                       (qresult_to_table (eval query2)))
  eval (TableJoin colname query1 query2) = Table (tjoin colname (qresult_to_table (eval query1))
                                                  (qresult_to_table (eval query2)))
  eval (Cartesian op colnames query1 query2) = Table (cartesian op colnames 
                                                      (qresult_to_table (eval query1))
                                                      (qresult_to_table (eval query2)))
  eval (Projection colnames q) = Table (projection colnames (qresult_to_table (eval q)))
  eval (Filter (FieldEq name1 name2) q) = Table (filter_table
                                   (feval (head (qresult_to_table (eval q))) ((FieldEq name1 name2)::(FilterCondition String)))
                                   (qresult_to_table (eval q)))
  eval (Filter condition q) = Table (filter_table
                              (feval (head (qresult_to_table (eval q))) condition)
                              (qresult_to_table (eval q)))
  eval (Graph op q) = Table (create_graph (qresult_to_table (eval q)) op)

-- 3.2 & 3.3

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

-- Functia gaseste un string intr-o lista, extrage indexul si afiseaza valoarea
-- de la indexul respectiv din randul primit ca parametru.
find_element_by_string :: [String] -> String -> Row -> Value
find_element_by_string [] name row = ""
find_element_by_string (x:xs) name row
  | x == name = (head row)
  | otherwise = (find_element_by_string xs name (tail row))

-- Filtram randurile dintr-un tabel dupa o conditie, lasand header-ul asa cum era
filter_table :: FilterOp -> Table -> Table
filter_table op t = (head t):(filter op (tail t))

value_in_row :: Eq a => a -> [a] -> Bool
value_in_row x [] = False
value_in_row x (y:ys)
  | x == y = True
  | otherwise = (value_in_row x ys)

instance FEval Float where
  feval colnames (Eq colname x) = \r -> (read (find_element_by_string colnames colname r)::Float) == x
  feval colnames (Lt colname x) = \r -> (read (find_element_by_string colnames colname r)::Float) < x
  feval colnames (Gt colname x) = \r -> (read (find_element_by_string colnames colname r)::Float) > x
  feval colnames (In colname l) = \r -> value_in_row (read (find_element_by_string colnames colname r)::Float) l
  feval colnames (FNot condition) = \r -> not (feval colnames condition r)
  feval colnames (FieldEq name1 name2) = \r -> (read (find_element_by_string colnames name1 r)::Float) ==
                                               (read (find_element_by_string colnames name2 r)::Float)

instance FEval String where
  feval colnames (Eq colname x) = \r -> (find_element_by_string colnames colname r) == x
  feval colnames (Lt colname x) = \r -> (find_element_by_string colnames colname r) < x
  feval colnames (Gt colname x) = \r -> (find_element_by_string colnames colname r) > x
  feval colnames (In colname l) = \r -> value_in_row (find_element_by_string colnames colname r) l
  feval colnames (FNot condition) = \r -> not (feval colnames condition r)
  feval colnames (FieldEq name1 name2) = \r -> (find_element_by_string colnames name1 r) ==
                                               (find_element_by_string colnames name2 r)

-- 3.4

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

maybe_to_value :: Maybe Value -> Value
maybe_to_value Nothing = ""
maybe_to_value (Just v) = v

graph_header = ["From", "To", "Value"]

{-
  Functia primeste un rand si un tabel, iar apoi ne plimbam prin tabel sa vedem
  daca trebuie sau nu sa cream o muchie. Daca rezultatul operatiei EdgeOp nu este
  Nothing, atunci verificam daca From e mai mic lexicografic decat To, si daca nu,
  atunci pur si introducem in tabel muchia cu cele 2 noduri inversate.
-}
apply_op_on_row :: Row -> Table -> EdgeOp -> Table
apply_op_on_row row t op = foldr (\row2 acc -> f row row2 acc op) [] t
                           where f row1 row2 acc op
                                  | (op row1 row2) == Nothing = acc
                                  | otherwise = if (head row1) < (head row2)
                                                then [(head row1), (head row2), (maybe_to_value (op row1 row2))]:acc
                                                else [(head row2), (head row1), (maybe_to_value (op row1 row2))]:acc

{-
  Functia primeste un tabel o operatie si un acumulator. Aceasta functie creaza
  un graf creand toate muchiile posibile pentru fiecare nod nou. Pentru un rand,
  ne uitam la ce muchii noi pot sa apara cu randurile de dupa el, intrucat muchiile
  cu randurile precedente au fost deja create.
-}
create_graph_sequentially :: Table -> EdgeOp -> Table -> Table
create_graph_sequentially [] op acc = acc
create_graph_sequentially (x:xs) op acc = create_graph_sequentially xs op (acc ++ (apply_op_on_row x xs op))

--Functia creaza un graf dupa ce primeste un tabel si o operatie.
create_graph :: Table -> EdgeOp -> Table
create_graph t op = (graph_header):(create_graph_sequentially (tail t) op [])

-- 3.5

calculate_distance :: Row -> Row -> Int
calculate_distance row1 row2  = foldr (\x acc -> f x acc) 0 (zipWith (==) (tail row1) (tail row2))
                                where f x acc
                                        | x == True = acc + 1
                                        | otherwise = acc

distance_op row1 row2
  | (calculate_distance row1 row2) == 0 = Nothing
  | otherwise = Just (show (calculate_distance row1 row2))

{-
  Mai intai transformam tabelul in graf, folosind ca Value distanta. Apoi, filtram
  valorile din tabel sa fie mai mari ca 4, adica >= 5. Apoi, la final trebuie doar
  sa sortam randurile astfel obtinute dupa Value.
-}
similarities_query :: Query
similarities_query = Sort "Value" (Filter (Gt "Value" (read "4"::Float)) (Graph distance_op (FromTable eight_hours)))

-- 3.6 (Typos)

-- Pentru a calcula distanta dintre 2 nume vom folosi distanta Levenshtein.
all_zeros = 0:all_zeros

-- Functia pur si simplu creaza o lista cu elementele de la 0 la n, unde n e
-- lungimea primului cuvant.
create_first_row_of_dp :: String -> [Int]
create_first_row_of_dp s = f s 1 [0]
                           where
                             f [] _ acc = acc
                             f (x:xs) i acc = f xs (i + 1) (acc ++ [i])

{-
  Functia primeste randul anterior, elementul antrior elementului curent pe
  care vrem sa-l calculam (d[i][j-1] daca am gandi ca matrice), iar apoi
  indicii elementului calculat si stringurile pe care trebuie sa calculam
  distanta. Functia se bazeaza pe algoritmul lui Levenshtein utilizand
  programare dinamica. Aceasta are relatia de recurenta:
  min (d[i][j-1]+1, d[i-1][j-1]+cost, d[i-1][j]), unde cost e 0 daca
  literele de la indicele i din s2 si j din s1 coincid. Pentru s1 ne
  vom plimba pe coloane, iar pentru s2 pe linii.
-}
compute_cell_of_dp :: [Int] -> Int -> Int -> Int -> String -> String -> Int
compute_cell_of_dp prev_row prev i j s1 s2
  | (s1 !! (j - 1)) == (s2 !! (i - 1)) = minimum [(prev_row !! (j - 1)), (prev_row !! j) + 1, prev + 1]
  | otherwise = minimum [(prev_row !! (j - 1)) + 1, (prev_row !! j) + 1, prev + 1]

{-
  Functia primeste randul anterior randului pe care vrem sa-l calculam, indicele
  randului curent si cele stringuri. Acesta creaza randul curent utilizand
  recurenta lui Levenshtein.
-}
compute_random_row_of_dp :: [Int] -> Int -> String -> String -> [Int]
compute_random_row_of_dp prev_row i s1 s2 = i:(f prev_row i i 1 s1 s2 [])
                                        where
                                          f prev_row prev i j s1 s2 acc
                                            | j > (length s1) = acc
                                            | otherwise = f prev_row 
                                                    (compute_cell_of_dp prev_row prev i j s1 s2) 
                                                    i (j + 1) s1 s2 
                                                    (acc ++ 
                                                    ((compute_cell_of_dp prev_row prev i j s1 s2):[]))

-- Functia calculeaza matricea de programare dinamica pentru distanta Levenshtein.
compute_lev_matrix :: [Int] -> String -> String -> [[Int]]
compute_lev_matrix first_row s1 s2 = first_row:(f first_row 1 s1 s2 [])
                                     where 
                                       f prev_row i s1 s2 acc
                                        | i > (length s2) = acc
                                        | otherwise = f (compute_random_row_of_dp prev_row i s1 s2)
                                          (i + 1) s1 s2
                                          (acc ++ ((compute_random_row_of_dp prev_row i s1 s2):[]))

-- Functia creaza matricea si extrage elementul din coltul drepta jos. Cum
-- pe linii este s2, iar pe coloane s1, ultima linie este length s2.
lev_distance :: String -> String -> Int
lev_distance s1 s2 = (compute_lev_matrix (create_first_row_of_dp s1) s1 s2) !! (length s2) !! (length s1)

value_is_in_row :: Value -> Row -> Bool
value_is_in_row _ [] = False
value_is_in_row x (y:ys) = if x == y then True else (value_is_in_row x ys)

-- Functia returneaza prima coloana din tabel. Daca tabelul este o coloana
-- atunci transforma tabelul intr-un row.
table_to_row :: Table -> Row
table_to_row t = foldr (\x acc -> (head x):acc) [] t

-- Functia primeste un rand si elimina din acesta toate valorile care se regasesc
-- si in al doilea rand.
filter_correct_names :: Row -> Row -> Row
filter_correct_names row correct_row = filter (\x -> (value_is_in_row x correct_row) == False) row

{-
  Pentru a corecta typo-ul trecem prin toate valorile din randul primit ca parametru
  care reprezinta numele corecte, calculam distanta Levenshtein, si cel care o are
  pe cea mai buna este valoarea cautata fara typo. Ca valoare initiala cu care
  comparam celelalte distante consideram distanta Levenshtein dintre valoare
  si primul element de pe rand.
-}
correct_typo :: Value -> Row -> Value
correct_typo typo row = f typo (tail row) (lev_distance typo (head row)) (head row)
                        where
                          f typo [] cur_best val_best = val_best
                          f typo (x:xs) cur_best val_best
                            | (lev_distance typo x) < cur_best = f typo xs (lev_distance typo x) x
                            | otherwise = f typo xs cur_best val_best

{-
  Functia primeste un rand cu valorile gresite si un rand cu cele corecte,
  iar apoi fiecare valoare gresita este corectata. Se va intoarce un
  tabel care contine pe un rand valoarea cu typo si valoarea corectata.
-}
correct_typos_from_row :: Row -> Row -> Table
correct_typos_from_row row correct_row = foldr (\x acc -> [x, (correct_typo x correct_row)]:acc) [] row

-- Functia primeste un typo si cauta in tabelul de corectat greseli. Acesta
-- se va gasi in acest tabel deoarece toate typo-urile sunt incluse in el.
find_correct_variant :: Value -> Table -> Value
find_correct_variant v (x:xs)
  | (head x) == v = head (tail x)
  | otherwise = find_correct_variant v xs

{-
  Functia primeste randul pe care vrem sa-l corectam, indicele coloanei care trebuie
  sa fie corectata, coloana initiala care contine numele corecte si tabelul
  de corecturi ale typo-urilor. Mai intai, verificam daca valoarea pe care o primeste
  este deja corecta, daca da, atunci randul ramane neschimbat. Daca nu, atunci
  cautam in tabelul de typo-uri valoarea noastra. Stim ca tabelul de typo-uri are
  pe prima coloana cuvintele inainte de corectare, iar pe a 2-a pe cele corecte.
-}
correct_row :: Row -> Int -> Row -> Table -> Row
correct_row row i init_col typo_table = (take (i - 1) row) ++ 
                                        ((f (head (drop (i - 1) row)) 
                                        init_col typo_table):
                                        (drop i row))
                                        where 
                                          f x init_col typo_table
                                            | (value_is_in_row x init_col) == True = x
                                            | otherwise = (find_correct_variant x typo_table)

-- Functia extrage o coloana data dintr-un tabel
extract_col :: String -> Table -> Row
extract_col col t = table_to_row (projection [col] t)

-- Functia extrage din tabel o coloana cu typouri, ignorand valorile care erau
-- initial corecte.
typo_row :: String -> Table -> Table -> Row
typo_row col t1 t2 = filter_correct_names (extract_col col t1)
                                          (extract_col col t2)

{-
  Functia aplica un map pe tabel, ignorand header-ul, si corecteaza fiecare rand
  din tabel in parte.
-}
correct_table :: String -> Table -> Table -> Table
correct_table col csv1 csv2 = (head csv1):
                              (map (\x -> correct_row x 
                              (find_column_number col csv1) 
                              (extract_col col csv2) 
                              (correct_typos_from_row 
                              (typo_row col csv1 csv2) (extract_col col csv2)))
                              (tail csv1))
