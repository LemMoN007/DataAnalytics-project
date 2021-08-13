{-
    PP Project 2021

    This is where you will write the implementation for the given tasks.
    You can add other modules aswell.
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Tasks where

import Dataset
import Data.List
import Text.Read
import Text.Printf

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
    TASK SET 1
-}

-- functie pentru a calcula Float dintr-un String cu caractere invalide
floatValue :: String -> Float
floatValue "" = 0.0
floatValue "?" = 0.0
floatValue str = read str :: Float

-- primeste o lista de String-uri de tip Float si returneaza suma lor
sum_string :: [String] -> Float
sum_string [] = 0.0
sum_string (x:xs) = (floatValue x) + (sum_string xs)

-- Task 1
compute_exam_grades :: Table -> Table
compute_exam_grades table = map modify table
    where
        modify line
            | line !! 0 == "Nume" = ["Nume", "Punctaj Exam"]
            | otherwise = [line !! 0, printf "%.2f" (exam_grade line)]

        exam_grade :: [String] -> Float
        exam_grade [] = 0.0
        exam_grade list = (sum_string $ init $ tail list) / 4 + (floatValue $ last list)

-- Task 2
-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Int
get_passed_students_num table = foldr (+) 0 (map modify $ tail $ compute_exam_grades table)
    where
        modify el
            -- mapez tabelul cu 1 pentru trecut (>=2.5), 0 picat
            | (read (el!!1) :: Float) >= 2.5 = 1
            | otherwise = 0

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage table = cf / cp
    where 
        cf = fromIntegral (get_passed_students_num table) :: Float
        cp = fromIntegral ((length table) - 1) :: Float

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg table = sum_grades / nr_stud
    where
        -- construiesc tabelul cu note, sar peste prima linie unde sunt numele coloanelor
        -- iar apoi transform in [Float] cu notele obtinute, pe care le adun cu foldr
        sum_grades = foldr (+) 0 $ map parse_float $ tail $ compute_exam_grades table
            where
                parse_float line = read (line!!1) :: Float
        -- numarul de studenti participanti la examen (nu neaparat trecuti)
        nr_stud = fromIntegral ((length table) - 1) :: Float

-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
get_passed_hw_num table = foldr (+) 0 $ map modify $ tail table
    where
        modify el
            | sum_hw el >= 1.5 = 1
            | otherwise = 0

        -- suma temelor
        sum_hw :: [String] -> Float
        sum_hw [] = 0.0
        sum_hw list = floatValue (list!!2) + floatValue (list!!3) + floatValue (list!!4)

-- Task 3
task3_line = ["Q1","Q2","Q3","Q4","Q5","Q6"]
-- mapez tabelul fara prima linie si obtin [[Int]], pe care aplic Transpose
-- pentru a obtine cele 6 liste corespunzatoare celor 6 coloane si folosesc
-- functia 'getAvg' pentru a obtine media, pe care o transform apoi in String
get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs table = [task3_line, map (printf "%.2f") $ map getAvg $ 
                                    transpose $ map modify $ tail table]
    where
        -- elimin restul coloanelor din tabel, iar punctajele le fac Int
        modify line = format_line (init $ tail line)

        -- transforma String in Int iar pentru caracterele invalide pune 0
        format_line :: [String] -> [Int]
        format_line [] = []
        format_line (x:xs)
            | x == "" = 0 : (format_line xs)
            | x == "?" = 0 : (format_line xs)
            | otherwise = (read x :: Int) : (format_line xs)

        -- suma elementelor unei liste de Int
        sumList :: [Int] -> Int
        sumList [] = 0
        sumList (x:xs) = x + (sumList xs)

        -- foloseste functia de suma anterioara si imparte la numarul de intrebari
        getAvg :: [Int] -> Float
        getAvg [] = 0.0
        getAvg list = sum / nr
            where
                sum = fromIntegral (sumList list) :: Float
                nr = fromIntegral ((length table) - 1) :: Float

-- Task 4
task4_line = ["Q","0","1","2"]
get_exam_summary :: Table -> Table
get_exam_summary table = task4_line : (map occurrences $ transpose $ map modify table)
    where
        -- prima coloana Q1,Q2,... iar celelalte sunt formatate
        modify line 
            | line !! 0 == "Nume" = ["Q1","Q2","Q3","Q4","Q5","Q6"]
            | otherwise = format_line (init $ tail line)

        -- inlocuieste toate elementele " " si "?" cu "0"
        format_line :: [String] -> [String]
        format_line [] = []
        format_line (x:xs)
            | x == "" = "0" : (format_line xs)
            | x == "?" = "0" : (format_line xs)
            | otherwise = x : (format_line xs)

        -- un nou [String] cu numarul de aparitii al fiecarei intrebari
        occurrences :: [String] -> [String]
        occurrences [] = []
        occurrences el = [el !! 0, countOcc "0" el, countOcc "1" el, countOcc "2" el]

        -- de cate ori apare un String in [String]
        countOcc :: String -> [String] -> String
        countOcc x list = printf "%.2d" $ length $ filter (==x) list

-- Task 5
get_ranking :: Table -> Table
get_ranking table = sortBy cmp $ compute_exam_grades table
    where
        cmp :: [String] -> [String] -> Ordering
        cmp s1 s2
            -- ma asigur ca ["Nume", "Punctaj Exam"] ramane primul in lista
            | (s1 !! 0) == "Nume" = LT
            | (s2 !! 0) == "Nume" = GT
            -- sortare crescatoare dupa note
            | (s1 !! 1) > (s2 !! 1) = GT
            | (s1 !! 1) < (s2 !! 1) = LT
            -- notele sunt egale deci sortam alfabetic
            | (s1 !! 0) > (s2 !! 0) = GT
            | (s1 !! 0) < (s2 !! 0) = LT
            | otherwise = EQ

-- Task 6
task6_line = ["Nume","Punctaj interviu","Punctaj scris","Diferenta"]
-- mapez tabelul fara prima linie si il transform in tabelul cerut
-- iar apoi il sortez conform cerintei si adaug prima linie cu numele coloanelor
get_exam_diff_table :: Table -> Table
get_exam_diff_table table = task6_line : (sortBy cmp $ map modify $ tail table)
    where
        -- creez noul tabel prin calcularea punctajelor
        modify line = [name, interview, exam, difference]
            where 
                name = head line
                interview = printf "%.2f" (pct_int line)
                exam = printf "%.2f" (read (last line) :: Float)
                difference = diff (pct_int line) (floatValue $ last line)

        -- calculeaza punctajul de interviu
        pct_int :: [String] -> Float
        pct_int [] = 0.0
        pct_int list = (sum_string $ init $ tail list) / 4

        -- calculeaza diferenta dintre interviu si examen (mereu pozitiva)
        diff :: Float -> Float -> String
        diff n1 n2
            | n1 > n2 = printf "%.2f" (n1 - n2)
            | otherwise = printf "%.2f" (n2 - n1)

        -- sorteaza studentii dupa diferenta si apoi alfabetic
        cmp :: [String] -> [String] -> Ordering
        cmp s1 s2
            -- verificarea pentru sortarea notelor ascendent
            | (s1 !! 3) > (s2 !! 3) = GT
            | (s1 !! 3) < (s2 !! 3) = LT
            -- notele sunt egale deci sortam alfabetic
            | (s1 !! 0) > (s2 !! 0) = GT
            | (s1 !! 0) < (s2 !! 0) = LT
            | otherwise = EQ


{-
    TASK SET 2
-}
-- functie de split dupa un anumit Char
split_by :: Char -> String -> [String]
split_by del = foldr op []
    where
        op x [] = [[x]]
        op x (y:ys)
            | x /= del = (x:y):ys
            | otherwise = []:(y:ys)

-- functie citire CSV
read_csv :: CSV -> Table
read_csv csv = map changelast $ map (split_by ',') $ split_by '\n' csv
    where 
        changelast :: [String] -> [String]
        changelast [] = []
        changelast str
            | last (last str) == ',' = init str ++ [init (last str), ""]
            | otherwise = str

-- functie scriere CSV
write_csv :: Table -> CSV
write_csv table = intercalate "\n" $ map (intercalate ",") table

-- Task 1
-- folosesc transpose sa inversez lista pentru a obtine coloanele, iar apoi le parcurg pe
-- fiecare si caut coloana buna, pe celelalte le inlocuiesc cu [], iar la sfarsit filtrez si concatenez
as_list :: String -> Table -> [String]
as_list str table = concat $ filter (\x -> length x > 0) $ map (get_col str) $ transpose table
    where 
        get_col :: String -> [String] -> [String]
        get_col "" _ = []
        get_col str col
            | str == head col = tail col
            | otherwise = []

-- Task 2
-- sortez restul tabelului fara prima linie, iar apoi o adaug la sfarsit
tsort :: String -> Table -> Table
tsort str table = (head table) : sortBy cmp (tail table)
    where 
        -- numarul coloanei dupa care se sorteaza
        col_nr = get_col str $ head table

        cmp :: [String] -> [String] -> Ordering
        cmp s1 s2
            | val1 /= Nothing && val2 /= Nothing && val1 > val2 = GT
            | val1 /= Nothing && val2 /= Nothing && val1 < val2 = LT
            -- verificarea pentru sortarea ascendenta
            | (s1 !! col_nr) > (s2 !! col_nr) = GT
            | (s1 !! col_nr) < (s2 !! col_nr) = LT
            -- egale deci sortam alfabetic
            | (s1 !! 0) > (s2 !! 0) = GT
            | (s1 !! 0) < (s2 !! 0) = LT
            | otherwise = EQ
            where
                val1 = readMaybe (s1 !! col_nr) :: Maybe Int
                val2 = readMaybe (s2 !! col_nr) :: Maybe Int

        -- returneaza pozitia elementului in lista
        get_col :: String -> [String] -> Int
        get_col str col
            | str == head col = 0
            | otherwise = 1 + (get_col str $ tail col)

-- Task 3
vmap :: (Value -> Value) -> Table -> Table
vmap _ [] = []
vmap func (x:xs) = map func x : vmap func xs 

-- Task 4
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap _ _ [] = []
rmap func names table = names : map func (tail table)

-- sar peste primele 2 valorile din linie, calculez suma celorlalte si o fac String
get_hw_grade_total :: Row -> Row
get_hw_grade_total [] = []
get_hw_grade_total row = [head row, printf "%.2f" $ sum_string $ tail $ tail row]

-- Task 5
vunion :: Table -> Table -> Table
vunion table1 table2
    | (head table1) == (head table2) = table1 ++ (tail table2)
    | otherwise = table1

-- Task 6
-- adaug al 2-lea tabel la primul folosind transpose, iar apoi completez liniile ramase
hunion :: Table -> Table -> Table
hunion table1 table2 = modify $ transpose $ (transpose table1) ++ (transpose table2)
    where
        modify :: Table -> Table
        modify table = map (change_row $ length $ head table) table

        -- verifica ficare rand daca are toate coloanele completate, daca nu atunci pun ""
        change_row :: Int -> Row -> Row
        change_row col_nr row
            | length row < col_nr = row ++ (replicate (col_nr - length row) "")
            | otherwise = row

-- Task 7
-- returneaza valorile  entry-ului respectiv din tabel, daca exista
find_entry :: Value -> Table -> Row
find_entry _ [] = []
find_entry str table = concat $ map modify table
    where
        modify row
            | str == head row = row
            | otherwise = []

-- prima pozitie a unui String in lista
get_first_pos :: String -> [String] -> Int
get_first_pos str arr
    | str == head arr = 0
    | otherwise = 1 + (get_first_pos str $ tail arr)

-- ultima pozitie a unui String in lista
get_last_pos :: String -> [String] -> Int
get_last_pos str arr = (length arr) - 1 - (get_pos str arr)
    where 
        get_pos str arr
            | str == last arr = 1
            | otherwise = 1 + (get_last_pos str $ init arr)

duplicate_col :: Table -> Table
duplicate_col table = map modify $ transpose table
    where
        col_names = head table
        duplicate row
            | (get_first_pos (head row) col_names) /= (get_last_pos (head row) col_names) = True
            | otherwise = False

        modify :: Row -> Row
        modify row
            | duplicate row = table!!(get_last_pos (head row) col_names)
            | otherwise = row

tjoin :: String -> Table -> Table -> Table
tjoin str table1 table2 = map modify table1
    where
        col_pos = get_col str (head table2)

        modify :: Row -> Row
        modify row
            | length vals > 0 = row ++ (change_row col_pos vals)
            | otherwise = row ++ (replicate ((length $ (head table2)) - 1) "")
            where 
                vals = find_entry (head row) table2

        -- returneaza lista fara elementul de pe pozitia data
        change_row :: Int -> Row -> Row
        change_row _ [] = []
        change_row 0 (x:xs) = xs
        change_row n (x:xs) = x : change_row (n-1) xs

        -- returneaza indexul pozitiei string-ului in Row
        get_col :: String -> Row -> Int
        get_col str col
            | str == head col = 0
            | otherwise = 1 + (get_col str $ tail col)

-- Task 8
-- am folosit list comprehensions
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian func names table1 table2 = names : [(func x y) | x<-(tail table1), y<-(tail table2)]

-- Task 9
-- folosesc transpose pentru a gasi liniile ce trebuie pastrate, restul le elimin
projection :: [String] -> Table -> Table
projection str table = transpose $ map modify $ transpose table
    where
        modify :: Row -> Row
        modify row
            | elem (head row) str = row
            | otherwise = []

{-
    TASK SET 3
-}
data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query
 
type EdgeOp = Row -> Row -> Maybe Value
data QResult = CSV CSV | Table Table | List [String]

-- Task 3.1 & 3.2
instance Show QResult where
    show (Table table) = write_csv table
    show (List list) = show list
    show (CSV csv) = show csv

class Eval a where
    eval :: a -> QResult

-- transform un query intr-un table
get_table :: Query -> Table
get_table query = read_csv $ show $ eval query

instance Eval Query where
    eval (FromCSV csv) = Table $ read_csv csv
    eval (ToCSV query) = CSV $ show $ eval query
    eval (AsList colname query) = List $ as_list colname $ get_table query
    eval (Sort colname query) = Table $ tsort colname $ get_table query
    eval (ValueMap op query) = Table $ vmap op $ get_table query
    eval (RowMap op colnames query) = Table $ rmap op colnames $ get_table query
    eval (VUnion query1 query2) = Table $ vunion (get_table query1) (get_table query2)
    eval (HUnion query1 query2) = Table $ hunion (get_table query1) (get_table query2)
    eval (TableJoin colname query1 query2) = Table $ tjoin colname (get_table query1) (get_table query2)
    eval (Cartesian op colnames query1 query2) = Table $ cartesian op colnames (get_table query1) (get_table query2)
    eval (Projection colnames query) = Table $ projection colnames $ get_table query
    eval (Filter cond query) = Table $ filter_table cond $ get_table query
    eval (Graph edgeop query) = Table $ graph_query edgeop $ get_table query

-- Task 3.3 & 3.4
data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

-- pozitia coloanei in tabel
get_col_pos :: String -> [String] -> Int
get_col_pos str arr
    | str == head arr = 0
    | otherwise = 1 + (get_col_pos str $ tail arr)

instance FEval Float where
    feval cols (Eq condCol condVal) row = condVal == read (row !! colpos)
        where 
            colpos = get_col_pos condCol cols
    feval cols (Lt condCol condVal) row = condVal > read (row !! colpos)
        where 
            colpos = get_col_pos condCol cols
    feval cols (Gt condCol condVal) row = condVal < read (row !! colpos)
        where 
            colpos = get_col_pos condCol cols
    feval cols (In condCol condVal) row = (row !! colpos) /= "" && elem (read (row !! colpos)) condVal
        where 
            colpos = get_col_pos condCol cols
    feval cols (FNot cond) row
        | feval cols cond row == True = False
        | otherwise = True
    feval cols (FieldEq colname1 colname2) row = (row !! colpos1) == (row !! colpos2)
        where 
            colpos1 = get_col_pos colname1 cols
            colpos2 = get_col_pos colname2 cols

instance FEval String where
    feval cols (Eq condCol condVal) row = condVal == row !! colpos
        where 
            colpos = get_col_pos condCol cols
    feval cols (Lt condCol condVal) row = condVal > row !! colpos
        where 
            colpos = get_col_pos condCol cols
    feval cols (Gt condCol condVal) row = condVal < row !! colpos
        where 
            colpos = get_col_pos condCol cols
    feval cols (In condCol condVal) row = elem (row !! colpos) condVal
        where 
            colpos = get_col_pos condCol cols
    feval cols (FNot cond) row
        | feval cols cond row == True = False
        | otherwise = True
    feval cols (FieldEq colname1 colname2) row = (row !! colpos1) == (row !! colpos2)
        where 
            colpos1 = get_col_pos colname1 cols
            colpos2 = get_col_pos colname2 cols

-- functia apelata ce filtreaza tabelul
filter_table :: FEval a => FilterCondition a -> Table -> Table
filter_table cond table = head table : filter (feval (head table) cond) (tail table)

-- Task 3.5
-- transforma in valoare, verificarea pentru Nothing se face in functie inainte de apelare
just_to_val :: Maybe a -> a
just_to_val (Just a) = a

graph_query :: EdgeOp -> Table -> Table
graph_query op table = ["From", "To", "Value"] : (filter (\x -> length x > 0) $ map (op_row op) combinations)
    where
        -- combinatiile posibile si in ordine (pozitia i doar cu cele de la i+1, i+2,...)
        combinations = [(x,y) | (x:rest) <- tails (tail table), y <- rest]

        op_row op (row1, row2)
            | op row1 row2 == Nothing = []
            | row1 !! 0 < row2 !! 0 = [row1 !! 0, row2 !! 0, just_to_val $ op row1 row2]
            | otherwise = [row2 !! 0, row1 !! 0, just_to_val $ op row1 row2]


-- Task 3.6
-- numara intrebarile cu acelasi raspuns
nr_same :: [String] -> [String] -> Integer
nr_same [] _ = 0
nr_same _ [] = 0
nr_same (x:xs) (y:ys)
    | x == y = 1 + nr_same xs ys
    | otherwise = nr_same xs ys

-- compara 2 randuri, verifica daca exista valoare pe coloana,
-- si daca da atunci verifica conditia >=5
edge_op3 row1 row2
            | row1 !! 0 == "" = Nothing
            | row2 !! 0 == "" = Nothing
            | nr_same row1 row2 >= 5 = Just (show $ nr_same row1 row2)
            | otherwise = Nothing

similarities_query = Sort "Value" $ Graph edge_op3 $ FromCSV lecture_grades_csv

{-
    TASK SET 4
-}
-- numarul de caractere diferite dintre cele 2 string-uri
compare_strings :: String -> String -> Int
compare_strings "" str = length str
compare_strings str "" = length str
compare_strings (x:xs) (y:ys)
    | x /= y = 1 + compare_strings xs ys
    | otherwise = compare_strings xs ys

-- cel mai bun match din lista data
compare_string_table :: String -> Int -> [String] -> [String]
compare_string_table str colpos list = head $ tsort "Diff" $ ["Diff", "Nume"] : map modify list
    where
        modify elem = [show $ compare_strings str elem, elem]

-- inlatura valorile care se repeta in cele 2 liste
remove_same :: Row -> Row -> Row
remove_same [] row = row
remove_same _ [] = []
remove_same row (x:xs)
    | x `elem` row = remove_same row xs
    | otherwise = x : remove_same row xs

correct_table :: String -> CSV -> CSV -> CSV
correct_table col_name email_csv hw_csv = write_csv $ head email_table : (map modify $ tail $ email_table)
    where
        email_table = read_csv email_csv
        hw_table = read_csv hw_csv

        colpos1 = get_col_pos col_name (head email_table)
        colpos2 = get_col_pos col_name (head hw_table)
        -- valorile de pe coloanele cautate
        values1 = tail $ (transpose email_table)!!colpos1
        values2 = tail $ (transpose hw_table)!!colpos2
        -- valorile din cele 2 tabele care nu se regasesc in ambele
        changed_values1 = remove_same values2 values1
        changed_values2 = remove_same values1 values2

        modify :: Row -> Row
        modify line
            | find_entry (line!!colpos1) hw_table /= [] = line
            | otherwise = [changed_values2!!new_pos, line!!(colpos1 + 1)]
                where 
                    new_pos = get_col_pos (line!!colpos1) changed_values1

-- punctaj teme
get_hw_grade :: [String] -> String
get_hw_grade [] = ""
get_hw_grade cols = printf "%.2f" $ sum_string (tail cols)

-- punctaj curs
get_lecture_grade :: [String] -> String
get_lecture_grade [] = ""
get_lecture_grade cols = printf "%.2f" $ (sum / nr)
    where
        sum = 2 * (sum_string $ tail cols)
        nr = fromIntegral (length $ tail cols) :: Float

-- punctaj examen
get_exam_grade :: [String] -> String
get_exam_grade [] = ""
get_exam_grade list = printf "%.2f" $ (sum + exam)
    where
        sum = (sum_string $ init $ tail list) / 4
        exam = floatValue $ last list

-- punctaj total
get_total :: String -> String -> String -> String
get_total hw_gr lecture_gr exam_gr
    | hw + lecture < 2.5 = "4.00"
    | exam < 2.5 = "4.00"
    | otherwise = printf "%.2f" $ (min (hw + lecture) 5) + exam
        where
            hw = floatValue hw_gr
            lecture = floatValue lecture_gr
            exam = floatValue exam_gr

head_col = ["Nume", "Punctaj Teme", "Punctaj Curs", "Punctaj Exam", "Punctaj Total"]
grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades email_csv hw_csv exam_csv lecture_csv = write_csv $ (tsort "Nume" $ head_col : map modify table)
    where
        table = tail $ read_csv $ correct_table "Nume" email_csv hw_csv
        hw_table = tail $ read_csv hw_csv
        exam_table = tail $ read_csv exam_csv
        lecture_table = tail $ read_csv lecture_csv

        modify :: Row -> Row
        modify line = [line!!0, hw_grade, lecture_grade, exam_grade, total]
                where
                    hw_grade = get_hw_grade $ find_entry (line!!0) hw_table
                    lecture_grade = get_lecture_grade $ find_entry (line!!1) lecture_table
                    exam_grade = get_exam_grade $ find_entry (line!!0) exam_table
                    total = get_total hw_grade lecture_grade exam_grade


