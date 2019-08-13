{-|
  Description: ブラックジャックの手札の合計点数を計算するmodule
-}
module Lib
    ( 
    -- * exportする関数や型，値コンストラクタの一覧
    -- | このmodule `Lib` を `import` した時，以下の関数・型・値コンストラクタのみが利用可能になる．
    -- | 型と値コンストラクタを利用者に公開．
    Card(A, J, Q, K)
    -- , cardForTestData
    , deck        -- デッキ
    , heartSuit   -- ❤️のデッキ
    , diaSuit     -- ♦️のデッキ
    , cloverSuit  -- ♣️のデッキ
    , spadeSuit   -- ♠️のデッキ
    , sumHand     -- 手札のカードの合計を計算する関数
    ) where
    
{-|
  カードを表現する `Card` 型
-}
data Card = 
  A | N Int | J | Q | K deriving (Eq, Show)

{-|
  手札（ `[Card]` ： `Card` のリスト）からブラックジャックのルール上もっとも最適な得点（ `Int` ）を計算して返す関数
-}
sumHand :: [Card] -> Int
sumHand cards =
  let possiblePoints = map toPoint cards
      scoreCanditates = foldl plusEach [0] possiblePoints
      noBust = filter (<= 21) scoreCanditates
  in
    if null noBust
      then head scoreCanditates
      else maximum noBust

{-|
  カード（ `Card` ）をルール上取りうる得点のリスト（ `[Int]` ）に変換する関数
-}
toPoint :: Card -> [Int]
toPoint A     = [1, 11] -- Aのカードは1か11として評価
toPoint (N n) = [n]     -- 2~9の数字のカードは書かれている数字の通りに評価
toPoint _     = [10]    -- その他のカード（JQK）は10として評価

{-|
  得点列（ `[[Int]]` ）を引数に，ルール上ありえる得点を全て計算して返す関数
-}
plusEach :: [Int] -> [Int] -> [Int]
plusEach list1 list2 = 
  concatMap (\element1 ->
    map (\element2 -> element1 + element2) list2
  ) list1
  
{-|
  カードの山を表す `deck` 型
-}
deck :: [Card]
deck = heartSuit ++ diaSuit ++ cloverSuit ++ spadeSuit

suit, heartSuit, diaSuit, cloverSuit, spadeSuit :: [Card]
suit = [A] ++ map N [2..10] ++ [J, Q, K]

heartSuit = suit
diaSuit = suit
cloverSuit = suit
spadeSuit = suit
