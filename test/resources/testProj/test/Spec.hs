import Lib
import Test.HUnit

task1Test =
  TestList
    [ TestCase (assertEqual "pred 3, " 2 (churchToInt (task1 three))),
      TestCase (assertEqual "pred 2, " 1 (churchToInt (task1 two))),
      TestCase (assertEqual "pred 1, " 0 (churchToInt (task1 one))),
      TestCase (assertEqual "pred 0, " 0 (churchToInt (task1 zero)))
    ]
  where
    zero = flip const
    one = id
    two = \s z -> s (s z)
    three = \s z -> s (s (s z))
    churchToInt c = (c (+ 1) 0) :: Int

task2Test =
  TestList
    [ TestCase (assertEqual "чётность 5" False (task2 5)),
      TestCase (assertEqual "нечётность 5" True (task2' 5)),
      TestCase (assertEqual "чётность 6" True (task2 6)),
      TestCase (assertEqual "нечётность 6" False (task2' 6)),
      TestCase (assertEqual "чётность 0" True (task2 0)),
      TestCase (assertEqual "нечётность 0" False (task2' 0)),
      TestCase (assertEqual "чётность -2" True (task2 (-2))),
      TestCase (assertEqual "нечётность -2" False (task2' (-2))),
      TestCase (assertEqual "чётность -3" False (task2 (-3))),
      TestCase (assertEqual "нечётность -3" True (task2' (-3)))
    ]

task3Test =
  TestList
    ((\x -> TestCase (assertEqual ("collatz #" ++ show x) 1 (task3 x))) <$> [1 .. 30])

task4Test =
  TestList
    [ TestCase (assertEqual "4 +??? 5, " 20 (4 +??? 5)),
      TestCase (assertEqual "5 +??? 4, " 9 (5 +??? 4)),
      TestCase (assertEqual "4 +??? 5 + 6 +??? 7 * 2, " 40 (4 +??? 5 + 6 +??? 7 * 2))
    ]

task5Test =
  TestList
    [ TestCase (assertEqual "битов в 0, " 1 (fst (task5 0))),
      TestCase (assertEqual "установленных битов в 0, " 0 (snd (task5 0))),
      TestCase (assertEqual "битов в 1, " 1 (fst (task5 1))),
      TestCase (assertEqual "установленных битов в 1, " 1 (snd (task5 1))),
      TestCase (assertEqual "битов в 2, " 2 (fst (task5 2))),
      TestCase (assertEqual "установленных битов в 2, " 1 (snd (task5 2))),
      TestCase (assertEqual "битов в 3, " 2 (fst (task5 3))),
      TestCase (assertEqual "установленных битов в 3, " 2 (snd (task5 3))),
      TestCase (assertEqual "битов в 4, " 3 (fst (task5 4))),
      TestCase (assertEqual "установленных битов в 4, " 1 (snd (task5 4))),
      TestCase (assertEqual "битов в 5, " 3 (fst (task5 5))),
      TestCase (assertEqual "установленных битов в 5, " 2 (snd (task5 5))),
      TestCase (assertEqual "битов в 6, " 3 (fst (task5 6))),
      TestCase (assertEqual "установленных битов в 6, " 2 (snd (task5 6))),
      TestCase (assertEqual "битов в 7, " 3 (fst (task5 7))),
      TestCase (assertEqual "установленных битов в 7, " 3 (snd (task5 7))),
      TestCase (assertEqual "битов в 8, " 4 (fst (task5 8))),
      TestCase (assertEqual "установленных битов в 8, " 1 (snd (task5 8))),
      TestCase (assertEqual "битов в -6, " 3 (fst (task5 (-6)))),
      TestCase (assertEqual "установленных битов в -6, " 2 (snd (task5 (-6)))),
      TestCase (assertEqual "битов в -7, " 3 (fst (task5 (-7)))),
      TestCase (assertEqual "установленных битов в -7, " 3 (snd (task5 (-7))))
    ]

task6Test =
  TestList
    [ TestCase (assertEqual "task6 на числах" (6, 10) (task6 (* 2) (+ 7) 3)),
      TestCase (assertEqual "task6 на строках" ("ccc", "3") (task6 (\x -> replicate x 'c') show 3)),
      TestCase (assertEqual "task6 на разных типах" ("3", 3) (task6 show id 3))
    ]

task7Test =
  TestList
    [ TestCase (assertEqual "task7 на числах" (1, 2, 3) (task7 (+) (* 2) 1)),
      TestCase (assertEqual "task7 на строках" ("a", "zaz", "azaz") (task7 (++) (\x -> "z" ++ x ++ "z") "a"))
    ]

task8Test =
  TestList
    [ TestCase (assertEqual "6!!" 48 (task8 6)),
      TestCase (assertEqual "7!!" 105 (task8 7)),
      TestCase (assertEqual "1!!" 1 (task8 1)),
      TestCase (assertEqual "2!!" 2 (task8 2)),
      TestCase (assertEqual "3!!" 3 (task8 3)),
      TestCase (assertEqual "4!!" 8 (task8 4))
    ]

tests = TestList [TestList [], task1Test, task2Test, task3Test, task4Test, task5Test, task6Test, task7Test, task8Test]

main = runTestTT tests
