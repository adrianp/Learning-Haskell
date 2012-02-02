doubleMe x = x + x

--doubleUs x y = x * 2 + y * 2

doubleUs x y = doubleMe x + doubleMe y

--doubleSmallNumber x = if x > 100
--                         then x
--                         else x*2

doubleSmallNumber x = if x > 100
                         then x
                         else doubleMe x

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | x <- xs]

removeNonUppercase st = [ c | c <- st, elem c ['A'..'Z']]

removeOddsInner xxs = [ [ x | x <- xs, even x ] | xs <- xxs]

