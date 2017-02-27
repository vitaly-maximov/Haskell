module Tasks where

data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 { lastName = lastName p1 }

abbrFirstName :: Person -> Person
abbrFirstName person @ Person { firstName = name } | length name > 1 = person { firstName = [head name, '.'] }
abbrFirstName person = person