

user <- c("pedro", "Juan", "Diego", "marino", "Ana")
con <- dbConnect(RSQLITE::SQLITE() , ":memory:")

SELECT * FROM users
where users IN names