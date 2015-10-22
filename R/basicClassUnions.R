setClassUnion("characterOrMissing", c("character", "missing"))
setClassUnion(name = "listOrCharacter", members = c("list", "character"))
setClassUnion("logicalOrMissing", c("logical", "missing"))
setClassUnion("numericOrCharacter", c("numeric", "character"))