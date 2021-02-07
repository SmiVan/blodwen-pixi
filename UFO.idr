module UFO -- Unified Foreign Object

mutual
    -- depends on UFO, marshalECMAType
    data ECMAType = ECMAString
                | ECMANumber
                | ECMAArray ECMAType
                | ECMAObject (List UFO)
                | ECMAUnion (List ECMAType)
                | ECMAStrictly (esty : ECMAType ** List (marshalECMAType esty)) -- only one of following values

    -- depends on ECMAType
    data UFO : Type where
        Named : ECMAType -> String -> UFO

    -- depends on ECMAType
    marshalECMAType : ECMAType -> Type
    marshalECMAType ECMAString = String
    marshalECMAType ECMANumber = Double
    marshalECMAType (ECMAArray esty) = List (marshalECMAType esty)
    marshalECMAType (ECMAObject list) = ?marshal_obj
    marshalECMAType (ECMAUnion list) = ?marshal_union
    marshalECMAType (ECMAStrictly (esty**_)) = marshalECMAType esty

marshal : UFO -> Type
marshal (Named (ty) _) = marshalECMAType ty

myStr : UFO
myStr = ECMAString `Named` "string"

-- string = "haha"

myArr : UFO
myArr = ECMAArray ECMAString `Named` "strings"

-- strings = ["abra", "cadabra"]

myNumberObject : UFO
myNumberObject = ECMAObject [
                    ECMANumber `Named` "one",
                    ECMANumber `Named` "two",
                    ECMANumber `Named` "three"
                    ] `Named` "obj"

-- obj = {one=1, two=2, three=3}

myOptionalThing : UFO
myOptionalThing = ECMAUnion [
                    ECMANumber,
                    ECMAArray ECMANumber
                    ] `Named` "numOrArr"

-- numOrArr = 1
-- numOrArr = [1,2]

myStrictThing : UFO
myStrictThing = ECMAStrictly (ECMAString ** ["please", "thank you"]) `Named` "magicword"

-- magicword = "please"
-- magicword = "thank you"

write : HasIO io => (parent : AnyPtr) -> (ufo : UFO) -> (marshal ufo) -> io ()
write = ?how_to_write

read : HasIO io => (parent : AnyPtr) -> (ufo : UFO) -> io (marshal ufo)
read = ?how_to_read