module UFO -- Unified Foreign Object
import Data.SortedMap

%foreign "browser:lambda: (_, a) => console.debug(a)"
prim__consoleDebug : a -> PrimIO ()

export
debug : HasIO io => a -> io ()
debug = primIO . prim__consoleDebug

export
DEBUG : String -> a -> a
DEBUG msg a = unsafePerformIO (do
    debug msg
    pure a
    )

export
Cast Bool Double where
    cast True = 1.0
    cast False = 0.0

export
Cast Double Bool where
    cast 1.0 = True
    cast _ = False

-- data Dictionary : SortedMap String ty -> Type where
--     DictNil : SortedMap String () -> Dictionary empty
--     Dictate : Dictionary smap -> Dictionary empty

mutual
    -- depends on UFO, marshalECMAType
    public export
    data ECMAType : Type where
        ECMAString : ECMAType 
        ECMANumber : ECMAType 
        ECMABoolean : ECMAType
        ECMAArray : ECMAType -> ECMAType
        -- ECMAObject : List UFO -> ECMAType
        -- ECMAUnion : List ECMAType -> ECMAType
        -- ECMAStrictly : (esty : ECMAType ** List (marshalECMAType esty)) -> ECMAType -- only one of following values

    -- depends on ECMAType
    public export
    data UFO : Type where
        Named : ECMAType -> String -> UFO

    -- depends on ECMAType
    public export
    marshalECMAType : ECMAType -> Type
    marshalECMAType ECMAString = String
    marshalECMAType ECMANumber = Double
    marshalECMAType ECMABoolean = Bool
    marshalECMAType (ECMAArray esty) = List (marshalECMAType esty)
    -- marshalECMAType (ECMAObject Nil) = () -- hmm
    -- marshalECMAType (ECMAObject (item::items)) = SortedMap String (ty : Type ** ty) -- but not quite
    -- marshalECMAType (ECMAUnion Nil) = () -- technically invalid but hm
    -- marshalECMAType (ECMAUnion (item::Nil)) = marshalECMAType item
    -- marshalECMAType (ECMAUnion (item::items)) = Either (marshalECMAType item) (marshalECMAType (ECMAUnion items))
    -- marshalECMAType (ECMAStrictly (esty**_)) = marshalECMAType esty

public export
marshal : UFO -> Type
marshal (Named (ty) _) = marshalECMAType ty

myStr : UFO
myStr = ECMAString `Named` "string"

-- string = "haha"

myArr : UFO
myArr = ECMAArray ECMAString `Named` "strings"

-- strings = ["abra", "cadabra"]

-- myNumberObject : UFO
-- myNumberObject = ECMAObject [
--                     ECMANumber `Named` "one",
--                     ECMANumber `Named` "two",
--                     ECMANumber `Named` "three"
--                     ] `Named` "obj"

-- obj = {one=1, two=2, three=3}

-- myOptionalThing : UFO
-- myOptionalThing = ECMAUnion [
--                     ECMANumber,
--                     ECMAArray ECMANumber
--                     ] `Named` "numOrArr"

-- numOrArr = 1
-- numOrArr = [1,2]

-- myStrictThing : UFO
-- myStrictThing = ECMAStrictly (ECMAString ** ["please", "thank you"]) `Named` "magicword"

-- magicword = "please"
-- magicword = "thank you"

access : {default Nil args: List String} -> {default id act: String -> String} -> {default False offset: Bool} -> String
access {args} {act} {offset} = 
    "javascript:lambda: (" ++ offset_str ++ "p,n" ++ args_str ++ ") =>" ++ act "p[n]"
    where
        offset_str : String
        offset_str = if offset then "_," else ""
        args_str : String
        args_str = foldr (\elem => \acc => "," ++ elem ++ acc) "" args

parameters (parent : AnyPtr, name : String)
    %foreign access
    prim__get_str : PrimIO String
    %foreign access {args=["x"], act=(++"=x")}
    prim__put_str : String -> PrimIO ()

    %foreign access
    prim__get_num : PrimIO Double
    %foreign access {args=["x"], act=(++"=x")}
    prim__put_num : Double -> PrimIO ()

    %foreign access {act=(\obj => "__prim_js2idris_array(" ++ obj ++ ")")} -- TODO: check if this behaves correctly
    prim__get_arr : PrimIO (List ty)
    %foreign access {args=["_,arr"], act=(++"=__prim_idris2js_array(arr)")} -- TODO: check if this behaves correctly
    prim__put_arr : List ty -> PrimIO ()

export
get : HasIO io => (parent : AnyPtr) -> (ufo : UFO) -> io (marshal ufo)
get parent (ECMAString `Named` name) = primIO $ prim__get_str parent name 
get parent (ECMANumber `Named` name) = primIO $ prim__get_num parent name 
get parent (ECMABoolean `Named` name) = map cast $ primIO $ prim__get_num parent name 
get parent ((ECMAArray esty) `Named` name) = primIO $ prim__get_arr parent name 

export
put : HasIO io => (parent : AnyPtr) -> (ufo : UFO) -> (marshal ufo) -> io ()
put parent (ECMAString `Named` name) content = primIO $ prim__put_str parent name content
put parent (ECMANumber `Named` name) content = primIO $ prim__put_num parent name content
put parent (ECMABoolean `Named` name) content = primIO $ prim__put_num parent name (cast content)
put parent ((ECMAArray esty) `Named` name) content = primIO $ prim__put_arr parent name content