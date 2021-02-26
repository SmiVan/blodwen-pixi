module UFO -- Unified Foreign Object
import Data.Maybe -- for fromMaybe
-- import Data.List

%foreign "browser:lambda: (_, a) => console.debug(a)"
prim__consoleDebug : a -> PrimIO ()

%foreign "javascript:lambda: () => globalThis"
prim__global : PrimIO AnyPtr
export
global : HasIO io => io AnyPtr
global = primIO $ prim__global

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

public export
interface Cast a b => Cast b (Maybe a) => Domain a b where -- nothing

LabelledTuple : (types : List (String, Type)) -> Type
LabelledTuple Nil = ()
LabelledTuple ((str, ty) :: Nil) = ty
LabelledTuple ((str, ty) :: next) = Pair ty (LabelledTuple next)

----/!\ Mutually depending types: UFO, ECMAType, marshalECMAType /!\----
-- A special thank you to Denis Buzdalov and Edwin Brady for explaining this.

data UFO : Type -- forward declare
data ECMAType : Type -- forward declare
public export
marshalECMAType : ECMAType -> Type -- depends on ECMAType

-- depends on UFO, marshalECMAType
public export
data ECMAType : Type where
    ECMAString : ECMAType
    ECMANumber : ECMAType
    ECMABoolean : ECMAType
    ECMAArray : ECMAType -> ECMAType --  A kind of list or array
    ECMAObject : List UFO -> ECMAType -- A kind of tuple or struct
    Or : ECMAType -> ECMAType -> ECMAType -- A kind of either or union
    In : (esty : ECMAType) -> (ty : Type) -> Domain (ty) (marshalECMAType esty) => ECMAType -- values are restricted to the domain

-- depends on ECMAType
public export
data UFO : Type where
    Named : ECMAType -> String -> UFO

-- public export
-- marshalECMAType : ECMAType -> Type
marshalECMAType ECMAString = String
marshalECMAType ECMANumber = Double
marshalECMAType ECMABoolean = Bool
marshalECMAType (ECMAArray esty) = List (marshalECMAType esty)
marshalECMAType (ECMAObject index) = LabelledTuple (map (\(Named esty name) => (name, marshalECMAType esty)) index)
marshalECMAType (Or esty1 esty2) = Either (marshalECMAType esty1) (marshalECMAType esty2)
marshalECMAType (In esty ty) = ty

----/!\ Mutually depending types end here. /!\----

public export
marshal : UFO -> Type
marshal (Named (ty) _) = marshalECMAType ty

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

    %foreign access {act=(\obj => "__prim_js2idris_array(" ++ obj ++ ")")}
    prim__get_arr : PrimIO (List ty)
    %foreign access {args=["arr"], act=(++"=__prim_idris2js_array(arr)"), offset=True}
    prim__put_arr : List ty -> PrimIO ()

export
get : HasIO io => (parent : AnyPtr) -> (ufo : UFO) -> io (marshal ufo)
get parent (ECMAString `Named` name) = primIO $ prim__get_str parent name 
get parent (ECMANumber `Named` name) = primIO $ prim__get_num parent name 
get parent (ECMABoolean `Named` name) = map cast $ primIO $ prim__get_num parent name 
get parent ((ECMAArray esty) `Named` name) = primIO $ prim__get_arr parent name 
get parent ((esty `In` ty) `Named` name) = do
    raw <- get parent (esty `Named` name)
    case (cast raw) of
        Just val => pure val
        Nothing => do
            debug $ "/!\\ UFO GET FAILED: Value of variable " ++ name ++ " is outside its domain:"
            debug raw 
            debug $ "/!\\ Going off the rails!"
            believe_me () -- TODO: Implement different input/output marshalling later?
get parent (ECMAObject Nil `Named` _) = pure () -- skip pointless operation
get parent (ECMAObject memberList `Named` name) = ?get_obj
get parent ((esty1 `Or` esty2) `Named` name) = ?get_union -- This may require determination of type at javascript level!

export
put : HasIO io => (parent : AnyPtr) -> (ufo : UFO) -> (marshal ufo) -> io ()
put parent (ECMAString `Named` name) content = primIO $ prim__put_str parent name content
put parent (ECMANumber `Named` name) content = primIO $ prim__put_num parent name content
put parent (ECMABoolean `Named` name) content = primIO $ prim__put_num parent name (cast content)
put parent ((ECMAArray esty) `Named` name) content = primIO $ prim__put_arr parent name content
put parent ((esty `In` ty) `Named` name) content = put parent (esty `Named` name) (cast content)
put parent (ECMAObject Nil `Named` _) _ = pure () -- skip pointless operation
put parent (ECMAObject memberList `Named` name) content = ?put_obj
put parent ((esty1 `Or` esty2) `Named` name) content = ?put_union


---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

namespace SCRATCHPAD
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

    -- obj = {one:1, two:2, three:3}

    obj : (typeOf : (String -> Type) ** List (item : String ** typeOf item))
    obj = (types ** [("one" ** 1), ("two" ** 2), ("three" ** 3), ("truth" ** True)])
        where 
            types : String -> Type
            types "one" = Double
            types "two" = Double
            types "three" = Double
            types "truth" = Bool
            types _ = Double -- not technically correct

    obj2 : List (type : Type ** (String, type))
    obj2 = [(Double ** ("one", 1)), (Double ** ("two", 2)), (Double ** ("three", 3)), (Bool ** ("truth", True))] -- not quite right either

    obj3 : (Double, Double, Double)
    obj3 = (1, 2, 3) -- hmm

    obj4 : LabelledTuple [("one", Double), ("two", Double), ("three", Double)]
    obj4 = (1, 2, 3) -- good enough

    -- Probably the most optimal version for the moment.

    myOptionalThing : UFO
    myOptionalThing = (ECMANumber `Or` (ECMAArray ECMANumber)) `Named` "numOrArr"

    -- numOrArr = 1
    -- numOrArr = [1,2]

    data MyDomain = Please
                  | ThankYou
    
    Cast MyDomain String where
        cast Please = "please"
        cast ThankYou = "thank you"

    Cast String (Maybe MyDomain) where
        cast "please" = Just Please
        cast "thank you" = Just ThankYou
        cast _ = Nothing

    Domain MyDomain String where -- nothing

    magicword : MyDomain
    magicword = Please

    myStrictThing : UFO
    myStrictThing = (ECMAString `In` MyDomain) `Named` "magicword"

    -- magicword = "please"
    -- magicword = "thank you"